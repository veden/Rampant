if (chunkProcessorG) then
    return chunkProcessorG
end
local chunkProcessor = {}

-- imports

local chunkUtils = require("ChunkUtils")
local constants = require("Constants")

-- constants

local CHUNK_SIZE = constants.CHUNK_SIZE

-- imported functions

local registerEnemyBaseStructure = chunkUtils.registerEnemyBaseStructure
local unregisterEnemyBaseStructure = chunkUtils.unregisterEnemyBaseStructure

local createChunk = chunkUtils.createChunk
local initialScan = chunkUtils.initialScan
local chunkPassScan = chunkUtils.chunkPassScan

local next = next
local table_size = table_size

local tRemove = table.remove
local tInsert = table.insert
local mCeil = math.ceil

-- module code

local function findInsertionPoint(processQueue, chunk)
    local low = 1
    local high = #processQueue
    local pivot
    while (low <= high) do
        pivot = mCeil((low + high) * 0.5)
        local pivotChunk = processQueue[pivot]
        if (pivotChunk.dOrigin > chunk.dOrigin) then
            high = pivot - 1
        elseif (pivotChunk.dOrigin <= chunk.dOrigin) then
            low = pivot + 1
        end
    end
    return low
end

local function removeProcessQueueChunk(processQueue, chunk)
    local insertionPoint = findInsertionPoint(processQueue, chunk)
    for i=insertionPoint,1,-1 do
        if (processQueue[i] == chunk) then
            tRemove(processQueue, i)
        end
    end
end

function chunkProcessor.processPendingChunks(map, tick, flush)
    local processQueue = map.processQueue
    local pendingChunks = map.pendingChunks

    local area = map.universe.area

    local topOffset = area[1]
    local bottomOffset = area[2]

    local event = map.chunkProcessorIterator
    if not event then
        event = next(pendingChunks, nil)
    end
    local endCount = 2
    if flush then
        endCount = table_size(pendingChunks)
        event = next(pendingChunks, nil)
    end
    for _=1,endCount do
        if not event then
            map.chunkProcessorIterator = nil
            if (table_size(pendingChunks) == 0) then
                -- this is needed as the next command remembers the max length a table has been
                map.pendingChunks = {}
            end
            break
        else
            if not flush and (event.tick > tick) then
                map.chunkProcessorIterator = event
                return
            end
            local topLeft = event.area.left_top
            local x = topLeft.x
            local y = topLeft.y

            topOffset[1] = x
            topOffset[2] = y
            bottomOffset[1] = x + CHUNK_SIZE
            bottomOffset[2] = y + CHUNK_SIZE

            if not map[x] then
                map[x] = {}
            end

            if map[x][y] then
                local chunk = initialScan(map[x][y], map, tick)
                if (chunk == -1) then
                    removeProcessQueueChunk(processQueue, map[x][y])
                    map[x][y] = nil
                end
            else
                local chunk = createChunk(x, y)
                map[x][y] = chunk
                chunk = initialScan(chunk, map, tick)
                if (chunk ~= -1) then
                    map[x][y] = chunk
                    tInsert(
                        processQueue,
                        findInsertionPoint(processQueue, chunk),
                        chunk
                    )
                else
                    map[x][y] = nil
                end
            end

            local newEvent = next(pendingChunks, event)
            pendingChunks[event] = nil
            event = newEvent
        end
    end
    map.chunkProcessorIterator = event
end

function chunkProcessor.processPendingUpgrades(map, tick)
    local pendingUpgrades = map.pendingUpgrades
    local entity = map.pendingUpgradeIterator
    local entityData
    if not entity then
        entity, entityData = next(pendingUpgrades, nil)
    else
        entityData = pendingUpgrades[entity]
    end
    if entity then
        if entity.valid then
            map.pendingUpgradeIterator = next(pendingUpgrades, entity)
            pendingUpgrades[entity] = nil
            local universe = map.universe
            local query = universe.upgradeEntityQuery
            query.position = entityData.position or entity.position
            query.name = entityData.name
            local surface = entity.surface
            unregisterEnemyBaseStructure(map, entity)
            entity.destroy()
            if remote.interfaces["kr-creep"] then
                remote.call("kr-creep", "spawn_creep_at_position", surface, query.position)
            end
            local createdEntity = surface.create_entity(query)
            if createdEntity and createdEntity.valid then
                registerEnemyBaseStructure(map, createdEntity, tick, entityData.base)
            end
        else
            map.pendingUpgradeIterator = next(pendingUpgrades, entity)
            pendingUpgrades[entity] = nil
        end
    end
end

function chunkProcessor.processScanChunks(map)
    local area = map.universe.area

    local topOffset = area[1]
    local bottomOffset = area[2]

    local removals = map.chunkRemovals

    local chunkCount = 0

    local chunkToPassScan = map.chunkToPassScan

    for preScanChunk in pairs(chunkToPassScan) do
        local x = preScanChunk.x
        local y = preScanChunk.y

        topOffset[1] = x
        topOffset[2] = y
        bottomOffset[1] = x + CHUNK_SIZE
        bottomOffset[2] = y + CHUNK_SIZE

        if (chunkPassScan(preScanChunk, map) == -1) then
            map[x][y] = nil

            chunkCount = chunkCount + 1
            removals[chunkCount] = preScanChunk
        end

        chunkToPassScan[preScanChunk] = nil
    end

    if (chunkCount > 0) then
        local processQueue = map.processQueue
        for ri=chunkCount,1,-1 do
            removeProcessQueueChunk(processQueue, removals[ri])
        end
    end
end

chunkProcessorG = chunkProcessor
return chunkProcessor
