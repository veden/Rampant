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
        if (processQueue[i].id == chunk.id) then
            tRemove(processQueue, i)
        end
    end
end

function chunkProcessor.processPendingChunks(universe, tick, flush)
    local area = universe.area
    local topOffset = area[1]
    local bottomOffset = area[2]

    local pendingChunks = universe.pendingChunks
    local eventId = universe.chunkProcessorIterator
    local event

    if not eventId then
        eventId, event = next(pendingChunks, nil)
    else
        event = pendingChunks[eventId]
    end
    local endCount = 1
    if flush then
        endCount = table_size(pendingChunks)
        eventId, event = next(pendingChunks, nil)
    end
    for _=1,endCount do
        if not eventId then
            universe.chunkProcessorIterator = nil
            if (table_size(pendingChunks) == 0) then
                -- this is needed as the next command remembers the max length a table has been
                universe.pendingChunks = {}
            end
            break
        else
            if not flush and (event.tick > tick) then
                universe.chunkProcessorIterator = eventId
                return
            end
            local newEventId, newEvent = next(pendingChunks, eventId)
            pendingChunks[eventId] = nil
            local map = event.map
            if not map.surface.valid then
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
                local oldChunk = map[x][y]
                local chunk = initialScan(oldChunk, map, tick)
                if (chunk == -1) then
                    removeProcessQueueChunk(map.processQueue, oldChunk)
                    universe.chunkIdToChunk[oldChunk.id] = nil
                    map[x][y] = nil
                end
            else
                local initialChunk = createChunk(map, x, y)
                map[x][y] = initialChunk
                universe.chunkIdToChunk[initialChunk.id] = initialChunk
                local chunk = initialScan(initialChunk, map, tick)
                if (chunk ~= -1) then
                    tInsert(
                        map.processQueue,
                        findInsertionPoint(map.processQueue, chunk),
                        chunk
                    )
                else
                    universe.chunkIdToChunk[initialChunk.id] = nil
                    map[x][y] = nil
                end
            end

            eventId = newEventId
            event = newEvent
        end
    end
    universe.chunkProcessorIterator = eventId
end

function chunkProcessor.processPendingUpgrades(universe, tick)
    local entityId = universe.pendingUpgradeIterator
    local entityData
    if not entityId then
        entityId, entityData = next(universe.pendingUpgrades, nil)
    else
        entityData = universe.pendingUpgrades[entityId]
    end
    if not entityId then
        universe.pendingUpgradeIterator = nil
        if table_size(universe.pendingUpgrades) == 0 then
            universe.pendingUpgrades = {}
        end
    else
        local entity = entityData.entity
        if entity.valid then
            universe.pendingUpgradeIterator = next(universe.pendingUpgrades, entityId)
            universe.pendingUpgrades[entityId] = nil
            local surface = entity.surface
            local query = universe.upgradeEntityQuery
            query.position = entityData.position or entity.position
            query.name = entityData.name
            unregisterEnemyBaseStructure(entityData.map, entity)
            entity.destroy()
            local createdEntity = surface.create_entity(query)
            if createdEntity and createdEntity.valid then
                registerEnemyBaseStructure(entityData.map, createdEntity, tick, entityData.base)
                if remote.interfaces["kr-creep"] then
                    remote.call("kr-creep", "spawn_creep_at_position", surface, query.position)
                end
            end
        else
            universe.pendingUpgradeIterator = next(universe.pendingUpgrades, entityId)
            universe.pendingUpgrades[entityId] = nil
        end
    end
end


function chunkProcessor.processScanChunks(map)
    local chunkId = map.chunkToPassScanIterator
    local chunk
    if not chunkId then
        chunkId, chunk = next(map.chunkToPassScan, nil)
    else
        chunk = map.chunkToPassScan[chunkId]
    end

    if not chunkId then
        map.chunkToPassScanIterator = nil
        if (table_size(map.chunkToPassScan) == 0) then
            -- this is needed as the next command remembers the max length a table has been
            map.chunkToPassScan = {}
        end
    else
        map.chunkToPassScanIterator = next(map.chunkToPassScan, chunkId)
        map.chunkToPassScan[chunkId] = nil

        local area = map.universe.area
        local topOffset = area[1]
        local bottomOffset = area[2]
        topOffset[1] = chunk.x
        topOffset[2] = chunk.y
        bottomOffset[1] = chunk.x + CHUNK_SIZE
        bottomOffset[2] = chunk.y + CHUNK_SIZE

        if (chunkPassScan(chunk, map) == -1) then
            removeProcessQueueChunk(map.processQueue, chunk)
            map[chunk.x][chunk.y] = nil
            map.universe.chunkIdToChunk[chunk.id] = nil
        end
    end
end

chunkProcessorG = chunkProcessor
return chunkProcessor
