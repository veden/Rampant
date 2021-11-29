if (chunkProcessorG) then
    return chunkProcessorG
end
local chunkProcessor = {}

-- imports

local chunkUtils = require("ChunkUtils")
local constants = require("Constants")
local baseUtils = require("BaseUtils")
local mapUtils = require("MapUtils")

-- constants

local CHUNK_SIZE = constants.CHUNK_SIZE

-- imported functions

local queueGeneratedChunk = mapUtils.queueGeneratedChunk
local getChunkByPosition = mapUtils.getChunkByPosition

local findNearbyBase = baseUtils.findNearbyBase
local createBase = baseUtils.createBase

local mapScanEnemyChunk = chunkUtils.mapScanEnemyChunk
local mapScanPlayerChunk = chunkUtils.mapScanPlayerChunk
local mapScanResourceChunk = chunkUtils.mapScanResourceChunk

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
            if (event.tick > tick) then
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

            if map[x] and map[x][y] then
                local chunk = map[x][y]
                mapScanPlayerChunk(chunk, map)
                mapScanEnemyChunk(chunk, map)
                mapScanResourceChunk(chunk, map)
            else
                if not map[x] then
                    map[x] = {}
                end

                local chunk = createChunk(x, y)

                chunk = initialScan(chunk, map, tick)

                if (chunk ~= -1) then
                    map[x][y] = chunk
                    tInsert(
                        processQueue,
                        findInsertionPoint(processQueue, chunk),
                        chunk
                    )
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
            entity.destroy()
            if remote.interfaces["kr-creep"] then
                remote.call("kr-creep", "spawn_creep_at_position", surface, query.position)
            end
            local createdEntity = surface.create_entity(query)
            if createdEntity and createdEntity.valid and entityData.register then
                local chunk = getChunkByPosition(map, createdEntity.position)
                if (chunk ~= -1) then
                    local base = findNearbyBase(map, chunk)
                    if not base then
                        base = createBase(map,
                                          chunk,
                                          tick)
                    end
                    if base then
                        chunkUtils.registerEnemyBaseStructure(map, createdEntity, base)
                    end
                else
                    queueGeneratedChunk(
                        universe,
                        {
                            surface = createdEntity.surface,
                            area = {
                                left_top = {
                                    x = createdEntity.position.x,
                                    y = createdEntity.position.y
                                }
                            }
                        }
                    )
                end
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
        for i=#processQueue,1,-1 do
            for ri=chunkCount,1,-1 do
                if (removals[ri] == processQueue[i]) then
                    tRemove(processQueue, i)
                    break
                end
            end
        end
    end
end

chunkProcessorG = chunkProcessor
return chunkProcessor
