-- Copyright (C) 2022  veden

-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <https://www.gnu.org/licenses/>.


if (ChunkProcessorG) then
    return ChunkProcessorG
end
local ChunkProcessor = {}

--

local Universe

-- imports

local ChunkUtils = require("ChunkUtils")
local QueryUtils = require("QueryUtils")
local MapUtils = require("MapUtils")
local MathUtils = require("MathUtils")
local Constants = require("Constants")
local BaseUtils = require("BaseUtils")

-- Constants

local PROXY_ENTITY_LOOKUP = Constants.PROXY_ENTITY_LOOKUP
local BASE_DISTANCE_TO_EVO_INDEX = Constants.BASE_DISTANCE_TO_EVO_INDEX

local BUILDING_SPACE_LOOKUP = Constants.BUILDING_SPACE_LOOKUP

-- imported functions

local findInsertionPoint = MapUtils.findInsertionPoint
local removeChunkFromMap = MapUtils.removeChunkFromMap
local setPositionInQuery = QueryUtils.setPositionInQuery
local registerEnemyBaseStructure = ChunkUtils.registerEnemyBaseStructure
local unregisterEnemyBaseStructure = ChunkUtils.unregisterEnemyBaseStructure
local euclideanDistancePoints = MathUtils.euclideanDistancePoints

local findEntityUpgrade = BaseUtils.findEntityUpgrade

local createChunk = ChunkUtils.createChunk
local initialScan = ChunkUtils.initialScan
local chunkPassScan = ChunkUtils.chunkPassScan

local mMin = math.min
local mMax = math.max
local next = next
local table_size = table_size

local tInsert = table.insert

-- module code

function ChunkProcessor.processPendingChunks(tick, flush)
    local pendingChunks = Universe.pendingChunks
    local eventId = Universe.chunkProcessorIterator
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
            Universe.chunkProcessorIterator = nil
            if (table_size(pendingChunks) == 0) then
                -- this is needed as the next command remembers the max length a table has been
                Universe.pendingChunks = {}
            end
            break
        else
            if not flush and (event.tick > tick) then
                Universe.chunkProcessorIterator = eventId
                return
            end
            local newEventId, newEvent = next(pendingChunks, eventId)
            pendingChunks[eventId] = nil
            local map = event.map
            if not map.surface.valid then
                Universe.chunkProcessorIterator = newEventId
                return
            end

            local topLeft = event.area.left_top
            local x = topLeft.x
            local y = topLeft.y

            if not map[x] then
                map[x] = {}
            end

            if map[x][y] then
                local oldChunk = map[x][y]
                local chunk = initialScan(oldChunk, map, tick)
                if (chunk == -1) then
                    removeChunkFromMap(map, oldChunk)
                end
            else
                local initialChunk = createChunk(map, x, y)
                map[x][y] = initialChunk
                Universe.chunkIdToChunk[initialChunk.id] = initialChunk
                local chunk = initialScan(initialChunk, map, tick)
                if (chunk ~= -1) then
                    tInsert(
                        map.processQueue,
                        findInsertionPoint(map.processQueue, chunk),
                        chunk
                    )
                else
                    map[x][y] = nil
                    Universe.chunkIdToChunk[initialChunk.id] = nil
                end
            end

            eventId = newEventId
            event = newEvent
        end
    end
    Universe.chunkProcessorIterator = eventId
end

function ChunkProcessor.processPendingUpgrades(tick)
    local entityId = Universe.pendingUpgradeIterator
    local entityData
    if not entityId then
        entityId, entityData = next(Universe.pendingUpgrades, nil)
    else
        entityData = Universe.pendingUpgrades[entityId]
    end
    if not entityId then
        Universe.pendingUpgradeIterator = nil
        if table_size(Universe.pendingUpgrades) == 0 then
            Universe.pendingUpgrades = {}
        end
    else
        local entity = entityData.entity
        if entity.valid then
            Universe.pendingUpgradeIterator = next(Universe.pendingUpgrades, entityId)
            if entityData.delayTLL and tick < entityData.delayTLL then
                return
            end
            Universe.pendingUpgrades[entityId] = nil
            local base = entityData.base
            local map = base.map
            local baseAlignment = base.alignment
            local position = entityData.position or entity.position

            local pickedBaseAlignment
            if baseAlignment[2] then
                if map.random() < 0.75 then
                    pickedBaseAlignment = baseAlignment[2]
                else
                    pickedBaseAlignment = baseAlignment[1]
                end
            else
                pickedBaseAlignment = baseAlignment[1]
            end

            local currentEvo = entity.prototype.build_base_evolution_requirement or 0

            local distance = mMin(1, euclideanDistancePoints(position.x, position.y, 0, 0) * BASE_DISTANCE_TO_EVO_INDEX)
            local evoIndex = mMax(distance, Universe.evolutionLevel)

            local name = findEntityUpgrade(pickedBaseAlignment,
                                           currentEvo,
                                           evoIndex,
                                           entity,
                                           map,
                                           entityData.evolve)

            local entityName = entity.name
            if not name and PROXY_ENTITY_LOOKUP[entityName] then
                entity.destroy()
                return
            elseif (name == entityName) or not name then
                return
            end

            local surface = entity.surface
            local query = Universe.ppuUpgradeEntityQuery
            query.name = name

            unregisterEnemyBaseStructure(map, entity, nil, true)
            entity.destroy()
            local foundPosition = surface.find_non_colliding_position(BUILDING_SPACE_LOOKUP[name],
                                                                      position,
                                                                      2,
                                                                      1,
                                                                      true)
            setPositionInQuery(query, foundPosition or position)

            local createdEntity = surface.create_entity({
                    name = query.name,
                    position = query.position
            })
            if createdEntity and createdEntity.valid then
                if entityData.register then
                    registerEnemyBaseStructure(map, createdEntity, base, true)
                end
                if not entityData.evolve and Universe.printBaseUpgrades then
                    surface.print("["..base.id.."]:"..surface.name.." Upgrading ".. entityName .. " to " .. name .. " [gps=".. position.x ..",".. position.y .."]")
                end
                if remote.interfaces["kr-creep"] then
                    remote.call("kr-creep", "spawn_creep_at_position", surface, foundPosition or position, false, createdEntity.name)
                end
            end
        else
            Universe.pendingUpgradeIterator = next(Universe.pendingUpgrades, entityId)
            Universe.pendingUpgrades[entityId] = nil
        end
    end
end


function ChunkProcessor.processScanChunks()
    local chunkId = Universe.chunkToPassScanIterator
    local chunkPack
    if not chunkId then
        chunkId, chunkPack = next(Universe.chunkToPassScan, nil)
    else
        chunkPack = Universe.chunkToPassScan[chunkId]
    end

    if not chunkId then
        Universe.chunkToPassScanIterator = nil
        if (table_size(Universe.chunkToPassScan) == 0) then
            -- this is needed as the next command remembers the max length a table has been
            Universe.chunkToPassScan = {}
        end
    else
        Universe.chunkToPassScanIterator = next(Universe.chunkToPassScan, chunkId)
        Universe.chunkToPassScan[chunkId] = nil
        local map = chunkPack.map
        if not map.surface.valid then
            return
        end
        local chunk = chunkPack.chunk

        if (chunkPassScan(chunk, map) == -1) then
            removeChunkFromMap(map, chunk)
        end
    end
end

function ChunkProcessor.init(universe)
    Universe = universe
end

ChunkProcessorG = ChunkProcessor
return ChunkProcessor
