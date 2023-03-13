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


if ProcessorG then
    return ProcessorG
end
local Processor = {}

--

local Universe

-- imports

local Utils = require("Utils")
local MapUtils = require("MapUtils")
local Squad = require("Squad")
local Constants = require("Constants")
local ChunkUtils = require("ChunkUtils")
local ChunkPropertyUtils = require("ChunkPropertyUtils")
local BaseUtils = require("BaseUtils")
local MathUtils = require("MathUtils")

-- Constants

local PLAYER_PHEROMONE_GENERATOR_AMOUNT = Constants.PLAYER_PHEROMONE_GENERATOR_AMOUNT

local PROCESS_QUEUE_SIZE = Constants.PROCESS_QUEUE_SIZE
local RESOURCE_QUEUE_SIZE = Constants.RESOURCE_QUEUE_SIZE
local ENEMY_QUEUE_SIZE = Constants.ENEMY_QUEUE_SIZE
local PLAYER_QUEUE_SIZE = Constants.PLAYER_QUEUE_SIZE

local CLEANUP_QUEUE_SIZE = Constants.CLEANUP_QUEUE_SIZE

local PROCESS_PLAYER_BOUND = Constants.PROCESS_PLAYER_BOUND

local AI_VENGENCE_SQUAD_COST = Constants.AI_VENGENCE_SQUAD_COST

local COOLDOWN_DRAIN = Constants.COOLDOWN_DRAIN
local COOLDOWN_RALLY = Constants.COOLDOWN_RALLY
local COOLDOWN_RETREAT = Constants.COOLDOWN_RETREAT
local PROXY_ENTITY_LOOKUP = Constants.PROXY_ENTITY_LOOKUP
local BASE_DISTANCE_TO_EVO_INDEX = Constants.BASE_DISTANCE_TO_EVO_INDEX

local BUILDING_SPACE_LOOKUP = Constants.BUILDING_SPACE_LOOKUP

-- imported functions

local findInsertionPoint = MapUtils.findInsertionPoint

local createChunk = ChunkUtils.createChunk
local initialScan = ChunkUtils.initialScan
local euclideanDistancePoints = MathUtils.euclideanDistancePoints

local removeChunkFromMap = MapUtils.removeChunkFromMap
local chunkPassScan = ChunkUtils.chunkPassScan
local findEntityUpgrade = BaseUtils.findEntityUpgrade

local unregisterEnemyBaseStructure = ChunkUtils.unregisterEnemyBaseStructure
local registerEnemyBaseStructure = ChunkUtils.registerEnemyBaseStructure
local setPositionInQuery = Utils.setPositionInQuery

local addPlayerGenerator = ChunkPropertyUtils.addPlayerGenerator
local findNearbyBase = ChunkPropertyUtils.findNearbyBase

local removeChunkToNest = MapUtils.removeChunkToNest

local processPheromone = MapUtils.processPheromone

local getCombinedDeathGeneratorRating = ChunkPropertyUtils.getCombinedDeathGeneratorRating
local processBaseMutation = BaseUtils.processBaseMutation

local processNestActiveness = ChunkPropertyUtils.processNestActiveness

local formSquads = Squad.formSquads
local formVengenceSquad = Squad.formVengenceSquad
local formVengenceSettler = Squad.formVengenceSettler
local formSettlers = Squad.formSettlers

local getChunkByPosition = MapUtils.getChunkByPosition
local getChunkByXY = MapUtils.getChunkByXY

local validPlayer = Utils.validPlayer

local mapScanEnemyChunk = ChunkUtils.mapScanEnemyChunk
local mapScanPlayerChunk = ChunkUtils.mapScanPlayerChunk
local mapScanResourceChunk = ChunkUtils.mapScanResourceChunk

local getEnemyStructureCount = ChunkPropertyUtils.getEnemyStructureCount

local canAttack = BaseUtils.canAttack
local canMigrate = BaseUtils.canMigrate

local tableSize = table_size

local mMin = math.min
local mMax = math.max
local tableInsert = table.insert

local next = next

-- module code

--[[
    processing is not consistant as it depends on the number of chunks that have been generated
    so if we process 400 chunks an iteration and 200 chunks have been generated than these are
    processed 3 times a second and 1200 generated chunks would be processed once a second
    In theory, this might be fine as smaller bases have less surface to attack and need to have
    pheromone dissipate at a faster rate.
--]]
function Processor.processMap(map, tick)
    local processQueue = map.processQueue
    local processQueueLength = #processQueue

    if (processQueueLength == 0) then
        return
    end

    local startIndex = mMin(map.processIndex, processQueueLength)
    local step
    local endIndex
    if map.outgoingScanWave then
        step = 1
        endIndex = mMin(startIndex + PROCESS_QUEUE_SIZE, processQueueLength)
        if (endIndex == processQueueLength) then
            map.outgoingScanWave = false
            map.processIndex = processQueueLength
        else
            map.processIndex = endIndex + 1
        end
    else
        step = -1
        endIndex = mMax(startIndex - PROCESS_QUEUE_SIZE, 1)
        if (endIndex == 1) then
            map.outgoingScanWave = true
            map.processIndex = 1
        else
            map.processIndex = endIndex - 1
        end
    end
    Universe.processedChunks = Universe.processedChunks + ((startIndex - endIndex) * step)

    for x=startIndex,endIndex,step do
        processPheromone(processQueue[x], tick)
    end
end

--[[
    Localized player radius were processing takes place in realtime, doesn't store state
    between calls.
    vs
    the slower passive version processing the entire map in multiple passes.
--]]
function Processor.processPlayers(players, tick)
    -- put down player pheromone for player hunters
    -- randomize player order to ensure a single player isn't singled out
    -- not looping everyone because the cost is high enough already in multiplayer
    local playerCount = #players
    local playerMaxGenerator = playerCount * PLAYER_PHEROMONE_GENERATOR_AMOUNT
    for i=1,playerCount do
        local player = players[i]
        if validPlayer(player) then
            local char = player.character
            local map = Universe.maps[char.surface.index]
            if map then
                local playerChunk = getChunkByPosition(map, char.position)

                if (playerChunk ~= -1) then
                    addPlayerGenerator(playerChunk, playerMaxGenerator)
                end
            end
        end
    end

    if (#players > 0) then
        local player = players[Universe.random(#players)]
        if validPlayer(player) then
            local char = player.character
            local map = Universe.maps[char.surface.index]
            if map then
                local playerChunk = getChunkByPosition(map, char.position)

                if (playerChunk ~= -1) then
                    local base = findNearbyBase(playerChunk)
                    if not base then
                        return
                    end
                    local allowingAttacks = canAttack(base)
                    local vengence = allowingAttacks and
                        (base.unitPoints >= AI_VENGENCE_SQUAD_COST) and
                        ((getEnemyStructureCount(playerChunk) > 0) or
                            (getCombinedDeathGeneratorRating(playerChunk) < Universe.retreatThreshold))

                    for x=playerChunk.x - PROCESS_PLAYER_BOUND, playerChunk.x + PROCESS_PLAYER_BOUND, 32 do
                        for y=playerChunk.y - PROCESS_PLAYER_BOUND, playerChunk.y + PROCESS_PLAYER_BOUND, 32 do
                            local chunk = getChunkByXY(map, x, y)

                            if (chunk ~= -1) then
                                processPheromone(chunk, tick, true)

                                if chunk.nestCount then
                                    processNestActiveness(chunk, tick)

                                    if vengence then
                                        Universe.vengenceQueue[chunk.id] = chunk
                                    end
                                end
                            end
                        end
                    end
                end
            end
        end
    end
end

local function processCleanUp(chunks, tick, duration)
    local chunkId, eventTick = next(chunks, nil)
    if not chunkId then
        return
    end
    if (tick - eventTick) > duration then
        chunks[chunkId] = nil
    end
end

function Processor.cleanUpMapTables(tick)
    local retreats = Universe.chunkToRetreats
    local rallys = Universe.chunkToRallys
    local drained = Universe.chunkToDrained

    for _=1,CLEANUP_QUEUE_SIZE do
        processCleanUp(retreats, tick, COOLDOWN_RETREAT)

        processCleanUp(rallys, tick, COOLDOWN_RALLY)

        processCleanUp(drained, tick, COOLDOWN_DRAIN)
    end
end

--[[
    Passive scan to find entities that have been generated outside the factorio event system
--]]
function Processor.scanPlayerMap(map)
    local index = map.scanPlayerIndex

    local processQueue = map.processQueue
    local processQueueLength = #processQueue

    local endIndex = mMin(index + PLAYER_QUEUE_SIZE, processQueueLength)

    if (processQueueLength == 0) then
        return
    end

    for x=index,endIndex do
        mapScanPlayerChunk(processQueue[x], map)
    end

    if (endIndex == processQueueLength) then
        map.scanPlayerIndex = 1
    else
        map.scanPlayerIndex = endIndex + 1
    end
end

function Processor.scanEnemyMap(map, tick)
    local index = map.scanEnemyIndex

    local processQueue = map.processQueue
    local processQueueLength = #processQueue

    local endIndex = mMin(index + ENEMY_QUEUE_SIZE, #processQueue)

    if (processQueueLength == 0) then
        return
    end

    for x=index,endIndex do
        mapScanEnemyChunk(processQueue[x], map, tick)
    end

    if (endIndex == processQueueLength) then
        map.scanEnemyIndex = 1
    else
        map.scanEnemyIndex = endIndex + 1
    end
end

function Processor.scanResourceMap(map)
    local index = map.scanResourceIndex

    local processQueue = map.processQueue
    local processQueueLength = #processQueue

    local endIndex = mMin(index + RESOURCE_QUEUE_SIZE, processQueueLength)

    if (processQueueLength == 0) then
        return
    end

    for x=index,endIndex do
        mapScanResourceChunk(processQueue[x], map)
    end

    if (endIndex == processQueueLength) then
        map.scanResourceIndex = 1
    else
        map.scanResourceIndex = endIndex + 1
    end
end

function Processor.processVengence()
    local vengenceQueue = Universe.vengenceQueue
    local chunkId, chunk = next(vengenceQueue, nil)
    if not chunkId then
        if (tableSize(vengenceQueue) == 0) then
            Universe.vengenceQueue = {}
        end
        return
    end

    vengenceQueue[chunkId] = nil
    local map = chunk.map
    if not map.surface.valid then
        return
    end
    local base = chunk.base
    if canMigrate(base) and (Universe.random() < 0.075) then
        formVengenceSettler(chunk)
    else
        formVengenceSquad(chunk)
    end
end

function Processor.processNests(tick)
    local chunkId = Universe.processNestIterator
    local chunk
    if not chunkId then
        chunkId,chunk = next(Universe.chunkToNests, nil)
    else
        chunk = Universe.chunkToNests[chunkId]
    end
    if not chunkId then
        Universe.processNestIterator = nil
        return
    end

    Universe.processNestIterator = next(Universe.chunkToNests, chunkId)
    local map = chunk.map
    if not map.surface.valid then
        removeChunkToNest(chunkId)
        return
    end
    processNestActiveness(chunk, tick)

    if Universe.NEW_ENEMIES then
        processBaseMutation(chunk,
                            map,
                            chunk.base)
    end
end

local function processSpawnersBody(iterator, chunks)
    local chunkId = Universe[iterator]
    local chunk
    if not chunkId then
        chunkId,chunk = next(chunks, nil)
    else
        chunk = chunks[chunkId]
    end
    if not chunkId then
        Universe[iterator] = nil
        return
    end
    Universe[iterator] = next(chunks, chunkId)
    local map = chunk.map
    if not map.surface.valid then
        if (iterator == "processMigrationIterator") then
            removeChunkToNest(chunkId)
        else
            chunks[chunkId] = nil
        end
        return
    end
    local base = chunk.base
    local migrate = canMigrate(base)
    local attack = canAttack(base)
    if migrate then
        formSettlers(chunk)
    end
    if attack then
        formSquads(chunk)
    end
end

function Processor.processAttackWaves()
    processSpawnersBody(
        "processActiveSpawnerIterator",
        Universe.chunkToActiveNest
    )
    processSpawnersBody(
        "processActiveRaidSpawnerIterator",
        Universe.chunkToActiveRaidNest
    )
    processSpawnersBody(
        "processMigrationIterator",
        Universe.chunkToNests
    )
end

function Processor.processClouds(tick)
    local eventId, builderPack = next(Universe.settlePurpleCloud, nil)
    if not builderPack or (builderPack.tick > tick) then
        return
    end

    Universe.settlePurpleCloud[eventId] = nil
    local map = builderPack.map
    if not builderPack.group.valid or not map.surface.valid then
        return
    end

    setPositionInQuery(
        Universe.obaCreateBuildCloudQuery,
        builderPack.group.position
    )
    map.surface.create_entity(Universe.obaCreateBuildCloudQuery)
end


function Processor.processPendingChunks(tick, flush)
    local pendingChunks = Universe.pendingChunks
    local eventId, event = next(pendingChunks, nil)

    if not eventId then
        if (tableSize(pendingChunks) == 0) then
            -- this is needed as the next command remembers the max length a table has been
            Universe.pendingChunks = {}
        end
        return
    end

    local endCount = 1
    local flushAllChunks = flush or Universe.flushPendingChunks
    if flushAllChunks then
        endCount = tableSize(pendingChunks)
        Universe.flushPendingChunks = false
    end
    for _=1,endCount do
        if not flushAllChunks and (event.tick > tick) then
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
            local chunk = initialScan(initialChunk, map, tick)
            if (chunk ~= -1) then
                tableInsert(
                    map.processQueue,
                    findInsertionPoint(map.processQueue, chunk),
                    chunk
                )
            else
                map[x][y] = nil
            end
        end

        eventId = newEventId
        event = newEvent
        if not eventId then
            return
        end
    end
end

function Processor.processPendingUpgrades(tick)
    local entityId, entityData = next(Universe.pendingUpgrades, nil)
    if not entityId then
        if tableSize(Universe.pendingUpgrades) == 0 then
            Universe.pendingUpgrades = {}
        end
        return
    end
    local entity = entityData.entity
    if not entity.valid then
        Universe.pendingUpgrades[entityId] = nil
        return
    end

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
        if Universe.random() < 0.75 then
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
            registerEnemyBaseStructure(createdEntity, base, tick, true)
        end
        if not entityData.evolve and Universe.printBaseUpgrades then
            surface.print("["..base.id.."]:"..surface.name.." Upgrading ".. entityName .. " to " .. name .. " [gps=".. position.x ..",".. position.y .."]")
        end
        if remote.interfaces["kr-creep"] then
            remote.call("kr-creep", "spawn_creep_at_position", surface, foundPosition or position, false, createdEntity.name)
        end
    end
end


function Processor.processScanChunks()
    local chunkId, chunk = next(Universe.chunkToPassScan, nil)
    if not chunkId then
        if (tableSize(Universe.chunkToPassScan) == 0) then
            -- this is needed as the next command remembers the max length a table has been
            Universe.chunkToPassScan = {}
        end
        return
    end

    Universe.chunkToPassScan[chunkId] = nil
    local map = chunk.map
    if not map.surface.valid then
        return
    end

    if (chunkPassScan(chunk, map) == -1) then
        removeChunkFromMap(map, chunk)
    end
end

function Processor.init(universe)
    Universe = universe
end

ProcessorG = Processor
return Processor
