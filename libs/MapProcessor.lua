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


if mapProcessorG then
    return mapProcessorG
end
local mapProcessor = {}

-- imports

local queryUtils = require("QueryUtils")
local pheromoneUtils = require("PheromoneUtils")
local aiAttackWave = require("AIAttackWave")
local aiPredicates = require("AIPredicates")
local constants = require("Constants")
local mapUtils = require("MapUtils")
local playerUtils = require("PlayerUtils")
local chunkUtils = require("ChunkUtils")
local chunkPropertyUtils = require("ChunkPropertyUtils")
local baseUtils = require("BaseUtils")

-- constants

local DURATION_ACTIVE_NEST = constants.DURATION_ACTIVE_NEST

local PROCESS_QUEUE_SIZE = constants.PROCESS_QUEUE_SIZE
local RESOURCE_QUEUE_SIZE = constants.RESOURCE_QUEUE_SIZE
local ENEMY_QUEUE_SIZE = constants.ENEMY_QUEUE_SIZE
local PLAYER_QUEUE_SIZE = constants.PLAYER_QUEUE_SIZE

local CLEANUP_QUEUE_SIZE = constants.CLEANUP_QUEUE_SIZE

local PROCESS_PLAYER_BOUND = constants.PROCESS_PLAYER_BOUND
local CHUNK_TICK = constants.CHUNK_TICK

local PROCESS_STATIC_QUEUE_SIZE = constants.PROCESS_STATIC_QUEUE_SIZE

local AI_VENGENCE_SQUAD_COST = constants.AI_VENGENCE_SQUAD_COST

local BASE_AI_STATE_AGGRESSIVE = constants.BASE_AI_STATE_AGGRESSIVE
local BASE_AI_STATE_SIEGE = constants.BASE_AI_STATE_SIEGE
local BASE_AI_STATE_PEACEFUL = constants.BASE_AI_STATE_PEACEFUL
local BASE_AI_STATE_MIGRATING = constants.BASE_AI_STATE_MIGRATING

local COOLDOWN_DRAIN = constants.COOLDOWN_DRAIN
local COOLDOWN_RALLY = constants.COOLDOWN_RALLY
local COOLDOWN_RETREAT = constants.COOLDOWN_RETREAT

-- imported functions

local setPositionInQuery = queryUtils.setPositionInQuery

local findNearbyBase = chunkPropertyUtils.findNearbyBase

local removeChunkToNest = mapUtils.removeChunkToNest

local processStaticPheromone = pheromoneUtils.processStaticPheromone
local processPheromone = pheromoneUtils.processPheromone

local getCombinedDeathGeneratorRating = chunkPropertyUtils.getCombinedDeathGeneratorRating
local processBaseMutation = baseUtils.processBaseMutation

local processNestActiveness = chunkPropertyUtils.processNestActiveness
local getChunkBase = chunkPropertyUtils.getChunkBase

local formSquads = aiAttackWave.formSquads
local formVengenceSquad = aiAttackWave.formVengenceSquad
local formVengenceSettler = aiAttackWave.formVengenceSettler
local formSettlers = aiAttackWave.formSettlers

local getChunkByPosition = mapUtils.getChunkByPosition
local getChunkByXY = mapUtils.getChunkByXY
local getChunkById = mapUtils.getChunkById

local validPlayer = playerUtils.validPlayer

local addPlayerToChunk = chunkPropertyUtils.addPlayerToChunk

local mapScanEnemyChunk = chunkUtils.mapScanEnemyChunk
local mapScanPlayerChunk = chunkUtils.mapScanPlayerChunk
local mapScanResourceChunk = chunkUtils.mapScanResourceChunk

local getNestCount = chunkPropertyUtils.getNestCount
local getEnemyStructureCount = chunkPropertyUtils.getEnemyStructureCount
local getNestActiveness = chunkPropertyUtils.getNestActiveness

local getRaidNestActiveness = chunkPropertyUtils.getRaidNestActiveness

local canAttack = aiPredicates.canAttack
local canMigrate = aiPredicates.canMigrate

local tableSize = table_size

local mMin = math.min
local mMax = math.max

local next = next

-- module code

--[[
    processing is not consistant as it depends on the number of chunks that have been generated
    so if we process 400 chunks an iteration and 200 chunks have been generated than these are
    processed 3 times a second and 1200 generated chunks would be processed once a second
    In theory, this might be fine as smaller bases have less surface to attack and need to have
    pheromone dissipate at a faster rate.
--]]
function mapProcessor.processMap(map, tick)
    local outgoingWave = map.outgoingScanWave
    local processQueue = map.processQueue
    local processQueueLength = #processQueue
    local index = mMin(map.processIndex, processQueueLength)

    local step
    local endIndex
    if outgoingWave then
        step = 1
        endIndex = mMin(index + PROCESS_QUEUE_SIZE, processQueueLength)
    else
        step = -1
        endIndex = mMax(index - PROCESS_QUEUE_SIZE, 1)
    end

    if (processQueueLength == 0) then
        return
    end

    for x=index,endIndex,step do
        local chunk = processQueue[x]
        if chunk[CHUNK_TICK] ~= tick then
            chunk[CHUNK_TICK] = tick
            processPheromone(map, chunk)
        end
    end

    if (endIndex == processQueueLength) then
        map.outgoingScanWave = false
    elseif (endIndex == 1) then
        map.outgoingScanWave = true
    elseif outgoingWave then
        map.processIndex = endIndex + 1
    else
        map.processIndex = endIndex - 1
    end
    map.universe.processedChunks = map.universe.processedChunks + PROCESS_QUEUE_SIZE
end

function mapProcessor.processStaticMap(map)
    local outgoingWave = map.outgoingStaticScanWave
    local processQueue = map.processQueue
    local processQueueLength = #processQueue
    local index = mMin(map.processStaticIndex, processQueueLength)

    local step
    local endIndex
    if outgoingWave then
        step = 1
        endIndex = mMin(index + PROCESS_STATIC_QUEUE_SIZE, processQueueLength)
    else
        step = -1
        endIndex = mMax(index - PROCESS_STATIC_QUEUE_SIZE, 1)
    end

    if (processQueueLength == 0) then
        return
    end

    for x=index,endIndex,step do
        processStaticPheromone(map, processQueue[x])
    end

    if (endIndex == processQueueLength) then
        map.outgoingStaticScanWave = false
    elseif (endIndex == 1) then
        map.outgoingStaticScanWave = true
    elseif outgoingWave then
        map.processStaticIndex = endIndex + 1
    else
        map.processStaticIndex = endIndex - 1
    end
end

local function queueNestSpawners(map, chunk, tick)
    local processActiveNest = map.universe.processActiveNest

    local chunkId = chunk.id
    if not processActiveNest[chunkId] then
        if (getNestActiveness(map, chunk) > 0) or (getRaidNestActiveness(map, chunk) > 0) then
            processActiveNest[chunkId] = {
                map = map,
                chunk = chunk,
                tick = tick + DURATION_ACTIVE_NEST
            }
        end
    end
end

--[[
    Localized player radius were processing takes place in realtime, doesn't store state
    between calls.
    vs
    the slower passive version processing the entire map in multiple passes.
--]]
function mapProcessor.processPlayers(players, universe, tick)
    -- put down player pheromone for player hunters
    -- randomize player order to ensure a single player isn't singled out
    -- not looping everyone because the cost is high enough already in multiplayer
    for i=1,#players do
        local player = players[i]
        if validPlayer(player) then
            local char = player.character
            local map = universe.maps[char.surface.index]
            if map then
                local playerChunk = getChunkByPosition(map, char.position)

                if (playerChunk ~= -1) then
                    addPlayerToChunk(map, playerChunk, player.name)
                end
            end
        end
    end

    if (#players > 0) then
        local player = players[universe.random(#players)]
        if validPlayer(player) then
            local char = player.character
            local map = universe.maps[char.surface.index]
            if map then
                local playerChunk = getChunkByPosition(map, char.position)

                if (playerChunk ~= -1) then
                    local base = findNearbyBase(map, playerChunk)
                    if not base then
                        return
                    end
                    local allowingAttacks = canAttack(map, base)
                    local vengence = allowingAttacks and
                        (base.unitPoints >= AI_VENGENCE_SQUAD_COST) and
                        ((getEnemyStructureCount(map, playerChunk) > 0) or
                            (getCombinedDeathGeneratorRating(map, playerChunk) < universe.retreatThreshold))

                    for x=playerChunk.x - PROCESS_PLAYER_BOUND, playerChunk.x + PROCESS_PLAYER_BOUND, 32 do
                        for y=playerChunk.y - PROCESS_PLAYER_BOUND, playerChunk.y + PROCESS_PLAYER_BOUND, 32 do
                            local chunk = getChunkByXY(map, x, y)

                            if (chunk ~= -1) and (chunk[CHUNK_TICK] ~= tick) then
                                chunk[CHUNK_TICK] = tick
                                processPheromone(map, chunk, true)

                                if (getNestCount(map, chunk) > 0) then
                                    processNestActiveness(map, chunk)
                                    queueNestSpawners(map, chunk, tick)

                                    if vengence then
                                        local pack = universe.vengenceQueue[chunk.id]
                                        if not pack then
                                            pack = {
                                                v = 0,
                                                map = map,
                                                base = base
                                            }
                                            universe.vengenceQueue[chunk.id] = pack
                                        end
                                        pack.v = pack.v + 1
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

local function processCleanUp(universe, chunks, iterator, tick, duration)
    local chunkId = universe[iterator]
    local chunkPack
    if not chunkId then
        chunkId, chunkPack = next(chunks, nil)
    else
        chunkPack = chunks[chunkId]
    end
    if not chunkId then
        universe[iterator] = nil
    else
        universe[iterator] = next(chunks, chunkId)
        if (tick - chunkPack.tick) > duration then
            chunks[chunkId] = nil
        end
    end
end

function mapProcessor.cleanUpMapTables(universe, tick)
    local retreats = universe.chunkToRetreats
    local rallys = universe.chunkToRallys
    local drained = universe.chunkToDrained

    for _=1,CLEANUP_QUEUE_SIZE do
        processCleanUp(universe, retreats, "chunkToRetreatIterator", tick, COOLDOWN_RETREAT)

        processCleanUp(universe, rallys, "chunkToRallyIterator", tick, COOLDOWN_RALLY)

        processCleanUp(universe, drained, "chunkToDrainedIterator", tick, COOLDOWN_DRAIN)
    end
end

--[[
    Passive scan to find entities that have been generated outside the factorio event system
--]]
function mapProcessor.scanPlayerMap(map, tick)
    if (map.nextProcessMap == tick) or (map.nextPlayerScan == tick) or
        (map.nextEnemyScan == tick) or (map.nextChunkProcess == tick)
    then
        return
    end
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

function mapProcessor.scanEnemyMap(map, tick)
    if (map.nextProcessMap == tick) or (map.nextPlayerScan == tick) or (map.nextChunkProcess == tick) then
        return
    end

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

function mapProcessor.scanResourceMap(map, tick)
    if (map.nextProcessMap == tick) or (map.nextPlayerScan == tick) or
        (map.nextEnemyScan == tick) or (map.nextChunkProcess == tick)
    then
        return
    end
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

function mapProcessor.processActiveNests(universe, tick)
    local processActiveNest = universe.processActiveNest
    local chunkId = universe.processActiveNestIterator
    local chunkPack
    if not chunkId then
        chunkId, chunkPack = next(processActiveNest, nil)
    else
        chunkPack = processActiveNest[chunkId]
    end
    if not chunkId then
        universe.processActiveNestIterator = nil
    else
        universe.processActiveNestIterator = next(processActiveNest, chunkId)
        if chunkPack.tick < tick  then
            local map = chunkPack.map
            if not map.surface.valid then
                processActiveNest[chunkId] = nil
                return
            end
            local chunk = chunkPack.chunk
            processNestActiveness(map, chunk)
            if (getNestActiveness(map, chunk) == 0) and (getRaidNestActiveness(map, chunk) == 0) then
                processActiveNest[chunkId] = nil
            else
                chunkPack.tick = tick + DURATION_ACTIVE_NEST
            end
        end
    end
end

function mapProcessor.processVengence(universe)
    local vengenceQueue = universe.vengenceQueue
    local chunkId = universe.deployVengenceIterator
    local vengencePack
    if not chunkId then
        chunkId, vengencePack = next(vengenceQueue, nil)
    else
        vengencePack = vengenceQueue[chunkId]
    end
    if not chunkId then
        universe.deployVengenceIterator = nil
        if (tableSize(vengenceQueue) == 0) then
            universe.vengenceQueue = {}
        end
    else
        universe.deployVengenceIterator = next(vengenceQueue, chunkId)
        vengenceQueue[chunkId] = nil
        local map = vengencePack.map
        if not map.surface.valid then
            return
        end
        local chunk = getChunkById(map, chunkId)
        local base = vengencePack.base
        if canMigrate(map, base) and (universe.random() < 0.075) then
            formVengenceSettler(map, chunk, base)
        else
            formVengenceSquad(map, chunk, base)
        end
    end
end

function mapProcessor.processNests(universe, tick)
    local chunkId = universe.processNestIterator
    local chunkPack
    if not chunkId then
        chunkId,chunkPack = next(universe.chunkToNests, nil)
    else
        chunkPack = universe.chunkToNests[chunkId]
    end
    if not chunkId then
        universe.processNestIterator = nil
    else
        universe.processNestIterator = next(universe.chunkToNests, chunkId)
        local map = chunkPack.map
        if not map.surface.valid then
            removeChunkToNest(universe, chunkId)
            return
        end
        local chunk = getChunkById(map, chunkId)
        processNestActiveness(map, chunk)
        queueNestSpawners(map, chunk, tick)

        if universe.NEW_ENEMIES then
            processBaseMutation(chunk,
                                map,
                                getChunkBase(map, chunk))
        end
    end
end

local function processSpawnersBody(universe, iterator, chunks)
    local chunkId = universe[iterator]
    local chunkPack
    if not chunkId then
        chunkId,chunkPack = next(chunks, nil)
    else
        chunkPack = chunks[chunkId]
    end
    if not chunkId then
        universe[iterator] = nil
    else
        universe[iterator] = next(chunks, chunkId)
        local map = chunkPack.map
        if not map.surface.valid then
            if (iterator == "processMigrationIterator") then
                removeChunkToNest(universe, chunkId)
            else
                chunks[chunkId] = nil
            end
            return
        end
        local chunk = getChunkById(map, chunkId)
        local base = findNearbyBase(map, chunk)
        if base.stateAI == BASE_AI_STATE_PEACEFUL then
            return
        end
        if iterator == "processMigrationIterator" then
            if (base.stateAI ~= BASE_AI_STATE_MIGRATING) and (base.stateAI ~= BASE_AI_STATE_SIEGE) then
                return
            end
        elseif iterator == "processActiveRaidSpawnerIterator" then
            if (base.stateAI == BASE_AI_STATE_AGGRESSIVE) or (base.stateAI == BASE_AI_STATE_MIGRATING) then
                return
            end
        elseif iterator == "processActiveSpawnerIterator" then
            if (base.stateAI == BASE_AI_STATE_MIGRATING) then
                return
            end
        end

        local migrate = canMigrate(map, base)
        local attack = canAttack(map, base)
        if migrate then
            formSettlers(map, chunk, base)
        end
        if attack then
            formSquads(map, chunk, base)
        end
    end
end

function mapProcessor.processAttackWaves(universe)
    processSpawnersBody(universe,
                        "processActiveSpawnerIterator",
                        universe.chunkToActiveNest)
    processSpawnersBody(universe,
                        "processActiveRaidSpawnerIterator",
                        universe.chunkToActiveRaidNest)
    processSpawnersBody(universe,
                        "processMigrationIterator",
                        universe.chunkToNests)
end

function mapProcessor.processClouds(universe, tick)
    local len = universe.settlePurpleCloud.len
    local builderPack = universe.settlePurpleCloud[len]
    if builderPack and (builderPack.tick <= tick) then
        universe.settlePurpleCloud[len] = nil
        universe.settlePurpleCloud.len = len - 1
        local map = builderPack.map
        if builderPack.group.valid and map.surface.valid then
            setPositionInQuery(
                universe.obaCreateBuildCloudQuery,
                builderPack.position
            )
            map.surface.create_entity(universe.obaCreateBuildCloudQuery)
        end
    end
end

mapProcessorG = mapProcessor
return mapProcessor
