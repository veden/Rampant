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


if MapProcessorG then
    return MapProcessorG
end
local MapProcessor = {}

--

local Universe

-- imports

local QueryUtils = require("QueryUtils")
local PheromoneUtils = require("PheromoneUtils")
local AiAttackWave = require("AIAttackWave")
local AiPredicates = require("AIPredicates")
local Constants = require("Constants")
local MapUtils = require("MapUtils")
local PlayerUtils = require("PlayerUtils")
local ChunkUtils = require("ChunkUtils")
local ChunkPropertyUtils = require("ChunkPropertyUtils")
local BaseUtils = require("BaseUtils")

-- Constants

local PLAYER_PHEROMONE_GENERATOR_AMOUNT = Constants.PLAYER_PHEROMONE_GENERATOR_AMOUNT
local DURATION_ACTIVE_NEST = Constants.DURATION_ACTIVE_NEST

local PROCESS_QUEUE_SIZE = Constants.PROCESS_QUEUE_SIZE
local RESOURCE_QUEUE_SIZE = Constants.RESOURCE_QUEUE_SIZE
local ENEMY_QUEUE_SIZE = Constants.ENEMY_QUEUE_SIZE
local PLAYER_QUEUE_SIZE = Constants.PLAYER_QUEUE_SIZE

local CLEANUP_QUEUE_SIZE = Constants.CLEANUP_QUEUE_SIZE

local PROCESS_PLAYER_BOUND = Constants.PROCESS_PLAYER_BOUND

local AI_VENGENCE_SQUAD_COST = Constants.AI_VENGENCE_SQUAD_COST

local BASE_AI_STATE_AGGRESSIVE = Constants.BASE_AI_STATE_AGGRESSIVE
local BASE_AI_STATE_SIEGE = Constants.BASE_AI_STATE_SIEGE
local BASE_AI_STATE_PEACEFUL = Constants.BASE_AI_STATE_PEACEFUL
local BASE_AI_STATE_MIGRATING = Constants.BASE_AI_STATE_MIGRATING

local COOLDOWN_DRAIN = Constants.COOLDOWN_DRAIN
local COOLDOWN_RALLY = Constants.COOLDOWN_RALLY
local COOLDOWN_RETREAT = Constants.COOLDOWN_RETREAT

-- imported functions

local setPositionInQuery = QueryUtils.setPositionInQuery

local addPlayerGenerator = ChunkPropertyUtils.addPlayerGenerator
local findNearbyBase = ChunkPropertyUtils.findNearbyBase

local removeChunkToNest = MapUtils.removeChunkToNest

local processPheromone = PheromoneUtils.processPheromone

local getCombinedDeathGeneratorRating = ChunkPropertyUtils.getCombinedDeathGeneratorRating
local processBaseMutation = BaseUtils.processBaseMutation

local processNestActiveness = ChunkPropertyUtils.processNestActiveness

local formSquads = AiAttackWave.formSquads
local formVengenceSquad = AiAttackWave.formVengenceSquad
local formVengenceSettler = AiAttackWave.formVengenceSettler
local formSettlers = AiAttackWave.formSettlers

local getChunkByPosition = MapUtils.getChunkByPosition
local getChunkByXY = MapUtils.getChunkByXY
local getChunkById = MapUtils.getChunkById

local validPlayer = PlayerUtils.validPlayer

local mapScanEnemyChunk = ChunkUtils.mapScanEnemyChunk
local mapScanPlayerChunk = ChunkUtils.mapScanPlayerChunk
local mapScanResourceChunk = ChunkUtils.mapScanResourceChunk

local getEnemyStructureCount = ChunkPropertyUtils.getEnemyStructureCount

local canAttack = AiPredicates.canAttack
local canMigrate = AiPredicates.canMigrate

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
function MapProcessor.processMap(map, tick)
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
        processPheromone(map, processQueue[x], tick)
    end
end

--[[
    Localized player radius were processing takes place in realtime, doesn't store state
    between calls.
    vs
    the slower passive version processing the entire map in multiple passes.
--]]
function MapProcessor.processPlayers(players, tick)
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
                    local allowingAttacks = canAttack(map, base)
                    local vengence = allowingAttacks and
                        (base.unitPoints >= AI_VENGENCE_SQUAD_COST) and
                        ((getEnemyStructureCount(playerChunk) > 0) or
                            (getCombinedDeathGeneratorRating(playerChunk) < Universe.retreatThreshold))

                    for x=playerChunk.x - PROCESS_PLAYER_BOUND, playerChunk.x + PROCESS_PLAYER_BOUND, 32 do
                        for y=playerChunk.y - PROCESS_PLAYER_BOUND, playerChunk.y + PROCESS_PLAYER_BOUND, 32 do
                            local chunk = getChunkByXY(map, x, y)

                            if (chunk ~= -1) then
                                processPheromone(map, chunk, tick, true)

                                if chunk.nestCount then
                                    processNestActiveness(chunk, tick)

                                    if vengence then
                                        local pack = Universe.vengenceQueue[chunk.id]
                                        if not pack then
                                            pack = {
                                                v = 0,
                                                map = map,
                                                base = base
                                            }
                                            Universe.vengenceQueue[chunk.id] = pack
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

local function processCleanUp(chunks, iterator, tick, duration)
    local chunkId = Universe[iterator]
    local eventTick
    if not chunkId then
        chunkId, eventTick = next(chunks, nil)
    else
        eventTick = chunks[chunkId]
    end
    if not chunkId then
        Universe[iterator] = nil
    else
        Universe[iterator] = next(chunks, chunkId)
        if (tick - eventTick) > duration then
            chunks[chunkId] = nil
        end
    end
end

function MapProcessor.cleanUpMapTables(tick)
    local retreats = Universe.chunkToRetreats
    local rallys = Universe.chunkToRallys
    local drained = Universe.chunkToDrained

    for _=1,CLEANUP_QUEUE_SIZE do
        processCleanUp(retreats, "chunkToRetreatIterator", tick, COOLDOWN_RETREAT)

        processCleanUp(rallys, "chunkToRallyIterator", tick, COOLDOWN_RALLY)

        processCleanUp(drained, "chunkToDrainedIterator", tick, COOLDOWN_DRAIN)
    end
end

--[[
    Passive scan to find entities that have been generated outside the factorio event system
--]]
function MapProcessor.scanPlayerMap(map, tick)
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

function MapProcessor.scanEnemyMap(map, tick)
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

function MapProcessor.scanResourceMap(map, tick)
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

function MapProcessor.processVengence()
    local vengenceQueue = Universe.vengenceQueue
    local chunkId = Universe.deployVengenceIterator
    local vengencePack
    if not chunkId then
        chunkId, vengencePack = next(vengenceQueue, nil)
    else
        vengencePack = vengenceQueue[chunkId]
    end
    if not chunkId then
        Universe.deployVengenceIterator = nil
        if (tableSize(vengenceQueue) == 0) then
            Universe.vengenceQueue = {}
        end
    else
        Universe.deployVengenceIterator = next(vengenceQueue, chunkId)
        vengenceQueue[chunkId] = nil
        local map = vengencePack.map
        if not map.surface.valid then
            return
        end
        local chunk = getChunkById(chunkId)
        local base = vengencePack.base
        if canMigrate(map, base) and (Universe.random() < 0.075) then
            formVengenceSettler(map, chunk, base)
        else
            formVengenceSquad(map, chunk, base)
        end
    end
end

function MapProcessor.processNests(tick)
    local chunkId = Universe.processNestIterator
    local chunkPack
    if not chunkId then
        chunkId,chunkPack = next(Universe.chunkToNests, nil)
    else
        chunkPack = Universe.chunkToNests[chunkId]
    end
    if not chunkId then
        Universe.processNestIterator = nil
    else
        Universe.processNestIterator = next(Universe.chunkToNests, chunkId)
        local map = chunkPack.map
        if not map.surface.valid then
            removeChunkToNest(chunkId)
            return
        end
        local chunk = getChunkById(chunkId)
        processNestActiveness(chunk, tick)

        if Universe.NEW_ENEMIES then
            processBaseMutation(chunk,
                                map,
                                chunk.base)
        end
    end
end

local function processSpawnersBody(iterator, chunks)
    local chunkId = Universe[iterator]
    local chunkPack
    if not chunkId then
        chunkId,chunkPack = next(chunks, nil)
    else
        chunkPack = chunks[chunkId]
    end
    if not chunkId then
        Universe[iterator] = nil
    else
        Universe[iterator] = next(chunks, chunkId)
        local map = chunkPack.map
        if not map.surface.valid then
            if (iterator == "processMigrationIterator") then
                removeChunkToNest(chunkId)
            else
                chunks[chunkId] = nil
            end
            return
        end
        local chunk = getChunkById(chunkId)
        local base = findNearbyBase(chunk)
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

function MapProcessor.processAttackWaves()
    processSpawnersBody("processActiveSpawnerIterator",
                        Universe.chunkToActiveNest)
    processSpawnersBody("processActiveRaidSpawnerIterator",
                        Universe.chunkToActiveRaidNest)
    processSpawnersBody("processMigrationIterator",
                        Universe.chunkToNests)
end

function MapProcessor.processClouds(tick)
    local len = Universe.settlePurpleCloud.len
    local builderPack = Universe.settlePurpleCloud[len]
    if builderPack and (builderPack.tick <= tick) then
        Universe.settlePurpleCloud[len] = nil
        Universe.settlePurpleCloud.len = len - 1
        local map = builderPack.map
        if builderPack.group.valid and map.surface.valid then
            setPositionInQuery(
                Universe.obaCreateBuildCloudQuery,
                builderPack.position
            )
            map.surface.create_entity(Universe.obaCreateBuildCloudQuery)
        end
    end
end

function MapProcessor.init(universe)
    Universe = universe
end

MapProcessorG = MapProcessor
return MapProcessor
