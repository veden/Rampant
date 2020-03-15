if mapProcessorG then
    return mapProcessorG
end
local mapProcessor = {}

-- imports

local unitGroupUtils = require("UnitGroupUtils")
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

local PROCESS_QUEUE_SIZE = constants.PROCESS_QUEUE_SIZE

local SCAN_QUEUE_SIZE = constants.SCAN_QUEUE_SIZE

local CHUNK_SIZE = constants.CHUNK_SIZE
local TRIPLE_CHUNK_SIZE = constants.TRIPLE_CHUNK_SIZE

local PROCESS_PLAYER_BOUND = constants.PROCESS_PLAYER_BOUND
local CHUNK_TICK = constants.CHUNK_TICK

local SENTINEL_IMPASSABLE_CHUNK  = constants.SENTINEL_IMPASSABLE_CHUNK

local AI_SQUAD_COST = constants.AI_SQUAD_COST
local AI_VENGENCE_SQUAD_COST = constants.AI_VENGENCE_SQUAD_COST
local AI_SETTLER_COST = constants.AI_SETTLER_COST
local AI_STATE_AGGRESSIVE = constants.AI_STATE_AGGRESSIVE

local processNestActiveness = chunkPropertyUtils.processNestActiveness

local MOVEMENT_PHEROMONE = constants.MOVEMENT_PHEROMONE
local PLAYER_PHEROMONE = constants.PLAYER_PHEROMONE
local BASE_PHEROMONE = constants.BASE_PHEROMONE

local INTERVAL_RALLY = constants.INTERVAL_RALLY
local INTERVAL_RETREAT = constants.INTERVAL_RETREAT
local INTERVAL_SPAWNER = constants.INTERVAL_SPAWNER

local BASE_PROCESS_INTERVAL = constants.BASE_PROCESS_INTERVAL

-- imported functions

-- local scents = pheromoneUtils.scents
local processPheromone = pheromoneUtils.processPheromone
local commitPheromone = pheromoneUtils.commitPheromone
local playerScent = pheromoneUtils.playerScent

local formSquads = aiAttackWave.formSquads
local formAttackWave = aiAttackWave.formAttackWave
local formSettlers = aiAttackWave.formSettlers
local formVengenceSquad = aiAttackWave.formVengenceSquad

local getChunkByPosition = mapUtils.getChunkByPosition
local getChunkByXY = mapUtils.getChunkByXY

local recycleBiters = unitGroupUtils.recycleBiters

local validPlayer = playerUtils.validPlayer

local mapScanChunk = chunkUtils.mapScanChunk

local getNestCount = chunkPropertyUtils.getNestCount
local getEnemyStructureCount = chunkPropertyUtils.getEnemyStructureCount
local getNestActiveness = chunkPropertyUtils.getNestActiveness
local setNestActiveness = chunkPropertyUtils.setNestActiveness
local getRaidNestActiveness = chunkPropertyUtils.getRaidNestActiveness
local setRaidNestActiveness = chunkPropertyUtils.setRaidNestActiveness


local canAttack = aiPredicates.canAttack
local canMigrate = aiPredicates.canMigrate

local findNearbySquad = unitGroupUtils.findNearbySquad

local processBase = baseUtils.processBase

local mMin = math.min
-- local mMax = math.max

local mRandom = math.random

-- module code

--[[
    processing is not consistant as it depends on the number of chunks that have been generated
    so if we process 400 chunks an iteration and 200 chunks have been generated than these are
    processed 3 times a second and 1200 generated chunks would be processed once a second
    In theory, this might be fine as smaller bases have less surface to attack and need to have
    pheromone dissipate at a faster rate.
--]]
function mapProcessor.processMap(map, surface, tick)
    local roll = map.processRoll
    local index = map.processIndex

    local chunkToBase = map.chunkToBase

    if (index == 1) then
        roll = mRandom()
        map.processRoll = roll
    end

    local natives = map.natives

    local newEnemies = natives.newEnemies
    local scentStaging = map.scentStaging

    local squads = canAttack(natives, surface) and (0.11 <= roll) and (roll <= 0.35) and (natives.points >= AI_SQUAD_COST)
    if squads and (natives.state == AI_STATE_AGGRESSIVE) and (tick < natives.canAttackTick) then
        squads = false
    end
    local settlers = canMigrate(natives, surface) and (0.90 <= roll) and (natives.points >= AI_SETTLER_COST)

    local processQueue = map.processQueue
    local endIndex = mMin(index + PROCESS_QUEUE_SIZE, #processQueue)
    local i = 1
    for x=index,endIndex do
        local chunk = processQueue[x]

        if (chunk[CHUNK_TICK] ~= tick) then
            processPheromone(map, chunk, scentStaging[i])

            if settlers and (getNestCount(map, chunk) > 0) then
                settlers = formSettlers(map, surface, chunk, tick)
            end
            if squads then
                squads = formAttackWave(chunk, map, surface, tick)
            end

            if newEnemies then
                local base = chunkToBase[chunk]
                if base and ((tick - base.tick) > BASE_PROCESS_INTERVAL) then
                    processBase(chunk, surface, natives, tick, base)
                end
            end
        end
        i = i + 1
    end

    i = 1
    for x=index,endIndex do
        local chunk = processQueue[x]
        if (chunk[CHUNK_TICK] ~= tick) then
            commitPheromone(map, chunk, scentStaging[i], tick)
        end
        i = i + 1
    end

    if (endIndex == #processQueue) then
        map.processIndex = 1
    else
        map.processIndex = endIndex + 1
    end
end

--[[
    Localized player radius were processing takes place in realtime, doesn't store state
    between calls.
    vs
    the slower passive version processing the entire map in multiple passes.
--]]
function mapProcessor.processPlayers(players, map, surface, tick)
    -- put down player pheromone for player hunters
    -- randomize player order to ensure a single player isn't singled out
    local natives = map.natives

    local roll = mRandom()

    local allowingAttacks = canAttack(natives, surface)

    local scentStaging = map.scentStaging

    local squads = allowingAttacks and (0.11 <= roll) and (roll <= 0.20) and (natives.points >= AI_SQUAD_COST)

    -- not looping everyone because the cost is high enough already in multiplayer
    if (#players > 0) then
        local player = players[mRandom(#players)]
        if validPlayer(player, natives) then
            local playerChunk = getChunkByPosition(map, player.character.position)

            if (playerChunk ~= SENTINEL_IMPASSABLE_CHUNK) then
                local i = 1
                local vengence = (allowingAttacks and
                                      (natives.points >= AI_VENGENCE_SQUAD_COST) and
                                      ((getEnemyStructureCount(map, playerChunk) > 0) or (playerChunk[MOVEMENT_PHEROMONE] < -natives.retreatThreshold)))

                for x=playerChunk.x - PROCESS_PLAYER_BOUND, playerChunk.x + PROCESS_PLAYER_BOUND, 32 do
                    for y=playerChunk.y - PROCESS_PLAYER_BOUND, playerChunk.y + PROCESS_PLAYER_BOUND, 32 do
                        local chunk = getChunkByXY(map, x, y)

                        if (chunk ~= SENTINEL_IMPASSABLE_CHUNK) and (chunk[CHUNK_TICK] ~= tick) then
                            processPheromone(map, chunk, scentStaging[i])

                            processNestActiveness(map, chunk, natives, surface)

                            if vengence and (getNestCount(map, chunk) > 0) then
                                vengence = formVengenceSquad(map, surface, chunk)
                            end
                        end
                        i = i + 1
                    end
                end

                i = 1
                for x=playerChunk.x - PROCESS_PLAYER_BOUND, playerChunk.x + PROCESS_PLAYER_BOUND, 32 do
                    for y=playerChunk.y - PROCESS_PLAYER_BOUND, playerChunk.y + PROCESS_PLAYER_BOUND, 32 do
                        local chunk = getChunkByXY(map, x, y)
                        if (chunk ~= SENTINEL_IMPASSABLE_CHUNK) and (chunk[CHUNK_TICK] ~= tick) then
                            commitPheromone(map, chunk, scentStaging[i], tick)
                        end
                        i = i + 1
                    end
                end
            end
        end
    end

    for i=1,#players do
        local player = players[i]
        if validPlayer(player, natives) then
            local playerChunk = getChunkByPosition(map, player.character.position)

            if (playerChunk ~= SENTINEL_IMPASSABLE_CHUNK) then
                playerScent(playerChunk)
            end
        end
    end
end

--[[
    Passive scan to find entities that have been generated outside the factorio event system
--]]
function mapProcessor.scanMap(map, surface, tick)
    local index = map.scanIndex

    local unitCountQuery = map.filteredEntitiesEnemyUnitQuery
    local offset = unitCountQuery.area[2]
    local chunkBox = unitCountQuery.area[1]

    local retreats = map.chunkToRetreats
    local rallys = map.chunkToRallys
    -- local spawners = map.chunkToSpawner
    local settlers = map.chunkToSettler
    local drained = map.chunkToDrained

    local processQueue = map.processQueue
    local endIndex = mMin(index + SCAN_QUEUE_SIZE, #processQueue)

    local isFullMapScan = settings.global["rampant-enableFullMapScan"].value

    local natives = map.natives

    for x=index,endIndex do
        local chunk = processQueue[x]

        chunkBox[1] = chunk.x
        chunkBox[2] = chunk.y

        offset[1] = chunk.x + CHUNK_SIZE
        offset[2] = chunk.y + CHUNK_SIZE

        local retreatTick = retreats[chunk]
        if retreatTick and ((tick - retreatTick) > INTERVAL_RETREAT) then
            retreats[chunk] = nil
        end

        local rallyTick = rallys[chunk]
        if rallyTick and ((tick - rallyTick) > INTERVAL_RALLY) then
            rallys[chunk] = nil
        end

        local settlerTick = settlers[chunk]
        if settlerTick and ((tick - settlerTick) > 0) then
            settlers[chunk] = nil
        end

        local drainTick = drained[chunk]
        if drainTick and ((tick - drainTick) > 0) then
            drained[chunk] = nil
        end

        local closeBy = findNearbySquad(map, chunk, chunk)

        if not closeBy then
            local deadGroup = surface.count_entities_filtered(unitCountQuery) > 300

            if deadGroup then
                recycleBiters(natives, surface.find_enemy_units(chunk, TRIPLE_CHUNK_SIZE))
            end
        end

        if isFullMapScan then
           mapScanChunk(chunk, surface, map)
        end

        processNestActiveness(map, chunk, natives, surface)
    end

    if (endIndex == #processQueue) then
        map.scanIndex = 1
    else
        map.scanIndex = endIndex + 1
    end
end

mapProcessorG = mapProcessor
return mapProcessor
