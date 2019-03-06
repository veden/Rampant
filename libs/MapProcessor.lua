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

local RAIDING_MINIMUM_BASE_THRESHOLD = constants.RAIDING_MINIMUM_BASE_THRESHOLD

local MOVEMENT_PHEROMONE = constants.MOVEMENT_PHEROMONE
local PLAYER_PHEROMONE = constants.PLAYER_PHEROMONE
local BASE_PHEROMONE = constants.BASE_PHEROMONE

local INTERVAL_RALLY = constants.INTERVAL_RALLY
local INTERVAL_RETREAT = constants.INTERVAL_RETREAT
local INTERVAL_SPAWNER = constants.INTERVAL_SPAWNER

local BASE_PROCESS_INTERVAL = constants.BASE_PROCESS_INTERVAL

-- imported functions

local scents = pheromoneUtils.scents
local processPheromone = pheromoneUtils.processPheromone
local commitPheromone = pheromoneUtils.commitPheromone
local playerScent = pheromoneUtils.playerScent

local formSquads = aiAttackWave.formSquads
local formSettlers = aiAttackWave.formSettlers
local formVengenceSquad = aiAttackWave.formVengenceSquad

local getChunkByPosition = mapUtils.getChunkByPosition
local getChunkByXY = mapUtils.getChunkByXY

local recycleBiters = unitGroupUtils.recycleBiters

local validPlayer = playerUtils.validPlayer

local analyzeChunk = chunkUtils.analyzeChunk

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
local mMax = math.max

local mRandom = math.random

-- module code

local function nonRepeatingRandom(players)
    local ordering = {}
    for _,player in pairs(players) do
	ordering[#ordering+1] = player.index
    end
    for i=#ordering,1,-1 do
	local s = mRandom(i)
	local t = ordering[i]
	ordering[i] = ordering[s]
	ordering[s] = t
    end
    return ordering
end

--[[
    processing is not consistant as it depends on the number of chunks that have been generated
    so if we process 400 chunks an iteration and 200 chunks have been generated than these are
    processed 3 times a second and 1200 generated chunks would be processed once a second
    In theory, this might be fine as smaller bases have less surface to attack and need to have
    pheromone dissipate at a faster rate.
--]]
function mapProcessor.processMap(map, surface, natives, tick, evolutionFactor)
    local roll = map.processRoll
    local index = map.processIndex

    local chunkToBase = map.chunkToBase

    if (index == 1) then
        roll = mRandom()
        map.processRoll = roll
    end

    local newEnemies = natives.newEnemies
    local scentStaging = map.scentStaging

    local squads = canAttack(natives, surface) and (0.11 <= roll) and (roll <= 0.35) and (natives.points >= AI_SQUAD_COST)
    local settlers = canMigrate(natives, surface) and (0.90 <= roll) and (natives.points >= AI_SETTLER_COST)

    local processQueue = map.processQueue
    local endIndex = mMin(index + PROCESS_QUEUE_SIZE, #processQueue)
    local i = 1
    for x=index,endIndex do
        local chunk = processQueue[x]

	if (chunk[CHUNK_TICK] ~= tick) then
	    processPheromone(map, chunk, scentStaging[i])

            if squads then
                squads = formSquads(map, surface, natives, chunk)
            end
            if settlers and (getNestCount(map, chunk) > 0) then
                settlers = formSettlers(map, surface, natives, chunk, tick)
            end

	    if newEnemies then
		local base = chunkToBase[chunk]
		if base and ((tick - base.tick) > BASE_PROCESS_INTERVAL) and (mRandom() < 0.10) then
		    processBase(map, chunk, surface, natives, tick, base, evolutionFactor)
		end
	    end
	end
	i = i + 1
    end

    i = 1
    for x=index,endIndex do
	local chunk = processQueue[x]
	if (chunk[CHUNK_TICK] ~= tick) then
	    -- chunk[CHUNK_TICK] = tick

	    commitPheromone(map, chunk, scentStaging[i], tick)
	    -- scents(map, chunk)
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
function mapProcessor.processPlayers(players, map, surface, natives, tick)
    -- put down player pheromone for player hunters
    -- randomize player order to ensure a single player isn't singled out
    local playerOrdering = nonRepeatingRandom(players)

    local roll = mRandom()

    local allowingAttacks = canAttack(natives, surface)

    local scentStaging = map.scentStaging

    local squads = allowingAttacks and (0.11 <= roll) and (roll <= 0.20) and (natives.points >= AI_SQUAD_COST)

    -- not looping everyone because the cost is high enough already in multiplayer
    if (#playerOrdering > 0) then
	local player = players[playerOrdering[1]]
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

                            local nests = getNestCount(map, chunk)
                            if (nests > 0) then
                                local activeness = getNestActiveness(map, chunk)
                                local raidActiveness = getRaidNestActiveness(map, chunk)
                                if natives.attackUsePlayer and (chunk[PLAYER_PHEROMONE] > natives.attackPlayerThreshold) then
                                    setNestActiveness(map, chunk, mMin((activeness or 0) + 5, 20))
                                elseif (chunk[BASE_PHEROMONE] > 0) then
                                    if (surface.get_pollution(chunk) > 0) then
                                        setNestActiveness(map, chunk, mMin((activeness or 0) + 5, 20))
                                    else
                                        setNestActiveness(map, chunk, activeness - 2)
                                        if (chunk[BASE_PHEROMONE] > RAIDING_MINIMUM_BASE_THRESHOLD) then
                                            setRaidNestActiveness(map, chunk, mMin((raidActiveness or 0) + 3, 20))
                                        else
                                            setRaidNestActiveness(map, chunk, raidActiveness - 1)
                                        end
                                    end
                                else
                                    setNestActiveness(map, chunk, activeness - 5)
                                    setRaidNestActiveness(map, chunk, raidActiveness - 5)
                                end
                            else
                                setNestActiveness(map, chunk, 0)
                                setRaidNestActiveness(map, chunk, 0)
                            end

                            if squads then
                                squads = formSquads(map, surface, natives, chunk)
                            end
                            if vengence and (getNestCount(map, chunk) > 0) then
                                vengence = formVengenceSquad(map, surface, natives, chunk)
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
			    -- chunk[CHUNK_TICK] = tick
			    commitPheromone(map, chunk, scentStaging[i])
			    -- scents(map, chunk)
			end
			i = i + 1
		    end
		end
	    end
	end
    end

    for i=1,#playerOrdering do
	local player = players[playerOrdering[i]]
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
function mapProcessor.scanMap(map, surface, natives, tick)
    local index = map.scanIndex

    local unitCountQuery = map.filteredEntitiesEnemyUnitQuery
    local offset = unitCountQuery.area[2]
    local chunkBox = unitCountQuery.area[1]

    local retreats = map.chunkToRetreats
    local rallys = map.chunkToRallys
    local spawners = map.chunkToSpawner
    local settlers = map.chunkToSettler
    local drained = map.chunkToDrained

    local processQueue = map.processQueue
    local endIndex = mMin(index + SCAN_QUEUE_SIZE, #processQueue)

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

	local spawnerTick = spawners[chunk]
	if spawnerTick and ((tick - spawnerTick) > INTERVAL_SPAWNER) then
	    spawners[chunk] = nil
	end

	local settlerTick = spawners[chunk]
	if settlerTick and ((tick - settlerTick) > 0) then
	    settlers[chunk] = nil
	end

        local drainTick = drained[chunk]
	if drainTick and ((tick - drainTick) > 0) then
	    drained[chunk] = nil
	end

	local closeBy = findNearbySquad(map, chunk, chunk)

	if closeBy then
	    local deadGroup = surface.count_entities_filtered(unitCountQuery) > 300

	    if deadGroup then
		recycleBiters(natives, surface.find_enemy_units(chunk, TRIPLE_CHUNK_SIZE))
	    end
	end

	analyzeChunk(chunk, natives, surface, map)
        local nests = getNestCount(map, chunk)
        if (nests > 0) then
            local activeness = getNestActiveness(map, chunk)
            local raidActiveness = getRaidNestActiveness(map, chunk)
            if natives.attackUsePlayer and (chunk[PLAYER_PHEROMONE] > natives.attackPlayerThreshold) then
                setNestActiveness(map, chunk, mMin((activeness or 0) + 5, 20))
            elseif (chunk[BASE_PHEROMONE] > 0) then
                if (surface.get_pollution(chunk) > 0) then
                    setNestActiveness(map, chunk, mMin((activeness or 0) + 5, 20))
                else
                    setNestActiveness(map, chunk, activeness - 2)
                    if (chunk[BASE_PHEROMONE] > RAIDING_MINIMUM_BASE_THRESHOLD) then
                        setRaidNestActiveness(map, chunk, mMin((raidActiveness or 0) + 3, 20))
                    else
                        setRaidNestActiveness(map, chunk, raidActiveness - 1)
                    end
                end
            else
                setNestActiveness(map, chunk, activeness - 5)
                setRaidNestActiveness(map, chunk, raidActiveness - 5)
            end
        else
            setNestActiveness(map, chunk, 0)
            setRaidNestActiveness(map, chunk, 0)
        end

    end

    if (endIndex == #processQueue) then
        map.scanIndex = 1
    else
        map.scanIndex = endIndex + 1
    end
end

mapProcessorG = mapProcessor
return mapProcessor
