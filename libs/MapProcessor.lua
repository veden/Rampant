local mapProcessor = {}

-- imports

local pheromoneUtils = require("PheromoneUtils")
local aiBuilding = require("AIBuilding")
local aiPredicates = require("AIPredicates")
local constants = require("Constants")
local mapUtils = require("MapUtils")
local playerUtils = require("PlayerUtils")

-- constants

local PROCESS_QUEUE_SIZE = constants.PROCESS_QUEUE_SIZE

local SCAN_QUEUE_SIZE = constants.SCAN_QUEUE_SIZE

local CHUNK_SIZE = constants.CHUNK_SIZE
local DOUBLE_CHUNK_SIZE = constants.DOUBLE_CHUNK_SIZE
local TRIPLE_CHUNK_SIZE = constants.TRIPLE_CHUNK_SIZE

local PROCESS_PLAYER_BOUND = constants.PROCESS_PLAYER_BOUND
local CHUNK_TICK = constants.CHUNK_TICK

local NEST_COUNT = constants.NEST_COUNT
local WORM_COUNT = constants.WORM_COUNT

local AI_MAX_OVERFLOW_POINTS = constants.AI_MAX_OVERFLOW_POINTS
local AI_SQUAD_COST = constants.AI_SQUAD_COST
local AI_VENGENCE_SQUAD_COST = constants.AI_VENGENCE_SQUAD_COST

local MOVEMENT_PHEROMONE = constants.MOVEMENT_PHEROMONE
local PLAYER_BASE_GENERATOR = constants.PLAYER_BASE_GENERATOR
local RESOURCE_GENERATOR = constants.RESOURCE_GENERATOR
local BUILDING_PHEROMONES = constants.BUILDING_PHEROMONES

-- imported functions

local scents = pheromoneUtils.scents
local processPheromone = pheromoneUtils.processPheromone

local formSquads = aiBuilding.formSquads

local getChunkByIndex = mapUtils.getChunkByIndex
local getChunkByPosition = mapUtils.getChunkByPosition

local playerScent = pheromoneUtils.playerScent

local canAttack = aiPredicates.canAttack

local euclideanDistanceNamed = mapUtils.euclideanDistanceNamed

local mMin = math.min

local validPlayer = playerUtils.validPlayer

-- module code

local function nonRepeatingRandom(players)
    local ordering = {}
    for _,player in pairs(players) do
	ordering[#ordering+1] = player.index
    end
    for i=#ordering,1,-1 do
	local s = math.random(i)
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
function mapProcessor.processMap(regionMap, surface, natives, tick)
    local roll = regionMap.processRoll
    local index = regionMap.processPointer
    
    if (index == 1) then
        roll = math.random()
        regionMap.processRoll = roll
    end

    local tempNeighbors = {nil, nil, nil, nil}
    local tempNeighborsAll = {nil, nil, nil, nil, nil, nil, nil, nil}
    
    local squads = canAttack(natives, surface) and (0.11 <= roll) and (roll <= 0.35) and (natives.points >= AI_SQUAD_COST)
    
    local processQueue = regionMap.processQueue
    local endIndex = mMin(index + PROCESS_QUEUE_SIZE, #processQueue)
    for x=index,endIndex do
        local chunk = processQueue[x]
	
	if (chunk[CHUNK_TICK] ~= tick) then
	    chunk[CHUNK_TICK] = tick
	    
	    processPheromone(regionMap, chunk, tempNeighbors)

	    if squads and (chunk[NEST_COUNT] ~= 0) then
		formSquads(regionMap, surface, natives, chunk, AI_SQUAD_COST, tempNeighborsAll)
		squads = natives.points >= AI_SQUAD_COST
	    end
	    
	    scents(chunk)
	end
    end
    
    if (endIndex == #processQueue) then
        regionMap.processPointer = 1
    else
        regionMap.processPointer = endIndex + 1
    end
end

--[[
    Localized player radius were processing takes place in realtime, doesn't store state
    between calls.
    vs 
    the slower passive version processing the entire map in multiple passes.
--]]
function mapProcessor.processPlayers(players, regionMap, surface, natives, tick)
    -- put down player pheromone for player hunters
    -- randomize player order to ensure a single player isn't singled out
    local playerOrdering = nonRepeatingRandom(players)

    local roll = math.random()

    local allowingAttacks = canAttack(natives, surface)

    local squads = allowingAttacks and (0.11 <= roll) and (roll <= 0.20) and (natives.points >= AI_SQUAD_COST)
    
    local tempNeighbors = {nil, nil, nil, nil}
    local tempNeighborsAll = {nil, nil, nil, nil, nil, nil, nil, nil}
    
    for i=1,#playerOrdering do
	local player = players[playerOrdering[i]]
	if validPlayer(player) then 
	    local playerPosition = player.character.position
	    local playerChunk = getChunkByPosition(regionMap, playerPosition.x, playerPosition.y)
	    
	    if playerChunk then
		playerScent(playerChunk)
	    end
	end
    end
    for i=1,#playerOrdering do
	local player = players[playerOrdering[i]]
	if validPlayer(player) then 
	    local playerPosition = player.character.position
	    local playerChunk = getChunkByPosition(regionMap, playerPosition.x, playerPosition.y)
	    
	    if playerChunk then
		local vengence = allowingAttacks and ((playerChunk[NEST_COUNT] ~= 0) or (playerChunk[WORM_COUNT] ~= 0) or (playerChunk[MOVEMENT_PHEROMONE] < natives.retreatThreshold)) and (natives.points >= AI_VENGENCE_SQUAD_COST)
		
		for x=playerChunk.cX - PROCESS_PLAYER_BOUND, playerChunk.cX + PROCESS_PLAYER_BOUND do
		    for y=playerChunk.cY - PROCESS_PLAYER_BOUND, playerChunk.cY + PROCESS_PLAYER_BOUND do
			local chunk = getChunkByIndex(regionMap, x, y)
			if chunk and (chunk[CHUNK_TICK] ~= tick) then
			    chunk[CHUNK_TICK] = tick

			    processPheromone(regionMap, chunk, tempNeighbors)
			    
			    if squads then
				formSquads(regionMap, surface, natives, chunk, AI_SQUAD_COST, tempNeighborsAll)
				squads = natives.points >= AI_SQUAD_COST
			    end
			    if vengence then
				formSquads(regionMap, surface, natives, chunk, AI_VENGENCE_SQUAD_COST, tempNeighborsAll)
				vengence = natives.points >= AI_VENGENCE_SQUAD_COST
			    end

			    scents(chunk)

			end
		    end
		end
	    end
	end
    end
end

--[[
    Passive scan to find entities that have been generated outside the factorio event system
--]]
function mapProcessor.scanMap(regionMap, surface, natives)
    local index = regionMap.scanPointer

    local offset = {0, 0}
    local chunkBox = {false, offset}
    local playerQuery = {area = chunkBox,
			 force = "player"}
    local resourceQuery = {area = chunkBox,
			   type = "resource"}
    local unitCountQuery = { area = chunkBox,
			     type = "unit",
			     force = "enemy",
			     limit = 301 }

    local processQueue = regionMap.processQueue
    local endIndex = mMin(index + SCAN_QUEUE_SIZE, #processQueue)

    local unitRefundAmount = natives.unitRefundAmount
    
    for x=index,endIndex do
	local chunk = processQueue[x]

	chunkBox[1] = chunk
	
	offset[1] = chunk.x + CHUNK_SIZE
	offset[2] = chunk.y + CHUNK_SIZE
	
	local unitCount = surface.count_entities_filtered(unitCountQuery)

	if (unitCount > 300) then
	    local closeBy = false
	    local squads = natives.squads
	    for i=1, #squads do
		local squadGroup = squads[i].group
		if squadGroup.valid and (euclideanDistanceNamed(squadGroup.position, chunk) < DOUBLE_CHUNK_SIZE) then
		    closeBy = true
		    break
		end
	    end
	    
	    if not closeBy then
		local units = surface.find_enemy_units(chunk, TRIPLE_CHUNK_SIZE)

		unitCount = #units
		for i=1,unitCount do
		    units[i].destroy()
		end
		natives.points = natives.points + (unitCount * unitRefundAmount)

		if (natives.points > AI_MAX_OVERFLOW_POINTS) then
		    natives.points = AI_MAX_OVERFLOW_POINTS
		end
	    end
	end

	chunk[RESOURCE_GENERATOR] = surface.count_entities_filtered(resourceQuery)
	
	local entities = surface.find_entities_filtered(playerQuery)
	local playerBaseGenerator = 0
	local safeBuildings = natives.safeBuildings
	for i=1,#entities do
	    local entity = entities[i]
	    local value = BUILDING_PHEROMONES[entity.type]
	    if safeBuildings then
		if natives.safeEntities[entity.type] or natives.safeEntityName[entity.name] then
		    entity.destructible = false
		end
	    end
	    if (value ~= nil) then
		playerBaseGenerator = playerBaseGenerator + value
	    end
	end

	chunk[PLAYER_BASE_GENERATOR] = playerBaseGenerator
    end

    if (endIndex == #processQueue) then
	regionMap.scanPointer = 1
    else
	regionMap.scanPointer = endIndex + 1
    end
end

return mapProcessor
