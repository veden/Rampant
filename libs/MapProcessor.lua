local mapProcessor = {}

-- imports

local pheromoneUtils = require("PheromoneUtils")
local aiBuilding = require("AIBuilding")
local constants = require("Constants")
local mapUtils = require("MapUtils")

-- constants

local PROCESS_QUEUE_SIZE = constants.PROCESS_QUEUE_SIZE
local ENEMY_BASE_PHEROMONE_GENERATOR_AMOUNT = constants.ENEMY_BASE_PHEROMONE_GENERATOR_AMOUNT

local RETREAT_MOVEMENT_PHEROMONE_LEVEL = constants.RETREAT_MOVEMENT_PHEROMONE_LEVEL

local SCAN_QUEUE_SIZE = constants.SCAN_QUEUE_SIZE

local CHUNK_SIZE = constants.CHUNK_SIZE
local ENEMY_BASE_GENERATOR = constants.ENEMY_BASE_GENERATOR
local AI_STATE_AGGRESSIVE = constants.AI_STATE_AGGRESSIVE

local PROCESS_PLAYER_BOUND = constants.PROCESS_PLAYER_BOUND
local CHUNK_TICK = constants.CHUNK_TICK

local AI_SQUAD_COST = constants.AI_SQUAD_COST
local AI_VENGENCE_SQUAD_COST = constants.AI_VENGENCE_SQUAD_COST

local MOVEMENT_PHEROMONE = constants.MOVEMENT_PHEROMONE

-- imported functions

local scents = pheromoneUtils.scents
local processPheromone = pheromoneUtils.processPheromone

local makeScouts = aiBuilding.makeScouts
local formSquads = aiBuilding.formSquads

local getChunkByIndex = mapUtils.getChunkByIndex
local getChunkByPosition = mapUtils.getChunkByPosition

local playerScent = pheromoneUtils.playerScent

local mMin = math.min

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
function mapProcessor.processMap(regionMap, surface, natives, evolution_factor)   
    local roll = regionMap.processRoll
    local index = regionMap.processPointer
    local scouts = false
    local squads = false
    
    if (index == 1) then
        roll = math.random()
        regionMap.processRoll = roll
    end
    
    if (0.05 <= roll) and (roll <= 0.10) then
        scouts = true
    end
    
    if (natives.state == AI_STATE_AGGRESSIVE) and (0.11 <= roll) and (roll <= 0.35) then
	squads = true
    end
    
    local processQueue = regionMap.processQueue
    local endIndex = mMin(index + PROCESS_QUEUE_SIZE, #processQueue)
    for x=index,endIndex do
        local chunk = processQueue[x]
        
        scents(chunk)
        
        if scouts then
            makeScouts(surface, natives, chunk, evolution_factor)
        end
        if squads then
            formSquads(regionMap, surface, natives, chunk, evolution_factor, AI_SQUAD_COST)
        end
        
        processPheromone(regionMap, chunk)
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
function mapProcessor.processPlayers(players, regionMap, surface, natives, evolution_factor, tick)
    -- put down player pheromone for player hunters
    -- randomize player order to ensure a single player isn't singled out
    local playerOrdering = nonRepeatingRandom(players)
    
    local scouts = false
    local squads = false
    local vengenceThreshold = -(evolution_factor * RETREAT_MOVEMENT_PHEROMONE_LEVEL)
    local roll = math.random() 

    if (0.05 <= roll) and (roll <= 0.7) then
	scouts = true
    end
    
    if (natives.state == AI_STATE_AGGRESSIVE) and (0.11 <= roll) and (roll <= 0.20) then
	squads = true
    end
    
    for i=1,#playerOrdering do
	local player = players[playerOrdering[i]]
	if (player ~= nil) and player.connected and (player.character ~= nil) and player.character.valid and (player.character.surface.index == 1) then 
	    local playerPosition = player.character.position
	    local playerChunk = getChunkByPosition(regionMap, playerPosition.x, playerPosition.y)
	    
	    if (playerChunk ~= nil) then
		playerScent(playerChunk)
	    end
	end
    end
    for i=1,#playerOrdering do
	local player = players[playerOrdering[i]]
	if (player ~= nil) and player.connected and (player.character ~= nil) and player.character.valid and (player.character.surface.index == 1) then 
	    local playerPosition = player.character.position
	    local playerChunk = getChunkByPosition(regionMap, playerPosition.x, playerPosition.y)
	    
	    if (playerChunk ~= nil) then
		local vengence = false
		if (playerChunk[ENEMY_BASE_GENERATOR] ~= 0) or (playerChunk[MOVEMENT_PHEROMONE] < vengenceThreshold) then
		    vengence = true
		end
		for x=playerChunk.cX - PROCESS_PLAYER_BOUND, playerChunk.cX + PROCESS_PLAYER_BOUND do
		    for y=playerChunk.cY - PROCESS_PLAYER_BOUND, playerChunk.cY + PROCESS_PLAYER_BOUND do
			local chunk = getChunkByIndex(regionMap, x, y)
			
			if (chunk ~= nil) and (chunk[CHUNK_TICK] ~= tick) then
			    chunk[CHUNK_TICK] = tick
			    scents(chunk)
			    
			    if scouts then
				makeScouts(surface, natives, chunk, evolution_factor)
			    end
			    if squads then
				formSquads(regionMap, surface, natives, chunk, evolution_factor, AI_SQUAD_COST)
			    end
			    if vengence then
				formSquads(regionMap, surface, natives, chunk, evolution_factor, AI_VENGENCE_SQUAD_COST)
			    end
		
			    processPheromone(regionMap, chunk)
			end
		    end
		end
	    end
	end
    end
end

--[[
    Passive scan to find ai generated nests, required while ai creates bases or until an event is exposed by devs
--]]
function mapProcessor.scanMap(regionMap, surface)
    local index = regionMap.scanPointer
        
    local processQueue = regionMap.processQueue
    local endIndex = mMin(index + SCAN_QUEUE_SIZE, #processQueue)
    for x=index,endIndex do
	local chunk = processQueue[x]
        
	local spawners = surface.count_entities_filtered({area = {{chunk.pX, chunk.pY},
							      {chunk.pX + CHUNK_SIZE, chunk.pY + CHUNK_SIZE}},
							  type = "unit-spawner",
							  force = "enemy"})
	chunk[ENEMY_BASE_GENERATOR] = spawners * ENEMY_BASE_PHEROMONE_GENERATOR_AMOUNT
    end
    
    if (endIndex == #processQueue) then
	regionMap.scanPointer = 1
    else
	regionMap.scanPointer = endIndex + 1
    end
end

return mapProcessor
