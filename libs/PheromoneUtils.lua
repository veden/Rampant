local pheromoneUtils = {}

-- imports

local mapUtils = require("MapUtils")
local constants = require("Constants")

-- constants

local MOVEMENT_PHEROMONE = constants.MOVEMENT_PHEROMONE
local BASE_PHEROMONE = constants.BASE_PHEROMONE
local PLAYER_PHEROMONE = constants.PLAYER_PHEROMONE
local RESOURCE_PHEROMONE = constants.RESOURCE_PHEROMONE

local BUILDING_PHEROMONES = constants.BUILDING_PHEROMONES

local PLAYER_BASE_GENERATOR = constants.PLAYER_BASE_GENERATOR
local RESOURCE_GENERATOR = constants.RESOURCE_GENERATOR

local PLAYER_PHEROMONE_GENERATOR_AMOUNT = constants.PLAYER_PHEROMONE_GENERATOR_AMOUNT
local DEATH_PHEROMONE_GENERATOR_AMOUNT = constants.DEATH_PHEROMONE_GENERATOR_AMOUNT

local MOVEMENT_PHEROMONE_PERSISTANCE = constants.MOVEMENT_PHEROMONE_PERSISTANCE
local BASE_PHEROMONE_PERSISTANCE = constants.BASE_PHEROMONE_PERSISTANCE
local PLAYER_PHEROMONE_PERSISTANCE = constants.PLAYER_PHEROMONE_PERSISTANCE
local RESOURCE_PHEROMONE_PERSISTANCE = constants.RESOURCE_PHEROMONE_PERSISTANCE

local CHUNK_IMPASSABLE = constants.CHUNK_IMPASSABLE

local PASSABLE = constants.PASSABLE

local NEST_COUNT = constants.NEST_COUNT

local PATH_RATING = constants.PATH_RATING

local IMPASSABLE_TERRAIN_GENERATOR_AMOUNT = constants.IMPASSABLE_TERRAIN_GENERATOR_AMOUNT

-- imported functions

local getCardinalChunks = mapUtils.getCardinalChunks

local mMax = math.max

-- module code

function pheromoneUtils.scents(chunk)

    if (chunk[PASSABLE] == CHUNK_IMPASSABLE) then
	chunk[BASE_PHEROMONE] = IMPASSABLE_TERRAIN_GENERATOR_AMOUNT;
    else
	chunk[BASE_PHEROMONE] = chunk[BASE_PHEROMONE] + chunk[PLAYER_BASE_GENERATOR]
	local resourceGenerator = chunk[RESOURCE_GENERATOR]
	if (resourceGenerator > 0) and (chunk[NEST_COUNT] == 0) then
	    chunk[RESOURCE_PHEROMONE] = chunk[RESOURCE_PHEROMONE] + mMax(resourceGenerator * 100, 90)
	end
    end
    
end

function pheromoneUtils.victoryScent(chunk, entityType)
    local value = BUILDING_PHEROMONES[entityType]
    if (value ~= nil) and (chunk[PASSABLE] ~= CHUNK_IMPASSABLE) then
	chunk[MOVEMENT_PHEROMONE] = chunk[MOVEMENT_PHEROMONE] + (value * 100000)
    end
end

function pheromoneUtils.deathScent(chunk)
    if (chunk[PASSABLE] ~= CHUNK_IMPASSABLE) then
	chunk[MOVEMENT_PHEROMONE] = chunk[MOVEMENT_PHEROMONE] - DEATH_PHEROMONE_GENERATOR_AMOUNT
    end
end

function pheromoneUtils.playerScent(playerChunk)
    if (playerChunk[PASSABLE] ~= CHUNK_IMPASSABLE) then
	playerChunk[PLAYER_PHEROMONE] = playerChunk[PLAYER_PHEROMONE] + PLAYER_PHEROMONE_GENERATOR_AMOUNT
    end
end

function pheromoneUtils.processPheromone(regionMap, chunk)

    if (chunk[PASSABLE] == CHUNK_IMPASSABLE) then
	return
    end

    local tempNeighbors = getCardinalChunks(regionMap, chunk.cX, chunk.cY)
    
    local chunkMovement = chunk[MOVEMENT_PHEROMONE]
    local chunkBase = chunk[BASE_PHEROMONE]
    local chunkPlayer = chunk[PLAYER_PHEROMONE]
    local chunkResource = chunk[RESOURCE_PHEROMONE]
    local chunkPathRating = chunk[PATH_RATING]
    
    local totalMovement = 0
    local totalBase = 0
    local totalPlayer = 0
    local totalResource = 0

    for i=1,4 do
    	local neighborChunk = tempNeighbors[i]
    	if neighborChunk then
    	    totalMovement = totalMovement + (neighborChunk[MOVEMENT_PHEROMONE] - chunkMovement)
    	    totalBase = totalBase + (neighborChunk[BASE_PHEROMONE] - chunkBase)
    	    totalPlayer = totalPlayer + (neighborChunk[PLAYER_PHEROMONE] - chunkPlayer)
    	    totalResource = totalResource + (neighborChunk[RESOURCE_PHEROMONE] - chunkResource)
    	end
    end
    
    chunk[MOVEMENT_PHEROMONE] = (chunkMovement + (0.125 * totalMovement)) * MOVEMENT_PHEROMONE_PERSISTANCE * chunkPathRating
    chunk[BASE_PHEROMONE] = (chunkBase + (0.25 * totalBase)) * BASE_PHEROMONE_PERSISTANCE * chunkPathRating
    chunk[PLAYER_PHEROMONE] = (chunkPlayer + (0.25 * totalPlayer)) * PLAYER_PHEROMONE_PERSISTANCE * chunkPathRating
    chunk[RESOURCE_PHEROMONE] = (chunkResource + (0.25 * totalResource)) * RESOURCE_PHEROMONE_PERSISTANCE * chunkPathRating
end

return pheromoneUtils
