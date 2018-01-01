local pheromoneUtils = {}

-- imports

local mapUtils = require("MapUtils")
local constants = require("Constants")
local chunkUtils = require("ChunkUtils")

-- constants

local MOVEMENT_PHEROMONE = constants.MOVEMENT_PHEROMONE
local BASE_PHEROMONE = constants.BASE_PHEROMONE
local PLAYER_PHEROMONE = constants.PLAYER_PHEROMONE
local RESOURCE_PHEROMONE = constants.RESOURCE_PHEROMONE

local BUILDING_PHEROMONES = constants.BUILDING_PHEROMONES

local PLAYER_PHEROMONE_GENERATOR_AMOUNT = constants.PLAYER_PHEROMONE_GENERATOR_AMOUNT
local DEATH_PHEROMONE_GENERATOR_AMOUNT = constants.DEATH_PHEROMONE_GENERATOR_AMOUNT

local MOVEMENT_PHEROMONE_PERSISTANCE = constants.MOVEMENT_PHEROMONE_PERSISTANCE
local BASE_PHEROMONE_PERSISTANCE = constants.BASE_PHEROMONE_PERSISTANCE
local PLAYER_PHEROMONE_PERSISTANCE = constants.PLAYER_PHEROMONE_PERSISTANCE
local RESOURCE_PHEROMONE_PERSISTANCE = constants.RESOURCE_PHEROMONE_PERSISTANCE

local PATH_RATING = constants.PATH_RATING

-- imported functions

local getCardinalChunks = mapUtils.getCardinalChunks

local mMax = math.max
local getEnemyStructureCount = chunkUtils.getEnemyStructureCount
local getPlayerBaseGenerator = chunkUtils.getPlayerBaseGenerator
local getResourceGenerator = chunkUtils.getResourceGenerator

-- module code

function pheromoneUtils.scents(regionMap, chunk)
    chunk[BASE_PHEROMONE] = chunk[BASE_PHEROMONE] + getPlayerBaseGenerator(regionMap, chunk)
    local resourceGenerator = getResourceGenerator(regionMap, chunk)
    if (resourceGenerator > 0) and (getEnemyStructureCount(regionMap, chunk) == 0) then
	chunk[RESOURCE_PHEROMONE] = chunk[RESOURCE_PHEROMONE] + mMax(resourceGenerator * 100, 90)
    end
end

function pheromoneUtils.victoryScent(chunk, entityType)
    local value = BUILDING_PHEROMONES[entityType]
    if value then
	chunk[MOVEMENT_PHEROMONE] = chunk[MOVEMENT_PHEROMONE] + (value * 100000)
    end
end

function pheromoneUtils.deathScent(chunk)
    chunk[MOVEMENT_PHEROMONE] = chunk[MOVEMENT_PHEROMONE] - DEATH_PHEROMONE_GENERATOR_AMOUNT
end

function pheromoneUtils.playerScent(playerChunk)
    playerChunk[PLAYER_PHEROMONE] = playerChunk[PLAYER_PHEROMONE] + PLAYER_PHEROMONE_GENERATOR_AMOUNT
end

function pheromoneUtils.processPheromone(regionMap, chunk)

    local chunkMovement = chunk[MOVEMENT_PHEROMONE]
    local chunkBase = chunk[BASE_PHEROMONE]
    local chunkPlayer = chunk[PLAYER_PHEROMONE]
    local chunkResource = chunk[RESOURCE_PHEROMONE]
    local chunkPathRating = chunk[PATH_RATING]
    
    local tempNeighbors = getCardinalChunks(regionMap, chunk.x, chunk.y)

    local totalMovement = ((tempNeighbors[1][MOVEMENT_PHEROMONE] - chunkMovement) +
	    (tempNeighbors[2][MOVEMENT_PHEROMONE] - chunkMovement) +
	    (tempNeighbors[3][MOVEMENT_PHEROMONE] - chunkMovement) +
	    (tempNeighbors[4][MOVEMENT_PHEROMONE] - chunkMovement))
    local totalBase = ((tempNeighbors[1][BASE_PHEROMONE] - chunkBase) +
	    (tempNeighbors[2][BASE_PHEROMONE] - chunkBase) +
	    (tempNeighbors[3][BASE_PHEROMONE] - chunkBase) +
	    (tempNeighbors[4][BASE_PHEROMONE] - chunkBase))
    local totalPlayer = ((tempNeighbors[1][PLAYER_PHEROMONE] - chunkPlayer) +
	    (tempNeighbors[2][PLAYER_PHEROMONE] - chunkPlayer) +
	    (tempNeighbors[3][PLAYER_PHEROMONE] - chunkPlayer) +
	    (tempNeighbors[4][PLAYER_PHEROMONE] - chunkPlayer))
    local totalResource = ((tempNeighbors[1][RESOURCE_PHEROMONE] - chunkResource) +
	    (tempNeighbors[2][RESOURCE_PHEROMONE] - chunkResource) +
	    (tempNeighbors[3][RESOURCE_PHEROMONE] - chunkResource) +
	    (tempNeighbors[4][RESOURCE_PHEROMONE] - chunkResource))
    
    chunk[MOVEMENT_PHEROMONE] = (chunkMovement + (0.125 * totalMovement)) * MOVEMENT_PHEROMONE_PERSISTANCE * chunkPathRating
    chunk[BASE_PHEROMONE] = (chunkBase + (0.25 * totalBase)) * BASE_PHEROMONE_PERSISTANCE * chunkPathRating
    chunk[PLAYER_PHEROMONE] = (chunkPlayer + (0.25 * totalPlayer)) * PLAYER_PHEROMONE_PERSISTANCE * chunkPathRating
    chunk[RESOURCE_PHEROMONE] = (chunkResource + (0.25 * totalResource)) * RESOURCE_PHEROMONE_PERSISTANCE * chunkPathRating
end

return pheromoneUtils
