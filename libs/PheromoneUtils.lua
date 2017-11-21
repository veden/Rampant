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
local getNestCount = chunkUtils.getNestCount
local getWormCount = chunkUtils.getWormCount
local getPlayerBaseGenerator = chunkUtils.getPlayerBaseGenerator
local getResourceGenerator = chunkUtils.getResourceGenerator

-- module code

function pheromoneUtils.scents(regionMap, chunk)
    chunk[BASE_PHEROMONE] = chunk[BASE_PHEROMONE] + getPlayerBaseGenerator(regionMap, chunk)
    local resourceGenerator = getResourceGenerator(regionMap, chunk)
    if (resourceGenerator > 0) and (getNestCount(regionMap, chunk) == 0) and (getWormCount(regionMap, chunk) == 0) then
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
    
    local totalMovement = 0
    local totalBase = 0
    local totalPlayer = 0
    local totalResource = 0
    
    local tempNeighbors = getCardinalChunks(regionMap, chunk.x, chunk.y)

    local neighborChunk = tempNeighbors[1]
    totalMovement = totalMovement + (neighborChunk[MOVEMENT_PHEROMONE] - chunkMovement)
    totalBase = totalBase + (neighborChunk[BASE_PHEROMONE] - chunkBase)
    totalPlayer = totalPlayer + (neighborChunk[PLAYER_PHEROMONE] - chunkPlayer)
    totalResource = totalResource + (neighborChunk[RESOURCE_PHEROMONE] - chunkResource)

    neighborChunk = tempNeighbors[2]
    totalMovement = totalMovement + (neighborChunk[MOVEMENT_PHEROMONE] - chunkMovement)
    totalBase = totalBase + (neighborChunk[BASE_PHEROMONE] - chunkBase)
    totalPlayer = totalPlayer + (neighborChunk[PLAYER_PHEROMONE] - chunkPlayer)
    totalResource = totalResource + (neighborChunk[RESOURCE_PHEROMONE] - chunkResource)

    neighborChunk = tempNeighbors[3]
    totalMovement = totalMovement + (neighborChunk[MOVEMENT_PHEROMONE] - chunkMovement)
    totalBase = totalBase + (neighborChunk[BASE_PHEROMONE] - chunkBase)
    totalPlayer = totalPlayer + (neighborChunk[PLAYER_PHEROMONE] - chunkPlayer)
    totalResource = totalResource + (neighborChunk[RESOURCE_PHEROMONE] - chunkResource)

    neighborChunk = tempNeighbors[4]
    totalMovement = totalMovement + (neighborChunk[MOVEMENT_PHEROMONE] - chunkMovement)
    totalBase = totalBase + (neighborChunk[BASE_PHEROMONE] - chunkBase)
    totalPlayer = totalPlayer + (neighborChunk[PLAYER_PHEROMONE] - chunkPlayer)
    totalResource = totalResource + (neighborChunk[RESOURCE_PHEROMONE] - chunkResource)
    
    chunk[MOVEMENT_PHEROMONE] = (chunkMovement + (0.125 * totalMovement)) * MOVEMENT_PHEROMONE_PERSISTANCE * chunkPathRating
    chunk[BASE_PHEROMONE] = (chunkBase + (0.25 * totalBase)) * BASE_PHEROMONE_PERSISTANCE * chunkPathRating
    chunk[PLAYER_PHEROMONE] = (chunkPlayer + (0.25 * totalPlayer)) * PLAYER_PHEROMONE_PERSISTANCE * chunkPathRating
    chunk[RESOURCE_PHEROMONE] = (chunkResource + (0.25 * totalResource)) * RESOURCE_PHEROMONE_PERSISTANCE * chunkPathRating
end

return pheromoneUtils
