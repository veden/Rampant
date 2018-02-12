local pheromoneUtils = {}

-- imports

local mapUtils = require("MapUtils")
local constants = require("Constants")
local chunkPropertyUtils = require("ChunkPropertyUtils")

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
local getEnemyStructureCount = chunkPropertyUtils.getEnemyStructureCount
local getPlayerBaseGenerator = chunkPropertyUtils.getPlayerBaseGenerator
local getResourceGenerator = chunkPropertyUtils.getResourceGenerator

-- module code

function pheromoneUtils.scents(map, chunk)
    chunk[BASE_PHEROMONE] = chunk[BASE_PHEROMONE] + getPlayerBaseGenerator(map, chunk)
    local resourceGenerator = getResourceGenerator(map, chunk)
    local enemyCount = getEnemyStructureCount(map, chunk)
    if (resourceGenerator > 0) and (enemyCount == 0) then
	chunk[RESOURCE_PHEROMONE] = chunk[RESOURCE_PHEROMONE] + mMax(resourceGenerator * 1000, 900)
    end
    if (enemyCount > 0) then
	chunk[RESOURCE_PHEROMONE] = chunk[RESOURCE_PHEROMONE] - enemyCount
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

function pheromoneUtils.processPheromone(map, chunk)

    local chunkMovement = chunk[MOVEMENT_PHEROMONE]
    local chunkBase = chunk[BASE_PHEROMONE]
    local chunkPlayer = chunk[PLAYER_PHEROMONE]
    local chunkResource = chunk[RESOURCE_PHEROMONE]
    local chunkPathRating = chunk[PATH_RATING]
    
    local tempNeighbors = getCardinalChunks(map, chunk.x, chunk.y)

    local movementTotal = 0
    local baseTotal = 0
    local playerTotal = 0
    local resourceTotal = 0
    
    if not tempNeighbors[1].name then
	movementTotal = (movementTotal + tempNeighbors[1][MOVEMENT_PHEROMONE] - chunkMovement)
	baseTotal = baseTotal + (tempNeighbors[1][BASE_PHEROMONE] - chunkBase)
	playerTotal = playerTotal + tempNeighbors[1][PLAYER_PHEROMONE] - chunkPlayer
	resourceTotal = resourceTotal + (tempNeighbors[1][RESOURCE_PHEROMONE] - chunkResource)
    end

    if not tempNeighbors[2].name then
	movementTotal = movementTotal + (tempNeighbors[2][MOVEMENT_PHEROMONE] - chunkMovement)
	baseTotal = baseTotal + (tempNeighbors[2][BASE_PHEROMONE] - chunkBase)
	playerTotal = playerTotal + (tempNeighbors[2][PLAYER_PHEROMONE] - chunkPlayer)
	resourceTotal = resourceTotal + (tempNeighbors[2][RESOURCE_PHEROMONE] - chunkResource)
    end

    if not tempNeighbors[3].name then
	movementTotal = movementTotal + (tempNeighbors[3][MOVEMENT_PHEROMONE] - chunkMovement)
	baseTotal = baseTotal + (tempNeighbors[3][BASE_PHEROMONE] - chunkBase)
	playerTotal = playerTotal + (tempNeighbors[3][PLAYER_PHEROMONE] - chunkPlayer)
	resourceTotal = resourceTotal + (tempNeighbors[3][RESOURCE_PHEROMONE] - chunkResource)
    end
    
    if not tempNeighbors[4].name then
	movementTotal = movementTotal + (tempNeighbors[4][MOVEMENT_PHEROMONE] - chunkMovement)
	baseTotal = baseTotal + (tempNeighbors[4][BASE_PHEROMONE] - chunkBase)
	playerTotal = playerTotal + (tempNeighbors[4][PLAYER_PHEROMONE] - chunkPlayer)
	resourceTotal = resourceTotal + (tempNeighbors[4][RESOURCE_PHEROMONE] - chunkResource)
    end
    
    chunk[MOVEMENT_PHEROMONE] = (chunkMovement + (0.125 * movementTotal)) * MOVEMENT_PHEROMONE_PERSISTANCE * chunkPathRating
    chunk[BASE_PHEROMONE] = (chunkBase + (0.35 * baseTotal)) * BASE_PHEROMONE_PERSISTANCE * chunkPathRating
    chunk[PLAYER_PHEROMONE] = (chunkPlayer + (0.25 * playerTotal)) * PLAYER_PHEROMONE_PERSISTANCE * chunkPathRating
    chunk[RESOURCE_PHEROMONE] = (chunkResource + (0.35 * resourceTotal)) * RESOURCE_PHEROMONE_PERSISTANCE * chunkPathRating
end

return pheromoneUtils
