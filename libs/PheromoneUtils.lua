local pheromoneUtils = {}

-- imports
local mathUtils = require("MathUtils")
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

local MOVEMENT_PHEROMONE_PERSISTANCE = constants.MOVEMENT_PHEROMONE_PERSISTANCE
local BASE_PHEROMONE_PERSISTANCE = constants.BASE_PHEROMONE_PERSISTANCE
local PLAYER_PHEROMONE_PERSISTANCE = constants.PLAYER_PHEROMONE_PERSISTANCE
local RESOURCE_PHEROMONE_PERSISTANCE = constants.RESOURCE_PHEROMONE_PERSISTANCE

local DEATH_PHEROMONE_GENERATOR_AMOUNT = constants.DEATH_PHEROMONE_GENERATOR_AMOUNT

-- imported functions

local getCardinalChunks = mapUtils.getCardinalChunks
local getNeighborChunks = mapUtils.getNeighborChunks

local getEnemyStructureCount = chunkPropertyUtils.getEnemyStructureCount
local getPathRating = chunkPropertyUtils.getPathRating
local getPlayerBaseGenerator = chunkPropertyUtils.getPlayerBaseGenerator
local getResourceGenerator = chunkPropertyUtils.getResourceGenerator
local getDeathGenerator = chunkPropertyUtils.getDeathGenerator
local addDeathGenerator = chunkPropertyUtils.addDeathGenerator

local decayDeathGenerator = chunkPropertyUtils.decayDeathGenerator

local linearInterpolation = mathUtils.linearInterpolation 

-- module code

function pheromoneUtils.scents(map, chunk)
    chunk[BASE_PHEROMONE] = chunk[BASE_PHEROMONE] + (getPlayerBaseGenerator(map, chunk))
    local resourceGenerator = getResourceGenerator(map, chunk)
    local enemyCount = getEnemyStructureCount(map, chunk)
    chunk[MOVEMENT_PHEROMONE] = chunk[MOVEMENT_PHEROMONE] - (getDeathGenerator(map, chunk))
    
    if (resourceGenerator > 0) and (enemyCount == 0) then
	chunk[RESOURCE_PHEROMONE] = chunk[RESOURCE_PHEROMONE] + (linearInterpolation(resourceGenerator, 9000, 10000))
    end
end

function pheromoneUtils.victoryScent(map, chunk, entityType)
    local value = BUILDING_PHEROMONES[entityType]
    if value then
	-- chunk[MOVEMENT_PHEROMONE] = chunk[MOVEMENT_PHEROMONE] + (value * 1000)
	addDeathGenerator(map, chunk, -value)
	chunk[MOVEMENT_PHEROMONE] = chunk[MOVEMENT_PHEROMONE] + (value)
    end
end

function pheromoneUtils.deathScent(map, chunk)
    chunk[MOVEMENT_PHEROMONE] = chunk[MOVEMENT_PHEROMONE] - (DEATH_PHEROMONE_GENERATOR_AMOUNT * 2)
    addDeathGenerator(map, chunk, DEATH_PHEROMONE_GENERATOR_AMOUNT)
    -- chunk[MOVEMENT_PHEROMONE] = chunk[MOVEMENT_PHEROMONE] - DEATH_PHEROMONE_GENERATOR_AMOUNT
end

function pheromoneUtils.playerScent(playerChunk)
    playerChunk[PLAYER_PHEROMONE] = playerChunk[PLAYER_PHEROMONE] + PLAYER_PHEROMONE_GENERATOR_AMOUNT
end

function pheromoneUtils.commitPheromone(map, chunk, staging)
    chunk[MOVEMENT_PHEROMONE] = staging[MOVEMENT_PHEROMONE]
    chunk[BASE_PHEROMONE] = staging[BASE_PHEROMONE]
    chunk[PLAYER_PHEROMONE] = staging[PLAYER_PHEROMONE]
    chunk[RESOURCE_PHEROMONE] = staging[RESOURCE_PHEROMONE]
    
    decayDeathGenerator(map, chunk)
end

function pheromoneUtils.processPheromone(map, chunk, staging)
    
    local chunkMovement = chunk[MOVEMENT_PHEROMONE]
    local chunkBase = chunk[BASE_PHEROMONE]
    local chunkPlayer = chunk[PLAYER_PHEROMONE]
    local chunkResource = chunk[RESOURCE_PHEROMONE]
    local chunkPathRating = getPathRating(map, chunk)

    local clear = (getEnemyStructureCount(map, chunk) == 0)
    
    local tempNeighbors = getNeighborChunks(map, chunk.x, chunk.y)

    local movementTotal = 0
    local baseTotal = 0
    local playerTotal = 0
    local resourceTotal = 0
    
    local neighbor = tempNeighbors[1]
    if not neighbor.name then
	movementTotal = movementTotal + (neighbor[MOVEMENT_PHEROMONE] - chunkMovement)
	baseTotal = baseTotal + (neighbor[BASE_PHEROMONE] - chunkBase)
	playerTotal = playerTotal + neighbor[PLAYER_PHEROMONE] - chunkPlayer
	resourceTotal = resourceTotal + (neighbor[RESOURCE_PHEROMONE] - chunkResource)
    end

    neighbor = tempNeighbors[2]
    if not neighbor.name then
	movementTotal = movementTotal + (neighbor[MOVEMENT_PHEROMONE] - chunkMovement)
	baseTotal = baseTotal + (neighbor[BASE_PHEROMONE] - chunkBase)
	playerTotal = playerTotal + (neighbor[PLAYER_PHEROMONE] - chunkPlayer)
	resourceTotal = resourceTotal + (neighbor[RESOURCE_PHEROMONE] - chunkResource)
    end

    neighbor = tempNeighbors[3]
    if not neighbor.name then
	movementTotal = movementTotal + (neighbor[MOVEMENT_PHEROMONE] - chunkMovement)
	baseTotal = baseTotal + (neighbor[BASE_PHEROMONE] - chunkBase)
	playerTotal = playerTotal + (neighbor[PLAYER_PHEROMONE] - chunkPlayer)
	resourceTotal = resourceTotal + (neighbor[RESOURCE_PHEROMONE] - chunkResource)
    end
    
    neighbor = tempNeighbors[4]
    if not neighbor.name then
	movementTotal = movementTotal + (neighbor[MOVEMENT_PHEROMONE] - chunkMovement)
	baseTotal = baseTotal + (neighbor[BASE_PHEROMONE] - chunkBase)
	playerTotal = playerTotal + (neighbor[PLAYER_PHEROMONE] - chunkPlayer)
	resourceTotal = resourceTotal + (neighbor[RESOURCE_PHEROMONE] - chunkResource)
    end

    neighbor = tempNeighbors[5]
    if not neighbor.name then
	movementTotal = movementTotal + (neighbor[MOVEMENT_PHEROMONE] - chunkMovement)
	baseTotal = baseTotal + (neighbor[BASE_PHEROMONE] - chunkBase)
	playerTotal = playerTotal + neighbor[PLAYER_PHEROMONE] - chunkPlayer
	resourceTotal = resourceTotal + (neighbor[RESOURCE_PHEROMONE] - chunkResource)
    end

    neighbor = tempNeighbors[6]
    if not neighbor.name then
	movementTotal = movementTotal + (neighbor[MOVEMENT_PHEROMONE] - chunkMovement)
	baseTotal = baseTotal + (neighbor[BASE_PHEROMONE] - chunkBase)
	playerTotal = playerTotal + (neighbor[PLAYER_PHEROMONE] - chunkPlayer)
	resourceTotal = resourceTotal + (neighbor[RESOURCE_PHEROMONE] - chunkResource)
    end

    neighbor = tempNeighbors[7]
    if not neighbor.name then
	movementTotal = movementTotal + (neighbor[MOVEMENT_PHEROMONE] - chunkMovement)
	baseTotal = baseTotal + (neighbor[BASE_PHEROMONE] - chunkBase)
	playerTotal = playerTotal + (neighbor[PLAYER_PHEROMONE] - chunkPlayer)
	resourceTotal = resourceTotal + (neighbor[RESOURCE_PHEROMONE] - chunkResource)
    end
    
    neighbor = tempNeighbors[8]
    if not neighbor.name then
	movementTotal = movementTotal + (neighbor[MOVEMENT_PHEROMONE] - chunkMovement)
	baseTotal = baseTotal + (neighbor[BASE_PHEROMONE] - chunkBase)
	playerTotal = playerTotal + (neighbor[PLAYER_PHEROMONE] - chunkPlayer)
	resourceTotal = resourceTotal + (neighbor[RESOURCE_PHEROMONE] - chunkResource)
    end
    
    staging[MOVEMENT_PHEROMONE] = (chunkMovement + (0.125 * movementTotal)) * MOVEMENT_PHEROMONE_PERSISTANCE * chunkPathRating
    staging[BASE_PHEROMONE] = (chunkBase + (0.125 * baseTotal)) * BASE_PHEROMONE_PERSISTANCE * chunkPathRating
    staging[PLAYER_PHEROMONE] = (chunkPlayer + (0.125 * playerTotal)) * PLAYER_PHEROMONE_PERSISTANCE * chunkPathRating
    if clear then
	staging[RESOURCE_PHEROMONE] = (chunkResource + (0.125 * resourceTotal)) * RESOURCE_PHEROMONE_PERSISTANCE * chunkPathRating
    else
	staging[RESOURCE_PHEROMONE] = (chunkResource + (0.125 * resourceTotal)) * 0.01
    end
end

return pheromoneUtils
