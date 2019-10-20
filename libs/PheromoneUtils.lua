if pheromoneUtilsG then
    return pheromoneUtilsG
end
local pheromoneUtils = {}

-- imports
local mathUtils = require("MathUtils")
local mapUtils = require("MapUtils")
local constants = require("Constants")
local chunkPropertyUtils = require("ChunkPropertyUtils")

-- constants

local CHUNK_TICK = constants.CHUNK_TICK
local NEIGHBOR_DIVIDER = constants.NEIGHBOR_DIVIDER

local CHUNK_ALL_DIRECTIONS = constants.CHUNK_ALL_DIRECTIONS
local CHUNK_NORTH_SOUTH = constants.CHUNK_NORTH_SOUTH
local CHUNK_EAST_WEST = constants.CHUNK_EAST_WEST

local MOVEMENT_PHEROMONE = constants.MOVEMENT_PHEROMONE
local BASE_PHEROMONE = constants.BASE_PHEROMONE
local PLAYER_PHEROMONE = constants.PLAYER_PHEROMONE
local RESOURCE_PHEROMONE = constants.RESOURCE_PHEROMONE

local BUILDING_PHEROMONES = constants.BUILDING_PHEROMONES
local VICTORY_SCENT = constants.VICTORY_SCENT
local SENTINEL_IMPASSABLE_CHUNK = constants.SENTINEL_IMPASSABLE_CHUNK

local PLAYER_PHEROMONE_GENERATOR_AMOUNT = constants.PLAYER_PHEROMONE_GENERATOR_AMOUNT

local MOVEMENT_PHEROMONE_PERSISTANCE = constants.MOVEMENT_PHEROMONE_PERSISTANCE
local BASE_PHEROMONE_PERSISTANCE = constants.BASE_PHEROMONE_PERSISTANCE
local PLAYER_PHEROMONE_PERSISTANCE = constants.PLAYER_PHEROMONE_PERSISTANCE
local RESOURCE_PHEROMONE_PERSISTANCE = constants.RESOURCE_PHEROMONE_PERSISTANCE

local DEATH_PHEROMONE_GENERATOR_AMOUNT = constants.DEATH_PHEROMONE_GENERATOR_AMOUNT
local DOUBLE_DEATH_PHEROMONE_GENERATOR_AMOUNT = constants.DOUBLE_DEATH_PHEROMONE_GENERATOR_AMOUNT

-- imported functions

local getNeighborChunks = mapUtils.getNeighborChunks

local getEnemyStructureCount = chunkPropertyUtils.getEnemyStructureCount
local getPathRating = chunkPropertyUtils.getPathRating
local getPassable = chunkPropertyUtils.getPassable
local getPlayerBaseGenerator = chunkPropertyUtils.getPlayerBaseGenerator
local getResourceGenerator = chunkPropertyUtils.getResourceGenerator
local getDeathGenerator = chunkPropertyUtils.getDeathGenerator
local addDeathGenerator = chunkPropertyUtils.addDeathGenerator

local decayDeathGenerator = chunkPropertyUtils.decayDeathGenerator

local linearInterpolation = mathUtils.linearInterpolation

-- module code

function pheromoneUtils.victoryScent(map, chunk, entityType)
    local value = VICTORY_SCENT[entityType]
    if value then
        addDeathGenerator(map, chunk, -value)
        chunk[MOVEMENT_PHEROMONE] = chunk[MOVEMENT_PHEROMONE] + value
    end
end

function pheromoneUtils.deathScent(map, chunk)
    chunk[MOVEMENT_PHEROMONE] = chunk[MOVEMENT_PHEROMONE] - DOUBLE_DEATH_PHEROMONE_GENERATOR_AMOUNT
    addDeathGenerator(map, chunk, DEATH_PHEROMONE_GENERATOR_AMOUNT)
end

function pheromoneUtils.playerScent(playerChunk)
    playerChunk[PLAYER_PHEROMONE] = playerChunk[PLAYER_PHEROMONE] + PLAYER_PHEROMONE_GENERATOR_AMOUNT
end

function pheromoneUtils.commitPheromone(map, chunk, staging, tick)
    chunk[CHUNK_TICK] = tick

    local resourceGenerator = getResourceGenerator(map, chunk)

    decayDeathGenerator(map, chunk)

    chunk[MOVEMENT_PHEROMONE] = staging[MOVEMENT_PHEROMONE] - getDeathGenerator(map, chunk)
    chunk[BASE_PHEROMONE] = staging[BASE_PHEROMONE] + getPlayerBaseGenerator(map, chunk)
    chunk[PLAYER_PHEROMONE] = staging[PLAYER_PHEROMONE]
    if (resourceGenerator > 0) and (getEnemyStructureCount(map, chunk) == 0) then
       chunk[RESOURCE_PHEROMONE] = staging[RESOURCE_PHEROMONE] + (linearInterpolation(resourceGenerator, 15000, 20000))
    else
        chunk[RESOURCE_PHEROMONE] = staging[RESOURCE_PHEROMONE]
    end
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
    
    local neighborCount = 0

    local neighbor
    local neighborPass

    local chunkPass = getPassable(map, chunk)

    if (chunkPass == CHUNK_ALL_DIRECTIONS) then
        neighbor = tempNeighbors[2]
        neighborPass = getPassable(map, neighbor)
        if ((neighbor ~= SENTINEL_IMPASSABLE_CHUNK) and
            ((neighborPass == CHUNK_ALL_DIRECTIONS) or (neighborPass == CHUNK_NORTH_SOUTH))) then
            neighborCount = neighborCount + 1
            movementTotal = movementTotal + (neighbor[MOVEMENT_PHEROMONE] - chunkMovement)
            baseTotal = baseTotal + (neighbor[BASE_PHEROMONE] - chunkBase)
            playerTotal = playerTotal + (neighbor[PLAYER_PHEROMONE] - chunkPlayer)
            resourceTotal = resourceTotal + (neighbor[RESOURCE_PHEROMONE] - chunkResource)
        end

        neighbor = tempNeighbors[7]
        neighborPass = getPassable(map, neighbor)
        if ((neighbor ~= SENTINEL_IMPASSABLE_CHUNK) and
            ((neighborPass == CHUNK_ALL_DIRECTIONS) or (neighborPass == CHUNK_NORTH_SOUTH))) then
            neighborCount = neighborCount + 1
            movementTotal = movementTotal + (neighbor[MOVEMENT_PHEROMONE] - chunkMovement)
            baseTotal = baseTotal + (neighbor[BASE_PHEROMONE] - chunkBase)
            playerTotal = playerTotal + (neighbor[PLAYER_PHEROMONE] - chunkPlayer)
            resourceTotal = resourceTotal + (neighbor[RESOURCE_PHEROMONE] - chunkResource)
        end

        neighbor = tempNeighbors[4]
        neighborPass = getPassable(map, neighbor)
        if ((neighbor ~= SENTINEL_IMPASSABLE_CHUNK) and
            ((neighborPass == CHUNK_ALL_DIRECTIONS) or (neighborPass == CHUNK_EAST_WEST))) then
            neighborCount = neighborCount + 1
            movementTotal = movementTotal + (neighbor[MOVEMENT_PHEROMONE] - chunkMovement)
            baseTotal = baseTotal + (neighbor[BASE_PHEROMONE] - chunkBase)
            playerTotal = playerTotal + (neighbor[PLAYER_PHEROMONE] - chunkPlayer)
            resourceTotal = resourceTotal + (neighbor[RESOURCE_PHEROMONE] - chunkResource)
        end

        neighbor = tempNeighbors[5]
        neighborPass = getPassable(map, neighbor)
        if ((neighbor ~= SENTINEL_IMPASSABLE_CHUNK) and
            ((neighborPass == CHUNK_ALL_DIRECTIONS) or (neighborPass == CHUNK_EAST_WEST))) then
            neighborCount = neighborCount + 1
            movementTotal = movementTotal + (neighbor[MOVEMENT_PHEROMONE] - chunkMovement)
            baseTotal = baseTotal + (neighbor[BASE_PHEROMONE] - chunkBase)
            playerTotal = playerTotal + neighbor[PLAYER_PHEROMONE] - chunkPlayer
            resourceTotal = resourceTotal + (neighbor[RESOURCE_PHEROMONE] - chunkResource)
        end

        neighbor = tempNeighbors[1]
        neighborPass = getPassable(map, neighbor)
        if (neighbor ~= SENTINEL_IMPASSABLE_CHUNK) and (neighborPass == CHUNK_ALL_DIRECTIONS) then
            neighborCount = neighborCount + 1
            movementTotal = movementTotal + (neighbor[MOVEMENT_PHEROMONE] - chunkMovement)
            baseTotal = baseTotal + (neighbor[BASE_PHEROMONE] - chunkBase)
            playerTotal = playerTotal + neighbor[PLAYER_PHEROMONE] - chunkPlayer
            resourceTotal = resourceTotal + (neighbor[RESOURCE_PHEROMONE] - chunkResource)
        end

        neighbor = tempNeighbors[3]
        neighborPass = getPassable(map, neighbor)
        if (neighbor ~= SENTINEL_IMPASSABLE_CHUNK) and (neighborPass == CHUNK_ALL_DIRECTIONS) then
            neighborCount = neighborCount + 1
            movementTotal = movementTotal + (neighbor[MOVEMENT_PHEROMONE] - chunkMovement)
            baseTotal = baseTotal + (neighbor[BASE_PHEROMONE] - chunkBase)
            playerTotal = playerTotal + (neighbor[PLAYER_PHEROMONE] - chunkPlayer)
            resourceTotal = resourceTotal + (neighbor[RESOURCE_PHEROMONE] - chunkResource)
        end

        neighbor = tempNeighbors[6]
        neighborPass = getPassable(map, neighbor)
        if (neighbor ~= SENTINEL_IMPASSABLE_CHUNK) and (neighborPass == CHUNK_ALL_DIRECTIONS) then
            neighborCount = neighborCount + 1
            movementTotal = movementTotal + (neighbor[MOVEMENT_PHEROMONE] - chunkMovement)
            baseTotal = baseTotal + (neighbor[BASE_PHEROMONE] - chunkBase)
            playerTotal = playerTotal + (neighbor[PLAYER_PHEROMONE] - chunkPlayer)
            resourceTotal = resourceTotal + (neighbor[RESOURCE_PHEROMONE] - chunkResource)
        end

        neighbor = tempNeighbors[8]
        neighborPass = getPassable(map, neighbor)
        if (neighbor ~= SENTINEL_IMPASSABLE_CHUNK) and (neighborPass == CHUNK_ALL_DIRECTIONS) then
            neighborCount = neighborCount + 1
            movementTotal = movementTotal + (neighbor[MOVEMENT_PHEROMONE] - chunkMovement)
            baseTotal = baseTotal + (neighbor[BASE_PHEROMONE] - chunkBase)
            playerTotal = playerTotal + (neighbor[PLAYER_PHEROMONE] - chunkPlayer)
            resourceTotal = resourceTotal + (neighbor[RESOURCE_PHEROMONE] - chunkResource)
        end

    elseif (chunkPass == CHUNK_EAST_WEST) then

        neighbor = tempNeighbors[4]
        neighborPass = getPassable(map, neighbor)
        if ((neighbor ~= SENTINEL_IMPASSABLE_CHUNK) and
            ((neighborPass == CHUNK_ALL_DIRECTIONS) or (neighborPass == CHUNK_EAST_WEST))) then
            neighborCount = neighborCount + 1
            movementTotal = movementTotal + (neighbor[MOVEMENT_PHEROMONE] - chunkMovement)
            baseTotal = baseTotal + (neighbor[BASE_PHEROMONE] - chunkBase)
            playerTotal = playerTotal + (neighbor[PLAYER_PHEROMONE] - chunkPlayer)
            resourceTotal = resourceTotal + (neighbor[RESOURCE_PHEROMONE] - chunkResource)
        end

        neighbor = tempNeighbors[5]
        neighborPass = getPassable(map, neighbor)
        if ((neighbor ~= SENTINEL_IMPASSABLE_CHUNK) and
            ((neighborPass == CHUNK_ALL_DIRECTIONS) or (neighborPass == CHUNK_EAST_WEST))) then
            neighborCount = neighborCount + 1
            movementTotal = movementTotal + (neighbor[MOVEMENT_PHEROMONE] - chunkMovement)
            baseTotal = baseTotal + (neighbor[BASE_PHEROMONE] - chunkBase)
            playerTotal = playerTotal + neighbor[PLAYER_PHEROMONE] - chunkPlayer
            resourceTotal = resourceTotal + (neighbor[RESOURCE_PHEROMONE] - chunkResource)
        end

    elseif (chunkPass == CHUNK_NORTH_SOUTH) then

        neighbor = tempNeighbors[2]
        neighborPass = getPassable(map, neighbor)
        if ((neighbor ~= SENTINEL_IMPASSABLE_CHUNK) and
            ((neighborPass == CHUNK_ALL_DIRECTIONS) or (neighborPass == CHUNK_NORTH_SOUTH))) then
            neighborCount = neighborCount + 1
            movementTotal = movementTotal + (neighbor[MOVEMENT_PHEROMONE] - chunkMovement)
            baseTotal = baseTotal + (neighbor[BASE_PHEROMONE] - chunkBase)
            playerTotal = playerTotal + (neighbor[PLAYER_PHEROMONE] - chunkPlayer)
            resourceTotal = resourceTotal + (neighbor[RESOURCE_PHEROMONE] - chunkResource)
        end

        neighbor = tempNeighbors[7]
        neighborPass = getPassable(map, neighbor)
        if ((neighbor ~= SENTINEL_IMPASSABLE_CHUNK) and
            ((neighborPass == CHUNK_ALL_DIRECTIONS) or (neighborPass == CHUNK_NORTH_SOUTH))) then
            neighborCount = neighborCount + 1
            movementTotal = movementTotal + (neighbor[MOVEMENT_PHEROMONE] - chunkMovement)
            baseTotal = baseTotal + (neighbor[BASE_PHEROMONE] - chunkBase)
            playerTotal = playerTotal + (neighbor[PLAYER_PHEROMONE] - chunkPlayer)
            resourceTotal = resourceTotal + (neighbor[RESOURCE_PHEROMONE] - chunkResource)
        end
    end

    local neighborDiv = 0
    if neighborCount ~= 0 then
        neighborDiv = NEIGHBOR_DIVIDER[neighborCount]
    end

    staging[MOVEMENT_PHEROMONE] = (chunkMovement + (neighborDiv * movementTotal)) * MOVEMENT_PHEROMONE_PERSISTANCE * chunkPathRating
    staging[BASE_PHEROMONE] = (chunkBase + (neighborDiv * baseTotal)) * BASE_PHEROMONE_PERSISTANCE * chunkPathRating
    staging[PLAYER_PHEROMONE] = (chunkPlayer + (neighborDiv * playerTotal)) * PLAYER_PHEROMONE_PERSISTANCE * chunkPathRating
    if clear then
        staging[RESOURCE_PHEROMONE] = (chunkResource + (neighborDiv * resourceTotal)) * RESOURCE_PHEROMONE_PERSISTANCE * chunkPathRating
    else
        staging[RESOURCE_PHEROMONE] = (chunkResource + (neighborDiv * resourceTotal)) * 0.01
    end
end

pheromoneUtilsG = pheromoneUtils
return pheromoneUtils
