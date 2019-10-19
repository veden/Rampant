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

local CHUNK_ALL_DIRECTIONS = constants.CHUNK_ALL_DIRECTIONS
local CHUNK_NORTH_SOUTH = constants.CHUNK_NORTH_SOUTH
local CHUNK_EAST_WEST = constants.CHUNK_EAST_WEST

local MOVEMENT_PHEROMONE = constants.MOVEMENT_PHEROMONE
local BASE_PHEROMONE = constants.BASE_PHEROMONE
local PLAYER_PHEROMONE = constants.PLAYER_PHEROMONE
local RESOURCE_PHEROMONE = constants.RESOURCE_PHEROMONE

local BUILDING_PHEROMONES = constants.BUILDING_PHEROMONES
local SENTINEL_IMPASSABLE_CHUNK = constants.SENTINEL_IMPASSABLE_CHUNK

local PLAYER_PHEROMONE_GENERATOR_AMOUNT = constants.PLAYER_PHEROMONE_GENERATOR_AMOUNT

local MOVEMENT_PHEROMONE_PERSISTANCE = constants.MOVEMENT_PHEROMONE_PERSISTANCE
local BASE_PHEROMONE_PERSISTANCE = constants.BASE_PHEROMONE_PERSISTANCE
local PLAYER_PHEROMONE_PERSISTANCE = constants.PLAYER_PHEROMONE_PERSISTANCE
local RESOURCE_PHEROMONE_PERSISTANCE = constants.RESOURCE_PHEROMONE_PERSISTANCE

local DEATH_PHEROMONE_GENERATOR_AMOUNT = constants.DEATH_PHEROMONE_GENERATOR_AMOUNT

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
    local value = BUILDING_PHEROMONES[entityType]
    if value then
        local scaledVal = (value * 3)
        addDeathGenerator(map, chunk, -scaledVal)
        chunk[MOVEMENT_PHEROMONE] = chunk[MOVEMENT_PHEROMONE] + scaledVal
    end
end

function pheromoneUtils.deathScent(map, chunk)
    chunk[MOVEMENT_PHEROMONE] = chunk[MOVEMENT_PHEROMONE] - (DEATH_PHEROMONE_GENERATOR_AMOUNT * 2)
    addDeathGenerator(map, chunk, DEATH_PHEROMONE_GENERATOR_AMOUNT)
end

function pheromoneUtils.playerScent(playerChunk)
    playerChunk[PLAYER_PHEROMONE] = playerChunk[PLAYER_PHEROMONE] + PLAYER_PHEROMONE_GENERATOR_AMOUNT
end

function pheromoneUtils.commitPheromone(map, chunk, staging, tick)
    chunk[CHUNK_TICK] = tick
    
    chunk[MOVEMENT_PHEROMONE] = staging[MOVEMENT_PHEROMONE]
    chunk[BASE_PHEROMONE] = staging[BASE_PHEROMONE]
    chunk[PLAYER_PHEROMONE] = staging[PLAYER_PHEROMONE]
    chunk[RESOURCE_PHEROMONE] = staging[RESOURCE_PHEROMONE]

    decayDeathGenerator(map, chunk)

    chunk[BASE_PHEROMONE] = chunk[BASE_PHEROMONE] + (getPlayerBaseGenerator(map, chunk))
    local resourceGenerator = getResourceGenerator(map, chunk)
    local enemyCount = getEnemyStructureCount(map, chunk)
    chunk[MOVEMENT_PHEROMONE] = chunk[MOVEMENT_PHEROMONE] - (getDeathGenerator(map, chunk))

    if (resourceGenerator > 0) and (enemyCount == 0) then
        chunk[RESOURCE_PHEROMONE] = chunk[RESOURCE_PHEROMONE] + (linearInterpolation(resourceGenerator, 15000, 20000))
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

    local neighborFlagNW = 0
    local neighborFlagNE = 0
    local neighborFlagSW = 0
    local neighborFlagSE = 0

    local neighborCount = 0

    local neighbor1
    local neighbor2
    local neighbor3
    local neighbor4
    local neighbor5
    local neighbor6
    local neighbor7
    local neighbor8

    local neighborPass1
    local neighborPass2
    local neighborPass3
    local neighborPass4
    local neighborPass5
    local neighborPass6
    local neighborPass7
    local neighborPass8
    
    local chunkPass = getPassable(map, chunk)
    
    if (chunkPass == CHUNK_ALL_DIRECTIONS) then
        neighbor2 = tempNeighbors[2]
        neighborPass2 = getPassable(map, neighbor2)
        if ((neighbor2 ~= SENTINEL_IMPASSABLE_CHUNK) and
            ((neighborPass2 == CHUNK_ALL_DIRECTIONS) or (neighborPass2 == CHUNK_NORTH_SOUTH))) then
            neighborCount = neighborCount + 1
            movementTotal = movementTotal + (neighbor2[MOVEMENT_PHEROMONE] - chunkMovement)
            baseTotal = baseTotal + (neighbor2[BASE_PHEROMONE] - chunkBase)
            playerTotal = playerTotal + (neighbor2[PLAYER_PHEROMONE] - chunkPlayer)
            resourceTotal = resourceTotal + (neighbor2[RESOURCE_PHEROMONE] - chunkResource)
        end

        neighbor7 = tempNeighbors[7]
        neighborPass7 = getPassable(map, neighbor7)
        if ((neighbor7 ~= SENTINEL_IMPASSABLE_CHUNK) and
            ((neighborPass7 == CHUNK_ALL_DIRECTIONS) or (neighborPass7 == CHUNK_NORTH_SOUTH))) then
            neighborCount = neighborCount + 1
            movementTotal = movementTotal + (neighbor7[MOVEMENT_PHEROMONE] - chunkMovement)
            baseTotal = baseTotal + (neighbor7[BASE_PHEROMONE] - chunkBase)
            playerTotal = playerTotal + (neighbor7[PLAYER_PHEROMONE] - chunkPlayer)
            resourceTotal = resourceTotal + (neighbor7[RESOURCE_PHEROMONE] - chunkResource)
        end

        neighbor4 = tempNeighbors[4]
        neighborPass4 = getPassable(map, neighbor4)
        if ((neighbor4 ~= SENTINEL_IMPASSABLE_CHUNK) and
            ((neighborPass4 == CHUNK_ALL_DIRECTIONS) or (neighborPass4 == CHUNK_EAST_WEST))) then
            neighborCount = neighborCount + 1
            movementTotal = movementTotal + (neighbor4[MOVEMENT_PHEROMONE] - chunkMovement)
            baseTotal = baseTotal + (neighbor4[BASE_PHEROMONE] - chunkBase)
            playerTotal = playerTotal + (neighbor4[PLAYER_PHEROMONE] - chunkPlayer)
            resourceTotal = resourceTotal + (neighbor4[RESOURCE_PHEROMONE] - chunkResource)
        end

        neighbor5 = tempNeighbors[5]
        neighborPass5 = getPassable(map, neighbor5)
        if ((neighbor5 ~= SENTINEL_IMPASSABLE_CHUNK) and
            ((neighborPass5 == CHUNK_ALL_DIRECTIONS) or (neighborPass5 == CHUNK_EAST_WEST))) then
            neighborCount = neighborCount + 1
            movementTotal = movementTotal + (neighbor5[MOVEMENT_PHEROMONE] - chunkMovement)
            baseTotal = baseTotal + (neighbor5[BASE_PHEROMONE] - chunkBase)
            playerTotal = playerTotal + neighbor5[PLAYER_PHEROMONE] - chunkPlayer
            resourceTotal = resourceTotal + (neighbor5[RESOURCE_PHEROMONE] - chunkResource)                
        end

        neighbor1 = tempNeighbors[1]
        neighborPass1 = getPassable(map, neighbor1)
        if (neighbor1 ~= SENTINEL_IMPASSABLE_CHUNK) and (neighborPass1 == CHUNK_ALL_DIRECTIONS) then
            neighborCount = neighborCount + 1
            movementTotal = movementTotal + (neighbor1[MOVEMENT_PHEROMONE] - chunkMovement)
            baseTotal = baseTotal + (neighbor1[BASE_PHEROMONE] - chunkBase)
            playerTotal = playerTotal + neighbor1[PLAYER_PHEROMONE] - chunkPlayer
            resourceTotal = resourceTotal + (neighbor1[RESOURCE_PHEROMONE] - chunkResource)
        end

        neighbor3 = tempNeighbors[3]
        neighborPass3 = getPassable(map, neighbor3)        
        if (neighbor3 ~= SENTINEL_IMPASSABLE_CHUNK) and (neighborPass3 == CHUNK_ALL_DIRECTIONS) then
            neighborCount = neighborCount + 1
            movementTotal = movementTotal + (neighbor3[MOVEMENT_PHEROMONE] - chunkMovement)
            baseTotal = baseTotal + (neighbor3[BASE_PHEROMONE] - chunkBase)
            playerTotal = playerTotal + (neighbor3[PLAYER_PHEROMONE] - chunkPlayer)
            resourceTotal = resourceTotal + (neighbor3[RESOURCE_PHEROMONE] - chunkResource)
        end

        neighbor6 = tempNeighbors[6]
        neighborPass6 = getPassable(map, neighbor6)
        if (neighbor6 ~= SENTINEL_IMPASSABLE_CHUNK) and (neighborPass6 == CHUNK_ALL_DIRECTIONS) then
            neighborCount = neighborCount + 1
            movementTotal = movementTotal + (neighbor6[MOVEMENT_PHEROMONE] - chunkMovement)
            baseTotal = baseTotal + (neighbor6[BASE_PHEROMONE] - chunkBase)
            playerTotal = playerTotal + (neighbor6[PLAYER_PHEROMONE] - chunkPlayer)
            resourceTotal = resourceTotal + (neighbor6[RESOURCE_PHEROMONE] - chunkResource)
        end

        neighbor8 = tempNeighbors[8]
        neighborPass8 = getPassable(map, neighbor8)
        if (neighbor8 ~= SENTINEL_IMPASSABLE_CHUNK) and (neighborPass8 == CHUNK_ALL_DIRECTIONS) then
            neighborCount = neighborCount + 1
            movementTotal = movementTotal + (neighbor8[MOVEMENT_PHEROMONE] - chunkMovement)
            baseTotal = baseTotal + (neighbor8[BASE_PHEROMONE] - chunkBase)
            playerTotal = playerTotal + (neighbor8[PLAYER_PHEROMONE] - chunkPlayer)
            resourceTotal = resourceTotal + (neighbor8[RESOURCE_PHEROMONE] - chunkResource)
        end

    elseif (chunkPass == CHUNK_EAST_WEST) then

        neighbor4 = tempNeighbors[4]
        neighborPass4 = getPassable(map, neighbor4)
        if ((neighbor4 ~= SENTINEL_IMPASSABLE_CHUNK) and
            ((neighborPass4 == CHUNK_ALL_DIRECTIONS) or (neighborPass4 == CHUNK_EAST_WEST))) then
            neighborCount = neighborCount + 1
            movementTotal = movementTotal + (neighbor4[MOVEMENT_PHEROMONE] - chunkMovement)
            baseTotal = baseTotal + (neighbor4[BASE_PHEROMONE] - chunkBase)
            playerTotal = playerTotal + (neighbor4[PLAYER_PHEROMONE] - chunkPlayer)
            resourceTotal = resourceTotal + (neighbor4[RESOURCE_PHEROMONE] - chunkResource)
        end

        neighbor5 = tempNeighbors[5]
        neighborPass5 = getPassable(map, neighbor5)
        if ((neighbor5 ~= SENTINEL_IMPASSABLE_CHUNK) and
            ((neighborPass5 == CHUNK_ALL_DIRECTIONS) or (neighborPass5 == CHUNK_EAST_WEST))) then
            neighborCount = neighborCount + 1
            movementTotal = movementTotal + (neighbor5[MOVEMENT_PHEROMONE] - chunkMovement)
            baseTotal = baseTotal + (neighbor5[BASE_PHEROMONE] - chunkBase)
            playerTotal = playerTotal + neighbor5[PLAYER_PHEROMONE] - chunkPlayer
            resourceTotal = resourceTotal + (neighbor5[RESOURCE_PHEROMONE] - chunkResource)                
        end
        
    elseif (chunkPass == CHUNK_NORTH_SOUTH) then
        
        neighbor2 = tempNeighbors[2]
        neighborPass2 = getPassable(map, neighbor2)
        if ((neighbor2 ~= SENTINEL_IMPASSABLE_CHUNK) and
            ((neighborPass2 == CHUNK_ALL_DIRECTIONS) or (neighborPass2 == CHUNK_NORTH_SOUTH))) then
            neighborCount = neighborCount + 1
            movementTotal = movementTotal + (neighbor2[MOVEMENT_PHEROMONE] - chunkMovement)
            baseTotal = baseTotal + (neighbor2[BASE_PHEROMONE] - chunkBase)
            playerTotal = playerTotal + (neighbor2[PLAYER_PHEROMONE] - chunkPlayer)
            resourceTotal = resourceTotal + (neighbor2[RESOURCE_PHEROMONE] - chunkResource)
        end

        neighbor7 = tempNeighbors[7]
        neighborPass7 = getPassable(map, neighbor7)
        if ((neighbor7 ~= SENTINEL_IMPASSABLE_CHUNK) and
            ((neighborPass7 == CHUNK_ALL_DIRECTIONS) or (neighborPass7 == CHUNK_NORTH_SOUTH))) then
            neighborCount = neighborCount + 1
            movementTotal = movementTotal + (neighbor7[MOVEMENT_PHEROMONE] - chunkMovement)
            baseTotal = baseTotal + (neighbor7[BASE_PHEROMONE] - chunkBase)
            playerTotal = playerTotal + (neighbor7[PLAYER_PHEROMONE] - chunkPlayer)
            resourceTotal = resourceTotal + (neighbor7[RESOURCE_PHEROMONE] - chunkResource)
        end
    end

    local neighborDiv
    if neighborCount == 0 then
        neighborDiv = 0
    else
        neighborDiv = (1/neighborCount)
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
