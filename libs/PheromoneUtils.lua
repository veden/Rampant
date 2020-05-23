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

local MAGIC_MAXIMUM_NUMBER = constants.MAGIC_MAXIMUM_NUMBER

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

local PLAYER_PHEROMONE_GENERATOR_AMOUNT = constants.PLAYER_PHEROMONE_GENERATOR_AMOUNT

local PLAYER_PHEROMONE_PERSISTANCE = constants.PLAYER_PHEROMONE_PERSISTANCE
local RESOURCE_PHEROMONE_PERSISTANCE = constants.RESOURCE_PHEROMONE_PERSISTANCE

local DEATH_PHEROMONE_GENERATOR_AMOUNT = constants.DEATH_PHEROMONE_GENERATOR_AMOUNT

-- imported functions

local getPlayersOnChunk = chunkPropertyUtils.getPlayersOnChunk

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

local mAbs = math.abs

-- module code

function pheromoneUtils.victoryScent(map, chunk, entityType)
    local value = VICTORY_SCENT[entityType]
    if value then
        addDeathGenerator(map, chunk, -value)
    end
end

function pheromoneUtils.deathScent(map, chunk)
    addDeathGenerator(map, chunk, DEATH_PHEROMONE_GENERATOR_AMOUNT)
end

function pheromoneUtils.processStaticPheromone(map, chunk)
    local chunkBase = -MAGIC_MAXIMUM_NUMBER
    local chunkResource = -MAGIC_MAXIMUM_NUMBER
    local chunkPathRating = getPathRating(map, chunk)

    local clear = getEnemyStructureCount(map, chunk)

    local tempNeighbors = getNeighborChunks(map, chunk.x, chunk.y)

    local neighbor
    local neighborPass

    local chunkPass = getPassable(map, chunk)
    local pheromone
    if (chunkPass == CHUNK_ALL_DIRECTIONS) then
        neighbor = tempNeighbors[2]
        if (neighbor ~= -1) then
            neighborPass = getPassable(map, neighbor)
            if ((neighborPass == CHUNK_ALL_DIRECTIONS) or (neighborPass == CHUNK_NORTH_SOUTH)) then
                pheromone = neighbor[BASE_PHEROMONE]
                if chunkBase < pheromone then
                    chunkBase = pheromone
                end
                pheromone = neighbor[RESOURCE_PHEROMONE]
                if chunkResource < pheromone then
                    chunkResource = pheromone
                end
            end
        end

        neighbor = tempNeighbors[7]
        if (neighbor ~= -1) then
            neighborPass = getPassable(map, neighbor)
            if ((neighborPass == CHUNK_ALL_DIRECTIONS) or (neighborPass == CHUNK_NORTH_SOUTH)) then
                pheromone = neighbor[BASE_PHEROMONE]
                if chunkBase < pheromone then
                    chunkBase = pheromone
                end
                pheromone = neighbor[RESOURCE_PHEROMONE]
                if chunkResource < pheromone then
                    chunkResource = pheromone
                end
            end
        end

        neighbor = tempNeighbors[4]
        if (neighbor ~= -1) then
            neighborPass = getPassable(map, neighbor)
            if ((neighborPass == CHUNK_ALL_DIRECTIONS) or (neighborPass == CHUNK_EAST_WEST)) then
                pheromone = neighbor[BASE_PHEROMONE]
                if chunkBase < pheromone then
                    chunkBase = pheromone
                end
                pheromone = neighbor[RESOURCE_PHEROMONE]
                if chunkResource < pheromone then
                    chunkResource = pheromone
                end
            end
        end

        neighbor = tempNeighbors[5]
        if (neighbor ~= -1) then
            neighborPass = getPassable(map, neighbor)
            if ((neighborPass == CHUNK_ALL_DIRECTIONS) or (neighborPass == CHUNK_EAST_WEST)) then
                pheromone = neighbor[BASE_PHEROMONE]
                if chunkBase < pheromone then
                    chunkBase = pheromone
                end
                pheromone = neighbor[RESOURCE_PHEROMONE]
                if chunkResource < pheromone then
                    chunkResource = pheromone
                end
            end
        end

        neighbor = tempNeighbors[1]
        if (neighbor ~= -1) then
            neighborPass = getPassable(map, neighbor)
            if (neighborPass == CHUNK_ALL_DIRECTIONS) then
                pheromone = neighbor[BASE_PHEROMONE]
                if chunkBase < pheromone then
                    chunkBase = pheromone
                end
                pheromone = neighbor[RESOURCE_PHEROMONE]
                if chunkResource < pheromone then
                    chunkResource = pheromone
                end
            end
        end

        neighbor = tempNeighbors[3]
        if (neighbor ~= -1) then
            neighborPass = getPassable(map, neighbor)
            if (neighborPass == CHUNK_ALL_DIRECTIONS) then
                pheromone = neighbor[BASE_PHEROMONE]
                if chunkBase < pheromone then
                    chunkBase = pheromone
                end
                pheromone = neighbor[RESOURCE_PHEROMONE]
                if chunkResource < pheromone then
                    chunkResource = pheromone
                end
            end
        end

        neighbor = tempNeighbors[6]
        if (neighbor ~= -1) then
            neighborPass = getPassable(map, neighbor)
            if (neighborPass == CHUNK_ALL_DIRECTIONS) then
                pheromone = neighbor[BASE_PHEROMONE]
                if chunkBase < pheromone then
                    chunkBase = pheromone
                end
                pheromone = neighbor[RESOURCE_PHEROMONE]
                if chunkResource < pheromone then
                    chunkResource = pheromone
                end
            end
        end

        neighbor = tempNeighbors[8]
        if (neighbor ~= -1) then
            neighborPass = getPassable(map, neighbor)
            if (neighborPass == CHUNK_ALL_DIRECTIONS) then
                pheromone = neighbor[BASE_PHEROMONE]
                if chunkBase < pheromone then
                    chunkBase = pheromone
                end
                pheromone = neighbor[RESOURCE_PHEROMONE]
                if chunkResource < pheromone then
                    chunkResource = pheromone
                end
            end
        end
    elseif (chunkPass == CHUNK_EAST_WEST) then

        neighbor = tempNeighbors[4]
        if (neighbor ~= -1) then
            neighborPass = getPassable(map, neighbor)
            if ((neighborPass == CHUNK_ALL_DIRECTIONS) or (neighborPass == CHUNK_EAST_WEST)) then
                pheromone = neighbor[BASE_PHEROMONE]
                if chunkBase < pheromone then
                    chunkBase = pheromone
                end
                pheromone = neighbor[RESOURCE_PHEROMONE]
                if chunkResource < pheromone then
                    chunkResource = pheromone
                end
            end
        end

        neighbor = tempNeighbors[5]
        if (neighbor ~= -1) then
            neighborPass = getPassable(map, neighbor)
            if ((neighborPass == CHUNK_ALL_DIRECTIONS) or (neighborPass == CHUNK_EAST_WEST)) then
                pheromone = neighbor[BASE_PHEROMONE]
                if chunkBase < pheromone then
                    chunkBase = pheromone
                end
                pheromone = neighbor[RESOURCE_PHEROMONE]
                if chunkResource < pheromone then
                    chunkResource = pheromone
                end
            end
        end
    elseif (chunkPass == CHUNK_NORTH_SOUTH) then

        neighbor = tempNeighbors[2]
        if (neighbor ~= -1) then
            neighborPass = getPassable(map, neighbor)
            if ((neighborPass == CHUNK_ALL_DIRECTIONS) or (neighborPass == CHUNK_NORTH_SOUTH)) then
                pheromone = neighbor[BASE_PHEROMONE]
                if chunkBase < pheromone then
                    chunkBase = pheromone
                end
                pheromone = neighbor[RESOURCE_PHEROMONE]
                if chunkResource < pheromone then
                    chunkResource = pheromone
                end
            end
        end

        neighbor = tempNeighbors[7]
        if (neighbor ~= -1) then
            neighborPass = getPassable(map, neighbor)
            if ((neighborPass == CHUNK_ALL_DIRECTIONS) or (neighborPass == CHUNK_NORTH_SOUTH)) then
                pheromone = neighbor[BASE_PHEROMONE]
                if chunkBase < pheromone then
                    chunkBase = pheromone
                end
                pheromone = neighbor[RESOURCE_PHEROMONE]
                if chunkResource < pheromone then
                    chunkResource = pheromone
                end
            end
        end
    end

    chunkBase = chunkBase * 0.9
    local pheromone = getPlayerBaseGenerator(map, chunk)
    if chunkBase < pheromone then
        chunk[BASE_PHEROMONE] = pheromone * chunkPathRating
    else
        chunk[BASE_PHEROMONE] = chunkBase * chunkPathRating
    end

    chunkResource = chunkResource * 0.9
    local pheromone = getResourceGenerator(map, chunk)
    if (pheromone > 0) and clear then
        pheromone = linearInterpolation(pheromone, 15000, 20000)
    end
    if chunkResource < pheromone then
        if clear then
            chunk[RESOURCE_PHEROMONE] = pheromone * chunkPathRating
        else
            chunk[RESOURCE_PHEROMONE] = pheromone * chunkPathRating * 0.1
        end
    else
        if clear then
            chunk[RESOURCE_PHEROMONE] = chunkResource * chunkPathRating
        else
            chunk[RESOURCE_PHEROMONE] = chunkResource * chunkPathRating * 0.1
        end
    end
end

function pheromoneUtils.processPheromone(map, chunk)
    local chunkPlayer = -MAGIC_MAXIMUM_NUMBER
    local chunkPathRating = getPathRating(map, chunk)

    local tempNeighbors = getNeighborChunks(map, chunk.x, chunk.y)

    local neighbor
    local neighborPass

    local chunkPass = getPassable(map, chunk)
    local pheromone
    if (chunkPass == CHUNK_ALL_DIRECTIONS) then
        neighbor = tempNeighbors[2]
        if (neighbor ~= -1) then
            neighborPass = getPassable(map, neighbor)
            if ((neighborPass == CHUNK_ALL_DIRECTIONS) or (neighborPass == CHUNK_NORTH_SOUTH)) then
                pheromone = neighbor[PLAYER_PHEROMONE]
                if chunkPlayer < pheromone then
                    chunkPlayer = pheromone
                end
            end
        end

        neighbor = tempNeighbors[7]
        if (neighbor ~= -1) then
            neighborPass = getPassable(map, neighbor)
            if ((neighborPass == CHUNK_ALL_DIRECTIONS) or (neighborPass == CHUNK_NORTH_SOUTH)) then
                pheromone = neighbor[PLAYER_PHEROMONE]
                if chunkPlayer < pheromone then
                    chunkPlayer = pheromone
                end
            end
        end

        neighbor = tempNeighbors[4]
        if (neighbor ~= -1) then
            neighborPass = getPassable(map, neighbor)
            if ((neighborPass == CHUNK_ALL_DIRECTIONS) or (neighborPass == CHUNK_EAST_WEST)) then
                pheromone = neighbor[PLAYER_PHEROMONE]
                if chunkPlayer < pheromone then
                    chunkPlayer = pheromone
                end
            end
        end

        neighbor = tempNeighbors[5]
        if (neighbor ~= -1) then
            neighborPass = getPassable(map, neighbor)
            if ((neighborPass == CHUNK_ALL_DIRECTIONS) or (neighborPass == CHUNK_EAST_WEST)) then
                pheromone = neighbor[PLAYER_PHEROMONE]
                if chunkPlayer < pheromone then
                    chunkPlayer = pheromone
                end
            end
        end

        neighbor = tempNeighbors[1]
        if (neighbor ~= -1) then
            neighborPass = getPassable(map, neighbor)
            if (neighborPass == CHUNK_ALL_DIRECTIONS) then
                pheromone = neighbor[PLAYER_PHEROMONE]
                if chunkPlayer < pheromone then
                    chunkPlayer = pheromone
                end
            end
        end

        neighbor = tempNeighbors[3]
        if (neighbor ~= -1) then
            neighborPass = getPassable(map, neighbor)
            if (neighborPass == CHUNK_ALL_DIRECTIONS) then
                pheromone = neighbor[PLAYER_PHEROMONE]
                if chunkPlayer < pheromone then
                    chunkPlayer = pheromone
                end
            end
        end

        neighbor = tempNeighbors[6]
        if (neighbor ~= -1) then
            neighborPass = getPassable(map, neighbor)
            if (neighborPass == CHUNK_ALL_DIRECTIONS) then
                pheromone = neighbor[PLAYER_PHEROMONE]
                if chunkPlayer < pheromone then
                    chunkPlayer = pheromone
                end
            end
        end

        neighbor = tempNeighbors[8]
        if (neighbor ~= -1) then
            neighborPass = getPassable(map, neighbor)
            if (neighborPass == CHUNK_ALL_DIRECTIONS) then
                pheromone = neighbor[PLAYER_PHEROMONE]
                if chunkPlayer < pheromone then
                    chunkPlayer = pheromone
                end
            end
        end
    elseif (chunkPass == CHUNK_EAST_WEST) then

        neighbor = tempNeighbors[4]
        if (neighbor ~= -1) then
            neighborPass = getPassable(map, neighbor)
            if ((neighborPass == CHUNK_ALL_DIRECTIONS) or (neighborPass == CHUNK_EAST_WEST)) then
                pheromone = neighbor[PLAYER_PHEROMONE]
                if chunkPlayer < pheromone then
                    chunkPlayer = pheromone
                end
            end
        end

        neighbor = tempNeighbors[5]
        if (neighbor ~= -1) then
            neighborPass = getPassable(map, neighbor)
            if ((neighborPass == CHUNK_ALL_DIRECTIONS) or (neighborPass == CHUNK_EAST_WEST)) then
                pheromone = neighbor[PLAYER_PHEROMONE]
                if chunkPlayer < pheromone then
                    chunkPlayer = pheromone
                end
            end
        end
    elseif (chunkPass == CHUNK_NORTH_SOUTH) then

        neighbor = tempNeighbors[2]
        if (neighbor ~= -1) then
            neighborPass = getPassable(map, neighbor)
            if ((neighborPass == CHUNK_ALL_DIRECTIONS) or (neighborPass == CHUNK_NORTH_SOUTH)) then
                pheromone = neighbor[PLAYER_PHEROMONE]
                if chunkPlayer < pheromone then
                    chunkPlayer = pheromone
                end
            end
        end

        neighbor = tempNeighbors[7]
        if (neighbor ~= -1) then
            neighborPass = getPassable(map, neighbor)
            if ((neighborPass == CHUNK_ALL_DIRECTIONS) or (neighborPass == CHUNK_NORTH_SOUTH)) then
                pheromone = neighbor[PLAYER_PHEROMONE]
                if chunkPlayer < pheromone then
                    chunkPlayer = pheromone
                end
            end
        end
    end

    decayDeathGenerator(map, chunk)

    chunkPlayer = chunkPlayer * 0.45
    local pheromone = getPlayersOnChunk(map, chunk) * PLAYER_PHEROMONE_GENERATOR_AMOUNT
    if chunkPlayer < pheromone then
        chunk[PLAYER_PHEROMONE] = pheromone * chunkPathRating
    else
        chunk[PLAYER_PHEROMONE] = chunkPlayer * chunkPathRating
    end
end

pheromoneUtilsG = pheromoneUtils
return pheromoneUtils
