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

local MOVEMENT_PHEROMONE_PERSISTANCE = constants.MOVEMENT_PHEROMONE_PERSISTANCE
local BASE_PHEROMONE_PERSISTANCE = constants.BASE_PHEROMONE_PERSISTANCE
local PLAYER_PHEROMONE_PERSISTANCE = constants.PLAYER_PHEROMONE_PERSISTANCE
local RESOURCE_PHEROMONE_PERSISTANCE = constants.RESOURCE_PHEROMONE_PERSISTANCE

local DEATH_PHEROMONE_GENERATOR_AMOUNT = constants.DEATH_PHEROMONE_GENERATOR_AMOUNT
local DOUBLE_DEATH_PHEROMONE_GENERATOR_AMOUNT = constants.DOUBLE_DEATH_PHEROMONE_GENERATOR_AMOUNT

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

-- module code

function pheromoneUtils.victoryScent(map, chunk, entityType)
    local value = VICTORY_SCENT[entityType]
    if value then
        addDeathGenerator(map, chunk, -value)
        -- chunk[MOVEMENT_PHEROMONE] = chunk[MOVEMENT_PHEROMONE] + value
    end
end

function pheromoneUtils.deathScent(map, chunk)
    -- chunk[MOVEMENT_PHEROMONE] = chunk[MOVEMENT_PHEROMONE] - DOUBLE_DEATH_PHEROMONE_GENERATOR_AMOUNT
    addDeathGenerator(map, chunk, DEATH_PHEROMONE_GENERATOR_AMOUNT)
end

-- function pheromoneUtils.commitPheromone(map, chunk, staging, tick)
--     decayDeathGenerator(map, chunk)

--     chunk[MOVEMENT_PHEROMONE] = staging[MOVEMENT_PHEROMONE] - getDeathGenerator(map, chunk)
--     chunk[PLAYER_PHEROMONE] = staging[PLAYER_PHEROMONE]
-- end

function pheromoneUtils.processStaticPheromone(map, chunk)

end

function pheromoneUtils.processPheromone(map, chunk)
    local chunkMovement = -MAGIC_MAXIMUM_NUMBER
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
                pheromone = neighbor[MOVEMENT_PHEROMONE]
                if chunkMovement < pheromone then
                    chunkMovement = pheromone
                end
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
                pheromone = neighbor[MOVEMENT_PHEROMONE]
                if chunkMovement < pheromone then
                    chunkMovement = pheromone
                end
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
                pheromone = neighbor[MOVEMENT_PHEROMONE]
                if chunkMovement < pheromone then
                    chunkMovement = pheromone
                end
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
                pheromone = neighbor[MOVEMENT_PHEROMONE]
                if chunkMovement < pheromone then
                    chunkMovement = pheromone
                end
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
                pheromone = neighbor[MOVEMENT_PHEROMONE]
                if chunkMovement < pheromone then
                    chunkMovement = pheromone
                end
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
                pheromone = neighbor[MOVEMENT_PHEROMONE]
                if chunkMovement < pheromone then
                    chunkMovement = pheromone
                end
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
                pheromone = neighbor[MOVEMENT_PHEROMONE]
                if chunkMovement < pheromone then
                    chunkMovement = pheromone
                end
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
                pheromone = neighbor[MOVEMENT_PHEROMONE]
                if chunkMovement < pheromone then
                    chunkMovement = pheromone
                end
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
                pheromone = neighbor[MOVEMENT_PHEROMONE]
                if chunkMovement < pheromone then
                    chunkMovement = pheromone
                end
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
                pheromone = neighbor[MOVEMENT_PHEROMONE]
                if chunkMovement < pheromone then
                    chunkMovement = pheromone
                end
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
                pheromone = neighbor[MOVEMENT_PHEROMONE]
                if chunkMovement < pheromone then
                    chunkMovement = pheromone
                end
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
                pheromone = neighbor[MOVEMENT_PHEROMONE]
                if chunkMovement < pheromone then
                    chunkMovement = pheromone
                end
                pheromone = neighbor[PLAYER_PHEROMONE]
                if chunkPlayer < pheromone then
                    chunkPlayer = pheromone
                end
            end
        end
    end

    chunkMovement = chunkMovement * 0.9    
    local pheromone = getDeathGenerator(map, chunk)
    if chunkMovement < pheromone then
        chunk[MOVEMENT_PHEROMONE] = pheromone * chunkPathRating
    else
        chunk[MOVEMENT_PHEROMONE] = chunkMovement * chunkPathRating
    end

    chunkPlayer = chunkPlayer * 0.9
    local pheromone = getPlayersOnChunk(map, chunk) * PLAYER_PHEROMONE_GENERATOR_AMOUNT
    if chunkPlayer < pheromone then
        chunk[PLAYER_PHEROMONE] = pheromone * chunkPathRating
    else
        chunk[PLAYER_PHEROMONE] = chunkPlayer * chunkPathRating
    end
end

pheromoneUtilsG = pheromoneUtils
return pheromoneUtils
