local pheromoneUtils = {}

-- imports

local mapUtils = require("MapUtils")
local constants = require("Constants")

-- constants

local MOVEMENT_PHEROMONE = constants.MOVEMENT_PHEROMONE
local BASE_PHEROMONE = constants.BASE_PHEROMONE
local PLAYER_PHEROMONE = constants.PLAYER_PHEROMONE

local BUILDING_PHEROMONES = constants.BUILDING_PHEROMONES

local PLAYER_BASE_GENERATOR = constants.PLAYER_BASE_GENERATOR

local PLAYER_PHEROMONE_GENERATOR_AMOUNT = constants.PLAYER_PHEROMONE_GENERATOR_AMOUNT
local DEATH_PHEROMONE_GENERATOR_AMOUNT = constants.DEATH_PHEROMONE_GENERATOR_AMOUNT

local MOVEMENT_PHEROMONE_PERSISTANCE = constants.MOVEMENT_PHEROMONE_PERSISTANCE
local BASE_PHEROMONE_PERSISTANCE = constants.BASE_PHEROMONE_PERSISTANCE
local PLAYER_PHEROMONE_PERSISTANCE = constants.PLAYER_PHEROMONE_PERSISTANCE

local NORTH_SOUTH_PASSABLE = constants.NORTH_SOUTH_PASSABLE
local EAST_WEST_PASSABLE = constants.EAST_WEST_PASSABLE

local IMPASSABLE_TERRAIN_GENERATOR_AMOUNT = constants.IMPASSABLE_TERRAIN_GENERATOR_AMOUNT

-- imported functions

local getCardinalChunks = mapUtils.getCardinalChunks

-- module code

function pheromoneUtils.scents(chunk)

    if not chunk[NORTH_SOUTH_PASSABLE] and not chunk[EAST_WEST_PASSABLE] then
	chunk[BASE_PHEROMONE] = IMPASSABLE_TERRAIN_GENERATOR_AMOUNT;
    else
	chunk[BASE_PHEROMONE] = chunk[BASE_PHEROMONE] + chunk[PLAYER_BASE_GENERATOR]
    end
    
end

function pheromoneUtils.victoryScent(chunk, entityType)
    local value = BUILDING_PHEROMONES[entityType]
    if (value ~= nil) then
	chunk[MOVEMENT_PHEROMONE] = chunk[MOVEMENT_PHEROMONE] + (value * 10000)
    end
end

function pheromoneUtils.deathScent(chunk)
    chunk[MOVEMENT_PHEROMONE] = chunk[MOVEMENT_PHEROMONE] - DEATH_PHEROMONE_GENERATOR_AMOUNT
end

function pheromoneUtils.playerScent(playerChunk)
    playerChunk[PLAYER_PHEROMONE] = playerChunk[PLAYER_PHEROMONE] + PLAYER_PHEROMONE_GENERATOR_AMOUNT
end

function pheromoneUtils.processPheromone(regionMap, chunk, tempNeighbors)

    if not chunk[NORTH_SOUTH_PASSABLE] and not chunk[EAST_WEST_PASSABLE] then
	return
    end

    getCardinalChunks(regionMap, chunk.cX, chunk.cY, tempNeighbors)
    
    local chunkMovement = chunk[MOVEMENT_PHEROMONE]
    local chunkBase = chunk[BASE_PHEROMONE]
    local chunkPlayer = chunk[PLAYER_PHEROMONE]
    
    local totalMovement = 0
    local totalBase = 0
    local totalPlayer = 0

    for i=1,4 do
	local neighborChunk = tempNeighbors[i]
	if neighborChunk then
	    totalMovement = totalMovement + (neighborChunk[MOVEMENT_PHEROMONE] - chunkMovement)
	    totalBase = totalBase + (neighborChunk[BASE_PHEROMONE] - chunkBase)
	    totalPlayer = totalPlayer + (neighborChunk[PLAYER_PHEROMONE] - chunkPlayer)
	end
    end
    chunk[MOVEMENT_PHEROMONE] = (chunkMovement + (0.125 * totalMovement)) * MOVEMENT_PHEROMONE_PERSISTANCE
    chunk[BASE_PHEROMONE] = (chunkBase + (0.25 * totalBase)) * BASE_PHEROMONE_PERSISTANCE
    chunk[PLAYER_PHEROMONE] = (chunkPlayer + (0.25 * totalPlayer)) * PLAYER_PHEROMONE_PERSISTANCE
end

return pheromoneUtils
