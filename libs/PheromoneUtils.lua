local pheromoneUtils = {}

-- imports

local mapUtils = require("MapUtils")
local constants = require("Constants")

-- constants

local MOVEMENT_PHEROMONE = constants.MOVEMENT_PHEROMONE
local BASE_PHEROMONE = constants.BASE_PHEROMONE
local PLAYER_PHEROMONE = constants.PLAYER_PHEROMONE

local PLAYER_BASE_GENERATOR = constants.PLAYER_BASE_GENERATOR
local ENEMY_BASE_GENERATOR = constants.ENEMY_BASE_GENERATOR

local PLAYER_PHEROMONE_GENERATOR_AMOUNT = constants.PLAYER_PHEROMONE_GENERATOR_AMOUNT
local DEATH_PHEROMONE_GENERATOR_AMOUNT = constants.MOVEMENT_PHEROMONE_GENERATOR_AMOUNT

local STANDARD_PHERONOME_DIFFUSION_AMOUNT = constants.STANDARD_PHERONOME_DIFFUSION_AMOUNT
local MOVEMENT_PHEROMONE_DIFFUSION_AMOUNT = constants.MOVEMENT_PHEROMONE_DIFFUSION_AMOUNT

local MOVEMENT_PHEROMONE_PERSISTANCE = constants.MOVEMENT_PHEROMONE_PERSISTANCE
local REDUCED_MOVEMENT_PHEROMONE_PERSISTANCE = constants.REDUCED_MOVEMENT_PHEROMONE_PERSISTANCE
local STANDARD_PHEROMONE_PERSISTANCE = constants.STANDARD_PHEROMONE_PERSISTANCE

local NORTH_SOUTH_PASSABLE = constants.NORTH_SOUTH_PASSABLE
local EAST_WEST_PASSABLE = constants.EAST_WEST_PASSABLE

-- imported functions

local getChunkByPosition = mapUtils.getChunkByPosition
local getCardinalChunks = mapUtils.getCardinalChunks

-- module code

function pheromoneUtils.scents(chunk)
    -- if (chunk[]) then
    -- end
    if not chunk[NORTH_SOUTH_PASSABLE] and not chunk[EAST_WEST_PASSABLE] then
	chunk[BASE_PHEROMONE] = -100;
    else
	chunk[BASE_PHEROMONE] = chunk[BASE_PHEROMONE] + chunk[PLAYER_BASE_GENERATOR] -- chunk[ENEMY_BASE_GENERATOR]
    end
    
end

function pheromoneUtils.deathScent(chunk)
    chunk[MOVEMENT_PHEROMONE] = chunk[MOVEMENT_PHEROMONE] - DEATH_PHEROMONE_GENERATOR_AMOUNT
end

function pheromoneUtils.playerScent(playerChunk)
    playerChunk[PLAYER_PHEROMONE] = playerChunk[PLAYER_PHEROMONE] + PLAYER_PHEROMONE_GENERATOR_AMOUNT
end

function pheromoneUtils.processPheromone(regionMap, chunk)
    local neighbors

    -- pheromone level indexes on chunks are 1 - 3
    -- unrolled loop one level
    local diffusionAmount = MOVEMENT_PHEROMONE_DIFFUSION_AMOUNT
    local persistence
    if (chunk[BASE_PHEROMONE] < 0) then
	persistence = REDUCED_MOVEMENT_PHEROMONE_PERSISTANCE
    else
	persistence = MOVEMENT_PHEROMONE_PERSISTANCE
    end
    local totalDiffused = 0
    local chunkValue = chunk[MOVEMENT_PHEROMONE] * persistence
    local diffusedAmount = chunkValue * diffusionAmount
    if (diffusedAmount > 1.5) or (diffusedAmount < -1.5) then
	neighbors = getCardinalChunks(regionMap, chunk.cX, chunk.cY)
	for i=1,#neighbors do
	    local neighborChunk = neighbors[i]
	    if (neighborChunk ~= nil) and (neighborChunk[NORTH_SOUTH_PASSABLE] or neighborChunk[EAST_WEST_PASSABLE]) then
		totalDiffused = totalDiffused + diffusedAmount
		neighborChunk[MOVEMENT_PHEROMONE] = neighborChunk[MOVEMENT_PHEROMONE] + diffusedAmount
	    end
	end
    end
    chunk[MOVEMENT_PHEROMONE] = (chunkValue - totalDiffused)
    
    diffusionAmount = STANDARD_PHERONOME_DIFFUSION_AMOUNT
    persistence = STANDARD_PHEROMONE_PERSISTANCE
    for x=2,3 do 
	totalDiffused = 0
	chunkValue = chunk[x] * persistence
	diffusedAmount = chunkValue * diffusionAmount
	if (diffusedAmount > 1.5) or (diffusedAmount < -1.5) then
	    if (neighbors == nil) then
		neighbors = getCardinalChunks(regionMap, chunk.cX, chunk.cY)
	    end
	    for i=1,#neighbors do
		local neighborChunk = neighbors[i]
		if (neighborChunk ~= nil) and (neighborChunk[NORTH_SOUTH_PASSABLE] or neighborChunk[EAST_WEST_PASSABLE]) then
		    totalDiffused = totalDiffused + diffusedAmount
		    neighborChunk[x] = neighborChunk[x] + diffusedAmount
		end
	    end
	end
	chunk[x] = (chunkValue - totalDiffused)
    end
end

return pheromoneUtils
