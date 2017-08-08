local mapUtils = {}

-- imports

local constants = require("Constants")

-- constants

local CHUNK_NORTH_SOUTH = constants.CHUNK_NORTH_SOUTH
local CHUNK_EAST_WEST = constants.CHUNK_EAST_WEST

local CHUNK_ALL_DIRECTIONS = constants.CHUNK_ALL_DIRECTIONS

local PASSABLE = constants.PASSABLE

local CHUNK_IMPASSABLE = constants.CHUNK_IMPASSABLE

local CHUNK_SIZE = constants.CHUNK_SIZE

-- imported functions

local mFloor = math.floor

-- module code

function mapUtils.getChunkByPosition(regionMap, x, y)
    local chunkX = regionMap[mFloor(x * 0.03125)]
    if chunkX then
        return chunkX[mFloor(y * 0.03125)]
    end
    return nil
end

function mapUtils.getChunkByIndex(regionMap, x, y)
    local chunkX = regionMap[x]
    if chunkX then
        return chunkX[y]
    end
    return nil
end

--[[
    1 2 3
    \|/
    4- -5
    /|\
    6 7 8
]]--
function mapUtils.getNeighborChunks(regionMap, chunkX, chunkY)
    local chunkYRow1 = chunkY - 1
    local chunkYRow3 = chunkY + 1
    local neighbors = regionMap.neighbors
    local xChunks = regionMap[chunkX-1]
    if xChunks then
        neighbors[1] = xChunks[chunkYRow1]
        neighbors[4] = xChunks[chunkY]
        neighbors[6] = xChunks[chunkYRow3]
    else
	neighbors[1] = nil
	neighbors[4] = nil
	neighbors[6] = nil
    end
    
    xChunks = regionMap[chunkX+1]
    if xChunks then
        neighbors[3] = xChunks[chunkYRow1]
        neighbors[5] = xChunks[chunkY]
        neighbors[8] = xChunks[chunkYRow3]
    else
	neighbors[3] = nil
	neighbors[5] = nil
	neighbors[8] = nil
    end
    
    xChunks = regionMap[chunkX]
    if xChunks then
        neighbors[2] = xChunks[chunkYRow1]
        neighbors[7] = xChunks[chunkYRow3]
    else
	neighbors[2] = nil
	neighbors[7] = nil
    end
    return neighbors
end

function mapUtils.canMoveChunkDirection(direction, startChunk, endChunk)
    local canMove = false
    local startPassable = startChunk[PASSABLE]
    local endPassable = endChunk[PASSABLE]
    if ((direction == 2) or (direction == 7)) and (startPassable == CHUNK_NORTH_SOUTH) and (endPassable == CHUNK_NORTH_SOUTH) then
        canMove = true
    elseif ((direction == 4) or (direction == 5)) and (startPassable == CHUNK_EAST_WEST) and (endPassable == CHUNK_EAST_WEST) then
        canMove = true
    elseif (startPassable == CHUNK_ALL_DIRECTIONS) and (endPassable == CHUNK_ALL_DIRECTIONS) then
	canMove = true
    elseif (startPassable ~= CHUNK_IMPASSABLE) and (endPassable == CHUNK_ALL_DIRECTIONS) then
	canMove = true
    end
    return canMove
end

function mapUtils.getCardinalChunks(regionMap, chunkX, chunkY)
    local xChunks = regionMap[chunkX]
    local neighbors = regionMap.cardinalNeighbors
    if xChunks then
	neighbors[1] = xChunks[chunkY-1]
	neighbors[4] = xChunks[chunkY+1]
    else
	neighbors[1] = nil
	neighbors[4] = nil
    end
    
    xChunks = regionMap[chunkX-1]
    if xChunks then
	neighbors[2] = xChunks[chunkY]
    else
	neighbors[2] = nil
    end
    
    xChunks = regionMap[chunkX+1]
    if xChunks then
	neighbors[3] = xChunks[chunkY]
    else
	neighbors[3] = nil
    end
    return neighbors
end

function mapUtils.positionFromDirectionAndChunk(direction, startPosition, position, scaling)
    if (direction == 1) then
	position.x = startPosition.x - CHUNK_SIZE * scaling
	position.y = startPosition.y - CHUNK_SIZE * scaling
    elseif (direction == 2) then
	position.x = startPosition.x
	position.y = startPosition.y - CHUNK_SIZE * scaling
    elseif (direction == 3) then
	position.x = startPosition.x + CHUNK_SIZE * scaling
	position.y = startPosition.y - CHUNK_SIZE * scaling
    elseif (direction == 4) then
	position.x = startPosition.x - CHUNK_SIZE * scaling
	position.y = startPosition.y
    elseif (direction == 5) then
	position.x = startPosition.x + CHUNK_SIZE * scaling
	position.y = startPosition.y
    elseif (direction == 6) then
	position.x = startPosition.x - CHUNK_SIZE * scaling 
	position.y = startPosition.y + CHUNK_SIZE * scaling
    elseif (direction == 7) then
	position.x = startPosition.x
	position.y = startPosition.y + CHUNK_SIZE * scaling
    elseif (direction == 8) then
	position.x = startPosition.x + CHUNK_SIZE * scaling
	position.y = startPosition.y + CHUNK_SIZE * scaling
    end
    return position
end

return mapUtils
