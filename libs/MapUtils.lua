local mapUtils = {}

-- imports

local constants = require("Constants")

local mathUtils = require("MathUtils")

-- constants

local CHUNK_NORTH_SOUTH = constants.CHUNK_NORTH_SOUTH
local CHUNK_EAST_WEST = constants.CHUNK_EAST_WEST

local CHUNK_ALL_DIRECTIONS = constants.CHUNK_ALL_DIRECTIONS

local PASSABLE = constants.PASSABLE

local CHUNK_IMPASSABLE = constants.CHUNK_IMPASSABLE

local CHUNK_SIZE = constants.CHUNK_SIZE

-- imported functions

local gaussianRandomRange = mathUtils.gaussianRandomRange

local mFloor = math.floor

-- module code

function mapUtils.getChunkByPosition(regionMap, x, y)
    local chunkX = regionMap[mFloor(x * 0.03125)]
    if (chunkX ~= nil) then
        return chunkX[mFloor(y * 0.03125)]
    end
    return nil
end

function mapUtils.getChunkByIndex(regionMap, x, y)
    local chunkX = regionMap[x]
    if (chunkX ~= nil) then
        return chunkX[y]
    end
    return nil
end

function mapUtils.positionToChunkOffset(position)
    return mFloor(position.x * 0.03125), mFloor(position.y * 0.03125)
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
    end
    
    xChunks = regionMap[chunkX+1]
    if xChunks then
        neighbors[3] = xChunks[chunkYRow1]
        neighbors[5] = xChunks[chunkY]
        neighbors[8] = xChunks[chunkYRow3]
    end
    
    xChunks = regionMap[chunkX]
    if xChunks then
        neighbors[2] = xChunks[chunkYRow1]
        neighbors[7] = xChunks[chunkYRow3]
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
    end
    
    xChunks = regionMap[chunkX-1]
    if xChunks then
	neighbors[2] = xChunks[chunkY]
    end
    
    xChunks = regionMap[chunkX+1]
    if xChunks then
	neighbors[3] = xChunks[chunkY]
    end
    return neighbors
end

function mapUtils.euclideanDistanceNamed(p1, p2)
    local xs = p1.x - p2.x
    local ys = p1.y - p2.y
    return ((xs * xs) + (ys * ys)) ^ 0.5
end

function mapUtils.euclideanDistancePoints(x1, y1, x2, y2)
    local xs = x1 - x2
    local ys = y1 - y2
    return ((xs * xs) + (ys * ys)) ^ 0.5
end

function mapUtils.euclideanDistanceArray(p1, p2)
    local xs = p1[1] - p2[1]
    local ys = p1[2] - p2[2]
    return ((xs * xs) + (ys * ys)) ^ 0.5
end

function mapUtils.distortPosition(position)
    local xDistort = gaussianRandomRange(1, 0.5, 0, 2) - 1
    local yDistort = gaussianRandomRange(1, 0.5, 0, 2) - 1
    position.x = position.x + (xDistort * 48)
    position.y = position.y + (yDistort * 48)
end

function mapUtils.positionFromDirectionAndChunk(direction, startPosition, position, scaling)
    --    local position = {x=0, y=0}
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
