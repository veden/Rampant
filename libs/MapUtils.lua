local mapUtils = {}

-- imports

local constants = require("Constants")

-- constants

local CHUNK_NORTH_SOUTH = constants.CHUNK_NORTH_SOUTH
local CHUNK_EAST_WEST = constants.CHUNK_EAST_WEST

local CHUNK_ALL_DIRECTIONS = constants.CHUNK_ALL_DIRECTIONS

local PASSABLE = constants.PASSABLE

local CHUNK_SIZE = constants.CHUNK_SIZE

local SENTINEL_IMPASSABLE_CHUNK = constants.SENTINEL_IMPASSABLE_CHUNK

local CHUNK_SIZE_DIVIDER = constants.CHUNK_SIZE_DIVIDER

-- imported functions

local mFloor = math.floor

-- module code

function mapUtils.getChunkByXY(map, x, y)
    local chunkX = map[x]
    if chunkX then
        return chunkX[y] or SENTINEL_IMPASSABLE_CHUNK
    end
    return SENTINEL_IMPASSABLE_CHUNK
end

function mapUtils.getChunkByPosition(map, position)
    local chunkX = map[mFloor(position.x * CHUNK_SIZE_DIVIDER) * CHUNK_SIZE]
    if chunkX then
	local chunkY = mFloor(position.y * CHUNK_SIZE_DIVIDER) * CHUNK_SIZE
        return chunkX[chunkY] or SENTINEL_IMPASSABLE_CHUNK
    end
    return SENTINEL_IMPASSABLE_CHUNK
end

function mapUtils.getChunkByUnalignedXY(map, x, y)
    local chunkX = map[mFloor(x * CHUNK_SIZE_DIVIDER) * CHUNK_SIZE]
    if chunkX then
	local chunkY = mFloor(y * CHUNK_SIZE_DIVIDER) * CHUNK_SIZE
        return chunkX[chunkY] or SENTINEL_IMPASSABLE_CHUNK
    end
    return SENTINEL_IMPASSABLE_CHUNK
end

function mapUtils.positionToChunkXY(position)
    local chunkX = mFloor(position.x * CHUNK_SIZE_DIVIDER) * CHUNK_SIZE
    local chunkY = mFloor(position.y * CHUNK_SIZE_DIVIDER) * CHUNK_SIZE
    return chunkX, chunkY
end

--[[
    1 2 3
    \|/
    4- -5
    /|\
    6 7 8
]]--
function mapUtils.getNeighborChunks(map, x, y)
    local neighbors = map.neighbors
    local chunkYRow1 = y - CHUNK_SIZE
    local chunkYRow3 = y + CHUNK_SIZE
    local xChunks = map[x-CHUNK_SIZE]
    if xChunks then
        neighbors[1] = xChunks[chunkYRow1] or SENTINEL_IMPASSABLE_CHUNK
        neighbors[4] = xChunks[y] or SENTINEL_IMPASSABLE_CHUNK
        neighbors[6] = xChunks[chunkYRow3] or SENTINEL_IMPASSABLE_CHUNK
    else
	neighbors[1] = SENTINEL_IMPASSABLE_CHUNK
	neighbors[4] = SENTINEL_IMPASSABLE_CHUNK
	neighbors[6] = SENTINEL_IMPASSABLE_CHUNK
    end

    xChunks = map[x+CHUNK_SIZE]
    if xChunks then
        neighbors[3] = xChunks[chunkYRow1] or SENTINEL_IMPASSABLE_CHUNK
        neighbors[5] = xChunks[y] or SENTINEL_IMPASSABLE_CHUNK
        neighbors[8] = xChunks[chunkYRow3] or SENTINEL_IMPASSABLE_CHUNK
    else
	neighbors[3] = SENTINEL_IMPASSABLE_CHUNK 
	neighbors[5] = SENTINEL_IMPASSABLE_CHUNK
	neighbors[8] = SENTINEL_IMPASSABLE_CHUNK
    end
    
    xChunks = map[x]
    if xChunks then
        neighbors[2] = xChunks[chunkYRow1] or SENTINEL_IMPASSABLE_CHUNK
        neighbors[7] = xChunks[chunkYRow3] or SENTINEL_IMPASSABLE_CHUNK
    else
	neighbors[2] = SENTINEL_IMPASSABLE_CHUNK
	neighbors[7] = SENTINEL_IMPASSABLE_CHUNK
    end
    return neighbors
end

function mapUtils.canMoveChunkDirection(direction, startChunk, endChunk)
    local canMove = false
    local startPassable = startChunk[PASSABLE]
    local endPassable = endChunk[PASSABLE]
    if (startPassable == CHUNK_ALL_DIRECTIONS) and (endPassable == CHUNK_ALL_DIRECTIONS) then
	canMove = true
    elseif ((direction == 2) or (direction == 7)) and (startPassable == CHUNK_NORTH_SOUTH) and (endPassable == CHUNK_NORTH_SOUTH) then
	canMove = true
    elseif ((direction == 4) or (direction == 5)) and (startPassable == CHUNK_EAST_WEST) and (endPassable == CHUNK_EAST_WEST) then
	canMove = true
    elseif (startChunk == SENTINEL_IMPASSABLE_CHUNK) and (endPassable == CHUNK_ALL_DIRECTIONS) then
	canMove = true
    end
    return canMove
end

function mapUtils.getCardinalChunks(map, x, y)
    local neighbors = map.cardinalNeighbors
    local xChunks = map[x]
    if xChunks then
	neighbors[1] = xChunks[y-CHUNK_SIZE] or SENTINEL_IMPASSABLE_CHUNK
	neighbors[4] = xChunks[y+CHUNK_SIZE] or SENTINEL_IMPASSABLE_CHUNK
    else
	neighbors[1] = SENTINEL_IMPASSABLE_CHUNK
	neighbors[4] = SENTINEL_IMPASSABLE_CHUNK
    end
    
    xChunks = map[x-CHUNK_SIZE]
    if xChunks then
	neighbors[2] = xChunks[y] or SENTINEL_IMPASSABLE_CHUNK
    else
	neighbors[2] = SENTINEL_IMPASSABLE_CHUNK
    end
    
    xChunks = map[x+CHUNK_SIZE]
    if xChunks then
	neighbors[3] = xChunks[y] or SENTINEL_IMPASSABLE_CHUNK
    else
	neighbors[3] = SENTINEL_IMPASSABLE_CHUNK
    end
    return neighbors
end

function mapUtils.positionFromDirectionAndChunk(direction, startPosition, endPosition, scaling)
    if (direction == 1) then
	endPosition.x = startPosition.x - CHUNK_SIZE * scaling
	endPosition.y = startPosition.y - CHUNK_SIZE * scaling
    elseif (direction == 2) then
	endPosition.x = startPosition.x
	endPosition.y = startPosition.y - CHUNK_SIZE * scaling
    elseif (direction == 3) then
	endPosition.x = startPosition.x + CHUNK_SIZE * scaling
	endPosition.y = startPosition.y - CHUNK_SIZE * scaling
    elseif (direction == 4) then
	endPosition.x = startPosition.x - CHUNK_SIZE * scaling
	endPosition.y = startPosition.y
    elseif (direction == 5) then
	endPosition.x = startPosition.x + CHUNK_SIZE * scaling
	endPosition.y = startPosition.y
    elseif (direction == 6) then
	endPosition.x = startPosition.x - CHUNK_SIZE * scaling 
	endPosition.y = startPosition.y + CHUNK_SIZE * scaling
    elseif (direction == 7) then
	endPosition.x = startPosition.x
	endPosition.y = startPosition.y + CHUNK_SIZE * scaling
    elseif (direction == 8) then
	endPosition.x = startPosition.x + CHUNK_SIZE * scaling
	endPosition.y = startPosition.y + CHUNK_SIZE * scaling
    end
    return endPosition
end

return mapUtils
