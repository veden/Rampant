if mapUtilsG then
    return mapUtilsG
end
local mapUtils = {}

-- imports

local constants = require("Constants")
local chunkPropertyUtils = require("ChunkPropertyUtils")

-- constants

local CHUNK_NORTH_SOUTH = constants.CHUNK_NORTH_SOUTH
local CHUNK_EAST_WEST = constants.CHUNK_EAST_WEST
local CHUNK_IMPASSABLE = constants.CHUNK_IMPASSABLE
local CHUNK_ALL_DIRECTIONS = constants.CHUNK_ALL_DIRECTIONS

-- local PASSABLE = constants.PASSABLE

local CHUNK_SIZE = constants.CHUNK_SIZE

local CHUNK_SIZE_DIVIDER = constants.CHUNK_SIZE_DIVIDER

-- imported functions

local mFloor = math.floor
local getPassable = chunkPropertyUtils.getPassable

-- module code

function mapUtils.getChunkByXY(map, x, y)
    local chunkX = map[x]
    if chunkX then
        return chunkX[y] or -1
    end
    return -1
end

function mapUtils.getChunkByPosition(map, position)
    local chunkX = map[mFloor(position.x * CHUNK_SIZE_DIVIDER) * CHUNK_SIZE]
    if chunkX then
        local chunkY = mFloor(position.y * CHUNK_SIZE_DIVIDER) * CHUNK_SIZE
        return chunkX[chunkY] or -1
    end
    return -1
end

function mapUtils.getChunkByUnalignedXY(map, x, y)
    local chunkX = map[mFloor(x * CHUNK_SIZE_DIVIDER) * CHUNK_SIZE]
    if chunkX then
        local chunkY = mFloor(y * CHUNK_SIZE_DIVIDER) * CHUNK_SIZE
        return chunkX[chunkY] or -1
    end
    return -1
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
        neighbors[1] = xChunks[chunkYRow1] or -1
        neighbors[4] = xChunks[y] or -1
        neighbors[6] = xChunks[chunkYRow3] or -1
    else
        neighbors[1] = -1
        neighbors[4] = -1
        neighbors[6] = -1
    end

    xChunks = map[x+CHUNK_SIZE]
    if xChunks then
        neighbors[3] = xChunks[chunkYRow1] or -1
        neighbors[5] = xChunks[y] or -1
        neighbors[8] = xChunks[chunkYRow3] or -1
    else
        neighbors[3] = -1
        neighbors[5] = -1
        neighbors[8] = -1
    end

    xChunks = map[x]
    if xChunks then
        neighbors[2] = xChunks[chunkYRow1] or -1
        neighbors[7] = xChunks[chunkYRow3] or -1
    else
        neighbors[2] = -1
        neighbors[7] = -1
    end
    return neighbors
end


--[[
    1 2 3
    \|/
    4- -5
    /|\
    6 7 8
]]--
function mapUtils.canMoveChunkDirection(map, direction, startChunk, endChunk)
    local canMove = false
    local startPassable = getPassable(map, startChunk)
    local endPassable = getPassable(map, endChunk)
    -- print(direction, startPassable, endPassable)
    if (startPassable == CHUNK_ALL_DIRECTIONS) then
        if ((direction == 1) or (direction == 3) or (direction == 6) or (direction == 8)) then
            canMove = (endPassable == CHUNK_ALL_DIRECTIONS)
        elseif (direction == 2) or (direction == 7) then
            canMove = ((endPassable == CHUNK_NORTH_SOUTH) or (endPassable == CHUNK_ALL_DIRECTIONS))
        elseif (direction == 4) or (direction == 5) then
            canMove = ((endPassable == CHUNK_EAST_WEST) or (endPassable == CHUNK_ALL_DIRECTIONS))
        end
    elseif (startPassable == CHUNK_NORTH_SOUTH) then
        if ((direction == 1) or (direction == 3) or (direction == 6) or (direction == 8)) then
            canMove = (endPassable == CHUNK_ALL_DIRECTIONS)
        elseif (direction == 2) or (direction == 7) then
            canMove = ((endPassable == CHUNK_NORTH_SOUTH) or (endPassable == CHUNK_ALL_DIRECTIONS))
        end
    elseif (startPassable == CHUNK_EAST_WEST) then
        if ((direction == 1) or (direction == 3) or (direction == 6) or (direction == 8)) then
            canMove = (endPassable == CHUNK_ALL_DIRECTIONS)
        elseif (direction == 4) or (direction == 5) then
            canMove = ((endPassable == CHUNK_EAST_WEST) or (endPassable == CHUNK_ALL_DIRECTIONS))
        end
    else
        canMove = (endPassable ~= CHUNK_IMPASSABLE)
    end
    return canMove
end

function mapUtils.getCardinalChunks(map, x, y)
    local neighbors = map.cardinalNeighbors
    local xChunks = map[x]
    if xChunks then
        neighbors[1] = xChunks[y-CHUNK_SIZE] or -1
        neighbors[4] = xChunks[y+CHUNK_SIZE] or -1
    else
        neighbors[1] = -1
        neighbors[4] = -1
    end

    xChunks = map[x-CHUNK_SIZE]
    if xChunks then
        neighbors[2] = xChunks[y] or -1
    else
        neighbors[2] = -1
    end

    xChunks = map[x+CHUNK_SIZE]
    if xChunks then
        neighbors[3] = xChunks[y] or -1
    else
        neighbors[3] = -1
    end
    return neighbors
end

function mapUtils.positionFromDirectionAndChunk(direction, startPosition, endPosition, scaling)
    if (direction == 1) then
        endPosition.x = startPosition.x - CHUNK_SIZE * (scaling - 0.1)
        endPosition.y = startPosition.y - CHUNK_SIZE * (scaling - 0.1)
    elseif (direction == 2) then
        endPosition.x = startPosition.x
        endPosition.y = startPosition.y - CHUNK_SIZE * (scaling + 0.25)
    elseif (direction == 3) then
        endPosition.x = startPosition.x + CHUNK_SIZE * (scaling - 0.1)
        endPosition.y = startPosition.y - CHUNK_SIZE * (scaling - 0.1)
    elseif (direction == 4) then
        endPosition.x = startPosition.x - CHUNK_SIZE * (scaling + 0.25)
        endPosition.y = startPosition.y
    elseif (direction == 5) then
        endPosition.x = startPosition.x + CHUNK_SIZE * (scaling + 0.25)
        endPosition.y = startPosition.y
    elseif (direction == 6) then
        endPosition.x = startPosition.x - CHUNK_SIZE * (scaling - 0.1)
        endPosition.y = startPosition.y + CHUNK_SIZE * (scaling - 0.1)
    elseif (direction == 7) then
        endPosition.x = startPosition.x
        endPosition.y = startPosition.y + CHUNK_SIZE * (scaling + 0.25)
    elseif (direction == 8) then
        endPosition.x = startPosition.x + CHUNK_SIZE * (scaling - 0.1)
        endPosition.y = startPosition.y + CHUNK_SIZE * (scaling - 0.1)
    end
    return endPosition
end

function mapUtils.positionFromDirectionAndFlat(direction, startPosition, endPosition)
    local lx = startPosition.x
    local ly = startPosition.y
    if (direction == 1) then
        lx = lx - CHUNK_SIZE
        ly = ly - CHUNK_SIZE
    elseif (direction == 2) then
        ly = ly - CHUNK_SIZE
    elseif (direction == 3) then
        lx = lx + CHUNK_SIZE
        ly = ly - CHUNK_SIZE
    elseif (direction == 4) then
        lx = lx - CHUNK_SIZE
    elseif (direction == 5) then
        lx = lx + CHUNK_SIZE
    elseif (direction == 6) then
        lx = lx - CHUNK_SIZE
        ly = ly + CHUNK_SIZE
    elseif (direction == 7) then
        ly = ly + CHUNK_SIZE
    elseif (direction == 8) then
        lx = lx + CHUNK_SIZE
        ly = ly + CHUNK_SIZE        
    end
    endPosition.x = lx
    endPosition.y = ly
end


mapUtilsG = mapUtils
return mapUtils
