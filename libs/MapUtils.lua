local mapUtils = {}

-- imports

local constants = require("Constants")

-- constants

local NORTH_SOUTH_PASSABLE = constants.NORTH_SOUTH_PASSABLE
local EAST_WEST_PASSABLE = constants.EAST_WEST_PASSABLE

local HALF_CHUNK_SIZE = constants.HALF_CHUNK_SIZE
local CHUNK_SIZE = constants.CHUNK_SIZE

-- imported functions

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
    local neighbors = {1,2,3,4,5,6,7,8}
    local xChunks = regionMap[chunkX-1]
    if (xChunks ~= nil) then
        neighbors[1] = xChunks[chunkY-1]
        neighbors[4] = xChunks[chunkY]
        neighbors[6] = xChunks[chunkY+1]
    else
        neighbors[1] = nil
        neighbors[4] = nil
        neighbors[6] = nil
    end
    
    xChunks = regionMap[chunkX+1]
    if (xChunks ~= nil) then
        neighbors[3] = xChunks[chunkY-1]
        neighbors[5] = xChunks[chunkY]
        neighbors[8] = xChunks[chunkY+1]
    else
        neighbors[3] = nil
        neighbors[5] = nil
        neighbors[8] = nil
    end
    
    xChunks = regionMap[chunkX]
    if (xChunks ~= nil) then
        neighbors[2] = xChunks[chunkY-1]
        neighbors[7] = xChunks[chunkY+1]
    else
        neighbors[2] = nil
        neighbors[7] = nil
    end
    return neighbors
end

function mapUtils.canMoveChunkDirection(direction, startChunk, endChunk)
    local canMove = false
    if ((direction == 2) or (direction == 7)) and startChunk[NORTH_SOUTH_PASSABLE] and endChunk[NORTH_SOUTH_PASSABLE] then
        canMove = true
    elseif ((direction == 4) or (direction == 5)) and startChunk[EAST_WEST_PASSABLE] and endChunk[EAST_WEST_PASSABLE] then
        canMove = true
    elseif (startChunk[NORTH_SOUTH_PASSABLE] and startChunk[EAST_WEST_PASSABLE] and endChunk[NORTH_SOUTH_PASSABLE] and
	    endChunk[EAST_WEST_PASSABLE]) then
	canMove = true
    end
    return canMove
end

function mapUtils.getNeighborChunksWithDirection(regionMap, chunkX, chunkY)   
    local neighbors = {{d=1},{d=2},{d=3},{d=4},{d=5},{d=6},{d=7},{d=8}}
    local xChunks = regionMap[chunkX-1]
    if (xChunks ~= nil) then
	neighbors[1].c = xChunks[chunkY-1]
	neighbors[4].c = xChunks[chunkY]
	neighbors[6].c = xChunks[chunkY+1]
    end
    
    xChunks = regionMap[chunkX+1]
    if (xChunks ~= nil) then
	neighbors[3].c = xChunks[chunkY-1]
	neighbors[5].c = xChunks[chunkY]
	neighbors[8].c = xChunks[chunkY+1]
    end
    
    xChunks = regionMap[chunkX]
    if (xChunks ~= nil) then
	neighbors[2].c = xChunks[chunkY-1]
	neighbors[7].c = xChunks[chunkY+1]
    end
    return neighbors
end

--[[
    1
    |
    2- -3
    |
    4 
]]--
function mapUtils.getCardinalChunksWithDirection(regionMap, chunkX, chunkY)   
    local neighbors = {{d=1},{d=2},{d=3},{d=4}}
    local xChunks = regionMap[chunkX]
    if (xChunks ~= nil) then
	neighbors[1].c = xChunks[chunkY-1]
	neighbors[4].c = xChunks[chunkY+1]
    end
    
    xChunks = regionMap[chunkX-1]
    if (xChunks ~= nil) then
	neighbors[2].c = xChunks[chunkY]
    end
    
    xChunks = regionMap[chunkX+1]
    if (xChunks ~= nil) then
	neighbors[3].c = xChunks[chunkY]
    end
    return neighbors
end

function mapUtils.getCardinalChunks(regionMap, chunkX, chunkY)   
    local neighbors = {1,2,3,4}
    local xChunks = regionMap[chunkX]
    if (xChunks ~= nil) then
	neighbors[1] = xChunks[chunkY-1]
	neighbors[4] = xChunks[chunkY+1]
    else
	neighbors[1] = nil
	neighbors[4] = nil
    end
    
    xChunks = regionMap[chunkX-1]
    if (xChunks ~= nil) then
	neighbors[2] = xChunks[chunkY]
    else
	neighbors[2] = nil
    end
    
    xChunks = regionMap[chunkX+1]
    if (xChunks ~= nil) then
	neighbors[3] = xChunks[chunkY]
    else
	neighbors[3] = nil
    end
    return neighbors
end

function mapUtils.euclideanDistanceNamed(p1, p2)
    local xs = p1.x - p2.x
    local ys = p1.y - p2.y
    return ((xs * xs) + (ys * ys)) ^ 0.5
end

function mapUtils.euclideanDistanceArray(p1, p2)
    local xs = p1[1] - p2[1]
    local ys = p1[2] - p2[2]
    return ((xs * xs) + (ys * ys)) ^ 0.5
end

--[[
    1
    |
    2- -3
    |
    4 
]]--
function mapUtils.canMoveChunkDirectionCardinal(direction, startChunk, endChunk)
    local canMove = false
    if ((direction == 1) or (direction == 4)) and startChunk[NORTH_SOUTH_PASSABLE] and endChunk[NORTH_SOUTH_PASSABLE] then
	canMove = true
    elseif ((direction == 2) or (direction == 3)) and startChunk[EAST_WEST_PASSABLE] and endChunk[EAST_WEST_PASSABLE] then
	canMove = true
    end
    return canMove
end

function mapUtils.positionFromDirectionAndChunkCardinal(direction, startPosition, position)
    -- local position = {x=0,y=0}
    if (direction == 1) then
	position.x = startPosition.x 
	position.y = startPosition.y - CHUNK_SIZE
    elseif (direction == 2) then
	position.x = startPosition.x - CHUNK_SIZE
	position.y = startPosition.y 
    elseif (direction == 3) then
	position.x = startPosition.x + CHUNK_SIZE
	position.y = startPosition.y 
    elseif (direction == 4) then
	position.x = startPosition.x 
	position.y = startPosition.y + CHUNK_SIZE
    end
    -- return position
end

function mapUtils.positionFromDirectionAndChunk(direction, startPosition, position)
    --    local position = {x=0, y=0}
    if (direction == 1) then
	position.x = startPosition.x - CHUNK_SIZE * 1.25   
	position.y = startPosition.y - CHUNK_SIZE * 1.25
    elseif (direction == 2) then
	position.x = startPosition.x
	position.y = startPosition.y - CHUNK_SIZE * 1.25
    elseif (direction == 3) then
	position.x = startPosition.x + CHUNK_SIZE * 1.25
	position.y = startPosition.y - CHUNK_SIZE * 1.25
    elseif (direction == 4) then
	position.x = startPosition.x - CHUNK_SIZE * 1.25
	position.y = startPosition.y
    elseif (direction == 5) then
	position.x = startPosition.x + CHUNK_SIZE * 1.25
	position.y = startPosition.y
    elseif (direction == 6) then
	position.x = startPosition.x - CHUNK_SIZE * 1.25 
	position.y = startPosition.y + CHUNK_SIZE * 1.25
    elseif (direction == 7) then
	position.x = startPosition.x
	position.y = startPosition.y + CHUNK_SIZE * 1.25
    elseif (direction == 8) then
	position.x = startPosition.x + CHUNK_SIZE * 1.25
	position.y = startPosition.y + CHUNK_SIZE * 1.25
    end
    return position
end

return mapUtils
