local mapUtils = {}

local constants = require("Constants")

local mFloor = math.floor
local mMin = math.min
local mMax = math.max

function mapUtils.getChunkByPosition(regionMap, x, y)
    local cX = mFloor(x * 0.03125)
    if (regionMap[cX] ~= nil) then
        return regionMap[cX][mFloor(y * 0.03125)]
    end
    return nil
end

function mapUtils.removeUnitSpawner(regionMap, x, y)
    local bases = mapUtils.getChunkByPosition(regionMap, x, y)
    if (bases ~= nil) then
        bases.bG = mMax(0, bases.bG - 1)
    end
end

--[[
    1 2 3
     \|/
    4- -5
     /|\
    6 7 8
]]--
function mapUtils.getNeighborChunks(regionMap, chunkX, chunkY, neighbors)   
    local xChunks = regionMap[chunkX-1]
    if (xChunks ~= nil) then
        neighbors[1] = xChunks[chunkY-1]
        neighbors[4] = xChunks[chunkY]
        neighbors[6] = xChunks[chunkY+1]
    end
    
    xChunks = regionMap[chunkX+1]
    if (xChunks ~= nil) then
        neighbors[3] = xChunks[chunkY-1]
        neighbors[5] = xChunks[chunkY]
        neighbors[8] = xChunks[chunkY+1]
    end
    
    xChunks = regionMap[chunkX]
    neighbors[2] = xChunks[chunkY-1]
    neighbors[7] = xChunks[chunkY+1]
end

return mapUtils