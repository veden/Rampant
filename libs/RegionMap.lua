local DIFFUSION_AMOUNT = 0.04
local MAX_PLAYER_PHEROMONE = 7000
local neighborArray = {1,2,3,4,5,6,7,8}
local mExp = math.exp
local mFloor = math.floor
local mMin = math.min
local mMax = math.max

function addChunkToRegionMap(regionMap, x, y, surface)

    local chunkX = mFloor(x * 0.03125)
    local chunkY = mFloor(y * 0.03125)
    
    local chunk = createChunk(x, y, surface)
    chunk.x = chunkX
    chunk.y = chunkY
    if (regionMap == nil) then
        regionMap = {}
    end
    if regionMap[chunkX] == nil then
        regionMap[chunkX] = {}
    end
    regionMap[chunkX][chunkY] = chunk
end

function getChunkByPosition(regionMap, x, y)
    local chunkX = mFloor(x * 0.03125)
    local chunkY = mFloor(y * 0.03125)
    
    return regionMap[chunkX][chunkY]
end

function getChunkByIndex(regionMap, chunkX, chunkY)
    return regionMap[chunkX][chunkY]
end

function processPheromone(regionMap, surface)
    local mathMin = mMin
    local mathExp = mExp
    local neighbors = neighborArray
    for _,ys in pairs(regionMap) do
        for _,chunk in pairs(ys) do
            local chunks
            for x=1,3 do
                if (chunk[x] > 50) then
                    if (chunks == nil) then
                        chunks = getNeighborChunks(regionMap, chunk.x, chunk.y, neighbors)
                    end
                    local totalDiffused = 0
                    for i=1,8 do
                        local neighborChunk = chunks[i]
                        if (neighborChunk ~= nil) then
                            local diffusedAmount = (chunk[x] * DIFFUSION_AMOUNT)
                            totalDiffused = totalDiffused + diffusedAmount
                            neighborChunk[x] = mathMin(MAX_PLAYER_PHEROMONE, neighborChunk[x] + diffusedAmount)
                        end
                    end
                    chunk[x] = chunk[x] - totalDiffused
                end
                chunk[x] = chunk[x] * 0.95
                if (chunk[x] < 2) then
                    chunk[x] = 0
                end
            end
            -- local printMe = false
            -- for x=1,3 do
                -- if (chunk[x] > 0) then
                    -- printMe = true
                -- end
            -- end
            -- if (printMe) then
                -- print(serpent.dump(chunk))
            -- end
        end
    end
    -- print("--")
end

function placePheromone(regionMap, x, y, pType, amount)
    local chunk = getChunkByPosition(regionMap, x, y)
    chunk[pType] = mMin(MAX_PLAYER_PHEROMONE, chunk[pType] + amount)
end

--[[
    1 2 3
     \|/
    4- -5
     /|\
    6 7 8
]]--
function getNeighborChunks(regionMap, chunkX, chunkY, neighbors)   
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
    
    return neighbors
end
