local regionUtils = {}

local constants = require("Constants")

local regionMaps
local neighborArray = {1,2,3,4,5,6,7,8}
local mExp = math.exp
local mFloor = math.floor
local mMin = math.min
local mMax = math.max

function regionUtils.addChunkToRegionMap(regionMap, chunk)

    local chunkX = mFloor(chunk.x * 0.03125)
    local chunkY = mFloor(chunk.y * 0.03125)

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

function regionUtils.getChunkByPosition(regionMap, x, y)
    local chunkX = mFloor(x * 0.03125)
    local chunkY = mFloor(y * 0.03125)
    
    return regionMap[chunkX][chunkY]
end

function regionUtils.getChunkByIndex(regionMap, chunkX, chunkY)
    return regionMap[chunkX][chunkY]
end

function regionUtils.processPheromone(regionMap, surface)
    local mathMin = mMin
    local neighbors = neighborArray
    local getNeighborChunks = regionUtils.getNeighborChunks
    local DIFFUSION_AMOUNT = constants.DIFFUSION_AMOUNT
    local MAX_PHEROMONE = constants.MAX_PHEROMONE
    local BASE_PHEROMONE = constants.BASE_PHEROMONE
    
    local count = 0
    
    for _,ys in pairs(regionMap) do
        for _,chunk in pairs(ys) do
            if (chunk.bG > 0) then
                chunk[BASE_PHEROMONE] = chunk[BASE_PHEROMONE] + (chunk.bG * 10)
            end
            local chunks
            for x=1,3 do
                if (chunk[x] > 75) then
                    if (chunks == nil) then
                        chunks = getNeighborChunks(regionMap, chunk.x, chunk.y, neighbors)
                    end
                    local totalDiffused = 0
                    for i=1,8 do
                        local neighborChunk = chunks[i]
                        if (neighborChunk ~= nil) then
                            local diffusedAmount = (chunk[x] * DIFFUSION_AMOUNT)
                            totalDiffused = totalDiffused + diffusedAmount
                            neighborChunk[x] = mathMin(MAX_PHEROMONE, neighborChunk[x] + diffusedAmount)
                        end
                    end
                    chunk[x] = chunk[x] - totalDiffused
                end
                chunk[x] = chunk[x] * 0.95
                if (chunk[x] < 2) then
                    chunk[x] = 0
                end
            end
            count = count + 1
            if (count % 1250 == 0) then
                coroutine.yield()
            end
        end
    end
end

function regionUtils.placePheromone(regionMap, x, y, pType, amount)
    local chunk = regionUtils.getChunkByPosition(regionMap, x, y)
    chunk[pType] = mMin(constants.MAX_PHEROMONE, chunk[pType] + amount)
end

--[[
    1 2 3
     \|/
    4- -5
     /|\
    6 7 8
]]--
function regionUtils.getNeighborChunks(regionMap, chunkX, chunkY, neighbors)   
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

function regionUtils.init(maps)
    regionMaps = maps
end

function regionUtils.pheromoneProcess()
    local player = game.players[1]
    regionUtils.placePheromone(regionMaps[player.surface.index], player.position.x, player.position.y, constants.PLAYER_PHEROMONE, 100)
    regionUtils.processPheromone(regionMaps[player.surface.index], player.surface, 2)
end

return regionUtils