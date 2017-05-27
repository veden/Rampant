local chunkProcessor = {}

-- imports

local chunkUtils = require("ChunkUtils")

-- imported functions

local createChunk = chunkUtils.createChunk
local checkChunkPassability = chunkUtils.checkChunkPassability
local scoreChunk = chunkUtils.scoreChunk

-- module code

function chunkProcessor.processPendingChunks(natives, regionMap, surface, pendingStack, tick)
    local processQueue = regionMap.processQueue
    
    for _=#pendingStack, 1, -1 do
        local event = pendingStack[#pendingStack]
        pendingStack[#pendingStack] = nil

        local chunk = createChunk(event.area.left_top.x, event.area.left_top.y)

        local chunkX = chunk.cX
        
        if regionMap[chunkX] == nil then
            regionMap[chunkX] = {}
        end
        regionMap[chunkX][chunk.cY] = chunk
        
        checkChunkPassability(chunk, surface)
        scoreChunk(regionMap, chunk, surface, natives, tick)
        processQueue[#processQueue+1] = chunk
    end
end

return chunkProcessor
