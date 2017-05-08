local chunkProcessor = {}

-- imports

local chunkUtils = require("ChunkUtils")
local baseUtils = require("BaseUtils")

-- imported functions

local createChunk = chunkUtils.createChunk
local checkChunkPassability = chunkUtils.checkChunkPassability
local scoreChunk = chunkUtils.scoreChunk

local annexNest = baseUtils.annexNest

-- module code

function chunkProcessor.processPendingChunks(regionMap, surface, pendingStack, natives)
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
        scoreChunk(chunk, surface)
	annexNest(natives, chunk, event.tick)
        processQueue[#processQueue+1] = chunk
    end
end

return chunkProcessor
