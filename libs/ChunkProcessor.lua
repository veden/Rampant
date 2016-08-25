local chunkProcessor = {}

-- imports

local chunkUtils = require("ChunkUtils")
local constants = require("Constants")

-- constants

local CHUNK_MAX_QUEUE_SIZE = constants.CHUNK_MAX_QUEUE_SIZE

-- imported functions

local createChunk = chunkUtils.createChunk
local checkChunkPassability = chunkUtils.checkChunkPassability
local scoreChunk = chunkUtils.scoreChunk

-- module code

function chunkProcessor.processPendingChunks(regionMap, surface, natives, pendingStack)
  
    for i=#pendingStack, 1, -1 do
        local event = pendingStack[#pendingStack]
        pendingStack[#pendingStack] = nil

        local chunk = createChunk(event.area.left_top.x, event.area.left_top.y)

        local chunkX = chunk.cX
        
        if regionMap[chunkX] == nil then
            regionMap[chunkX] = {}
        end
        regionMap[chunkX][chunk.cY] = chunk
        
        checkChunkPassability(chunk, surface, natives)
        scoreChunk(chunk, surface, natives)
        
        local processQueue = regionMap.pQ[regionMap.pI]
        if (#processQueue == CHUNK_MAX_QUEUE_SIZE) then
            regionMap.pI = regionMap.pI + 1
            regionMap.pQ[regionMap.pI] = {}
            processQueue = regionMap.pQ[regionMap.pI]
        end
        
        processQueue[#processQueue+1] = chunk
    end
end

return chunkProcessor