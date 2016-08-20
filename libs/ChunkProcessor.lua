local chunkProcessor = {}

-- imports

local chunkUtils = require("ChunkUtils")
local constants = require("Constants")

-- constants

local CHUNK_MAX_QUEUE_SIZE = constants.CHUNK_MAX_QUEUE_SIZE

-- imported functions

local createChunk = chunkUtils.createChunk

-- premade tables

local processors = {}

-- module code

function chunkProcessor.processPendingChunks(regionMap, surface, natives, pendingStack)
    local processAll = false
    
    if (#pendingStack > 100) then
        processAll = true
    end
    
    local count = 0
    for i=#pendingStack, 1, -1 do
        local event = pendingStack[#pendingStack]
        pendingStack[#pendingStack] = nil

        local chunk = createChunk(event.area.left_top.x,
                                  event.area.left_top.y)

        local chunkX = chunk.cX
        
        if regionMap[chunkX] == nil then
            regionMap[chunkX] = {}
        end
        regionMap[chunkX][chunk.cY] = chunk
        
        for i=1, #processors do
            processors[i](chunk, surface, natives)
        end
        
        local processQueue = regionMap.pQ[regionMap.pI]
        if (#processQueue == CHUNK_MAX_QUEUE_SIZE) then
            regionMap.pI = regionMap.pI + 1
            regionMap.pQ[regionMap.pI] = {}
            processQueue = regionMap.pQ[regionMap.pI]
        end
        
        processQueue[#processQueue+1] = chunk
        
        count = count + 1
        if (count == 5) and not processAll then
            break
        end
    end
end

function chunkProcessor.install(processor)
    processors[#processors+1] = processor
end

return chunkProcessor