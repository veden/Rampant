local chunkProcessor = {}

local chunkUtils = require("ChunkUtils")

local processors = {}

function chunkProcessor.processPendingChunks(regionMap, surface, natives, pendingStack)
    local createChunk = chunkUtils.createChunk
    
    local count = 0
    while (#pendingStack > 0) do
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

        count = count + 1
        if (count % 5 == 0) and (#pendingStack < 20) then
            coroutine.yield()
        end
    end
end

function chunkProcessor.install(processor)
    processors[#processors+1] = processor
end

return chunkProcessor