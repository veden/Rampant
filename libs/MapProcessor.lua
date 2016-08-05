local mapProcessor = {}

local neighborsArray = {1,2,3,4,5,6,7,8}
local processors = {}

function mapProcessor.processMap(regionMap, surface, natives)
    local neighbors = neighborsArray
    local validNeighbors = false
    
    local count = 0
    
    for _,ys in pairs(regionMap) do
        for _,chunk in pairs(ys) do
        
            validNeighbors = false
            -- validNeighbors flag if the processor retrieved the neighbors of the chunk then true
            for i=1, #processors do
                validNeighbors = processors[i](regionMap, surface, natives, chunk, neighbors, validNeighbors)
            end
            
            count = count + 1
            if (count % 1100 == 0) then
                coroutine.yield()
            end
        end
    end
end


function mapProcessor.install(processor)
    processors[#processors+1] = processor
end

return mapProcessor