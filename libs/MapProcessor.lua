local mapProcessor = {}

local mapUtils = require("MapUtils")

local mRandom = math.random
local neighborsArray = {1,2,3,4,5,6,7,8}
local cardinalArray = {1,2,3,4}
local processors = {}
local processorsProbabilityLow = {}
local processorsProbabilityHigh = {}

function mapProcessor.processMap(regionMap, surface, natives)
    local getNeighborChunks = mapUtils.getCardinalChunks
    local mathRandom = mRandom
    
    local neighbors = cardinalArray    
    local count = 0
    
    local roll = mathRandom()
    print("--")
    print(roll)
    print("--")
    for _,ys in pairs(regionMap) do
        for _,chunk in pairs(ys) do
        
            getNeighborChunks(regionMap, chunk.cX, chunk.cY, neighbors)
            -- validNeighbors flag if the processor retrieved the neighbors of the chunk then true
            for i=1, #processors do
                if (processorsProbabilityLow[i] <= roll) and (roll <= processorsProbabilityHigh[i]) then
                    validNeighbors = processors[i](regionMap, surface, natives, chunk, neighbors)
                end
            end
            
            count = count + 1
            if (count % 1000 == 0) then
                coroutine.yield()
            end
        end
    end
end


function mapProcessor.install(processor, useProbabilityLow, useProbabilityHigh)
    processors[#processors+1] = processor
    processorsProbabilityLow[#processorsProbabilityLow+1] = useProbabilityLow
    processorsProbabilityHigh[#processorsProbabilityHigh+1] = useProbabilityHigh
end

return mapProcessor