local mapProcessor = {}

-- imports

local mapUtils = require("MapUtils")

-- imported functions

local getCardinalChunks = mapUtils.getCardinalChunks

local mRandom = math.random

-- premade tables 

local processors = {}
local processorsProbabilityLow = {}
local processorsProbabilityHigh = {}

-- module code

function mapProcessor.processMap(regionMap, surface, natives, evolution_factor)   
    local count = 0
    
    if (regionMap.pP == 1) then
        regionMap.pR = mRandom()
    end
    
    local roll = regionMap.pR
    
    local chunkQueue = regionMap.pQ[regionMap.pP]
    for x=1, #chunkQueue do
        local chunk = chunkQueue[x]
        local cardinalArray = getCardinalChunks(regionMap, chunk.cX, chunk.cY)
        for i=1, #processors do
            if (processorsProbabilityLow[i] <= roll) and (roll <= processorsProbabilityHigh[i]) then
                processors[i](regionMap, surface, natives, chunk, cardinalArray, evolution_factor)
            end
        end
    end
    
    regionMap.pP = regionMap.pP + 1
    if (regionMap.pP > regionMap.pI) then
        regionMap.pP = 1
    end
end


function mapProcessor.install(processor, useProbabilityLow, useProbabilityHigh)
    processors[#processors+1] = processor
    processorsProbabilityLow[#processorsProbabilityLow+1] = useProbabilityLow
    processorsProbabilityHigh[#processorsProbabilityHigh+1] = useProbabilityHigh
end

return mapProcessor