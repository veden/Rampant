local mapProcessor = {}

-- imports

local mapUtils = require("MapUtils")
local pheromoneUtils = require("PheromoneUtils")
local aiBuilding = require("AIBuilding")

-- imported functions

local scents = pheromoneUtils.scents
local processPheromone = pheromoneUtils.processPheromone

local sendScouts = aiBuilding.sendScouts
local formSquads = aiBuilding.formSquads

local getCardinalChunks = mapUtils.getCardinalChunks

local mRandom = math.random

-- module code

-- processing is not consistant as it depends on the number of chunks that have been generated
-- so 200 chunks is processed 3 times a second and 1200 chunks is processed once a second
-- In theory, this might be fine as smaller bases have less surface to attack and need to have 
-- pheromone dissipate at a faster rate.
function mapProcessor.processMap(regionMap, surface, natives, evolution_factor)   
    if (regionMap.pP == 1) then
        regionMap.pR = mRandom()
    end
    
    local roll = regionMap.pR

    local chunkQueue = regionMap.pQ[regionMap.pP]
    for x=1, #chunkQueue do
        local chunk = chunkQueue[x]
        local cardinalArray = getCardinalChunks(regionMap, chunk.cX, chunk.cY)
        
        scents(regionMap, surface, natives, chunk, cardinalArray, evolution_factor)
        
        if (0.05 <= roll) and (roll <= 0.10) then
            sendScouts(regionMap, surface, natives, chunk, cardinalArray, evolution_factor)
        end
        if (0.11 <= roll) and (roll <= 0.25) then
            formSquads(regionMap, surface, natives, chunk, cardinalArray, evolution_factor)
        end
        
        processPheromone(regionMap, surface, natives, chunk, cardinalArray, evolution_factor)
    end
    
    regionMap.pP = regionMap.pP + 1
    if (regionMap.pP > regionMap.pI) then
        regionMap.pP = 1
    end
end

return mapProcessor