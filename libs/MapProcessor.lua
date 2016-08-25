local mapProcessor = {}

-- imports

local mapUtils = require("MapUtils")
local pheromoneUtils = require("PheromoneUtils")
local aiBuilding = require("AIBuilding")

-- imported functions

local scents = pheromoneUtils.scents
local processPheromone = pheromoneUtils.processPheromone

local makeScouts = aiBuilding.makeScouts
local formSquads = aiBuilding.formSquads

local getCardinalChunks = mapUtils.getCardinalChunks

-- module code

-- processing is not consistant as it depends on the number of chunks that have been generated
-- so 200 chunks is processed 3 times a second and 1200 chunks is processed once a second
-- In theory, this might be fine as smaller bases have less surface to attack and need to have 
-- pheromone dissipate at a faster rate.
function mapProcessor.processMap(regionMap, surface, natives, evolution_factor)   
    local roll = regionMap.pR
    local index = regionMap.pP
    local scouts = false
    local squads = false
    
    if (regionMap.pP == 1) then
        roll = math.random()
        regionMap.pR = roll
    end
    
    if (0.05 <= roll) and (roll <= 0.10) then
        scouts = true
    end
    
    if (0.11 <= roll) and (roll <= 0.35) then
        squads = true
    end
    
    local chunkQueue = regionMap.pQ[index]
    for x=1,#chunkQueue do
        local chunk = chunkQueue[x]
        
        scents(chunk)
        
        if scouts then
            makeScouts(surface, natives, chunk, evolution_factor)
        end
        if squads then
            formSquads(regionMap, surface, natives, chunk, evolution_factor)
        end
        
        processPheromone(chunk, getCardinalChunks(regionMap, chunk.cX, chunk.cY))
    end
    
    regionMap.pP = regionMap.pP + 1
    if (regionMap.pP > regionMap.pI) then
        regionMap.pP = 1
    end
end

return mapProcessor