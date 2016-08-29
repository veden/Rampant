local mapProcessor = {}

-- imports

local mapUtils = require("MapUtils")
local pheromoneUtils = require("PheromoneUtils")
local aiBuilding = require("AIBuilding")
local constants = require("Constants")

-- imported functions

local scents = pheromoneUtils.scents
local processPheromone = pheromoneUtils.processPheromone

local makeScouts = aiBuilding.makeScouts
local formSquads = aiBuilding.formSquads

local getCardinalChunks = mapUtils.getCardinalChunks

local mMin = math.min

-- module code

-- processing is not consistant as it depends on the number of chunks that have been generated
-- so 200 chunks is processed 3 times a second and 1200 chunks is processed once a second
-- In theory, this might be fine as smaller bases have less surface to attack and need to have 
-- pheromone dissipate at a faster rate.
function mapProcessor.processMap(regionMap, surface, natives, pheromoneTotals, evolution_factor)   
    local roll = regionMap.processRoll
    local index = regionMap.processPointer
    local scouts = false
    local squads = false
    
    if (index == 1) then
        roll = math.random()
        regionMap.processRoll = roll
    end
    
    if (0.05 <= roll) and (roll <= 0.10) then
        scouts = true
    end
    
    if (0.11 <= roll) and (roll <= 0.35) then
        squads = true
    end
    
    local processQueue = regionMap.processQueue
    local endIndex = mMin(index + constants.PROCESS_QUEUE_SIZE, #processQueue)
    for x=index,endIndex do
        local chunk = processQueue[x]
        
        scents(chunk, pheromoneTotals)
        
        if scouts then
            makeScouts(surface, natives, chunk, evolution_factor)
        end
        if squads then
            formSquads(regionMap, surface, natives, chunk, evolution_factor)
        end
        
        processPheromone(chunk, getCardinalChunks(regionMap, chunk.cX, chunk.cY), pheromoneTotals)
    end
    
    if (endIndex == #processQueue) then
        regionMap.processPointer = 1
    else
        regionMap.processPointer = endIndex + 1
    end
end

function mapProcessor.scanMap(regionMap, surface)
    local index = regionMap.scanPointer
        
    local processQueue = regionMap.processQueue
    local endIndex = mMin(index + constants.SCAN_QUEUE_SIZE, #processQueue)
    for x=index,endIndex do
        local chunk = processQueue[x]
        
        local spawners = surface.count_entities_filtered({area = {{chunk.pX, chunk.pY},
                                                                  {chunk.pX + constants.CHUNK_SIZE, chunk.pY + constants.CHUNK_SIZE}},
                                                          type = "unit-spawner",
                                                          force = "enemy"})
        chunk[constants.ENEMY_BASE_GENERATOR] = spawners * constants.ENEMY_BASE_PHEROMONE_GENERATOR_AMOUNT
    end
    
    if (endIndex == #processQueue) then
        regionMap.scanPointer = 1
    else
        regionMap.scanPointer = endIndex + 1
    end
end

return mapProcessor