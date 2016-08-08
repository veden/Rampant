local pheromoneUtils = {}

local mapUtils = require("MapUtils")
local constants = require("Constants")
local mMin = math.min
local mFloor = math.floor

function pheromoneUtils.deathScent(regionMap, x, y, amount)
    local mathFloor = mFloor
    local DEATH_PHEROMONE = constants.DEATH_PHEROMONE
    
    local chunk = regionMap[mathFloor(x * 0.03125)]
    if (chunk ~= nil) then
        chunk = chunk[mathFloor(y * 0.03125)]
        if (chunk ~= nil) then
            chunk[DEATH_PHEROMONE] = chunk[DEATH_PHEROMONE] + amount
        end
    end
end

function pheromoneUtils.enemyBaseScent(regionMap, surface, natives, chunk, neighbors, validNeighbors)
    local BASE_PHEROMONE_PRODUCTION = constants.BASE_PHEROMONE_PRODUCTION
    local ENEMY_BASE_PHEROMONE = constants.ENEMY_BASE_PHEROMONE

    local spawners = chunk.bG
    if (spawners > 0) then
        chunk[ENEMY_BASE_PHEROMONE] = chunk[ENEMY_BASE_PHEROMONE] + (spawners * constants.BASE_PHEROMONE_PRODUCTION)
    end
    return validNeighbors
end

function pheromoneUtils.playerScent(regionMap, players)
    local placePheromoneByPosition = pheromoneUtils.placePheromoneByPosition
    local PLAYER_PHEROMONE = constants.PLAYER_PHEROMONE
    local mathFloor = mFloor
    
    for i=1, #players do
        local playerPosition = players[i].position
        local playerChunk = regionMap[mathFloor(playerPosition.x * 0.03125)][mathFloor(playerPosition.y * 0.03125)]
        playerChunk[PLAYER_PHEROMONE] = playerChunk[PLAYER_PHEROMONE] + 300
    end
end

function pheromoneUtils.processPheromone(regionMap, surface, natives, chunk, neighbors, validNeighbors)
    local mathMin = mMin
    local getNeighborChunks = mapUtils.getNeighborChunks
    local DIFFUSION_AMOUNT = constants.DIFFUSION_AMOUNT
    local DEATH_DIFFUSION_AMOUNT = constants.DEATH_DIFFUSION_AMOUNT
    local MAX_PHEROMONE = constants.MAX_PHEROMONE
    local DEATH_PHEROMONE = constants.DEATH_PHEROMONE

    for x=1,3 do
        local threshold
        local diffusionAmount
        if (x == DEATH_PHEROMONE) then
            threshold = 125
            diffusionAmount = DEATH_DIFFUSION_AMOUNT
        else
            threshold = 75
            diffusionAmount = DIFFUSION_AMOUNT
        end
        if (chunk[x] > threshold) then
            if not validNeighbors then
                getNeighborChunks(regionMap, chunk.cX, chunk.cY, neighbors)
                validNeighbors = true
            end
            local totalDiffused = 0
            for i=1,8 do
                local neighborChunk = neighbors[i]
                if (neighborChunk ~= nil) then
                    local diffusedAmount = (chunk[x] * diffusionAmount)
                    totalDiffused = totalDiffused + diffusedAmount
                    neighborChunk[x] = mathMin(MAX_PHEROMONE, neighborChunk[x] + diffusedAmount)
                end
            end
            chunk[x] = chunk[x] - totalDiffused
        end
        chunk[x] = chunk[x] * 0.995
        if (chunk[x] < 2) then
            chunk[x] = 0
        end
    end
    return validNeighbors
end

return pheromoneUtils