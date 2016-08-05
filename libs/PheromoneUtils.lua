local pheromoneUtils = {}

local mapUtils = require("MapUtils")
local constants = require("Constants")
local mMin = math.min
local mFloor = math.floor

function pheromoneUtils.deathScent(regionMap, x, y, amount)
    pheromoneUtils.placePheromoneByPosition(regionMap, 
                                            x, 
                                            y,
                                            constants.DEATH_PHEROMONE,
                                            200)
end

function pheromoneUtils.enemyBaseScent(regionMap, surface, natives, chunk, neighbors, validNeighbors)
    local BASE_PHEROMONE = constants.BASE_PHEROMONE
    local x = chunk.cX
    local y = chunk.cY
    
    local nativeBase = natives.bases[x]
    if (nativeBase ~= nil) then
        nativeBase = nativeBase[y]
        if (nativeBase ~= nil) then
            local spawners = nativeBase.bG
            if (spawners > 0) then
                local nativeChunk = regionMap[x][y]
                nativeChunk[BASE_PHEROMONE] = nativeChunk[BASE_PHEROMONE] + (spawners * 35)
            end
        end
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
        playerChunk[PLAYER_PHEROMONE] = playerChunk[PLAYER_PHEROMONE] + 100
    end
end

function pheromoneUtils.placePheromoneByPosition(regionMap, x, y, pType, amount)
    local chunk = mapUtils.getChunkByPosition(regionMap, x, y)
    if (chunk~=nil) then
        chunk[pType] = mMin(constants.MAX_PHEROMONE, chunk[pType] + amount)
    end
end

function pheromoneUtils.placePheromoneByChunk(regionMap, x, y, pType, amount)
    local chunk = regionMap[x][y]
    chunk[pType] = mMin(constants.MAX_PHEROMONE, chunk[pType] + amount)
end

function pheromoneUtils.processPheromone(regionMap, surface, natives, chunk, neighbors, validNeighbors)
    local mathMin = mMin
    local getNeighborChunks = mapUtils.getNeighborChunks
    local DIFFUSION_AMOUNT = constants.DIFFUSION_AMOUNT
    local MAX_PHEROMONE = constants.MAX_PHEROMONE

    for x=1,3 do
        if ((x ~= DEATH_PHEROMONE) and (chunk[x] > 75)) or ((x == DEATH_PHEROMONE) and (chunk[x] > 125)) then
            if not validNeighbors then
                getNeighborChunks(regionMap, chunk.cX, chunk.cY, neighbors)
                validNeighbors = true
            end
            local totalDiffused = 0
            for i=1,8 do
                local neighborChunk = neighbors[i]
                if (neighborChunk ~= nil) then
                    local diffusedAmount = (chunk[x] * DIFFUSION_AMOUNT)
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