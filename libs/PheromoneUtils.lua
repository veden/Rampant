local pheromoneUtils = {}

local mapUtils = require("MapUtils")
local constants = require("Constants")
local mMin = math.min
local mFloor = math.floor

local nearestEnemyPosition = {x=1,y=2}
local nearestTable = {position=nearestEnemyPosition,
                      max_distance=constants.CHUNK_SIZE,
                      force="enemy"}

function pheromoneUtils.deathScent(regionMap, surface, x, y, amount)
    
    -- nearestEnemyPosition.x = x
    -- nearestEnemyPosition.y = y
    -- local playerKiller = surface.find_nearest_enemy(nearestTable)
    -- if (playerKiller ~= nil) then
        -- local chunk = regionMap[mathFloor(playerKiller.position.x * 0.03125)]
        -- if (chunk ~= nil) then
            -- chunk = chunk[mathFloor(playerKiller.position.y * 0.03125)]
            -- if (chunk ~= nil) then
                -- chunk[DEATH_PHEROMONE] = chunk[DEATH_PHEROMONE] + amount
            -- end
        -- end 
    -- end
    
    local chunk = mapUtils.getChunkByPosition(regionMap, x, y)
    if (chunk ~= nil) then
        chunk[constants.DEATH_PHEROMONE] = chunk[constants.DEATH_PHEROMONE] + amount
    end
end

function pheromoneUtils.playerDefenseScent(regionMap, surface, natives, chunk, neighbors)    
    local baseScore = chunk[constants.PLAYER_DEFENSE_GENERATOR]
    if (baseScore > 0) then
        chunk[constants.PLAYER_DEFENSE_PHEROMONE] = chunk[constants.PLAYER_DEFENSE_PHEROMONE] + baseScore
    end
end

function pheromoneUtils.playerBaseScent(regionMap, surface, natives, chunk, neighbors)
    local baseScore = chunk[constants.PLAYER_BASE_GENERATOR]
    if (baseScore > 0) then
        chunk[constants.PLAYER_BASE_PHEROMONE] = chunk[constants.PLAYER_BASE_PHEROMONE] + baseScore
    end
end

function pheromoneUtils.enemyBaseScent(regionMap, surface, natives, chunk, neighbors)
    local spawners = chunk[constants.ENEMY_BASE_GENERATOR]
    if (spawners > 0) then
        chunk[constants.ENEMY_BASE_PHEROMONE] = chunk[constants.ENEMY_BASE_PHEROMONE] + spawners
    end
end

function pheromoneUtils.playerScent(regionMap, players)
    local PLAYER_PHEROMONE_GENERATOR_AMOUNT = constants.PLAYER_PHEROMONE_GENERATOR_AMOUNT
    local PLAYER_PHEROMONE = constants.PLAYER_PHEROMONE
    local getChunkByPosition = mapUtils.getChunkByPosition
    local mathFloor = mFloor
    
    for i=1, #players do
        local playerPosition = players[i].position
        local playerChunk = getChunkByPosition(regionMap, playerPosition.x, playerPosition.y)
        if (playerChunk ~= nil) then
            playerChunk[PLAYER_PHEROMONE] = playerChunk[PLAYER_PHEROMONE] + PLAYER_PHEROMONE_GENERATOR_AMOUNT
        end
    end
end

function pheromoneUtils.processPheromone(regionMap, surface, natives, chunk, neighbors)   
    local STANDARD_PHERONOME_DIFFUSION_AMOUNT = constants.STANDARD_PHERONOME_DIFFUSION_AMOUNT
    local DEATH_PHEROMONE_DIFFUSION_AMOUNT = constants.DEATH_PHEROMONE_DIFFUSION_AMOUNT
    local DEATH_PHEROMONE = constants.DEATH_PHEROMONE
    -- local MOVEMENT_PHEROMONE = constants.MOVEMENT_PHEROMONE

    for x=1,5 do
        local diffusionAmount
        local persistence
        if (x == DEATH_PHEROMONE) then
            diffusionAmount = DEATH_PHEROMONE_DIFFUSION_AMOUNT
            persistence = 0.99
        else
            diffusionAmount = STANDARD_PHERONOME_DIFFUSION_AMOUNT
            persistence = 0.98
        end
        local totalDiffused = 0
        for i=1,4 do
            local neighborChunk = neighbors[i]
            if (neighborChunk ~= nil) then
                local diffusedAmount = (chunk[x] * diffusionAmount)
                totalDiffused = totalDiffused + diffusedAmount
                neighborChunk[x] = neighborChunk[x] + diffusedAmount
            end
        end
        chunk[x] = chunk[x] - totalDiffused
        chunk[x] = chunk[x] * persistence
    end
end

return pheromoneUtils