local pheromoneUtils = {}

-- imports

local mapUtils = require("MapUtils")
local constants = require("Constants")

-- constants

local CHUNK_SIZE = constants.CHUNK_SIZE

local DEATH_PHEROMONE = constants.DEATH_PHEROMONE
local PLAYER_DEFENSE_PHEROMONE = constants.PLAYER_DEFENSE_PHEROMONE
local PLAYER_BASE_PHEROMONE = constants.PLAYER_BASE_PHEROMONE
local ENEMY_BASE_PHEROMONE = constants.ENEMY_BASE_PHEROMONE
local PLAYER_PHEROMONE = constants.PLAYER_PHEROMONE

local PLAYER_DEFENSE_GENERATOR = constants.PLAYER_DEFENSE_GENERATOR
local PLAYER_BASE_GENERATOR = constants.PLAYER_BASE_GENERATOR
local ENEMY_BASE_GENERATOR = constants.ENEMY_BASE_GENERATOR

local PLAYER_PHEROMONE_GENERATOR_AMOUNT = constants.PLAYER_PHEROMONE_GENERATOR_AMOUNT

local STANDARD_PHERONOME_DIFFUSION_AMOUNT = constants.STANDARD_PHERONOME_DIFFUSION_AMOUNT
local DEATH_PHEROMONE_DIFFUSION_AMOUNT = constants.DEATH_PHEROMONE_DIFFUSION_AMOUNT

-- imported functions

local getChunkByPosition = mapUtils.getChunkByPosition

local mFloor = math.floor

-- module code
                      
function pheromoneUtils.deathScent(regionMap, surface, x, y, squad, amount)

    if (squad ~= nil) and (squad.cX ~= nil) then
        local chunk = getChunkByPosition(regionMap, x, y)
        if (chunk ~= nil) then
            chunk[DEATH_PHEROMONE] = chunk[DEATH_PHEROMONE] + (amount * 5)
        end
    end
    
    local chunk = getChunkByPosition(regionMap, x, y)
    if (chunk ~= nil) then
        chunk[DEATH_PHEROMONE] = chunk[DEATH_PHEROMONE] + amount
    end
end

function pheromoneUtils.playerDefenseScent(regionMap, surface, natives, chunk, neighbors, evolution_factor)    
    local baseScore = chunk[PLAYER_DEFENSE_GENERATOR]
    if (baseScore > 0) then
        chunk[PLAYER_DEFENSE_PHEROMONE] = chunk[PLAYER_DEFENSE_PHEROMONE] + baseScore
    end
end

function pheromoneUtils.playerBaseScent(regionMap, surface, natives, chunk, neighbors, evolution_factor)
    local baseScore = chunk[PLAYER_BASE_GENERATOR]
    if (baseScore > 0) then
        chunk[PLAYER_BASE_PHEROMONE] = chunk[PLAYER_BASE_PHEROMONE] + baseScore
    end
end

function pheromoneUtils.enemyBaseScent(regionMap, surface, natives, chunk, neighbors, evolution_factor)
    local spawners = chunk[ENEMY_BASE_GENERATOR]
    if (spawners > 0) then
        chunk[ENEMY_BASE_PHEROMONE] = chunk[ENEMY_BASE_PHEROMONE] + spawners
    end
end

function pheromoneUtils.playerScent(regionMap, players)
    for i=1, #players do
        local playerPosition = players[i].position
        local playerChunk = getChunkByPosition(regionMap, playerPosition.x, playerPosition.y)
        if (playerChunk ~= nil) then
            playerChunk[PLAYER_PHEROMONE] = playerChunk[PLAYER_PHEROMONE] + PLAYER_PHEROMONE_GENERATOR_AMOUNT
        end
    end
end

function pheromoneUtils.processPheromone(regionMap, surface, natives, chunk, neighbors, evolution_factor)
    for x=1,6 do
        local diffusionAmount
        local persistence
        if (x == DEATH_PHEROMONE) then
            diffusionAmount = DEATH_PHEROMONE_DIFFUSION_AMOUNT
            persistence = 0.99
        else
            diffusionAmount = STANDARD_PHERONOME_DIFFUSION_AMOUNT
            persistence = 0.95
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