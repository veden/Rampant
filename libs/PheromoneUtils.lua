local pheromoneUtils = {}

-- imports

local mapUtils = require("MapUtils")
local constants = require("Constants")

-- constants

local DEATH_PHEROMONE = constants.DEATH_PHEROMONE
local PLAYER_DEFENSE_PHEROMONE = constants.PLAYER_DEFENSE_PHEROMONE
local PLAYER_BASE_PHEROMONE = constants.PLAYER_BASE_PHEROMONE
local ENEMY_BASE_PHEROMONE = constants.ENEMY_BASE_PHEROMONE
local PLAYER_PHEROMONE = constants.PLAYER_PHEROMONE

local PLAYER_DEFENSE_GENERATOR = constants.PLAYER_DEFENSE_GENERATOR
local PLAYER_BASE_GENERATOR = constants.PLAYER_BASE_GENERATOR
local ENEMY_BASE_GENERATOR = constants.ENEMY_BASE_GENERATOR

local PLAYER_PHEROMONE_GENERATOR_AMOUNT = constants.PLAYER_PHEROMONE_GENERATOR_AMOUNT
local DEATH_PHEROMONE_GENERATOR_AMOUNT = constants.DEATH_PHEROMONE_GENERATOR_AMOUNT

local STANDARD_PHERONOME_DIFFUSION_AMOUNT = constants.STANDARD_PHERONOME_DIFFUSION_AMOUNT
local DEATH_PHEROMONE_DIFFUSION_AMOUNT = constants.DEATH_PHEROMONE_DIFFUSION_AMOUNT

local DEATH_PHEROMONE_PERSISTANCE = constants.DEATH_PHEROMONE_PERSISTANCE
local STANDARD_PHEROMONE_PERSISTANCE = constants.STANDARD_PHEROMONE_PERSISTANCE

-- imported functions

local getChunkByPosition = mapUtils.getChunkByPosition

-- module code
              
function pheromoneUtils.scents(chunk)
    local amount = chunk[PLAYER_DEFENSE_GENERATOR]
    if (amount > 0) then
        chunk[PLAYER_DEFENSE_PHEROMONE] = chunk[PLAYER_DEFENSE_PHEROMONE] + amount
    end
    
    amount = chunk[PLAYER_BASE_GENERATOR]
    if (amount > 0) then
        chunk[PLAYER_BASE_PHEROMONE] = chunk[PLAYER_BASE_PHEROMONE] + amount
    end
    
    amount = chunk[ENEMY_BASE_GENERATOR]
    if (amount > 0) then
        chunk[ENEMY_BASE_PHEROMONE] = chunk[ENEMY_BASE_PHEROMONE] + amount
    end
end
            
function pheromoneUtils.deathScent(regionMap, position)
    local chunk = getChunkByPosition(regionMap, position.x, position.y)
    if (chunk ~= nil) then
        chunk[DEATH_PHEROMONE] = chunk[DEATH_PHEROMONE] + DEATH_PHEROMONE_GENERATOR_AMOUNT
    end
end

function pheromoneUtils.playerScent(regionMap, players)
    for i=1,#players do
        local player = players[i]
        if (player ~= nil) and player.connected and (player.character ~= nil) and player.character.valid and (player.character.surface.index == 1) then
            local playerPosition = player.character.position
            local playerChunk = getChunkByPosition(regionMap, playerPosition.x, playerPosition.y)
            if (playerChunk ~= nil) then
                playerChunk[PLAYER_PHEROMONE] = playerChunk[PLAYER_PHEROMONE] + PLAYER_PHEROMONE_GENERATOR_AMOUNT
            end
        end
    end
end

function pheromoneUtils.processPheromone(chunk, neighbors)
    local diffusionAmount
    local persistence
    for x=1,6 do -- pheromone level indexes on chunks are 1 - 6
        if (x == DEATH_PHEROMONE) then
            diffusionAmount = DEATH_PHEROMONE_DIFFUSION_AMOUNT
            persistence = DEATH_PHEROMONE_PERSISTANCE
        else
            diffusionAmount = STANDARD_PHERONOME_DIFFUSION_AMOUNT
            persistence = STANDARD_PHEROMONE_PERSISTANCE
        end
        local totalDiffused = 0
        local chunkValue = chunk[x]
        for i=1,#neighbors do
            local neighborChunk = neighbors[i]
            if (neighborChunk ~= nil) then
                local diffusedAmount = chunkValue * diffusionAmount
                totalDiffused = totalDiffused + diffusedAmount
                neighborChunk[x] = neighborChunk[x] + diffusedAmount
            end
        end
        chunk[x] = (chunkValue - totalDiffused) * persistence
    end
end

return pheromoneUtils