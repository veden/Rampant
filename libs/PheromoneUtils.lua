-- Copyright (C) 2022  veden

-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <https://www.gnu.org/licenses/>.


if pheromoneUtilsG then
    return pheromoneUtilsG
end
local pheromoneUtils = {}

-- imports
local mathUtils = require("MathUtils")
local mapUtils = require("MapUtils")
local constants = require("Constants")
local chunkPropertyUtils = require("ChunkPropertyUtils")

-- constants

local CHUNK_TICK = constants.CHUNK_TICK

local ENEMY_PHEROMONE_MULTIPLER = constants.ENEMY_PHEROMONE_MULTIPLER
local VICTORY_SCENT_MULTIPLER = constants.VICTORY_SCENT_MULTIPLER
local VICTORY_SCENT_BOUND = constants.VICTORY_SCENT_BOUND

local MAGIC_MAXIMUM_NUMBER = constants.MAGIC_MAXIMUM_NUMBER

local BASE_PHEROMONE = constants.BASE_PHEROMONE
local PLAYER_PHEROMONE = constants.PLAYER_PHEROMONE
local RESOURCE_PHEROMONE = constants.RESOURCE_PHEROMONE
local ENEMY_PHEROMONE = constants.ENEMY_PHEROMONE

local VICTORY_SCENT = constants.VICTORY_SCENT

local DEATH_PHEROMONE_GENERATOR_AMOUNT = constants.DEATH_PHEROMONE_GENERATOR_AMOUNT
local TEN_DEATH_PHEROMONE_GENERATOR_AMOUNT = constants.TEN_DEATH_PHEROMONE_GENERATOR_AMOUNT

-- imported functions

local decayPlayerGenerator = chunkPropertyUtils.decayPlayerGenerator
local addVictoryGenerator = chunkPropertyUtils.addVictoryGenerator
local getCombinedDeathGenerator = chunkPropertyUtils.getCombinedDeathGenerator
local getCombinedDeathGeneratorRating = chunkPropertyUtils.getCombinedDeathGeneratorRating
local setDeathGenerator = chunkPropertyUtils.setDeathGenerator

local getPlayerGenerator = chunkPropertyUtils.getPlayerGenerator

local addPermanentDeathGenerator = chunkPropertyUtils.addPermanentDeathGenerator

local canMoveChunkDirection = mapUtils.canMoveChunkDirection
local getNeighborChunks = mapUtils.getNeighborChunks
local getChunkById = mapUtils.getChunkById

local getEnemyStructureCount = chunkPropertyUtils.getEnemyStructureCount
local getPathRating = chunkPropertyUtils.getPathRating
local getPlayerBaseGenerator = chunkPropertyUtils.getPlayerBaseGenerator
local getResourceGenerator = chunkPropertyUtils.getResourceGenerator
local addDeathGenerator = chunkPropertyUtils.addDeathGenerator

local decayDeathGenerator = chunkPropertyUtils.decayDeathGenerator

local linearInterpolation = mathUtils.linearInterpolation

local getChunkByXY = mapUtils.getChunkByXY

local next = next
local mMax = math.max

-- module code

function pheromoneUtils.victoryScent(map, chunk, entityType)
    local value = VICTORY_SCENT[entityType]
    if value then
        addVictoryGenerator(map, chunk, value)
    end
end

function pheromoneUtils.disperseVictoryScent(universe)
    local chunkId = universe.victoryScentIterator
    local chunkToVictory = universe.chunkToVictory
    local pheromonePack
    if not chunkId then
        chunkId, pheromonePack = next(chunkToVictory, nil)
    else
        pheromonePack = chunkToVictory[chunkId]
    end
    if not chunkId then
        universe.victoryScentIterator = nil
    else
        universe.victoryScentIterator = next(chunkToVictory, chunkId)
        chunkToVictory[chunkId] = nil
        local map = pheromonePack.map
        if not map.surface.valid then
            return
        end
        local chunk = getChunkById(map, chunkId)
        local chunkX = chunk.x
        local chunkY = chunk.y
        local i = 1
        for x=chunkX - VICTORY_SCENT_BOUND, chunkX + VICTORY_SCENT_BOUND,32 do
            for y = chunkY - VICTORY_SCENT_BOUND, chunkY + VICTORY_SCENT_BOUND,32 do
                local c = getChunkByXY(map, x, y)
                if (c ~= -1) then
                    local amount = pheromonePack.v * VICTORY_SCENT_MULTIPLER[i]
                    addDeathGenerator(map, c, amount)
                    addPermanentDeathGenerator(map, c, amount)
                end
                i = i + 1
            end
        end
    end
end

function pheromoneUtils.deathScent(map, chunk, structure)
    local amount = -DEATH_PHEROMONE_GENERATOR_AMOUNT
    if structure then
        amount = -TEN_DEATH_PHEROMONE_GENERATOR_AMOUNT
    end
    addDeathGenerator(map, chunk, amount)
    addPermanentDeathGenerator(map, chunk, amount)
end

function pheromoneUtils.processPheromone(map, chunk, tick, player)
    if chunk[CHUNK_TICK] > tick then
        return
    end
    chunk[CHUNK_TICK] = tick

    local chunkPlayer = chunk[PLAYER_PHEROMONE]
    local chunkBase = -MAGIC_MAXIMUM_NUMBER
    local chunkDeath = getCombinedDeathGenerator(map, chunk)
    local chunkResource = -MAGIC_MAXIMUM_NUMBER
    local chunkEnemy = chunk[ENEMY_PHEROMONE]

    local chunkCount = 1

    local enemyStructureCount = getEnemyStructureCount(map, chunk)

    local tempNeighbors = getNeighborChunks(map, chunk.x, chunk.y)
    for i=1,8 do
        local tempPheromone
        local neighbor = tempNeighbors[i]
        if (neighbor ~= -1) then
            if canMoveChunkDirection(map, i, chunk, neighbor) then
                chunkCount = chunkCount + 1
                chunkPlayer = chunkPlayer + neighbor[PLAYER_PHEROMONE]
                chunkEnemy = chunkEnemy + neighbor[ENEMY_PHEROMONE]
                chunkDeath = chunkDeath + getCombinedDeathGenerator(map, neighbor)
                tempPheromone = neighbor[BASE_PHEROMONE]
                if chunkBase < tempPheromone then
                    chunkBase = tempPheromone
                end
                tempPheromone = neighbor[RESOURCE_PHEROMONE]
                if chunkResource < tempPheromone then
                    chunkResource = tempPheromone
                end
            end
        end
    end

    setDeathGenerator(map, chunk, (chunkDeath / chunkCount) * 0.75)

    if not player then
        decayDeathGenerator(map, chunk)
    end

    decayPlayerGenerator(map, chunk)

    local chunkDeathRating = getCombinedDeathGeneratorRating(map, chunk) * getPathRating(map, chunk)

    chunk[PLAYER_PHEROMONE] = chunkDeathRating * mMax(
        getPlayerGenerator(map, chunk),
        (chunkPlayer / chunkCount) * 0.98
                                                     )

    chunk[BASE_PHEROMONE] = chunkDeathRating * mMax(
        getPlayerBaseGenerator(map, chunk),
        chunkBase * 0.9
                                                   )

    chunk[ENEMY_PHEROMONE] = chunkDeathRating * mMax(
        enemyStructureCount * ENEMY_PHEROMONE_MULTIPLER,
        (chunkEnemy / chunkCount) * 0.9
                                                    )

    local resourcePheromoneGenerator = getResourceGenerator(map, chunk)
    if (resourcePheromoneGenerator > 0) then
        chunkResource = linearInterpolation(resourcePheromoneGenerator, 15000, 20000)
    end
    if enemyStructureCount ~= 0 then
        chunkResource = chunkResource * 0.0001
    end
    chunk[RESOURCE_PHEROMONE] = chunkDeathRating * chunkResource * 0.9
end

pheromoneUtilsG = pheromoneUtils
return pheromoneUtils
