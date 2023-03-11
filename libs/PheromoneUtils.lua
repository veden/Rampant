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


if PheromoneUtilsG then
    return PheromoneUtilsG
end
local PheromoneUtils = {}

--

local Universe

-- imports
local MathUtils = require("MathUtils")
local MapUtils = require("MapUtils")
local Constants = require("Constants")
local ChunkPropertyUtils = require("ChunkPropertyUtils")

-- Constants

local CHUNK_TICK = Constants.CHUNK_TICK

local ENEMY_PHEROMONE_MULTIPLER = Constants.ENEMY_PHEROMONE_MULTIPLER
local VICTORY_SCENT_MULTIPLER = Constants.VICTORY_SCENT_MULTIPLER
local VICTORY_SCENT_BOUND = Constants.VICTORY_SCENT_BOUND

local MAGIC_MAXIMUM_NUMBER = Constants.MAGIC_MAXIMUM_NUMBER

local BASE_PHEROMONE = Constants.BASE_PHEROMONE
local PLAYER_PHEROMONE = Constants.PLAYER_PHEROMONE
local RESOURCE_PHEROMONE = Constants.RESOURCE_PHEROMONE
local ENEMY_PHEROMONE = Constants.ENEMY_PHEROMONE

local VICTORY_SCENT = Constants.VICTORY_SCENT

local DEATH_PHEROMONE_GENERATOR_AMOUNT = Constants.DEATH_PHEROMONE_GENERATOR_AMOUNT
local TEN_DEATH_PHEROMONE_GENERATOR_AMOUNT = Constants.TEN_DEATH_PHEROMONE_GENERATOR_AMOUNT

-- imported functions

local decayPlayerGenerator = ChunkPropertyUtils.decayPlayerGenerator
local addVictoryGenerator = ChunkPropertyUtils.addVictoryGenerator
local getCombinedDeathGenerator = ChunkPropertyUtils.getCombinedDeathGenerator
local getCombinedDeathGeneratorRating = ChunkPropertyUtils.getCombinedDeathGeneratorRating
local setDeathGenerator = ChunkPropertyUtils.setDeathGenerator

local getPlayerGenerator = ChunkPropertyUtils.getPlayerGenerator

local addPermanentDeathGenerator = ChunkPropertyUtils.addPermanentDeathGenerator

local canMoveChunkDirection = MapUtils.canMoveChunkDirection
local getNeighborChunks = MapUtils.getNeighborChunks
local getChunkById = MapUtils.getChunkById

local getEnemyStructureCount = ChunkPropertyUtils.getEnemyStructureCount
local getPathRating = ChunkPropertyUtils.getPathRating
local getPlayerBaseGenerator = ChunkPropertyUtils.getPlayerBaseGenerator
local getResourceGenerator = ChunkPropertyUtils.getResourceGenerator
local addDeathGenerator = ChunkPropertyUtils.addDeathGenerator

local decayDeathGenerator = ChunkPropertyUtils.decayDeathGenerator

local linearInterpolation = MathUtils.linearInterpolation

local getChunkByXY = MapUtils.getChunkByXY

local next = next
local mMax = math.max

-- module code

function PheromoneUtils.victoryScent(map, chunk, entityType)
    local value = VICTORY_SCENT[entityType]
    if value then
        addVictoryGenerator(map, chunk, value)
    end
end

function PheromoneUtils.disperseVictoryScent()
    local chunkId = Universe.victoryScentIterator
    local chunkToVictory = Universe.chunkToVictory
    local pheromonePack
    if not chunkId then
        chunkId, pheromonePack = next(chunkToVictory, nil)
    else
        pheromonePack = chunkToVictory[chunkId]
    end
    if not chunkId then
        Universe.victoryScentIterator = nil
    else
        Universe.victoryScentIterator = next(chunkToVictory, chunkId)
        chunkToVictory[chunkId] = nil
        local map = pheromonePack.map
        if not map.surface.valid then
            return
        end
        local chunk = getChunkById(chunkId)
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

function PheromoneUtils.deathScent(map, chunk, structure)
    local amount = -DEATH_PHEROMONE_GENERATOR_AMOUNT
    if structure then
        amount = -TEN_DEATH_PHEROMONE_GENERATOR_AMOUNT
    end
    addDeathGenerator(map, chunk, amount)
    addPermanentDeathGenerator(map, chunk, amount)
end

function PheromoneUtils.processPheromone(map, chunk, tick, player)
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

function PheromoneUtils.init(universe)
    Universe = universe
end

PheromoneUtilsG = PheromoneUtils
return PheromoneUtils
