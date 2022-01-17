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

local VICTORY_SCENT_MULTIPLER = constants.VICTORY_SCENT_MULTIPLER
local VICTORY_SCENT_BOUND = constants.VICTORY_SCENT_BOUND

local MAGIC_MAXIMUM_NUMBER = constants.MAGIC_MAXIMUM_NUMBER

local CHUNK_ALL_DIRECTIONS = constants.CHUNK_ALL_DIRECTIONS
local CHUNK_NORTH_SOUTH = constants.CHUNK_NORTH_SOUTH
local CHUNK_EAST_WEST = constants.CHUNK_EAST_WEST

local BASE_PHEROMONE = constants.BASE_PHEROMONE
local PLAYER_PHEROMONE = constants.PLAYER_PHEROMONE
local RESOURCE_PHEROMONE = constants.RESOURCE_PHEROMONE

local VICTORY_SCENT = constants.VICTORY_SCENT

local PLAYER_PHEROMONE_GENERATOR_AMOUNT = constants.PLAYER_PHEROMONE_GENERATOR_AMOUNT

local DEATH_PHEROMONE_GENERATOR_AMOUNT = constants.DEATH_PHEROMONE_GENERATOR_AMOUNT

-- imported functions

local addVictoryGenerator = chunkPropertyUtils.addVictoryGenerator

local getPlayersOnChunk = chunkPropertyUtils.getPlayersOnChunk

local getNeighborChunks = mapUtils.getNeighborChunks
local getChunkById = mapUtils.getChunkById

local getEnemyStructureCount = chunkPropertyUtils.getEnemyStructureCount
local getPathRating = chunkPropertyUtils.getPathRating
local getPassable = chunkPropertyUtils.getPassable
local getPlayerBaseGenerator = chunkPropertyUtils.getPlayerBaseGenerator
local getResourceGenerator = chunkPropertyUtils.getResourceGenerator
local addDeathGenerator = chunkPropertyUtils.addDeathGenerator

local decayDeathGenerator = chunkPropertyUtils.decayDeathGenerator

local linearInterpolation = mathUtils.linearInterpolation

local getChunkByXY = mapUtils.getChunkByXY

local next = next

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
                    addDeathGenerator(map, c, -pheromonePack.v * VICTORY_SCENT_MULTIPLER[i] * getPathRating(map, c))
                end
                i = i + 1
            end
        end
    end
end

function pheromoneUtils.deathScent(map, chunk)
    addDeathGenerator(map, chunk, DEATH_PHEROMONE_GENERATOR_AMOUNT)
end

function pheromoneUtils.processStaticPheromone(map, chunk)
    local chunkBase = -MAGIC_MAXIMUM_NUMBER
    local chunkResource = -MAGIC_MAXIMUM_NUMBER
    local chunkPathRating = getPathRating(map, chunk)

    local clear = getEnemyStructureCount(map, chunk)

    local tempNeighbors = getNeighborChunks(map, chunk.x, chunk.y)

    local neighbor
    local neighborPass

    local chunkPass = getPassable(map, chunk)
    local pheromone
    if (chunkPass == CHUNK_ALL_DIRECTIONS) then
        neighbor = tempNeighbors[2]
        if (neighbor ~= -1) then
            neighborPass = getPassable(map, neighbor)
            if ((neighborPass == CHUNK_ALL_DIRECTIONS) or (neighborPass == CHUNK_NORTH_SOUTH)) then
                pheromone = neighbor[BASE_PHEROMONE]
                if chunkBase < pheromone then
                    chunkBase = pheromone
                end
                pheromone = neighbor[RESOURCE_PHEROMONE]
                if chunkResource < pheromone then
                    chunkResource = pheromone
                end
            end
        end

        neighbor = tempNeighbors[7]
        if (neighbor ~= -1) then
            neighborPass = getPassable(map, neighbor)
            if ((neighborPass == CHUNK_ALL_DIRECTIONS) or (neighborPass == CHUNK_NORTH_SOUTH)) then
                pheromone = neighbor[BASE_PHEROMONE]
                if chunkBase < pheromone then
                    chunkBase = pheromone
                end
                pheromone = neighbor[RESOURCE_PHEROMONE]
                if chunkResource < pheromone then
                    chunkResource = pheromone
                end
            end
        end

        neighbor = tempNeighbors[4]
        if (neighbor ~= -1) then
            neighborPass = getPassable(map, neighbor)
            if ((neighborPass == CHUNK_ALL_DIRECTIONS) or (neighborPass == CHUNK_EAST_WEST)) then
                pheromone = neighbor[BASE_PHEROMONE]
                if chunkBase < pheromone then
                    chunkBase = pheromone
                end
                pheromone = neighbor[RESOURCE_PHEROMONE]
                if chunkResource < pheromone then
                    chunkResource = pheromone
                end
            end
        end

        neighbor = tempNeighbors[5]
        if (neighbor ~= -1) then
            neighborPass = getPassable(map, neighbor)
            if ((neighborPass == CHUNK_ALL_DIRECTIONS) or (neighborPass == CHUNK_EAST_WEST)) then
                pheromone = neighbor[BASE_PHEROMONE]
                if chunkBase < pheromone then
                    chunkBase = pheromone
                end
                pheromone = neighbor[RESOURCE_PHEROMONE]
                if chunkResource < pheromone then
                    chunkResource = pheromone
                end
            end
        end

        neighbor = tempNeighbors[1]
        if (neighbor ~= -1) then
            neighborPass = getPassable(map, neighbor)
            if (neighborPass == CHUNK_ALL_DIRECTIONS) then
                pheromone = neighbor[BASE_PHEROMONE]
                if chunkBase < pheromone then
                    chunkBase = pheromone
                end
                pheromone = neighbor[RESOURCE_PHEROMONE]
                if chunkResource < pheromone then
                    chunkResource = pheromone
                end
            end
        end

        neighbor = tempNeighbors[3]
        if (neighbor ~= -1) then
            neighborPass = getPassable(map, neighbor)
            if (neighborPass == CHUNK_ALL_DIRECTIONS) then
                pheromone = neighbor[BASE_PHEROMONE]
                if chunkBase < pheromone then
                    chunkBase = pheromone
                end
                pheromone = neighbor[RESOURCE_PHEROMONE]
                if chunkResource < pheromone then
                    chunkResource = pheromone
                end
            end
        end

        neighbor = tempNeighbors[6]
        if (neighbor ~= -1) then
            neighborPass = getPassable(map, neighbor)
            if (neighborPass == CHUNK_ALL_DIRECTIONS) then
                pheromone = neighbor[BASE_PHEROMONE]
                if chunkBase < pheromone then
                    chunkBase = pheromone
                end
                pheromone = neighbor[RESOURCE_PHEROMONE]
                if chunkResource < pheromone then
                    chunkResource = pheromone
                end
            end
        end

        neighbor = tempNeighbors[8]
        if (neighbor ~= -1) then
            neighborPass = getPassable(map, neighbor)
            if (neighborPass == CHUNK_ALL_DIRECTIONS) then
                pheromone = neighbor[BASE_PHEROMONE]
                if chunkBase < pheromone then
                    chunkBase = pheromone
                end
                pheromone = neighbor[RESOURCE_PHEROMONE]
                if chunkResource < pheromone then
                    chunkResource = pheromone
                end
            end
        end
    elseif (chunkPass == CHUNK_EAST_WEST) then

        neighbor = tempNeighbors[4]
        if (neighbor ~= -1) then
            neighborPass = getPassable(map, neighbor)
            if ((neighborPass == CHUNK_ALL_DIRECTIONS) or (neighborPass == CHUNK_EAST_WEST)) then
                pheromone = neighbor[BASE_PHEROMONE]
                if chunkBase < pheromone then
                    chunkBase = pheromone
                end
                pheromone = neighbor[RESOURCE_PHEROMONE]
                if chunkResource < pheromone then
                    chunkResource = pheromone
                end
            end
        end

        neighbor = tempNeighbors[5]
        if (neighbor ~= -1) then
            neighborPass = getPassable(map, neighbor)
            if ((neighborPass == CHUNK_ALL_DIRECTIONS) or (neighborPass == CHUNK_EAST_WEST)) then
                pheromone = neighbor[BASE_PHEROMONE]
                if chunkBase < pheromone then
                    chunkBase = pheromone
                end
                pheromone = neighbor[RESOURCE_PHEROMONE]
                if chunkResource < pheromone then
                    chunkResource = pheromone
                end
            end
        end
    elseif (chunkPass == CHUNK_NORTH_SOUTH) then

        neighbor = tempNeighbors[2]
        if (neighbor ~= -1) then
            neighborPass = getPassable(map, neighbor)
            if ((neighborPass == CHUNK_ALL_DIRECTIONS) or (neighborPass == CHUNK_NORTH_SOUTH)) then
                pheromone = neighbor[BASE_PHEROMONE]
                if chunkBase < pheromone then
                    chunkBase = pheromone
                end
                pheromone = neighbor[RESOURCE_PHEROMONE]
                if chunkResource < pheromone then
                    chunkResource = pheromone
                end
            end
        end

        neighbor = tempNeighbors[7]
        if (neighbor ~= -1) then
            neighborPass = getPassable(map, neighbor)
            if ((neighborPass == CHUNK_ALL_DIRECTIONS) or (neighborPass == CHUNK_NORTH_SOUTH)) then
                pheromone = neighbor[BASE_PHEROMONE]
                if chunkBase < pheromone then
                    chunkBase = pheromone
                end
                pheromone = neighbor[RESOURCE_PHEROMONE]
                if chunkResource < pheromone then
                    chunkResource = pheromone
                end
            end
        end
    end

    chunkBase = chunkBase * 0.9
    pheromone = getPlayerBaseGenerator(map, chunk)
    if chunkBase < pheromone then
        chunk[BASE_PHEROMONE] = pheromone * chunkPathRating
    else
        chunk[BASE_PHEROMONE] = chunkBase * chunkPathRating
    end

    chunkResource = chunkResource * 0.9
    pheromone = getResourceGenerator(map, chunk)
    if (pheromone > 0) and clear then
        pheromone = linearInterpolation(pheromone, 15000, 20000)
    end
    if chunkResource < pheromone then
        if clear then
            chunk[RESOURCE_PHEROMONE] = pheromone * chunkPathRating
        else
            chunk[RESOURCE_PHEROMONE] = pheromone * chunkPathRating * 0.01
        end
    else
        if clear then
            chunk[RESOURCE_PHEROMONE] = chunkResource * chunkPathRating
        else
            chunk[RESOURCE_PHEROMONE] = chunkResource * chunkPathRating * 0.01
        end
    end
end

function pheromoneUtils.processPheromone(map, chunk, player)
    local chunkPlayer = -MAGIC_MAXIMUM_NUMBER
    local chunkPathRating = getPathRating(map, chunk)

    local tempNeighbors = getNeighborChunks(map, chunk.x, chunk.y)

    local neighbor
    local neighborPass

    local chunkPass = getPassable(map, chunk)
    local pheromone
    if (chunkPass == CHUNK_ALL_DIRECTIONS) then
        neighbor = tempNeighbors[2]
        if (neighbor ~= -1) then
            neighborPass = getPassable(map, neighbor)
            if ((neighborPass == CHUNK_ALL_DIRECTIONS) or (neighborPass == CHUNK_NORTH_SOUTH)) then
                pheromone = neighbor[PLAYER_PHEROMONE]
                if chunkPlayer < pheromone then
                    chunkPlayer = pheromone
                end
            end
        end

        neighbor = tempNeighbors[7]
        if (neighbor ~= -1) then
            neighborPass = getPassable(map, neighbor)
            if ((neighborPass == CHUNK_ALL_DIRECTIONS) or (neighborPass == CHUNK_NORTH_SOUTH)) then
                pheromone = neighbor[PLAYER_PHEROMONE]
                if chunkPlayer < pheromone then
                    chunkPlayer = pheromone
                end
            end
        end

        neighbor = tempNeighbors[4]
        if (neighbor ~= -1) then
            neighborPass = getPassable(map, neighbor)
            if ((neighborPass == CHUNK_ALL_DIRECTIONS) or (neighborPass == CHUNK_EAST_WEST)) then
                pheromone = neighbor[PLAYER_PHEROMONE]
                if chunkPlayer < pheromone then
                    chunkPlayer = pheromone
                end
            end
        end

        neighbor = tempNeighbors[5]
        if (neighbor ~= -1) then
            neighborPass = getPassable(map, neighbor)
            if ((neighborPass == CHUNK_ALL_DIRECTIONS) or (neighborPass == CHUNK_EAST_WEST)) then
                pheromone = neighbor[PLAYER_PHEROMONE]
                if chunkPlayer < pheromone then
                    chunkPlayer = pheromone
                end
            end
        end

        neighbor = tempNeighbors[1]
        if (neighbor ~= -1) then
            neighborPass = getPassable(map, neighbor)
            if (neighborPass == CHUNK_ALL_DIRECTIONS) then
                pheromone = neighbor[PLAYER_PHEROMONE]
                if chunkPlayer < pheromone then
                    chunkPlayer = pheromone
                end
            end
        end

        neighbor = tempNeighbors[3]
        if (neighbor ~= -1) then
            neighborPass = getPassable(map, neighbor)
            if (neighborPass == CHUNK_ALL_DIRECTIONS) then
                pheromone = neighbor[PLAYER_PHEROMONE]
                if chunkPlayer < pheromone then
                    chunkPlayer = pheromone
                end
            end
        end

        neighbor = tempNeighbors[6]
        if (neighbor ~= -1) then
            neighborPass = getPassable(map, neighbor)
            if (neighborPass == CHUNK_ALL_DIRECTIONS) then
                pheromone = neighbor[PLAYER_PHEROMONE]
                if chunkPlayer < pheromone then
                    chunkPlayer = pheromone
                end
            end
        end

        neighbor = tempNeighbors[8]
        if (neighbor ~= -1) then
            neighborPass = getPassable(map, neighbor)
            if (neighborPass == CHUNK_ALL_DIRECTIONS) then
                pheromone = neighbor[PLAYER_PHEROMONE]
                if chunkPlayer < pheromone then
                    chunkPlayer = pheromone
                end
            end
        end
    elseif (chunkPass == CHUNK_EAST_WEST) then

        neighbor = tempNeighbors[4]
        if (neighbor ~= -1) then
            neighborPass = getPassable(map, neighbor)
            if ((neighborPass == CHUNK_ALL_DIRECTIONS) or (neighborPass == CHUNK_EAST_WEST)) then
                pheromone = neighbor[PLAYER_PHEROMONE]
                if chunkPlayer < pheromone then
                    chunkPlayer = pheromone
                end
            end
        end

        neighbor = tempNeighbors[5]
        if (neighbor ~= -1) then
            neighborPass = getPassable(map, neighbor)
            if ((neighborPass == CHUNK_ALL_DIRECTIONS) or (neighborPass == CHUNK_EAST_WEST)) then
                pheromone = neighbor[PLAYER_PHEROMONE]
                if chunkPlayer < pheromone then
                    chunkPlayer = pheromone
                end
            end
        end
    elseif (chunkPass == CHUNK_NORTH_SOUTH) then

        neighbor = tempNeighbors[2]
        if (neighbor ~= -1) then
            neighborPass = getPassable(map, neighbor)
            if ((neighborPass == CHUNK_ALL_DIRECTIONS) or (neighborPass == CHUNK_NORTH_SOUTH)) then
                pheromone = neighbor[PLAYER_PHEROMONE]
                if chunkPlayer < pheromone then
                    chunkPlayer = pheromone
                end
            end
        end

        neighbor = tempNeighbors[7]
        if (neighbor ~= -1) then
            neighborPass = getPassable(map, neighbor)
            if ((neighborPass == CHUNK_ALL_DIRECTIONS) or (neighborPass == CHUNK_NORTH_SOUTH)) then
                pheromone = neighbor[PLAYER_PHEROMONE]
                if chunkPlayer < pheromone then
                    chunkPlayer = pheromone
                end
            end
        end
    end

    if not player then
        decayDeathGenerator(map, chunk)
    end

    chunkPlayer = chunkPlayer * 0.45
    pheromone = getPlayersOnChunk(map, chunk) * PLAYER_PHEROMONE_GENERATOR_AMOUNT
    if chunkPlayer < pheromone then
        chunk[PLAYER_PHEROMONE] = pheromone * chunkPathRating
    else
        chunk[PLAYER_PHEROMONE] = chunkPlayer * chunkPathRating
    end
end

pheromoneUtilsG = pheromoneUtils
return pheromoneUtils
