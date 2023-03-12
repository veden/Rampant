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


if MapUtilsG then
    return MapUtilsG
end
local MapUtils = {}

--

local Universe
local NeighborChunks

-- imports

local Constants = require("Constants")
local ChunkPropertyUtils = require("ChunkPropertyUtils")

-- Constants

local CHUNK_NORTH_SOUTH = Constants.CHUNK_NORTH_SOUTH
local CHUNK_EAST_WEST = Constants.CHUNK_EAST_WEST
local CHUNK_IMPASSABLE = Constants.CHUNK_IMPASSABLE
local CHUNK_ALL_DIRECTIONS = Constants.CHUNK_ALL_DIRECTIONS

local CHUNK_SIZE = Constants.CHUNK_SIZE

local CHUNK_SIZE_DIVIDER = Constants.CHUNK_SIZE_DIVIDER

-- imported functions

local mFloor = math.floor
local getPassable = ChunkPropertyUtils.getPassable
local tRemove = table.remove
local mCeil = math.ceil

-- module code

function MapUtils.getChunkByXY(map, x, y)
    local chunkX = map[x]
    if chunkX then
        return chunkX[y] or -1
    end
    return -1
end

function MapUtils.getChunkByPosition(map, position)
    local chunkX = map[mFloor(position.x * CHUNK_SIZE_DIVIDER) * CHUNK_SIZE]
    if chunkX then
        local chunkY = mFloor(position.y * CHUNK_SIZE_DIVIDER) * CHUNK_SIZE
        return chunkX[chunkY] or -1
    end
    return -1
end

function MapUtils.getChunkById(chunkId)
    return Universe.chunkIdToChunk[chunkId] or -1
end

function MapUtils.positionToChunkXY(position)
    local chunkX = mFloor(position.x * CHUNK_SIZE_DIVIDER) * CHUNK_SIZE
    local chunkY = mFloor(position.y * CHUNK_SIZE_DIVIDER) * CHUNK_SIZE
    return chunkX, chunkY
end

function MapUtils.queueGeneratedChunk(event)
    local map = Universe.maps[event.surface.index]
    if not map then
        return
    end
    event.tick = (event.tick or game.tick) + 20
    event.id = Universe.eventId
    event.map = map
    Universe.pendingChunks[event.id] = event
    Universe.eventId = Universe.eventId + 1
end

function MapUtils.nextMap()
    local mapIterator = Universe.mapIterator
    repeat
        local map
        Universe.mapIterator, map = next(Universe.maps, Universe.mapIterator)
        if map and map.activeSurface then
            return map
        end
    until mapIterator == Universe.mapIterator
end

function MapUtils.removeChunkToNest(chunkId)
    Universe.chunkToNests[chunkId] = nil
    if (chunkId == Universe.processNestIterator) then
        Universe.processNestIterator = nil
    end
    if (chunkId == Universe.processMigrationIterator) then
        Universe.processMigrationIterator = nil
    end
end

function MapUtils.findInsertionPoint(processQueue, chunk)
    local low = 1
    local high = #processQueue
    local pivot
    while (low <= high) do
        pivot = mCeil((low + high) * 0.5)
        local pivotChunk = processQueue[pivot]
        if (pivotChunk.dOrigin > chunk.dOrigin) then
            high = pivot - 1
        elseif (pivotChunk.dOrigin <= chunk.dOrigin) then
            low = pivot + 1
        end
    end
    return low
end

function MapUtils.removeProcessQueueChunk(processQueue, chunk)
    local insertionPoint = MapUtils.findInsertionPoint(processQueue, chunk)
    if insertionPoint > #processQueue then
        insertionPoint = insertionPoint - 1
    end
    for i=insertionPoint,1,-1 do
        local pqChunk = processQueue[i]
        if pqChunk.id == chunk.id then
            tRemove(processQueue, i)
            return
        elseif pqChunk.dOrigin < chunk.dOrigin then
            return
        end
    end
end

function MapUtils.removeChunkFromMap(map, chunk)
    local chunkId = chunk.id
    local x = chunk.x
    local y = chunk.y
    MapUtils.removeProcessQueueChunk(map.processQueue, chunk)
    map[x][y] = nil
    Universe.chunkIdToChunk[chunkId] = nil
    Universe.chunkToActiveNest[chunkId] = nil
    Universe.chunkToActiveRaidNest[chunkId] = nil
    Universe.chunkToDrained[chunkId] = nil
    Universe.chunkToRetreats[chunkId] = nil
    Universe.chunkToRallys[chunkId] = nil
    Universe.chunkToPassScan[chunkId] = nil
    Universe.chunkToNests[chunkId] = nil
    Universe.vengenceQueue[chunkId] = nil
    Universe.processActiveNest[chunkId] = nil
    Universe.chunkToVictory[chunkId] = nil
    local base = map.chunkToBase[chunkId]
    if base then
        base.chunkCount = base.chunkCount - 1
        map.chunkToBase[chunkId] = nil
    end
    map.chunkToTurrets[chunkId] = nil
    map.chunkToTraps[chunkId] = nil
    map.chunkToUtilities[chunkId] = nil
    map.chunkToHives[chunkId] = nil
    map.chunkToNestIds[chunkId] = nil
    map.chunkToHiveIds[chunkId] = nil
    map.chunkToTrapIds[chunkId] = nil
    map.chunkToTurretIds[chunkId] = nil
    map.chunkToUtilityIds[chunkId] = nil
    map.chunkToPlayerBase[chunkId] = nil
    map.chunkToResource[chunkId] = nil
    map.chunkToPlayerCount[chunkId] = nil
    map.chunkToSquad[chunkId] = nil
    map.chunkToPassable[chunkId] = nil
    map.chunkToPathRating[chunkId] = nil
    map.chunkToDeathGenerator[chunkId] = nil

    if Universe.processActiveNestIterator == chunkId then
        Universe.processActiveNestIterator = nil
    end
    if Universe.victoryScentIterator == chunkId then
        Universe.victoryScentIterator = nil
    end
    if Universe.processNestIterator == chunkId then
        Universe.processNestIterator = nil
    end
    if Universe.chunkToDrainedIterator == chunkId then
        Universe.chunkToDrainedIterator = nil
    end
    if Universe.chunkToRetreatIterator == chunkId then
        Universe.chunkToRetreatIterator = nil
    end
    if Universe.chunkToRallyIterator == chunkId then
        Universe.chunkToRallyIterator = nil
    end
    if Universe.chunkToPassScanIterator == chunkId then
        Universe.chunkToPassScanIterator = nil
    end
    if Universe.processActiveSpawnerIterator == chunkId then
        Universe.processActiveSpawnerIterator = nil
    end
    if Universe.processActiveRaidSpawnerIterator == chunkId then
        Universe.processActiveRaidSpawnerIterator = nil
    end
    if Universe.processMigrationIterator == chunkId then
        Universe.processMigrationIterator = nil
    end
    if Universe.deployVengenceIterator == chunkId then
        Universe.deployVengenceIterator = nil
    end
end

--[[
    1 2 3
    \|/
    4- -5
    /|\
    6 7 8
]]--
function MapUtils.getNeighborChunks(map, x, y)
    local chunkYRow1 = y - CHUNK_SIZE
    local chunkYRow3 = y + CHUNK_SIZE
    local xChunks = map[x-CHUNK_SIZE]
    if xChunks then
        NeighborChunks[1] = xChunks[chunkYRow1] or -1
        NeighborChunks[4] = xChunks[y] or -1
        NeighborChunks[6] = xChunks[chunkYRow3] or -1
    else
        NeighborChunks[1] = -1
        NeighborChunks[4] = -1
        NeighborChunks[6] = -1
    end

    xChunks = map[x+CHUNK_SIZE]
    if xChunks then
        NeighborChunks[3] = xChunks[chunkYRow1] or -1
        NeighborChunks[5] = xChunks[y] or -1
        NeighborChunks[8] = xChunks[chunkYRow3] or -1
    else
        NeighborChunks[3] = -1
        NeighborChunks[5] = -1
        NeighborChunks[8] = -1
    end

    xChunks = map[x]
    if xChunks then
        NeighborChunks[2] = xChunks[chunkYRow1] or -1
        NeighborChunks[7] = xChunks[chunkYRow3] or -1
    else
        NeighborChunks[2] = -1
        NeighborChunks[7] = -1
    end
    return NeighborChunks
end


--[[
    1 2 3
    \|/
    4- -5
    /|\
    6 7 8
]]--
function MapUtils.canMoveChunkDirection(map, direction, startChunk, endChunk)
    local canMove = false
    local startPassable = getPassable(startChunk)
    local endPassable = getPassable(endChunk)
    if (startPassable == CHUNK_ALL_DIRECTIONS) then
        if ((direction == 1) or (direction == 3) or (direction == 6) or (direction == 8)) then
            canMove = (endPassable == CHUNK_ALL_DIRECTIONS)
        elseif (direction == 2) or (direction == 7) then
            canMove = ((endPassable == CHUNK_NORTH_SOUTH) or (endPassable == CHUNK_ALL_DIRECTIONS))
        elseif (direction == 4) or (direction == 5) then
            canMove = ((endPassable == CHUNK_EAST_WEST) or (endPassable == CHUNK_ALL_DIRECTIONS))
        end
    elseif (startPassable == CHUNK_NORTH_SOUTH) then
        if ((direction == 1) or (direction == 3) or (direction == 6) or (direction == 8)) then
            canMove = (endPassable == CHUNK_ALL_DIRECTIONS)
        elseif (direction == 2) or (direction == 7) then
            canMove = ((endPassable == CHUNK_NORTH_SOUTH) or (endPassable == CHUNK_ALL_DIRECTIONS))
        end
    elseif (startPassable == CHUNK_EAST_WEST) then
        if ((direction == 1) or (direction == 3) or (direction == 6) or (direction == 8)) then
            canMove = (endPassable == CHUNK_ALL_DIRECTIONS)
        elseif (direction == 4) or (direction == 5) then
            canMove = ((endPassable == CHUNK_EAST_WEST) or (endPassable == CHUNK_ALL_DIRECTIONS))
        end
    else
        canMove = (endPassable ~= CHUNK_IMPASSABLE)
    end
    return canMove
end

function MapUtils.getCardinalChunks(map, x, y)
    local neighbors = Universe.cardinalNeighbors
    local xChunks = map[x]
    if xChunks then
        neighbors[1] = xChunks[y-CHUNK_SIZE] or -1
        neighbors[4] = xChunks[y+CHUNK_SIZE] or -1
    else
        neighbors[1] = -1
        neighbors[4] = -1
    end

    xChunks = map[x-CHUNK_SIZE]
    if xChunks then
        neighbors[2] = xChunks[y] or -1
    else
        neighbors[2] = -1
    end

    xChunks = map[x+CHUNK_SIZE]
    if xChunks then
        neighbors[3] = xChunks[y] or -1
    else
        neighbors[3] = -1
    end
    return neighbors
end

function MapUtils.positionFromDirectionAndChunk(direction, startPosition, scaling)
    local endPosition = {}
    if (direction == 1) then
        endPosition.x = startPosition.x - CHUNK_SIZE * (scaling - 0.1)
        endPosition.y = startPosition.y - CHUNK_SIZE * (scaling - 0.1)
    elseif (direction == 2) then
        endPosition.x = startPosition.x
        endPosition.y = startPosition.y - CHUNK_SIZE * (scaling + 0.25)
    elseif (direction == 3) then
        endPosition.x = startPosition.x + CHUNK_SIZE * (scaling - 0.1)
        endPosition.y = startPosition.y - CHUNK_SIZE * (scaling - 0.1)
    elseif (direction == 4) then
        endPosition.x = startPosition.x - CHUNK_SIZE * (scaling + 0.25)
        endPosition.y = startPosition.y
    elseif (direction == 5) then
        endPosition.x = startPosition.x + CHUNK_SIZE * (scaling + 0.25)
        endPosition.y = startPosition.y
    elseif (direction == 6) then
        endPosition.x = startPosition.x - CHUNK_SIZE * (scaling - 0.1)
        endPosition.y = startPosition.y + CHUNK_SIZE * (scaling - 0.1)
    elseif (direction == 7) then
        endPosition.x = startPosition.x
        endPosition.y = startPosition.y + CHUNK_SIZE * (scaling + 0.25)
    elseif (direction == 8) then
        endPosition.x = startPosition.x + CHUNK_SIZE * (scaling - 0.1)
        endPosition.y = startPosition.y + CHUNK_SIZE * (scaling - 0.1)
    end
    return endPosition
end

function MapUtils.positionFromDirectionAndFlat(direction, startPosition, multipler)
    local lx = startPosition.x
    local ly = startPosition.y
    if not multipler then
        multipler = 1
    end
    if (direction == 1) then
        lx = lx - CHUNK_SIZE * multipler
        ly = ly - CHUNK_SIZE * multipler
    elseif (direction == 2) then
        ly = ly - CHUNK_SIZE * multipler
    elseif (direction == 3) then
        lx = lx + CHUNK_SIZE * multipler
        ly = ly - CHUNK_SIZE * multipler
    elseif (direction == 4) then
        lx = lx - CHUNK_SIZE * multipler
    elseif (direction == 5) then
        lx = lx + CHUNK_SIZE * multipler
    elseif (direction == 6) then
        lx = lx - CHUNK_SIZE * multipler
        ly = ly + CHUNK_SIZE * multipler
    elseif (direction == 7) then
        ly = ly + CHUNK_SIZE * multipler
    elseif (direction == 8) then
        lx = lx + CHUNK_SIZE * multipler
        ly = ly + CHUNK_SIZE * multipler
    end
    return {
        x = lx,
        y = ly
    }
end

function MapUtils.init(universe)
    Universe = universe
    NeighborChunks = universe.neighbors
end

MapUtilsG = MapUtils
return MapUtils
