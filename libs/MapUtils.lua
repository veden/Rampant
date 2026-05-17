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
local MapPosition

-- imports

local Constants = require("Constants")
local ChunkPropertyUtils = require("ChunkPropertyUtils")
local MathUtils = require("MathUtils")

-- Constants

local MAGIC_MAXIMUM_NUMBER = Constants.MAGIC_MAXIMUM_NUMBER
local ENEMY_PHEROMONE_MULTIPLER = Constants.ENEMY_PHEROMONE_MULTIPLER
local BASE_PHEROMONE = Constants.BASE_PHEROMONE
local PLAYER_PHEROMONE = Constants.PLAYER_PHEROMONE
local RESOURCE_PHEROMONE = Constants.RESOURCE_PHEROMONE
local ENEMY_PHEROMONE = Constants.ENEMY_PHEROMONE
local KAMIKAZE_PHEROMONE = Constants.KAMIKAZE_PHEROMONE

local CHUNK_TICK = Constants.CHUNK_TICK

local VICTORY_SCENT = Constants.VICTORY_SCENT
local VICTORY_SCENT_MULTIPLER = Constants.VICTORY_SCENT_MULTIPLER
local VICTORY_SCENT_BOUND = Constants.VICTORY_SCENT_BOUND
local DEATH_PHEROMONE_GENERATOR_AMOUNT = Constants.DEATH_PHEROMONE_GENERATOR_AMOUNT
local TEN_DEATH_PHEROMONE_GENERATOR_AMOUNT = Constants.TEN_DEATH_PHEROMONE_GENERATOR_AMOUNT
local CHUNK_NORTH_SOUTH = Constants.CHUNK_NORTH_SOUTH
local CHUNK_EAST_WEST = Constants.CHUNK_EAST_WEST
local CHUNK_IMPASSABLE = Constants.CHUNK_IMPASSABLE
local CHUNK_ALL_DIRECTIONS = Constants.CHUNK_ALL_DIRECTIONS

local CHUNK_SIZE = Constants.CHUNK_SIZE

local CHUNK_SIZE_DIVIDER = Constants.CHUNK_SIZE_DIVIDER

-- imported functions

local addVictoryGenerator = ChunkPropertyUtils.addVictoryGenerator
local addPermanentDeathGenerator = ChunkPropertyUtils.addPermanentDeathGenerator
local addDeathGenerator = ChunkPropertyUtils.addDeathGenerator
local getCombinedDeathGenerator = ChunkPropertyUtils.getCombinedDeathGenerator
local getCombinedDeathGeneratorRating = ChunkPropertyUtils.getCombinedDeathGeneratorRating
local getEnemyStructureCount = ChunkPropertyUtils.getEnemyStructureCount
local setDeathGenerator = ChunkPropertyUtils.setDeathGenerator
local decayPlayerGenerator = ChunkPropertyUtils.decayPlayerGenerator
local getPathRating = ChunkPropertyUtils.getPathRating
local linearInterpolation = MathUtils.linearInterpolation
local decayDeathGenerator = ChunkPropertyUtils.decayDeathGenerator

local mMax = math.max
local mFloor = math.floor
local getPassable = ChunkPropertyUtils.getPassable
local tRemove = table.remove
local mCeil = math.ceil
local sFind = string.find

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
    Universe.eventId = Universe.eventId + 1
    event.id = Universe.eventId
    event.map = map
    Universe.pendingChunks[event.id] = event
end

function MapUtils.activateMap(map)
    local mapId = map.id
    if Universe.activeMaps[mapId] then
	return
    end

    Universe.activeMaps[mapId] = map
end

function MapUtils.deactivateMap(map)
    local mapId = map.id
    if not Universe.activeMaps[mapId] then
	return
    end

    Universe.activeMaps[mapId] = nil
end

function MapUtils.excludeSurface()
    for mapId,map in pairs(Universe.maps) do
        local toBeRemoved = not map.surface.valid
            or MapUtils.isExcludedSurface(map.surface.name)
            or Universe.excludedSurfaces[map.surface.name]

        if toBeRemoved then
            if Universe.mapIterator == mapId then
                Universe.mapIterator, Universe.currentMap = next(
                    Universe.activeMaps,
                    Universe.mapIterator
                )
            end
            if Universe.processMapAIIterator == mapId then
                Universe.processMapAIIterator = nil
            end
            Universe.maps[mapId] = nil
        end
    end
end

function MapUtils.addExcludedSurface(surfaceName)
    Universe.excludedSurfaces[surfaceName] = true
    MapUtils.excludeSurface()
end

function MapUtils.removeExcludedSurface(surfaceName)
    Universe.excludedSurfaces[surfaceName] = nil
    local surface = game.get_surface(surfaceName)
    if surface then
        MapUtils.prepMap(surface)
    end
end

function MapUtils.isExcludedSurface(surfaceName)
    return
        (surfaceName == "aai-signals") or
        (surfaceName == "RTStasisRealm") or
        (surfaceName == "minime_dummy_dungeon") or
        (surfaceName == "minime-preview-character") or
        (surfaceName == "pipelayer") or
        (surfaceName == "beltlayer") or

        sFind(surfaceName, "Factory floor") or
        sFind(surfaceName, " Orbit") or
        sFind(surfaceName, "clonespace") or
        sFind(surfaceName, "BPL_TheLabplayer") or
        sFind(surfaceName, "starmap%-") or
        sFind(surfaceName, "NiceFill") or
        sFind(surfaceName, "Asteroid Belt") or
        sFind(surfaceName, "Vault ") or
        sFind(surfaceName, "spaceship") or
        sFind(surfaceName, "bpsb%-lab%-")
end

function MapUtils.queueChunks(surface, chunks, flush, tick)
    for chunk in chunks do
        if surface.is_chunk_generated(chunk) then
            MapUtils.queueGeneratedChunk(
                {
                    surface = surface,
                    tick = tick,
                    area = {
                        left_top = {
                            x = chunk.x * 32,
                            y = chunk.y * 32
                        }
                    }
                }
            )
        end
    end

    Universe.flushPendingChunks = flush
end

function MapUtils.prepMap(surface)
    if Universe.maps[surface.index] then
        return Universe.maps[surface.index]
    end

    local surfaceName = surface.name
    if MapUtils.isExcludedSurface(surfaceName) or Universe.excludedSurfaces[surfaceName] then
        return
    end

    game.print("Rampant - Indexing surface:" .. surfaceName .. ", index:" .. tostring(surface.index) .. ", please wait.")

    local map = {
        id = surface.index,
        processedChunks = 0,
        processQueue = {},
        processIndex = 1,
        scanPlayerIndex = 1,
        scanResourceIndex = 1,
        scanEnemyIndex = 1,
        outgoingScanWave = true,
        outgoingStaticScanWave = true,

        drainPylons = {},

        surface = surface,

        bases = {}
    }

    Universe.maps[map.id] = map
    local tick = game.tick

    -- queue all current chunks that wont be generated during play
    MapUtils.queueChunks(surface, surface.get_chunks(), true, tick)

    return map
end

function MapUtils.nextMap()
    local map = Universe.currentMap
    if map and (Universe.processedChunks < (#map.processQueue * 0.05)) then
        return map
    end

    local mapIterator = Universe.mapIterator
    repeat
        Universe.mapIterator, map = next(Universe.activeMaps, Universe.mapIterator)
        if map then
            Universe.processedChunks = 0
            Universe.currentMap = map
            return map
        end
    until mapIterator == Universe.mapIterator

    return nil
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
    local x = chunk.x
    local y = chunk.y

    if not map[x][y] then
        return
    end

    map[x][y] = nil
    local chunkId = chunk.id
    MapUtils.removeProcessQueueChunk(map.processQueue, chunk)

    Universe.chunkToActiveNest[chunkId] = nil
    Universe.chunkToActiveRaidNest[chunkId] = nil
    Universe.chunkToDrained[chunkId] = nil
    Universe.chunkToRetreats[chunkId] = nil
    Universe.chunkToRallys[chunkId] = nil
    Universe.chunkToPassScan[chunkId] = nil
    Universe.chunkToNests[chunkId] = nil
    Universe.chunkToUtilities[chunkId] = nil
    Universe.chunkToHives[chunkId] = nil
    Universe.vengenceQueue[chunkId] = nil
    Universe.chunkToVictory[chunkId] = nil

    local base = chunk.base
    if base then
        base.chunkCount = base.chunkCount - 1
        chunk.base = nil
    end

    if Universe.processNestIterator == chunkId then
        Universe.processNestIterator = nil
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
end

--[[
    1 2 3
    \|/
    4- -5
    /|\
    6 7 8
]]--
function MapUtils.getNeighborChunks(map, x, y)
    if not NeighborChunks then return {} end
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
function MapUtils.canMoveChunkDirection(direction, startChunk, endChunk)
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

function MapUtils.positionFromScaledDirections(startPosition, multipler, direction, nextDirection)
    local lx = startPosition.x
    local ly = startPosition.y
    if (direction == 1) then
        lx = lx - (CHUNK_SIZE * multipler)
        ly = ly - (CHUNK_SIZE * multipler)
    elseif (direction == 2) then
        ly = ly - (CHUNK_SIZE * multipler)
    elseif (direction == 3) then
        lx = lx + (CHUNK_SIZE * multipler)
        ly = ly - (CHUNK_SIZE * multipler)
    elseif (direction == 4) then
        lx = lx - (CHUNK_SIZE * multipler)
    elseif (direction == 5) then
        lx = lx + (CHUNK_SIZE * multipler)
    elseif (direction == 6) then
        lx = lx - (CHUNK_SIZE * multipler)
        ly = ly + (CHUNK_SIZE * multipler)
    elseif (direction == 7) then
        ly = ly + (CHUNK_SIZE * multipler)
    elseif (direction == 8) then
        lx = lx + (CHUNK_SIZE * multipler)
        ly = ly + (CHUNK_SIZE * multipler)
    end
    if nextDirection then
        if (nextDirection == 1) then
            lx = lx - (CHUNK_SIZE * multipler)
            ly = ly - (CHUNK_SIZE * multipler)
        elseif (nextDirection == 2) then
            ly = ly - (CHUNK_SIZE * multipler)
        elseif (nextDirection == 3) then
            lx = lx + (CHUNK_SIZE * multipler)
            ly = ly - (CHUNK_SIZE * multipler)
        elseif (nextDirection == 4) then
            lx = lx - (CHUNK_SIZE * multipler)
        elseif (nextDirection == 5) then
            lx = lx + (CHUNK_SIZE * multipler)
        elseif (nextDirection == 6) then
            lx = lx - (CHUNK_SIZE * multipler)
            ly = ly + (CHUNK_SIZE * multipler)
        elseif (nextDirection == 7) then
            ly = ly + (CHUNK_SIZE * multipler)
        elseif (nextDirection == 8) then
            lx = lx + (CHUNK_SIZE * multipler)
            ly = ly + (CHUNK_SIZE * multipler)
        end
    end
    MapPosition.x = lx
    MapPosition.y = ly
    return MapPosition
end

function MapUtils.positionFromScaledSearchPath(startPosition, multipler, searchPath)
    local lx = startPosition.x
    local ly = startPosition.y
    for i = 1, 4 do
        local path = searchPath[i]
        if path.chunk == -1 then
            MapPosition.x = lx
            MapPosition.y = ly
            return MapPosition
        end
        local direction = path.direction
        if (direction == 1) then
            lx = lx - (CHUNK_SIZE * multipler)
            ly = ly - (CHUNK_SIZE * multipler)
        elseif (direction == 2) then
            ly = ly - (CHUNK_SIZE * multipler)
        elseif (direction == 3) then
            lx = lx + (CHUNK_SIZE * multipler)
            ly = ly - (CHUNK_SIZE * multipler)
        elseif (direction == 4) then
            lx = lx - (CHUNK_SIZE * multipler)
        elseif (direction == 5) then
            lx = lx + (CHUNK_SIZE * multipler)
        elseif (direction == 6) then
            lx = lx - (CHUNK_SIZE * multipler)
            ly = ly + (CHUNK_SIZE * multipler)
        elseif (direction == 7) then
            ly = ly + (CHUNK_SIZE * multipler)
        elseif (direction == 8) then
            lx = lx + (CHUNK_SIZE * multipler)
            ly = ly + (CHUNK_SIZE * multipler)
        end
    end
    MapPosition.x = lx
    MapPosition.y = ly
    return MapPosition
end

function MapUtils.victoryScent(chunk, entityType)
    local value = VICTORY_SCENT[entityType]
    if value then
        addVictoryGenerator(chunk, value)
    end
end

-- PURE: Calculate victory scent amount for distance
function MapUtils.calculateVictoryScentAmount(baseAmount, distanceIndex)
    return baseAmount * VICTORY_SCENT_MULTIPLER[distanceIndex]
end

-- IMPURE: Apply victory scent to chunks (side effects)
function MapUtils.applyVictoryScent(chunk, amount)
    addDeathGenerator(chunk, amount)
    addPermanentDeathGenerator(chunk, amount)
end

-- PUBLIC: Disperse victory scent (reads global.universe)
function MapUtils.disperseVictoryScent(universe)
    universe = universe or Universe
    local chunkToVictory = universe.chunkToVictory
    local chunkId, pheromonePack = next(chunkToVictory, nil)
    if not chunkId then
        return
    end

    chunkToVictory[chunkId] = nil
    local chunk = pheromonePack.chunk
    local map = chunk.map
    if not map.surface.valid then
        return
    end
    local chunkX = chunk.x
    local chunkY = chunk.y
    local i = 1
    for x = chunkX - VICTORY_SCENT_BOUND, chunkX + VICTORY_SCENT_BOUND, 32 do
        for y = chunkY - VICTORY_SCENT_BOUND, chunkY + VICTORY_SCENT_BOUND, 32 do
            local c = MapUtils.getChunkByXY(map, x, y)
            if c ~= -1 then
                local amount = MapUtils.calculateVictoryScentAmount(pheromonePack.v, i)
                MapUtils.applyVictoryScent(c, amount)
            end
            i = i + 1
        end
    end
end

function MapUtils.deathScent(chunk, structure)
    local amount = -DEATH_PHEROMONE_GENERATOR_AMOUNT
    if structure then
        amount = -TEN_DEATH_PHEROMONE_GENERATOR_AMOUNT
    end
    addDeathGenerator(chunk, amount)
    addPermanentDeathGenerator(chunk, amount)
end

-- PURE: Calculate pheromone updates without side effects
-- All inputs explicit, uses Constants directly (immutable)
-- Returns table with new pheromone values
function MapUtils.calculatePheromoneUpdates(chunk, neighbors, tick)
    local chunkPlayer = chunk[PLAYER_PHEROMONE]
    local chunkBase = -MAGIC_MAXIMUM_NUMBER
    local chunkDeath = getCombinedDeathGenerator(chunk)
    local chunkResource = -MAGIC_MAXIMUM_NUMBER
    local chunkEnemy = chunk[ENEMY_PHEROMONE]
    local chunkKamikaze = chunk[KAMIKAZE_PHEROMONE]

    local chunkCount = 1
    local enemyStructureCount = getEnemyStructureCount(chunk)

    for i = 1, 8 do
        local neighbor = neighbors[i]
        if neighbor and neighbor ~= -1 then
            if MapUtils.canMoveChunkDirection(i, chunk, neighbor) then
                chunkCount = chunkCount + 1
                chunkPlayer = chunkPlayer + neighbor[PLAYER_PHEROMONE]
                chunkEnemy = chunkEnemy + neighbor[ENEMY_PHEROMONE]
                chunkDeath = chunkDeath + getCombinedDeathGenerator(neighbor)
                
                local tempPheromone = neighbor[KAMIKAZE_PHEROMONE]
                if chunkKamikaze < tempPheromone then
                    chunkKamikaze = tempPheromone
                end
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

    local newDeathGenerator = (chunkDeath / chunkCount) * 0.75
    local chunkDeathRating = getCombinedDeathGeneratorRating(chunk) * getPathRating(chunk)

    local newPlayer = mMax(
        chunk.playerGenerator or 0,
        (chunkPlayer / chunkCount) * 0.98
    ) * chunkDeathRating

    local newBase = mMax(
        chunk.playerBaseGenerator or 0,
        chunkBase * 0.9
    ) * chunkDeathRating

    local newEnemy = chunkDeathRating * mMax(
        enemyStructureCount * ENEMY_PHEROMONE_MULTIPLER,
        (chunkEnemy / chunkCount) * 0.9
    )

    local newKamikaze = mMax(
        chunkKamikaze,
        newPlayer + newBase
    ) * 0.95

    local newResource = 0
    local resourcePheromoneGenerator = chunk.resourceGenerator or 0
    if resourcePheromoneGenerator > 0 then
        newResource = linearInterpolation(resourcePheromoneGenerator, 15000, 20000)
    end
    if enemyStructureCount ~= 0 then
        newResource = newResource * 0.0001
    end
    newResource = chunkDeathRating * newResource * 0.9

    return {
        deathGenerator = newDeathGenerator,
        player = newPlayer,
        base = newBase,
        enemy = newEnemy,
        kamikaze = newKamikaze,
        resource = newResource,
        enemyStructureCount = enemyStructureCount,
    }
end

-- IMPURE: Apply pheromone updates to chunk (side effects)
function MapUtils.applyPheromoneUpdates(chunk, updates)
    setDeathGenerator(chunk, updates.deathGenerator)
    decayPlayerGenerator(chunk)

    chunk[PLAYER_PHEROMONE] = updates.player
    chunk[BASE_PHEROMONE] = updates.base
    chunk[ENEMY_PHEROMONE] = updates.enemy
    chunk[KAMIKAZE_PHEROMONE] = updates.kamikaze
    chunk[RESOURCE_PHEROMONE] = updates.resource
end

-- PUBLIC: Orchestrate pheromone processing (reads global.universe)
function MapUtils.processPheromone(chunk, tick, player)
    if chunk[CHUNK_TICK] > tick then
        return
    end
    chunk[CHUNK_TICK] = tick

    local neighbors = MapUtils.getNeighborChunks(chunk.map, chunk.x, chunk.y)
    
    -- Pure calculation
    local updates = MapUtils.calculatePheromoneUpdates(chunk, neighbors, tick)
    
    -- Apply updates (side effects)
    MapUtils.applyPheromoneUpdates(chunk, updates)

    if not player then
        decayDeathGenerator(chunk)
    end
end

function MapUtils.init(universe)
    Universe = universe
    if universe.mapUtilsQueries then
        NeighborChunks = universe.mapUtilsQueries.neighbors
        MapPosition = universe.mapUtilsQueries.position
    end
    
    -- Initialize dirty chunk tracking
    Universe.dirtyChunks = {
        priority1 = {},  -- Player-adjacent (within 5 chunks)
        priority2 = {},  -- Squad-active chunks
        priority3 = {},  -- Recent events (last 10 ticks)
        priority4 = {},  -- Background propagation
        priority5 = {}   -- Stale chunks
    }
    Universe.dirtyChunkSet = {}  -- Quick lookup to prevent duplicates
end

-- ============================================
-- ACO: Dirty Chunk Management
-- ============================================

function MapUtils.markChunkDirty(chunkId, priority, tick, universe)
    universe = universe or Universe
    if not chunkId or chunkId == -1 then return end
    
    -- Clamp priority to valid range (1-5)
    priority = math.min(math.max(priority, 1), 5)
    
    -- Convert priority to string key
    local priorityKey = "priority" .. priority
    
    -- Prevent duplicates
    if universe.dirtyChunkSet[chunkId] then
        -- Update priority if higher (lower number = higher priority)
        local existingPriority = universe.dirtyChunkSet[chunkId]
        if priority < existingPriority then
            -- Remove from old priority list
            local oldList = universe.dirtyChunks["priority" .. existingPriority]
            for i, id in ipairs(oldList) do
                if id == chunkId then
                    table.remove(oldList, i)
                    break
                end
            end
            -- Add to new priority list
            table.insert(universe.dirtyChunks[priorityKey], chunkId)
            universe.dirtyChunkSet[chunkId] = priority
        end
        return
    end
    
    -- Add to priority list
    table.insert(universe.dirtyChunks[priorityKey], chunkId)
    universe.dirtyChunkSet[chunkId] = priority
end

-- ACO: Get effective pheromone value with decay-on-read
function MapUtils.getEffectivePheromone(chunk, type, tick)
    local value = chunk[type] or 0
    if value == 0 then return 0 end
    
    local timestampField = type .. "LastRefresh"
    local lastRefresh = chunk[timestampField] or 0
    local ticksElapsed = tick - lastRefresh
    
    -- Select appropriate decay table
    local decayTable
    if type == "PLAYER_PHEROMONE" then
        decayTable = Constants.DECAY_098
    elseif type == "BASE_PHEROMONE" or type == "RESOURCE_PHEROMONE" or type == "ENEMY_PHEROMONE" then
        decayTable = Constants.DECAY_090
    elseif type == "KAMIKAZE_PHEROMONE" then
        decayTable = Constants.DECAY_095
    else
        return value  -- No decay
    end
    
    -- Apply decay
    local decayFactor = decayTable[ticksElapsed] or decayTable[Constants.DECAY_TABLE_DEPTH]
    return value * decayFactor
end

MapUtilsG = MapUtils
return MapUtils
