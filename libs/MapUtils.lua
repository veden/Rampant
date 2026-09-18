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
local ChunkLogic = require("chunk_logic")
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
local sFind = string.find

-- module code

function MapUtils.getChunkByXY(map, x, y)
    local chunkX = map[x]
    if chunkX then
        return chunkX[y] or -1
    end
    return -1
end

--- Find a chunk by its unique id. O(1) via the chunkById reverse index
--- (maintained by ChunkUtils.createChunk / MapUtils.removeChunkFromMap).
function MapUtils.getChunkById(chunkId, universe)
    universe = universe or storage.universe
    if not universe or not universe.chunkById then return nil end
    local chunk = universe.chunkById[chunkId]
    if MetricsG then
        MetricsG.inc("index.lookups")
        if not chunk then
            MetricsG.inc("index.misses")
        end
    end
    return chunk
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

function MapUtils.excludeSurface()
    for mapId,map in pairs(Universe.maps) do
        local toBeRemoved = not map.surface.valid
            or MapUtils.isExcludedSurface(map.surface.name)
            or Universe.excludedSurfaces[map.surface.name]

        if toBeRemoved then
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

function MapUtils.removeChunkToNest(chunkId)
    Universe.chunkToNests[chunkId] = nil
    if (chunkId == Universe.processNestIterator) then
        Universe.processNestIterator = nil
    end
    if (chunkId == Universe.processMigrationIterator) then
        Universe.processMigrationIterator = nil
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
    if Universe.chunkById then
        Universe.chunkById[chunkId] = nil
    end

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
    -- Pure decision delegated to ChunkLogic (lookup table). The table is proven
    -- equivalent to the original branch chain over the whole input domain by
    -- tests/test_chunk_logic.lua (exhaustive, 128 cases). getPassable is
    -- inlined here (2 calls x 8 directions per chunk in the hot loop).
    local startPassable = startChunk.passable or CHUNK_ALL_DIRECTIONS
    local endPassable = endChunk.passable or CHUNK_ALL_DIRECTIONS
    return ChunkLogic.canMove(startPassable, direction, endPassable)
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

-- PUBLIC: Disperse victory scent (reads storage.universe)
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
                -- getCombinedDeathGenerator inlined (8 calls/chunk in the
                -- hot loop; clamped sum, verbatim semantics).
                local dAmount = (neighbor.deathGenerator or 0) + (neighbor.permanentDeathGenerator or 0)
                chunkDeath = chunkDeath + (dAmount > 1 and 1 or (dAmount < -1 and -1 or dAmount))
                
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
        -- Diffuse to neighbors like base/enemy so squads can discover from distance
        newResource = mMax(newResource, chunkResource * 0.9)
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
function MapUtils.applyPheromoneUpdates(chunk, updates, tick)
    setDeathGenerator(chunk, updates.deathGenerator)
    decayPlayerGenerator(chunk)

    chunk[PLAYER_PHEROMONE] = updates.player
    chunk.playerLastRefresh = tick or 0

    chunk[BASE_PHEROMONE] = updates.base
    chunk.baseLastRefresh = tick or 0

    chunk[ENEMY_PHEROMONE] = updates.enemy
    chunk.enemyLastRefresh = tick or 0

    chunk[KAMIKAZE_PHEROMONE] = updates.kamikaze
    chunk.kamikazeLastRefresh = tick or 0

    chunk[RESOURCE_PHEROMONE] = updates.resource
    chunk.resourceLastRefresh = tick or 0
end

-- PUBLIC: Orchestrate pheromone processing (reads storage.universe)
function MapUtils.processPheromone(chunk, tick, player)
    if chunk[CHUNK_TICK] > tick then
        return
    end
    chunk[CHUNK_TICK] = tick

    local neighbors = MapUtils.getNeighborChunks(chunk.map, chunk.x, chunk.y)
    
    -- Pure calculation
    local updates = MapUtils.calculatePheromoneUpdates(chunk, neighbors, tick)
    
    -- Apply updates (side effects)
    MapUtils.applyPheromoneUpdates(chunk, updates, tick)

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
    
    -- Initialize chunk id -> chunk reverse index (O(1) lookups).
    -- Persisted in storage: create only if absent, NEVER reset on load.
    -- The chunk grid survives loads; wiping the index would silently kill
    -- dirty processing for every existing chunk.
    if not Universe.chunkById then
        Universe.chunkById = {}
    end

    -- Initialize dirty chunk tracking. Create-if-absent: on_load must never
    -- mutate storage (CRC check fails the load); the persisted queue is
    -- transient and drains on the next tick anyway.
    if not Universe.dirtyChunks then
        Universe.dirtyChunks = {
            priority1 = {},  -- Player-adjacent (within 5 chunks)
            priority2 = {},  -- Squad-active chunks
            priority3 = {},  -- Recent events (last 10 ticks)
            priority4 = {},  -- Background propagation
            priority5 = {}   -- Stale chunks
        }
    end
    if not Universe.dirtyChunkSet then
        Universe.dirtyChunkSet = {}  -- Quick lookup to prevent duplicates
    end
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
        -- Update priority if higher (lower number = higher priority).
        -- LAZY: append to the higher-priority list and update the set; the
        -- stale entry in the old list is skipped at drain time (the drain
        -- checks the set's priority). The old code scanned the whole old list
        -- and table.remove'd from the middle -- O(n) per upgrade.
        local existingPriority = universe.dirtyChunkSet[chunkId]
        if priority < existingPriority then
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
