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

local mFloor = math.floor
local getPassable = ChunkPropertyUtils.getPassable
local sFind = string.find

-- module code

-- Chunk coordinates are int32 tiles divided by 32, i.e. [-2^26, 2^26-1], and the
-- engine's map limit is the int32 tile range. Two axes of 27 bits is 54 bits,
-- one more than a double holds exactly, so the key is signed positional:
-- id = cx * 2^27 + cy. Every pair in the world except the westernmost column
-- (cx = -2^26, where the value would sit exactly on 2^53 and odd cy would round)
-- is exact, and that column asserts.
local CHUNK_KEY_SHIFT = 2^27
local CHUNK_KEY_MAX = 2^26

function MapUtils.chunkKey(x, y)
    local cx = mFloor(x * CHUNK_SIZE_DIVIDER)
    local cy = mFloor(y * CHUNK_SIZE_DIVIDER)
    if cx < -CHUNK_KEY_MAX or cx >= CHUNK_KEY_MAX or
        cy < -CHUNK_KEY_MAX or cy >= CHUNK_KEY_MAX then
        error("chunk coordinate outside the int32 map: " .. cx .. "," .. cy)
    end
    return cx * CHUNK_KEY_SHIFT + cy
end

function MapUtils.chunkKeyToXY(key)
    local cx = mFloor(key / CHUNK_KEY_SHIFT)
    return cx * CHUNK_SIZE, (key - cx * CHUNK_KEY_SHIFT) * CHUNK_SIZE
end

function MapUtils.getChunkByXY(map, x, y)
    local chunks = map.chunks
    if not chunks then return -1 end
    return chunks[MapUtils.chunkKey(x, y)] or -1
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
    local chunks = map.chunks
    if not chunks then return -1 end
    return chunks[MapUtils.chunkKey(position.x, position.y)] or -1
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
    if Universe.releasedSurfaces then
        Universe.releasedSurfaces[map.id] = nil
    end
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

    local key = MapUtils.chunkKey(x, y)
    if not map.chunks[key] then
        return
    end

    map.chunks[key] = nil
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

    -- H8 neighbor-id cache: the removed chunk and its neighbours may
    -- have cached adjacency that now includes the removed chunk.
    chunk.sourceVersionedNeighborIds = nil
    local neighbors = MapUtils.getNeighborChunks(map, chunk.x, chunk.y)
    for i = 1, 8 do
        local neighbor = neighbors[i]
        if neighbor and neighbor ~= -1 then
            neighbor.sourceVersionedNeighborIds = nil
        end
    end

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

    -- Field metadata is keyed by chunk id and nothing used to clear it here, so
    -- removing a chunk (or releasing a whole surface) traded chunk records for
    -- orphaned field entries - four tables per field plus the frontier, which is
    -- the larger half of the per-chunk cost.
    local fields = Universe.sourceVersionedFields
    if fields then
        for key, values in pairs(fields) do
            values[chunkId] = nil
            local top = Universe.sourceVersionedTop[key]
            if top then top[chunkId] = nil end
            local second = Universe.sourceVersionedSecond[key]
            if second then second[chunkId] = nil end
            local secondSource = Universe.sourceVersionedSecondSource[key]
            if secondSource then secondSource[chunkId] = nil end
            local frontier = Universe.sourceVersionedFrontier[key]
            if frontier then frontier[chunkId] = nil end
        end
    end
    if Universe.deathGeneratorChunks then
        Universe.deathGeneratorChunks[chunkId] = nil
    end
    -- Pending upgrades referencing this chunk: the entry's entity is about to be
    -- invalidated, and an orphan would sit in the queue until it was processed.
    if Universe.pendingUpgrades then
        for id, entry in pairs(Universe.pendingUpgrades) do
            if entry.chunkId == chunkId then
                Universe.pendingUpgrades[id] = nil
                Universe.pendingUpgradesLength = (Universe.pendingUpgradesLength or 1) - 1
            end
        end
    end
end

--- Release an entire surface: every chunk through removeChunkFromMap, its bases,
--- its hives, then the map table itself. This is what an idle-surface sweep and
--- on_surface_deleted call; without it a surface that stops having enemies keeps
--- its chunk records and field entries forever.
-- Any enemy activity on a surface resets its quiet timer; the sweep releases a
-- surface only after IDLE_SURFACE_TICKS of silence AND an empty registry AND a
-- confirming entity query.
local IDLE_SURFACE_TICKS = 5 * 60 * 60

function MapUtils.touchSurface(map)
    if map then
        map.lastEnemyTick = game.tick
    end
end

--- Cheap gate first (our own registries), expensive confirmation last.
--- Returns true when the surface was released.
local function surfaceHasEnemies(map)
    local chunkToNests = Universe.chunkToNests
    if chunkToNests then
        for _, chunk in pairs(chunkToNests) do
            if chunk.map == map then return true end
        end
    end
    for _, reg in ipairs({ Universe.chunkToHives, Universe.chunkToUtilities }) do
        if reg then
            for _, chunk in pairs(reg) do
                if chunk.map == map then return true end
            end
        end
    end
    local squads = Universe.groupNumberToSquad
    if squads then
        for _, squad in pairs(squads) do
            if squad.map == map then return true end
        end
    end
    return false
end

function MapUtils.sweepIdleSurfaces(tick)
    local maps = Universe.maps
    if not maps then return end
    local ids = {}
    for id in pairs(maps) do ids[#ids + 1] = id end
    if #ids == 0 then return end
    Universe.surfaceSweepIndex = (Universe.surfaceSweepIndex or 0) + 1
    local id = ids[(Universe.surfaceSweepIndex % #ids) + 1]
    local map = maps[id]
    if not map or not map.surface or not map.surface.valid then return end
    if (tick - (map.lastEnemyTick or 0)) < IDLE_SURFACE_TICKS then return end
    if surfaceHasEnemies(map) then
        map.lastEnemyTick = tick
        return
    end
    -- Confirmation: one query, only for a surface that is already silent and has
    -- an empty registry. This is the only expensive step and it is rare.
    if map.surface.count_entities_filtered({ force = "enemy", limit = 1 }) == 0 then
        MapUtils.releaseSurface(map)
    else
        map.lastEnemyTick = tick
    end
end

function MapUtils.releaseSurface(map)
    if not map then return 0 end
    local released = 0
    if map.chunks then
        local chunkList = {}
        for _, chunk in pairs(map.chunks) do
            chunkList[#chunkList + 1] = chunk
        end
        for i = 1, #chunkList do
            MapUtils.removeChunkFromMap(map, chunkList[i])
            released = released + 1
        end
        map.chunks = {}
    end
    if Universe.bases then
        for id, base in pairs(Universe.bases) do
            if base.map == map then
                Universe.bases[id] = nil
                Universe.baseCount = (Universe.baseCount or 1) - 1
            end
        end
    end
    if Universe.hives then
        for unitNumber, hive in pairs(Universe.hives) do
            if hive.map == map then
                Universe.hives[unitNumber] = nil
                Universe.hiveData[unitNumber] = nil
                if Universe.activeHives then Universe.activeHives[unitNumber] = nil end
            end
        end
    end
    Universe.maps[map.id] = nil
    Universe.releasedSurfaces = Universe.releasedSurfaces or {}
    Universe.releasedSurfaces[map.id] = true
    return released
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
    local chunks = map.chunks
    if not chunks then return NeighborChunks end
    local xm = x - CHUNK_SIZE
    local xp = x + CHUNK_SIZE
    NeighborChunks[1] = chunks[MapUtils.chunkKey(xm, y - CHUNK_SIZE)] or -1
    NeighborChunks[4] = chunks[MapUtils.chunkKey(xm, y)] or -1
    NeighborChunks[6] = chunks[MapUtils.chunkKey(xm, y + CHUNK_SIZE)] or -1
    NeighborChunks[3] = chunks[MapUtils.chunkKey(xp, y - CHUNK_SIZE)] or -1
    NeighborChunks[5] = chunks[MapUtils.chunkKey(xp, y)] or -1
    NeighborChunks[8] = chunks[MapUtils.chunkKey(xp, y + CHUNK_SIZE)] or -1
    NeighborChunks[2] = chunks[MapUtils.chunkKey(x, y - CHUNK_SIZE)] or -1
    NeighborChunks[7] = chunks[MapUtils.chunkKey(x, y + CHUNK_SIZE)] or -1
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

-- The transient fields (player, kamikaze) are still maintained here; base,
-- enemy and resource are solved by the source-versioned solver and are not
-- written by this pass. There is no H7 read path to avoid feeding back into.

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

end

MapUtilsG = MapUtils
return MapUtils
