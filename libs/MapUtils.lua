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
local RamState = require("libs/RamState")

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
-- Compact chunk ids when the counter approaches the provenance packing bound
-- (2^26 = 67,108,864). Compaction keeps ids dense, so the bound is never reached.
local CHUNK_ID_COMPACT_THRESHOLD = 60000000

local getPassable = ChunkPropertyUtils.getPassable
local sFind = string.find

-- module code

-- Chunk coordinates are int32 tiles divided by 32, and the engine's map limit is
-- the int32 tile range. The key packs the two chunk indices as digits in base
-- 2^27: id = cx * 2^27 + cy. Every pair in the world is exact except the
-- westernmost column (cx = -2^26), and neighbours are id +/- 1 (y) and
-- id +/- 2^27 (x), so adjacency is addition, not multiplication.
--
-- (x / 32) * 2^27 is x * 2^22, and y / 32 is exact for chunk-aligned y, so the
-- key needs no floor and no division: multiplications by powers of two are
-- exact in doubles, including for negative coordinates.
local CHUNK_INDEX_TO_KEY = 4194304   -- 2^22
local TILE_TO_CHUNK = 0.03125        -- 1/32
local CHUNK_KEY_X_STEP = 134217728   -- 2^27, one chunk in x
local CHUNK_KEY_MAX = 2^25           -- +/-33.5M chunks keeps +/-1 from carrying

--- Key for chunk-aligned coordinates (chunk.x/chunk.y, or x +/- CHUNK_SIZE).
--- Exact, no floor: the caller guarantees alignment. This is the hot form.
function MapUtils.chunkKey(x, y)
    return x * CHUNK_INDEX_TO_KEY + y * TILE_TO_CHUNK
end

--- Key for an arbitrary tile position. Must floor: a position like (2.5, 0) is
--- inside chunk (0,0), and the aligned form would produce a fractional key that
--- matches no chunk.
function MapUtils.chunkKeyFromPosition(x, y)
    return mFloor(x * TILE_TO_CHUNK) * CHUNK_KEY_X_STEP + mFloor(y * TILE_TO_CHUNK)
end

function MapUtils.chunkKeyFast(key, dx, dy)
    return key + dx * CHUNK_KEY_X_STEP + dy
end

MapUtils.CHUNK_KEY_X_STEP = CHUNK_KEY_X_STEP
MapUtils.CHUNK_KEY_MAX = CHUNK_KEY_MAX

function MapUtils.chunkKeyToXY(key)
    local cx = mFloor(key / CHUNK_KEY_SHIFT)
    return cx * CHUNK_SIZE, (key - cx * CHUNK_KEY_SHIFT) * CHUNK_SIZE
end

function MapUtils.getChunkByXY(map, x, y)
    -- A nil map (surface not tracked, or released for having no enemies) is a
    -- miss, not an error: callers already handle -1.
    if not map then return -1 end
    local chunks = map.chunks
    if not chunks then return -1 end
    return chunks[MapUtils.chunkKeyFromPosition(x, y)] or -1
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
    if not map then return -1 end
    local chunks = map.chunks
    if not chunks then return -1 end
    return chunks[MapUtils.chunkKeyFromPosition(position.x, position.y)] or -1
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
    if MetricsG then MetricsG.inc("scan.queued") end
    event.tick = (event.tick or game.tick) + 20
    Universe.eventId = Universe.eventId + 1
    event.id = Universe.eventId
    event.map = map
    Universe.pendingChunks[event.id] = event
    -- Distance bucket so the queue drains nearest-first; see
    -- Processor.processPendingChunks (which also adopts unbucketed events left in
    -- an older save).
    local buckets = Universe.pendingChunkBuckets
    if not buckets then
        buckets = {}
        Universe.pendingChunkBuckets = buckets
    end
    local topLeft = event.area.left_top
    local dx = topLeft.x - (Universe.scanFocusX or 0)
    local dy = topLeft.y - (Universe.scanFocusY or 0)
    local bucket = mFloor(math.sqrt(dx * dx + dy * dy) / 256)
    if bucket > 15 then bucket = 15 end
    event.bucket = bucket
    local bucketTable = buckets[bucket]
    if not bucketTable then
        bucketTable = {}
        buckets[bucket] = bucketTable
    end
    bucketTable[event.id] = event
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

-- Chunk ids are recycled after an age, so the id space is bounded by peak
-- concurrent chunks rather than by every chunk ever created. The age matters:
-- a queued executor command or pending upgrade can still hold an id for a few
-- ticks after its chunk is removed, and handing that id out immediately would
-- point it at a different chunk.
local CHUNK_ID_REUSE_AGE = 10 * 60          -- 10 game-seconds
local CHUNK_ID_FREE_CAP = 200000

function MapUtils.newChunkId()
    local freeIds = Universe.freeChunkIds
    local freeTicks = Universe.freeChunkTicks
    if freeIds and #freeIds > 0 then
        local tick = freeTicks[#freeIds]
        if tick and (game.tick - tick) >= CHUNK_ID_REUSE_AGE then
            local n = #freeIds
            local id = freeIds[n]
            freeIds[n] = nil
            freeTicks[n] = nil
            return id
        end
    end
    -- Keep the counter clear of the provenance packing bound by compacting ids
    -- back to a dense range. Without this the counter could eventually reach
    -- 2^26 and the packing assertion would fire, which in-game means a
    -- non-recoverable error (a bricked save). Compaction is O(live chunks) and
    -- happens once per ~60M chunk creations.
    if Universe.chunkId >= CHUNK_ID_COMPACT_THRESHOLD then
        MapUtils.compactChunkIds()
    end
    local id = Universe.chunkId
    Universe.chunkId = Universe.chunkId + 1
    return id
end

--- Re-key every chunk id to a dense range 1..N, then restart the counter. All
--- id-keyed state is rewritten in one pass: the reverse index, the per-field
--- tables, the chunk registries and the chunk records themselves. Field values
--- are not recomputed - only their keys change - so the fields stay exact.
function MapUtils.compactChunkIds()
    local chunkById = Universe.chunkById
    if not chunkById then return 0 end
    local mapping = {}
    local chunks = {}
    local nextId = 1
    for oldId, chunk in pairs(chunkById) do
        mapping[oldId] = nextId
        chunks[nextId] = chunk
        nextId = nextId + 1
    end
    local count = nextId - 1

    -- reverse index and the record's own id
    Universe.chunkById = {}
    for newId, chunk in ipairs(chunks) do
        chunk.id = newId
        Universe.chunkById[newId] = chunk
    end

    -- per-field tables keyed by chunk id
    local function rekey(tableList)
        for _, t in ipairs(tableList) do
            if type(t) == "table" then
                for fieldKey, fieldTable in pairs(t) do
                    if type(fieldTable) == "table" then
                        local fresh = {}
                        for oldId, value in pairs(fieldTable) do
                            local newId2 = mapping[oldId]
                            if newId2 then fresh[newId2] = value end
                        end
                        t[fieldKey] = fresh
                    end
                end
            end
        end
    end
    rekey({ Universe.sourceVersionedFields, Universe.sourceVersionedProvenance,
        Universe.sourceVersionedSecond })
    -- The frontier and removal heaps index chunk ids inside their own arrays
    -- (pos/heap slots), which rekeying the tables above cannot fix. Drop them:
    -- the field values are already exact, only pending propagation is lost, and
    -- a source that changes again re-queues.
    Universe.sourceVersionedFrontier = {}
    Universe.sourceVersionedRemoval = {}

    -- registries keyed by chunk id
    for _, name in ipairs({ "chunkToNests", "chunkToUtilities", "chunkToHives",
        "chunkToActiveNest", "chunkToActiveRaidNest", "chunkToDrained",
        "chunkToRetreats", "chunkToRallys", "chunkToPassScan", "chunkToVictory",
        "vengenceQueue", "deathGeneratorChunks", "playerGeneratorChunks" }) do
        local t = Universe[name]
        if type(t) == "table" then
            local fresh = {}
            for oldId, value in pairs(t) do
                local newId2 = mapping[oldId]
                if newId2 then fresh[newId2] = value end
            end
            Universe[name] = fresh
        end
    end

    -- Per-base sets are keyed by chunk id as well.
    if Universe.bases then
        for _, base in pairs(Universe.bases) do
            if type(base.resourceChunks) == "table" then
                local fresh = {}
                for oldId in pairs(base.resourceChunks) do
                    local newId2 = mapping[oldId]
                    if newId2 then fresh[newId2] = true end
                end
                base.resourceChunks = fresh
            end
        end
    end

    -- iterators hold old ids; drop them and let the walks restart
    Universe.processBaseAIIterator = nil
    Universe.processNestIterator = nil
    Universe.processActiveSpawnerIterator = nil
    Universe.processActiveRaidSpawnerIterator = nil
    Universe.processMigrationIterator = nil
    -- any id held by a pending command or upgrade is stale now
    Universe.pendingUpgrades = {}
    Universe.pendingUpgradesLength = 0
    Universe.freeChunkIds = nil
    Universe.freeChunkTicks = nil
    Universe.chunkId = count + 1
    -- Invalidate anything that cached a chunk id across calls: field builds
    -- (checked per solve) and squads (they resolve ids on use).
    Universe.chunkEpoch = (Universe.chunkEpoch or 0) + 1
    log({"", "[HEAP] compacted chunk ids: ", count, " live chunks, counter reset"})
    return count
end

--- Hand a removed chunk's id back for reuse, aged.
function MapUtils.recycleChunkId(id)
    if not id then return end
    local freeIds = Universe.freeChunkIds
    if not freeIds then
        freeIds = {}
        Universe.freeChunkIds = freeIds
        Universe.freeChunkTicks = {}
    end
    if #freeIds >= CHUNK_ID_FREE_CAP then return end
    freeIds[#freeIds + 1] = id
    Universe.freeChunkTicks[#freeIds] = game.tick
end

function MapUtils.removeChunkFromMap(map, chunk)
    local x = chunk.x
    local y = chunk.y

    local key = MapUtils.chunkKey(x, y)
    -- Do not bail out when the grid slot is already missing: the reverse index
    -- and the field entries are the authoritative copies, and returning early
    -- left orphans that held an id and its field entries forever (measured 3,837
    -- on the 24MB save).
    local chunkId = chunk.id
    if map.chunks then
        map.chunks[key] = nil
    end
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

    -- Hand the id back for reuse once everything keyed by it is cleared below.
    -- Field entries, registries and pending upgrades for this chunk are dropped
    -- first, so a recycled id starts clean.
    -- (recycling is done at the end of this function)
    -- Field metadata is keyed by chunk id and nothing used to clear it here, so
    -- removing a chunk (or releasing a whole surface) traded chunk records for
    -- orphaned field entries - four tables per field plus the frontier, which is
    -- the larger half of the per-chunk cost.
    local fields = Universe.sourceVersionedFields
    if fields then
        for key, values in pairs(fields) do
            values[chunkId] = nil
            local prov = Universe.sourceVersionedProvenance[key]
            if prov then prov[chunkId] = nil end
            local second = Universe.sourceVersionedSecond[key]
            if second then second[chunkId] = nil end
            
            
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
    -- Everything keyed by the id is gone now, so it can be recycled (aged).
    MapUtils.recycleChunkId(chunkId)
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
    -- Release on the registry gate alone. The old confirmation query counted
    -- every enemy entity on the whole surface, which cost 168ms on average and
    -- 432ms at worst on a large save (one sweep per second). A surface with no
    -- registered nest, hive or turret and no squad cannot spawn, and migration
    -- re-creates the base if units ever do appear there.
    MapUtils.releaseSurface(map)
end

--- Drop chunk records whose surface is no longer tracked. Called on load: a
--- released surface's chunks can outlive the map if an older build removed the
--- map without walking the reverse index.
-- Chunks examined per call. An orphan costs ~115us (it clears its entries in every
-- field and registry), so 512 per call was 59ms in one tick. 8 per call, resumed
-- by cursor every tick, sweeps ~9k chunks in ~1,100 ticks at ~1ms/tick worst case.
local RECONCILE_PER_CALL = 8

function MapUtils.reconcileOrphanChunks()
    local removed = 0
    local orphan
    local examined = 0
    local startId = Universe.reconcileCursor
    local started = false
    local function visit(chunkId, chunk)
        local map = chunk.map
        -- Three ways a record can be orphaned: no map at all, a map that is no
        -- longer the tracked one for that index (a re-acquired surface reuses the
        -- index), or a live map whose grid no longer points at this chunk. The
        -- last is what the measured 1,673 were.
        local orphaned = false
        if not map then
            orphaned = true
        elseif Universe.maps[map.id] ~= map then
            orphaned = true
        elseif not map.surface or not map.surface.valid then
            orphaned = true
        else
            local key = MapUtils.chunkKey(chunk.x, chunk.y)
            if not map.chunks or map.chunks[key] ~= chunk then
                orphaned = true
            end
        end
        if orphaned then
            orphan = orphan or {}
            orphan[#orphan + 1] = chunk
        end
    end
    for chunkId, chunk in pairs(Universe.chunkById or {}) do
        if not startId or started or chunkId > startId then
            started = true
            visit(chunkId, chunk)
            examined = examined + 1
            if examined >= RECONCILE_PER_CALL then
                Universe.reconcileCursor = chunkId
                break
            end
        end
    end
    if examined < RECONCILE_PER_CALL then
        Universe.reconcileCursor = nil      -- full pass finished
        -- Park the sweep until something can create orphans again (surface
        -- release, load, or the slow re-check). A clean index should not cost
        -- 344us every tick.
        if removed == 0 then
            Universe.reconcilePaused = true
        end
    end
    if orphan then
        for i = 1, #orphan do
            local chunk = orphan[i]
            MapUtils.removeChunkFromMap(chunk.map or { chunks = {} }, chunk)
            removed = removed + 1
        end
    end
    return removed
end

function MapUtils.releaseSurface(map)
    if not map then return 0 end
    local released = 0
    -- Walk the reverse index: a chunk whose grid slot was already lost would
    -- never be visited by walking map.chunks, and would survive the release.
    local chunkList = {}
    for _, chunk in pairs(Universe.chunkById or {}) do
        if chunk.map == map then
            chunkList[#chunkList + 1] = chunk
        end
    end
    for i = 1, #chunkList do
        MapUtils.removeChunkFromMap(map, chunkList[i])
        released = released + 1
    end
    map.chunks = {}
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
    Universe.reconcilePaused = nil        -- a release can orphan chunks
    Universe.maps[map.id] = nil
    log({"", "[REL] surface=", map.surface and map.surface.name or "?", " chunks=", released, " tick=", game.tick})
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
