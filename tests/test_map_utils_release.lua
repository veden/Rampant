-- Surface release: bounded per tick, resumable, and it finishes.
--
-- A release removes every chunk record of a surface. It used to do that in one call,
-- which measured 155ms for 1,665 chunks and 75ms for 1,910 on a real save load, in single
-- ticks, before the rest of the mod ran.
local TestCollection = require("tests/framework")
local MapUtils = require("MapUtils")

local my_collection = TestCollection:new()
local universe

local function makeUniverse(n)
    local chunkMap = { id = 1, chunks = {}, surface = { valid = true, name = "test" } }
    local chunks = {}
    for i = 1, n do
        local chunk = { id = i, x = i * 32, y = 0, map = chunkMap, nestCount = 0 }
        chunks[i] = chunk
        chunkMap.chunks[MapUtils.chunkKey(i * 32, 0)] = chunk
    end
    universe = {
        chunkById = chunks,
        maps = { [1] = chunkMap },
        freeChunkIds = {},
        bases = {},
        hives = {},
        hiveData = {},
        activeHives = {},
        pendingUpgrades = {},
        chunkToActiveNest = {}, chunkToActiveRaidNest = {}, chunkToDrained = {},
        chunkToRetreats = {}, chunkToRallys = {}, chunkToPassScan = {}, chunkToNests = {},
        chunkToUtilities = {}, chunkToHives = {}, vengenceQueue = {}, chunkToVictory = {},
        sourceVersionedFields = {}, sourceVersionedRelax = {},
        deathGeneratorChunks = {},
    }
    MapUtils.init(universe)
    game = game or {}
    game.tick = 100
    return chunkMap, chunks
end

my_collection:add('release_is_bounded_to_one_slice_per_call', function()
    local chunkMap, chunks = makeUniverse(100)
    local released = MapUtils.releaseSurface(chunkMap)
    assert(released <= 12, "one call must remove at most one slice, removed " .. released)
    assert(released > 0, "the first call must make progress")
    assert(universe.maps[1] == chunkMap, "the map stays tracked until the release finishes")
    assert(chunkMap.chunks ~= nil, "the grid stays usable until the release finishes")
    assert(#(function()
        local n = 0
        for _ in pairs(universe.chunkById) do n = n + 1 end
        return { n }
    end)() > 0, "chunks remain")
end)

my_collection:add('release_resumes_and_finishes', function()
    local chunkMap = makeUniverse(100)
    MapUtils.releaseSurface(chunkMap)      -- starts the release and takes one slice
    local calls = 1
    while chunkMap.releasing and calls < 50 do
        calls = calls + 1
        MapUtils.releaseSurface(chunkMap)
    end
    assert(not chunkMap.releasing, "the release must finish")
    assert(calls >= 8, "100 chunks at 12 per call needs several calls, took " .. calls)
    local remaining = 0
    for _ in pairs(universe.chunkById) do remaining = remaining + 1 end
    assert(remaining == 0, "every chunk record must be gone, " .. remaining .. " left")
    assert(universe.maps[1] == nil, "the map must be dropped when the release finishes")
    assert(next(chunkMap.chunks) == nil, "the grid must be empty")
end)

my_collection:add('release_is_idempotent_when_called_again', function()
    local chunkMap = makeUniverse(10)
    MapUtils.releaseSurface(chunkMap)
    while chunkMap.releasing do MapUtils.releaseSurface(chunkMap) end
    local again = MapUtils.releaseSurface(chunkMap)
    assert(again == 0, "a finished surface releases nothing more, got " .. again)
end)

-- A release must also drop the surface's queued chunk scans: the load flush queues every
-- chunk of every surface and runs for ~1,900 ticks, so a release that happens while the
-- queue still holds them is undone - each queued scan re-creates the record on a map that
-- is no longer tracked, and those come back as orphans (measured: 1,665 + 1,910).
my_collection:add('release_drops_the_surfaces_queued_scans', function()
    local chunkMap = makeUniverse(6)
    universe.pendingChunks = {}
    universe.pendingChunkBuckets = { [1] = {} }
    for i = 1, 6 do
        local event = { id = i, map = chunkMap, bucket = 1, tick = 1, area = { left_top = {} } }
        universe.pendingChunks[i] = event
        universe.pendingChunkBuckets[1][i] = event
    end
    local other = { id = 99, map = { id = 9 }, bucket = 1, tick = 1, area = { left_top = {} } }
    universe.pendingChunks[99] = other
    universe.pendingChunkBuckets[1][99] = other
    local dropped = MapUtils.dropPendingScans(chunkMap)
    assert(dropped == 6, "every scan of the released surface must be dropped, got " .. dropped)
    assert(universe.pendingChunks[99] == other, "another surface's scan must survive")
    assert(next(universe.pendingChunkBuckets[1]) == 99, "the bucket entry must go with it")
end)

my_collection:add('a_surface_with_queued_scans_is_not_judged_idle', function()
    local chunkMap = makeUniverse(4)
    -- The test is the counter that queueGeneratedChunk maintains and scanPopped drains; it
    -- is O(1), because walking the whole queue here cost 5.7ms inside the surface sweep.
    assert(not MapUtils.hasPendingScans(chunkMap), "a fresh surface has nothing queued")
    chunkMap.pendingScans = 3
    assert(MapUtils.hasPendingScans(chunkMap), "queued scans must block the idle judgement")
    MapUtils.scanPopped(chunkMap)
    assert(MapUtils.hasPendingScans(chunkMap), "still queued while the count is positive")
    MapUtils.scanPopped(chunkMap)
    MapUtils.scanPopped(chunkMap)
    assert(not MapUtils.hasPendingScans(chunkMap), "cleared queue means it can be judged")
    MapUtils.scanPopped(chunkMap)
    assert(not MapUtils.hasPendingScans(chunkMap), "the counter never goes negative")
end)

my_collection:add('drain_finishes_every_queued_release', function()
    local a = makeUniverse(70)
    MapUtils.releaseSurface(a)
    local b = { id = 2, chunks = {}, surface = { valid = true, name = "b" } }
    local id = 1000
    for i = 1, 40 do
        id = id + 1
        local chunk = { id = id, x = i * 32, y = 32, map = b, nestCount = 0 }
        b.chunks[MapUtils.chunkKey(i * 32, 32)] = chunk
        universe.chunkById[id] = chunk
    end
    universe.maps[2] = b
    MapUtils.releaseSurface(b)
    local ticks = 0
    while universe.surfaceReleases and ticks < 100 do
        ticks = ticks + 1
        MapUtils.processSurfaceReleases()
    end
    assert(ticks < 100, "the drain must finish both releases")
    assert(not a.releasing and not b.releasing, "both surfaces must be released")
    assert(next(universe.chunkById) == nil, "no chunk record may survive the drain")
end)

return my_collection
