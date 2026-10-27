-- Unit tests for the source-versioned field runtime adapter.

local TestCollection = require("tests/framework")
local Runtime = require("libs/source_versioned_field_runtime")
local MapUtils = require("MapUtils")
local Constants = require("Constants")

local my_collection = TestCollection:new()

local originalGetChunkById
local originalGetChunkByXY
local world
local testUniverse

local ENEMY_PHEROMONE = Constants.ENEMY_PHEROMONE
local ENEMY_MULTIPLIER = Constants.ENEMY_PHEROMONE_MULTIPLER

local function makeWorld()
    -- Production resolves neighbours through the chunk's surface index
    -- (map.chunks[chunkKey(x, y)]), so the fake world has to carry one too.
    -- A chain of five chunks along +x, so a budgeted removal can genuinely be
    -- left part-way (with two chunks a single pop finishes it).
    local chunkMap = { chunks = {} }
    local chunks = {}
    for i = 0, 4 do
        local chunk = { id = i + 1, x = i * 32, y = 0, map = chunkMap, nestCount = 0 }
        chunks[i + 1] = chunk
        chunkMap.chunks[MapUtils.chunkKey(i * 32, 0)] = chunk
    end
    chunks[1].nestCount = 1
    local universe = { chunkById = chunks }

    originalGetChunkById = MapUtils.getChunkById
    originalGetChunkByXY = MapUtils.getChunkByXY
    MapUtils.getChunkById = function(chunkId)
        return chunks[chunkId]
    end
    MapUtils.getChunkByXY = function(_, x, y)
        if x == 0 and y == 0 then return chunks[1] end
        if x == 32 and y == 0 then return chunks[2] end
        return -1
    end

    Runtime.init(universe, true)
    testUniverse = universe
    return chunks
end

local function restoreWorld()
    MapUtils.getChunkById = originalGetChunkById
    MapUtils.getChunkByXY = originalGetChunkByXY
end

my_collection:set_setup(function()
    world = makeWorld()
end)

my_collection:set_teardown(function()
    restoreWorld()
    world = nil
    testUniverse = nil
end)

my_collection:add('readChunk_solves_source_field', function()
    local value = Runtime.readChunk(ENEMY_PHEROMONE, world[1])
    assert(value == 1 * ENEMY_MULTIPLIER,
        "source chunk must read its source value")

    local neighborValue = Runtime.readChunk(ENEMY_PHEROMONE, world[2])
    assert(math.abs(neighborValue - (1 * ENEMY_MULTIPLIER * 0.9)) < 1e-9,
        "neighbor chunk must read the propagated value")
end)

-- The source queue item pool must recycle. Before the release call was wired into
-- the drain, every queued change allocated a fresh item, which made the decaying
-- generators the largest allocator in the mod (measured 11 new items per tick and
-- a pool hit rate of zero).
my_collection:add('source_queue_items_are_recycled', function()
    local function cycle()
        world[1].nestCount = (world[1].nestCount or 0) + 1
        Runtime.queueSource(world[1], ENEMY_PHEROMONE, 1, world[1].nestCount * ENEMY_MULTIPLIER)
        Runtime.process(ENEMY_PHEROMONE, 40, { decay = 0.9, eps = 0.0005 })
    end

    -- warm up so the pool is populated
    for _ = 1, 20 do cycle() end
    local createdAfterWarmup = Runtime.queueItemStats()
    for _ = 1, 100 do cycle() end
    local createdAfterMore = Runtime.queueItemStats()

    assert(createdAfterMore <= createdAfterWarmup,
        "queue items must be recycled, but " .. tostring(createdAfterWarmup) ..
        " warmup creations grew to " .. tostring(createdAfterMore))
end)

-- A budgeted removal leaves a heap when it runs out of allowance. Nothing used to
-- continue it unless another change arrived for the same chunk, so the field
-- stayed too high until the periodic reconcile - the "phantom pheromone" that
-- draws squads to dead nests.
my_collection:add('removal_resumes_without_a_new_change', function()
    -- settle the field with the source present
    Runtime.readChunk(ENEMY_PHEROMONE, world[1])
    local relative = Runtime.readChunk(ENEMY_PHEROMONE, world[2])
    assert(relative > 0, "neighbour must see the propagated value first")

    -- remove the source, but starve the per-tick allowance so the removal is
    -- genuinely stranded (removeSource overwrites maxWork with the allowance, so
    -- the allowance itself has to be the limit)
    world[1].nestCount = 0
    Runtime.queueSource(world[1], ENEMY_PHEROMONE, 1 * ENEMY_MULTIPLIER, 0)
    testUniverse.solveWorkTick = game.tick
    testUniverse.solveWorkLeft = 1
    Runtime.process(ENEMY_PHEROMONE, 40, { decay = 0.9, eps = 0.0005 })
    assert(Runtime.hasPendingRemoval(ENEMY_PHEROMONE),
        "the removal must be stranded by the allowance, or the test proves nothing")
    assert(Runtime.readChunk(ENEMY_PHEROMONE, world[5]) > 0,
        "the stranded removal must still leave the far end of the chain high")

    -- now drain with nothing new queued: only the resume path can finish it
    testUniverse.solveWorkLeft = 1e9
    for _ = 1, 20 do
        testUniverse.solveWorkLeft = 1e9
        Runtime.process(ENEMY_PHEROMONE, 40, { decay = 0.9, eps = 0.0005 })
    end

    assert(not Runtime.hasPendingRemoval(ENEMY_PHEROMONE),
        "the removal heap must be drained, not left stranded")
    for i = 2, 5 do
        assert(math.abs(Runtime.readChunk(ENEMY_PHEROMONE, world[i])) < 1e-9,
            "the removed source must stop influencing chunk " .. i)
    end
end)

my_collection:add('process_increase_propagates_locally', function()
    Runtime.readChunk(ENEMY_PHEROMONE, world[1])

    world[1].nestCount = 2
    Runtime.queueSource(world[1], ENEMY_PHEROMONE,
        1 * ENEMY_MULTIPLIER, 2 * ENEMY_MULTIPLIER)
    Runtime.process(ENEMY_PHEROMONE, 40, { decay = 0.9, eps = 0.0005 })

    assert(math.abs(Runtime.readChunk(ENEMY_PHEROMONE, world[1]) - 2 * ENEMY_MULTIPLIER) < 1e-9,
        "increased source must be updated")
    assert(math.abs(Runtime.readChunk(ENEMY_PHEROMONE, world[2]) - 2 * ENEMY_MULTIPLIER * 0.9) < 1e-9,
        "increased source must propagate to its neighbor")
end)

my_collection:add('process_decrease_recomputes_field', function()
    Runtime.readChunk(ENEMY_PHEROMONE, world[1])

    world[1].nestCount = 0
    Runtime.queueSource(world[1], ENEMY_PHEROMONE,
        1 * ENEMY_MULTIPLIER, 0)
    Runtime.process(ENEMY_PHEROMONE, 40, { decay = 0.9, eps = 0.0005 })

    assert(Runtime.readChunk(ENEMY_PHEROMONE, world[1]) == 0,
        "removed source must be cleared")
    assert(Runtime.readChunk(ENEMY_PHEROMONE, world[2]) == 0,
        "removed source must clear its propagated neighbor")
end)

my_collection:add('per_field_queues_drain_independently', function()
    local BASE_PHEROMONE = Constants.BASE_PHEROMONE
    Runtime.readChunk(BASE_PHEROMONE, world[1])
    Runtime.readChunk(ENEMY_PHEROMONE, world[1])

    -- Queue an enemy change and a base change, then drain ONLY the base field.
    local enemyBefore = Runtime.readChunk(ENEMY_PHEROMONE, world[1])
    world[1].nestCount = 3
    Runtime.queueSource(world[1], ENEMY_PHEROMONE,
        2 * ENEMY_MULTIPLIER, 3 * ENEMY_MULTIPLIER)
    world[1].playerBaseGenerator = 7
    Runtime.queueSource(world[1], BASE_PHEROMONE, 0, 7)
    Runtime.process(BASE_PHEROMONE, 40, { decay = 0.9, eps = 0.0005 })

    assert(math.abs(Runtime.readChunk(BASE_PHEROMONE, world[1]) - 7) < 1e-9,
        "drained field must be updated")
    assert(math.abs(Runtime.readChunk(ENEMY_PHEROMONE, world[1]) - enemyBefore) < 1e-9,
        "undrained field must NOT be updated by another field's drain")

    -- Now drain the enemy field; its change must land.
    Runtime.process(ENEMY_PHEROMONE, 40, { decay = 0.9, eps = 0.0005 })
    assert(math.abs(Runtime.readChunk(ENEMY_PHEROMONE, world[1]) - 3 * ENEMY_MULTIPLIER) < 1e-9,
        "draining the enemy field must apply its own queued change")
end)

my_collection:add('squad_logic_uses_injected_reader', function()
    local SquadLogic = require("squad_logic")
    SquadLogic.setSourceVersionedFieldReader(function(fieldType)
        if fieldType == Constants.BASE_PHEROMONE then return 11 end
        if fieldType == Constants.ENEMY_PHEROMONE then return 22 end
        return nil
    end)

    assert(SquadLogic.basePheromone(world[1], 0) == 11,
        "base pheromone must use the injected source-versioned reader")
    assert(SquadLogic.enemyPheromone(world[1], 0) == 22,
        "enemy pheromone must use the injected source-versioned reader")

    -- The accessors are total: a reader that declines (or no reader at all)
    -- yields 0, never nil, because callers do arithmetic on the result.
    assert(SquadLogic.resourcePheromone(world[1], 0) == 0,
        "a declining reader must yield 0, not nil")

    SquadLogic.setSourceVersionedFieldReader(nil)
    assert(SquadLogic.basePheromone(world[1], 0) == 0,
        "with no reader installed the accessor must still yield 0, not nil")
end)

return my_collection
