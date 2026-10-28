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
    -- Production relaxes every field; these pin read/queue semantics on that path.
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
-- A budgeted removal leaves a heap when it runs out of allowance. Nothing used to
-- continue it unless another change arrived for the same chunk, so the field
-- stayed too high until the periodic reconcile - the "phantom pheromone" that
-- draws squads to dead nests.
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
    -- A decrease converges geometrically (the value falls by `decay` per pass), so the
    -- frontier needs draining rather than one call: 1.0 at decay 0.9 reaches the eps
    -- threshold in ~73 passes.
    for _ = 1, 200 do
        Runtime.process(ENEMY_PHEROMONE, 64, { decay = 0.9, eps = 0.0005 })
    end

    -- The frontier stops when a pass moves a value by less than eps, so a decrease
    -- settles at a residual of eps/(1-decay^2) rather than exactly zero (measured
    -- 0.00246 for eps 5e-4, decay 0.9). Assert the bound, not zero.
    local residual = 0.0005 / (1 - 0.9 * 0.9)
    assert(Runtime.readChunk(ENEMY_PHEROMONE, world[1]) <= residual,
        "removed source must fall to the residual bound, got " ..
        tostring(Runtime.readChunk(ENEMY_PHEROMONE, world[1])))
    assert(Runtime.readChunk(ENEMY_PHEROMONE, world[2]) <= residual,
        "the propagated neighbour must fall to the same bound, got " ..
        tostring(Runtime.readChunk(ENEMY_PHEROMONE, world[2])))
end)

my_collection:add('per_field_queues_drain_independently', function()
    local BASE_PHEROMONE = Constants.BASE_PHEROMONE
    Runtime.readChunk(BASE_PHEROMONE, world[1])
    Runtime.readChunk(ENEMY_PHEROMONE, world[1])

    -- A write applies its OWN node immediately on every field (so a write is visible
    -- to a read in the same tick); what waits for that field's drain is the
    -- propagation to neighbours.
    local enemyNeighbourBefore = Runtime.readChunk(ENEMY_PHEROMONE, world[2])
    world[1].nestCount = 3
    Runtime.queueSource(world[1], ENEMY_PHEROMONE,
        2 * ENEMY_MULTIPLIER, 3 * ENEMY_MULTIPLIER)
    world[1].playerBaseGenerator = 7
    Runtime.queueSource(world[1], BASE_PHEROMONE, 0, 7)

    assert(math.abs(Runtime.readChunk(ENEMY_PHEROMONE, world[1]) - 3 * ENEMY_MULTIPLIER) < 1e-9,
        "a source write must be visible immediately")
    assert(math.abs(Runtime.readChunk(ENEMY_PHEROMONE, world[2]) - enemyNeighbourBefore) < 1e-9,
        "an undrained field must not have propagated to its neighbour yet")

    Runtime.process(BASE_PHEROMONE, 40, { decay = 0.9, eps = 0.0005 })
    assert(math.abs(Runtime.readChunk(BASE_PHEROMONE, world[1]) - 7) < 1e-9,
        "draining the base field must apply its own change")

    -- Draining the enemy field now propagates the write outward.
    for _ = 1, 50 do
        Runtime.process(ENEMY_PHEROMONE, 64, { decay = 0.9, eps = 0.0005 })
    end
    assert(math.abs(Runtime.readChunk(ENEMY_PHEROMONE, world[2]) - 3 * ENEMY_MULTIPLIER * 0.9) < 1e-6,
        "the neighbour must follow once the field is drained")
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
