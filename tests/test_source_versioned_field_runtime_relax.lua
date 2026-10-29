-- Relaxation solver: same fixed point as the ordered solver, reached iteratively.
local TestCollection = require("tests/framework")
local Runtime = require("libs/source_versioned_field_runtime")
local Solver = require("libs/source_versioned_field")
local MapUtils = require("MapUtils")
local Constants = require("Constants")

local my_collection = TestCollection:new()
local world, universe

local function makeWorld()
    -- a 9x1 strip so a source at one end decays across several hops
    local chunkMap = { chunks = {} }
    local chunks = {}
    for i = 0, 8 do
        local chunk = { id = i + 1, x = i * 32, y = 0, map = chunkMap, nestCount = 0 }
        chunks[i + 1] = chunk
        chunkMap.chunks[MapUtils.chunkKey(i * 32, 0)] = chunk
    end
    chunks[1].playerGenerator = 300      -- the player field is flagged relax
    universe = { chunkById = chunks, sourceVersionedRelax = {} }
    local oldById, oldByXY = MapUtils.getChunkById, MapUtils.getChunkByXY
    MapUtils.getChunkById = function(id) return chunks[id] end
    MapUtils.getChunkByXY = function(_, x, y)
        return chunkMap.chunks[MapUtils.chunkKey(x, y)] or -1
    end
    Runtime.init(universe, true)
    my_collection._restore = function()
        MapUtils.getChunkById, MapUtils.getChunkByXY = oldById, oldByXY
    end
    return chunks
end

my_collection:set_setup(function() world = makeWorld() end)
my_collection:set_teardown(function()
    if my_collection._restore then my_collection._restore() end
    world = nil
end)

local PLAYER = Constants.PLAYER_PHEROMONE
local DEPOSIT = 300

my_collection:add('relax_matches_ordered_solve', function()
    assert(universe.sourceVersionedRelax[tostring(PLAYER)] ~= nil or true, "")
    Runtime.ensureSolved(PLAYER, { decay = 0.9, eps = 0.0005, maxRadius = 32 })
    for _ = 1, 200 do Runtime.process(PLAYER, 64, { decay = 0.9, eps = 0.0005, maxRadius = 32 }) end
    assert(universe.sourceVersionedRelax[tostring(PLAYER)] ~= nil,
        "the relaxation path must own this field's state")
    local relaxed = universe.sourceVersionedFields[tostring(PLAYER)]
    local sources = Runtime.buildSources(PLAYER)
    local exact = Solver.solveWithMetadata(sources, Runtime.neighborOf,
        { decay = 0.9, eps = 0.0005, maxRadius = 32 })
    for id = 1, 9 do
        local a, b = relaxed[id] or 0, exact[id] or 0
        assert(math.abs(a - b) < 1e-3,
            "relaxed and ordered solvers disagree at chunk " .. id .. ": " .. a .. " vs " .. b)
    end
end)

my_collection:add('relax_decrease_falls_to_the_exact_value', function()
    Runtime.ensureSolved(PLAYER, { decay = 0.9, eps = 0.0005, maxRadius = 32 })
    for _ = 1, 200 do Runtime.process(PLAYER, 64, { decay = 0.9, eps = 0.0005, maxRadius = 32 }) end
    world[1].playerGenerator = nil            -- the source itself goes away
    Runtime.queueSource(world[1], PLAYER, DEPOSIT, 0)
    for _ = 1, 200 do Runtime.process(PLAYER, 64, { decay = 0.9, eps = 0.0005, maxRadius = 32 }) end
    local field = universe.sourceVersionedFields[tostring(PLAYER)]
    for id = 1, 9 do
        assert((field[id] or 0) == 0, "a removed source must fall to zero at chunk " .. id)
    end
end)

-- The radius cap is where the two solvers can disagree: the ordered solve truncates
-- at maxRadius, and relaxation has to arrive at the same truncation through the
-- per-node distance it carries.
my_collection:add('relax_truncates_at_the_same_radius_as_the_ordered_solve', function()
    for _, radius in ipairs({ 2, 4, 6 }) do
        local opts = { decay = 0.9, eps = 5e-4, maxRadius = radius }
        -- ordered: exact solve with the cap
        local exact = Solver.solveWithMetadata(Runtime.buildSources(PLAYER), Runtime.neighborOf, opts)
        -- relaxed: seed and drain to quiescence with the same cap
        universe.sourceVersionedRelax = {}
        universe.sourceVersionedFields[tostring(PLAYER)] = {}
        Runtime.relaxSeed(PLAYER)
        local guard = 0
        while Runtime.relaxDepth(PLAYER) > 0 and guard < 5000 do
            guard = guard + 1
            Runtime.relaxStep(PLAYER, 64, opts)
        end
        local relaxed = universe.sourceVersionedFields[tostring(PLAYER)] or {}

        -- support must match exactly: no entry beyond the cap, none missing inside it
        for id = 1, 9 do
            local e, r = exact[id], relaxed[id]
            if e then
                assert(r, "radius " .. radius .. ": relaxed field is missing chunk " .. id)
            elseif r and r > opts.eps then
                assert(false, string.format(
                    "radius %d: relaxed field has chunk %d (value %.6f) that the exact solve truncated",
                    radius, id, r))
            end
        end
        for id = 1, 9 do
            local e, r = exact[id] or 0, relaxed[id] or 0
            assert(math.abs(e - r) <= opts.eps * 10,
                string.format("radius %d: chunk %d differs (exact %.9f relaxed %.9f)", radius, id, e, r))
        end
    end
end)

my_collection:add('relax_is_quiescent_when_nothing_changes', function()
    Runtime.ensureSolved(PLAYER, { decay = 0.9, eps = 0.0005, maxRadius = 32 })
    for _ = 1, 200 do Runtime.process(PLAYER, 64, { decay = 0.9, eps = 0.0005, maxRadius = 32 }) end
    assert(Runtime.relaxDepth(PLAYER) == 0, "frontier must empty once settled")
    for _ = 1, 10 do Runtime.process(PLAYER, 64, { decay = 0.9, eps = 0.0005, maxRadius = 32 }) end
    assert(Runtime.relaxDepth(PLAYER) == 0, "a settled field must stay settled")
end)

return my_collection
