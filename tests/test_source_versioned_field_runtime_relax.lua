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

-- Diagnostics. A relaxed field carries no provenance, so the support chain is the only
-- way to say WHY a value is what it is; it has to walk to the source that grounds it.
local function settle()
    Runtime.ensureSolved(PLAYER, { decay = 0.9, eps = 0.0005, maxRadius = 32 })
    for _ = 1, 200 do Runtime.process(PLAYER, 64, { decay = 0.9, eps = 0.0005, maxRadius = 32 }) end
end

my_collection:add('diag_chain_walks_to_the_source_that_grounds_the_value', function()
    settle()
    local chain = Runtime.liveChain(PLAYER, 5)
    assert(chain:find("root=1", 1, true), "the chain must end at the source chunk: " .. chain)
    assert(chain:find("verdict=consistent", 1, true), "a settled value is consistent: " .. chain)
    assert(chain:find("grounded=source", 1, true), "the chain must ground in a source: " .. chain)
end)

my_collection:add('diag_support_reports_no_mismatch_on_a_settled_field', function()
    settle()
    local report = Runtime.supportDiag(PLAYER, 4)
    assert(report:find("mismatches=0", 1, true), "settled field must match: " .. report)
    assert(report:find("liveOnly=0", 1, true), "settled field must have no extra chunks: " .. report)
end)

my_collection:add('diag_support_flags_an_unsupported_value_and_names_its_root', function()
    settle()
    local key = tostring(PLAYER)
    local field = universe.sourceVersionedFields[key]
    field[7] = 999 -- no neighbour justifies this; the chain should say so
    local report = Runtime.supportDiag(PLAYER, 4)
    assert(report:find("mismatches=1", 1, true), "the poked value must count: " .. report)
    assert(report:find("root=1", 1, true), "the chain still walks to the source: " .. report)
    local chain = Runtime.liveChain(PLAYER, 7)
    assert(chain:find("verdict=aboveSupport", 1, true),
        "a value above its neighbours must be flagged: " .. chain)
end)

my_collection:add('diag_field_diag_prints_the_live_chain_for_a_relaxed_field', function()
    settle()
    local report = Runtime.fieldDiag(PLAYER, 3)
    assert(report:find("relax=true", 1, true), "the field is relaxed: " .. report)
    assert(report:find("chain=", 1, true), "relaxed fields print a live chain: " .. report)
    assert(report:find("root=1", 1, true), report)
end)

-- The audit is the backstop for a value the frontier lost. A ghost is repaired by
-- re-popping it, so the sweep plus a drain has to land on the exact solve again.
local function auditStats()
    local key = tostring(PLAYER)
    Runtime.auditStats[key] = Runtime.auditStats[key]
        or { ghosts = 0, starved = 0, sweeps = 0 }
    return Runtime.auditStats[key]
end

local function sweep(rounds)
    for _ = 1, rounds do
        Runtime.relaxAudit(PLAYER)
        Runtime.process(PLAYER, 64, { decay = 0.9, eps = 0.0005, maxRadius = 32 })
    end
end

my_collection:add('audit_repairs_a_ghost_the_frontier_lost', function()
    settle()
    local key = tostring(PLAYER)
    local field = universe.sourceVersionedFields[key]
    local honest = field[5]
    field[5] = honest * 3 -- a local peak nothing supports
    local stats = auditStats()
    stats.ghosts = 0
    sweep(400)
    assert(stats.ghosts > 0, "the sweep must see the ghost")
    assert(math.abs((field[5] or 0) - honest) < 1e-3,
        string.format("the sweep must restore the supported value: %.6f vs %.6f",
            field[5] or 0, honest))
    local report = Runtime.supportDiag(PLAYER, 4)
    assert(report:find("mismatches=0", 1, true), "repaired field must match: " .. report)
end)

my_collection:add('audit_repairs_a_starved_value_below_its_justification', function()
    settle()
    local key = tostring(PLAYER)
    local field = universe.sourceVersionedFields[key]
    local honest = field[5]
    field[5] = honest * 0.5 -- a wave that stopped short
    local stats = auditStats()
    stats.starved = 0
    sweep(400)
    assert(stats.starved > 0, "the sweep must see the starved value")
    assert(math.abs((field[5] or 0) - honest) < 1e-3,
        string.format("the sweep must restore the supported value: %.6f vs %.6f",
            field[5] or 0, honest))
end)

-- A chunk record can be removed after a pop for it was queued. Recomputing from the
-- neighbours then leaves a value at an id that is not a chunk: nothing visits it again,
-- so it and its cone fossilise (measured 1,052 entries on a settled field). The rule is
-- that a field entry belongs to a chunk, so no chunk means no value.
my_collection:add('removing_a_chunk_record_drops_its_value_and_its_cone', function()
    settle()
    local key = tostring(PLAYER)
    local field = universe.sourceVersionedFields[key]
    local kept = field[4]
    assert(kept and kept > 0 and (field[6] or 0) > 0, "fixture: the strip has a cone")
    local removed = world[5]              -- the record object survives its removal
    Runtime.dropChunk(removed)           -- what MapUtils.removeChunkFromMap calls first
    universe.chunkById[5] = nil
    Runtime.queueSource(removed, PLAYER, field[5] or 0, 0)
    for _ = 1, 400 do
        Runtime.process(PLAYER, 64, { decay = 0.9, eps = 0.0005, maxRadius = 32 })
    end
    assert(field[5] == nil, "a removed chunk must hold no value: " .. tostring(field[5]))
    for id = 6, 9 do
        assert((field[id] or 0) == 0,
            "the cone beyond the removed chunk must fall away, chunk " .. id ..
                " = " .. tostring(field[id]))
    end
    -- The disc clear also re-queues the near side, which refills from the source; it must
    -- land back on exactly the value it had.
    for _ = 1, 400 do
        Runtime.process(PLAYER, 64, { decay = 0.9, eps = 0.0005, maxRadius = 32 })
    end
    assert(math.abs((field[4] or 0) - kept) < 1e-6,
        string.format("the near side must refill to its supported value: %.6f vs %.6f",
            field[4] or 0, kept))
end)

my_collection:add('audit_leaves_a_settled_field_alone', function()
    settle()
    local stats = auditStats()
    stats.ghosts, stats.starved = 0, 0
    sweep(400)
    assert(stats.ghosts == 0 and stats.starved == 0,
        string.format("a settled field must not be touched (ghosts=%d starved=%d)",
            stats.ghosts, stats.starved))
    assert(Runtime.relaxDepth(PLAYER) == 0, "the sweep must not leave work queued")
end)

return my_collection
