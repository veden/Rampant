-- Copyright (C) 2022  veden
--
-- Unit tests for the source-versioned pheromone field solver.

local TestCollection = require("tests/framework")
local Solver = require("libs/source_versioned_field")

local my_collection = TestCollection:new()

local W, H = 16, 16
local DECAY = 0.9
local EPS = 0.0005
local RATING = 1.0

local function id(x, y) return x * 1000 + y end

local function makeNeighborOf()
    return function(cid)
        local x = math.floor(cid / 1000)
        local y = cid % 1000
        local out = {}
        for dx = -1, 1 do
            for dy = -1, 1 do
                if not (dx == 0 and dy == 0) then
                    local nx, ny = x + dx, y + dy
                    if nx >= 1 and nx <= W and ny >= 1 and ny <= H then
                        out[#out + 1] = id(nx, ny)
                    end
                end
            end
        end
        return out
    end
end

local function makeSources()
    local static = {}
    local dynamic = {}
    local seed = 12345
    local function rnd()
        seed = (seed * 1103515245 + 12345) % 2147483648
        return seed / 2147483648
    end
    for i = 1, 20 do
        static[#static + 1] = {
            id = id(1 + math.floor(rnd() * W), 1 + math.floor(rnd() * H)),
            gen = 1 + math.floor(rnd() * 100),
        }
    end
    for i = 1, 8 do
        dynamic[#dynamic + 1] = {
            id = id(1 + math.floor(rnd() * W), 1 + math.floor(rnd() * H)),
            gen = 1 + math.floor(rnd() * 100),
        }
    end
    return static, dynamic
end

local function allSources(static, dynamic)
    local out = {}
    for _, s in ipairs(static) do out[#out + 1] = { id = s.id, gen = s.gen } end
    for _, s in ipairs(dynamic) do out[#out + 1] = { id = s.id, gen = s.gen } end
    return out
end

local function reference(sources, neighborOf)
    local field = {}
    for _, s in ipairs(sources) do
        local v = s.gen * RATING
        if v > (field[s.id] or 0) then field[s.id] = v end
    end
    local lastDiff = 0
    for pass = 1, 500 do
        lastDiff = 0
        for cid = id(1, 1), id(W, H) do
            local x = math.floor(cid / 1000)
            local y = cid % 1000
            if x >= 1 and x <= W and y >= 1 and y <= H then
                local maxN = 0
                for _, nid in ipairs(neighborOf(cid)) do
                    local nv = field[nid] or 0
                    if nv > maxN then maxN = nv end
                end
                local gen = 0
                for _, s in ipairs(sources) do
                    if s.id == cid then gen = s.gen * RATING break end
                end
                local new = math.max(gen, maxN * DECAY)
                local d = math.abs(new - (field[cid] or 0))
                if d > lastDiff then lastDiff = d end
                field[cid] = new
            end
        end
        if lastDiff < 1e-6 then break end
    end
    return field
end

local function maxDiff(a, b)
    local m = 0
    for k, v in pairs(a) do
        local d = math.abs((b[k] or 0) - v)
        if d > m then m = d end
    end
    for k, v in pairs(b) do
        local d = math.abs((a[k] or 0) - v)
        if d > m then m = d end
    end
    return m
end

my_collection:add('solve_matches_iterative_relaxation', function()
    local static, dynamic = makeSources()
    local sources = allSources(static, dynamic)
    local neighborOf = makeNeighborOf()
    local ref = reference(sources, neighborOf)
    local field = Solver.solve(sources, neighborOf, {
        decay = DECAY, eps = EPS, rating = RATING,
    })
    assert(maxDiff(field, ref) < 1e-6,
        "source-versioned solve must match iterative relaxation")
end)

my_collection:add('propagate_increase_is_exact', function()
    local static, dynamic = makeSources()
    local sources = allSources(static, dynamic)
    local neighborOf = makeNeighborOf()
    local before = Solver.solve(sources, neighborOf, {
        decay = DECAY, eps = EPS, rating = RATING,
    })
    dynamic[1].gen = dynamic[1].gen + 10
    local afterRef = Solver.solve(sources, neighborOf, {
        decay = DECAY, eps = EPS, rating = RATING,
    })
    local after = {}
    for k, v in pairs(before) do after[k] = v end
    Solver.propagateIncrease(dynamic[1], after, neighborOf, {
        decay = DECAY, eps = EPS, rating = RATING,
    })
    assert(maxDiff(after, afterRef) < 1e-6,
        "increase-only propagation must match full solve")
end)

my_collection:add('max_radius_bounds_influence', function()
    local static, dynamic = makeSources()
    local sources = allSources(static, dynamic)
    local neighborOf = makeNeighborOf()
    local field, work = Solver.solve({ dynamic[1] }, neighborOf, {
        decay = DECAY, eps = EPS, rating = RATING, maxRadius = 0,
    })
    assert(work == 0, "radius 0 must not propagate beyond the source")
    local nonzero = 0
    for _, v in pairs(field) do
        if v ~= 0 then nonzero = nonzero + 1 end
    end
    assert(nonzero == 1, "radius 0 must keep only the source")
end)

my_collection:add('no_sources_returns_empty', function()
    local field, work = Solver.solve({}, makeNeighborOf(), {
        decay = DECAY, eps = EPS, rating = RATING,
    })
    assert(next(field) == nil, "no sources must return an empty field")
    assert(work == 0, "no sources must do no work")
end)

my_collection:add('recompute_fallback_is_exact', function()
    local static, dynamic = makeSources()
    local sources = allSources(static, dynamic)
    local neighborOf = makeNeighborOf()
    local ref = Solver.solve(sources, neighborOf, {
        decay = DECAY, eps = EPS, rating = RATING,
    })
    local field = Solver.recompute({ dynamic[1] }, sources, {}, neighborOf, {
        decay = DECAY, eps = EPS, rating = RATING,
    })
    assert(maxDiff(field, ref) < 1e-6,
        "recompute fallback must match full solve")
end)

local function metadataOptions()
    return { decay = DECAY, eps = EPS, rating = RATING }
end

local function assertMetadataConsistent(field, topSource, second, secondSource)
    for chunkId, value in pairs(field) do
        local sec = second[chunkId] or 0
        assert(sec <= value + EPS,
            "second contribution must not exceed the field value")
        if value > EPS then
            assert(topSource[chunkId] ~= nil,
                "every non-zero chunk must record its top source")
        end
    end
end

my_collection:add('solve_with_metadata_matches_solve', function()
    local static, dynamic = makeSources()
    local sources = allSources(static, dynamic)
    local neighborOf = makeNeighborOf()
    local ref = Solver.solve(sources, neighborOf, metadataOptions())
    local field, topSource, second, secondSource = Solver.solveWithMetadata(
        sources, neighborOf, metadataOptions())
    assert(maxDiff(field, ref) < 1e-6,
        "metadata solve must match the plain solve")
    assertMetadataConsistent(field, topSource, second, secondSource)
end)

my_collection:add('remove_source_matches_full_solve', function()
    local static, dynamic = makeSources()
    local sources = allSources(static, dynamic)
    local neighborOf = makeNeighborOf()
    local field, topSource, second, secondSource = Solver.solveWithMetadata(
        sources, neighborOf, metadataOptions())

    local afterSources = {}
    for _, source in ipairs(static) do
        afterSources[#afterSources + 1] = { id = source.id, gen = source.gen }
    end
    for i, source in ipairs(dynamic) do
        if i ~= 1 then
            afterSources[#afterSources + 1] = { id = source.id, gen = source.gen }
        end
    end
    local ref = Solver.solve(afterSources, neighborOf, metadataOptions())

    Solver.removeSource(dynamic[1], field, topSource, second, secondSource,
        neighborOf, metadataOptions())
    assert(maxDiff(field, ref) < 1e-6,
        "local source removal must match the full solve")
    assertMetadataConsistent(field, topSource, second, secondSource)
end)

my_collection:add('propagate_increase_with_metadata_matches_solve', function()
    local static, dynamic = makeSources()
    local sources = allSources(static, dynamic)
    local neighborOf = makeNeighborOf()
    local field, topSource, second, secondSource = Solver.solveWithMetadata(
        sources, neighborOf, metadataOptions())

    dynamic[1].gen = dynamic[1].gen + 10
    local afterSources = allSources(static, dynamic)
    local ref = Solver.solve(afterSources, neighborOf, metadataOptions())

    Solver.propagateIncreaseWithMetadata(dynamic[1], field,
        topSource, second, secondSource, neighborOf, metadataOptions())
    assert(maxDiff(field, ref) < 1e-6,
        "local metadata increase must match the full solve")
    assertMetadataConsistent(field, topSource, second, secondSource)
end)

my_collection:add('propagate_increases_with_metadata_matches_solve', function()
    local static, dynamic = makeSources()
    local sources = allSources(static, dynamic)
    local neighborOf = makeNeighborOf()
    local field, topSource, second, secondSource = Solver.solveWithMetadata(
        sources, neighborOf, metadataOptions())

    dynamic[1].gen = dynamic[1].gen + 10
    dynamic[2].gen = dynamic[2].gen + 25
    dynamic[3].gen = dynamic[3].gen + 5
    local afterSources = allSources(static, dynamic)
    local ref = Solver.solve(afterSources, neighborOf, metadataOptions())

    Solver.propagateIncreasesWithMetadata(
        { dynamic[1], dynamic[2], dynamic[3] },
        field, topSource, second, secondSource, neighborOf, metadataOptions())
    assert(maxDiff(field, ref) < 1e-6,
        "multi-source metadata increase must match the full solve")
    assertMetadataConsistent(field, topSource, second, secondSource)
end)

-- Test: a work-bounded propagation resumes and converges to the unbounded
-- field. This is what bounds the recurring source-field spikes: the frontier is
-- kept between calls, and re-seeding is idempotent.
my_collection:add('bounded_propagation_resumes_to_the_unbounded_field', function()
    local Solver = require("libs/source_versioned_field")

    local function neighborOf(id)
        local out = {}
        if id > 0 then out[#out + 1] = id - 1 end
        if id < 4 then out[#out + 1] = id + 1 end
        return out
    end

    local fieldA, topA, secondA, secondSourceA = {}, {}, {}, {}
    Solver.propagateIncreasesWithMetadata(
        { { id = 0, gen = 100 } }, fieldA, topA, secondA, secondSourceA,
        neighborOf, { decay = 0.9, eps = 0.0005 })

    local fieldB, topB, secondB, secondSourceB = {}, {}, {}, {}
    local heap = {}
    local calls = 0
    repeat
        local _, remaining = Solver.propagateIncreasesWithMetadata(
            { { id = 0, gen = 100 } }, fieldB, topB, secondB, secondSourceB,
            neighborOf, { decay = 0.9, eps = 0.0005, maxWork = 1 }, heap)
        heap = remaining
        calls = calls + 1
    until #heap == 0 or calls > 200

    assert(calls > 1, "maxWork of 1 must not finish in one call")
    assert(calls <= 200, "bounded propagation must terminate")
    for id = 0, 4 do
        assert(math.abs((fieldA[id] or 0) - (fieldB[id] or 0)) < 1e-9,
            "bounded propagation must converge to the unbounded field at id " .. id)
    end
end)

return my_collection
