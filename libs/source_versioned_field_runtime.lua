-- Copyright (C) 2022  veden
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

--[[
    Source-versioned field runtime adapter.

    Impure bridge between the pure Solver and the Factorio chunk grid.

    Responsibilities:
      * build the chunk-neighbour graph for the solver,
      * extract per-field source values from chunks,
      * solve static fields once,
      * propagate dynamic source changes,
      * expose an effective read path.
]]

local Solver = require("source_versioned_field")
local MapUtils = require("MapUtils")
local ChunkPropertyUtils = require("ChunkPropertyUtils")
local Constants = require("Constants")
local Utils = require("Utils")
local nextFrom = Utils.nextFrom
local RamState = require("libs/RamState")
local SquadLogic = require("squad_logic")
local MathUtils = require("MathUtils")

local Runtime = {}

local Universe
-- Per-field source-change queues. Each field drains on its own budget so a
-- slow field (resource) cannot starve a decision-critical one (base/enemy).
local FIELD_CADENCE = {}
local FIELD_DRAIN_PER_TICK = {}
local Initialized = false
local SolvedFields = {}
local FieldProvenance = {}
local FieldSecond = {}

-- Solved-field state is persisted in storage so a load does not re-solve. The
-- module locals alias the storage tables. `on_load` must not write storage, so
-- creation and the layout version check live in ensureStorage(), which is called
-- from init(addProperties=true) and from the tick path.
local SOLVE_VERSION = 3
local FIELD_PLAYER = Constants.PLAYER_PHEROMONE
local FIELD_BASE = Constants.BASE_PHEROMONE
local FIELD_ENEMY = Constants.ENEMY_PHEROMONE
local FIELD_RESOURCE = Constants.RESOURCE_PHEROMONE
local ENEMY_MULTIPLIER = Constants.ENEMY_PHEROMONE_MULTIPLER
local CHUNK_SIZE = Constants.CHUNK_SIZE
local NEIGHBOR_OFFSETS = {
    { -CHUNK_SIZE, -CHUNK_SIZE }, -- 1
    { 0, -CHUNK_SIZE },          -- 2
    { CHUNK_SIZE, -CHUNK_SIZE },  -- 3
    { -CHUNK_SIZE, 0 },           -- 4
    { CHUNK_SIZE, 0 },            -- 5
    { -CHUNK_SIZE, CHUNK_SIZE },  -- 6
    { 0, CHUNK_SIZE },            -- 7
    { CHUNK_SIZE, CHUNK_SIZE },   -- 8
}

-- Kamikaze is derived from the player field, not an independent quantity:
-- K(c) = 0.95 * max(max_n K(n), P(c)) at the pass level, which is a max-plus
-- propagation of the player's own sources at 0.95 per hop, scaled 0.95 at the
-- source. It is therefore a solver field sharing the player's sources with its
-- own decay, which makes the read a table lookup instead of a maintained slot.
local FIELD_KAMIKAZE = Constants.KAMIKAZE_PHEROMONE
local KAMIKAZE_DECAY = Constants.DECAY_KAMIKAZE
local PLAYER_FIELD_RADIUS = 8
local REGIONAL_FIELD_RADIUS = 32
local RESOURCE_FIELD_RADIUS = 24
local KAMIKAZE_SCALE = 0.95

-- The death field is signed and max-plus cannot lower a value, so the two signs
-- are separate fields and the read subtracts them (min-plus == negated
-- max-plus). Both are driven by the one death generator, like player/kamikaze.
local FIELD_DEATH_GAIN = Constants.DEATH_GAIN_PHEROMONE
local FIELD_DEATH_LOSS = Constants.DEATH_LOSS_PHEROMONE
local DEATH_DECAY = Constants.DEATH_FIELD_DECAY
local DEATH_RADIUS = Constants.DEATH_FIELD_RADIUS
local DEATH_SCALE = Constants.DEATH_FIELD_SOURCE_SCALE

-- Relaxation fields (docs/relaxation.md): the field is kept as the fixed point of
-- value(v) = max(ownSource(v), max(neighbour) * decay), recomputed only where
-- something changed. Cheap to reason about and exact at quiescence, at the cost of
-- reaching it iteratively - test/tune per field via FIELD_OPTS[field].relax.
local FIELD_OPTS = {}
-- Radius caps on the two fields the player writes every tick. Nominal reach at
-- decay 0.9 / eps 5e-4 is 126 hops (~4000 tiles), so one step improved most of
-- the map and the field filled every tracked chunk (991 chunks measured). 8 hops
-- is 256 tiles: far more than a hunter needs, and it bounds the work per move and
-- the field's memory to a fixed neighbourhood.
FIELD_OPTS[FIELD_PLAYER] = { maxRadius = PLAYER_FIELD_RADIUS, relax = true }
-- Base, enemy and resource change rarely (structure events, chunk scans) but
-- their nominal reach was also 126 hops, so every one of those rare changes
-- re-propagated the entire tracked map and paid O(map) work in a single tick.
FIELD_OPTS[FIELD_BASE] = { maxRadius = REGIONAL_FIELD_RADIUS, relax = true }
FIELD_OPTS[FIELD_ENEMY] = { maxRadius = REGIONAL_FIELD_RADIUS, relax = true }
FIELD_OPTS[FIELD_RESOURCE] = { maxRadius = RESOURCE_FIELD_RADIUS, relax = true }
FIELD_OPTS[FIELD_KAMIKAZE] = { decay = KAMIKAZE_DECAY, maxRadius = PLAYER_FIELD_RADIUS, relax = true }
-- The death field's whole range is small: a squad wipe step deposits
-- FIVE_DEATH_PHEROMONE_GENERATOR_AMOUNT (0.005) and the source scale is 0.75/8, so
-- one death is 4.7e-4 - BELOW the shared eps of 5e-4, meaning a single death used to
-- vanish from the field and only accumulated deaths (generator clamped at 1 -> 0.094)
-- were visible. eps per field keeps the threshold meaningful at every magnitude.
local DEATH_EPS = 1e-6
FIELD_OPTS[FIELD_DEATH_GAIN] = { decay = DEATH_DECAY, maxRadius = DEATH_RADIUS, eps = DEATH_EPS, relax = true }
FIELD_OPTS[FIELD_DEATH_LOSS] = { decay = DEATH_DECAY, maxRadius = DEATH_RADIUS, eps = DEATH_EPS, relax = true }

local function fieldOpts(fieldType, opts)
    return opts or FIELD_OPTS[fieldType]
end

-- Create or version-check the persisted solved state. Writes storage, so it
-- must NOT run on the load path; init(addProperties=true) and the tick path call it.
function Runtime.ensureStorage()
    local U = Universe
    if not U then return end
    if U.sourceVersionedSolveVersion == SOLVE_VERSION
        and U.sourceVersionedProvenance then
            return
    end
    -- New save, or state from an older layout. The field values and the top-two
    -- metadata must be dropped together: the decrease path is only exact while the
    -- metadata matches the field.
    Universe.sourceVersionedFields, Universe.sourceVersionedProvenance = {}, {}
    Universe.sourceVersionedSecond = {}
    Universe.sourceVersionedSolved, Universe.sourceVersionedSolvedAt = {}, {}
    -- A frontier from a dropped field is meaningless: the field restarts.
    Universe.sourceVersionedFrontier, Universe.sourceVersionedRemoval = {}, {}
    Universe.sourceVersionedRelax = {}
    U.sourceVersionedSolveVersion = SOLVE_VERSION
    SourceQueues = {}
    QueueIndex = {}
end

--- Force a reseed of every relaxed field. Called on every load: a save can carry a
--- frontier and a `seeded` flag written mid-settle (ours did: the base field came back
--- with a peak of 47,075 against a largest source of 15,500, i.e. values from an older
--- build that nothing would ever revisit). Rebuilding from sources is cheap and is the
--- only state a field needs to be correct.
function Runtime.resetRelaxState()
    Universe.sourceVersionedRelax = {}
    for fieldType, opts in pairs(FIELD_OPTS) do
        if opts.relax then
            Universe.sourceVersionedFields[tostring(fieldType)] = nil
        end
    end
end

--- Reseed every relaxed field now, and settle a slice of each. The load path calls
--- this: clearing the state alone was not enough (measured liveOnly 2,909 and a live
--- peak of 47,075 against a largest source of 15,500 until the fields were reseeded).
function Runtime.reseedRelaxedFields()
    Runtime.resetRelaxState()
    for fieldType, opts in pairs(FIELD_OPTS) do
        if opts.relax then
            Runtime.ensureSolved(fieldType, opts)
        end
    end
end

function Runtime.init(universe, addProperties)
    Universe = universe
    if addProperties then
        Universe.sourceVersionedFields = Universe.sourceVersionedFields or {}
        Universe.sourceVersionedRefresh = Universe.sourceVersionedRefresh or {}
    end
    -- Reseed here, not only on the load path: queueSource ignores writes until
    -- Initialized is true, so a reseed performed during on_load (before init) can miss
    -- the scan's writes. init runs after storage is available, which is exactly the
    -- state reseedAll produces when it repairs a field by hand.
    Runtime.reseedRelaxedFields()
    Initialized = true
    SourceQueues = {}
    QueueIndex = {}
    FIELD_DRAIN_ACC = {}
    FIELD_DRAIN_PER_TICK = {}
    -- Per-field drain budgets (source changes per tick). Round 1 keeps the sum
    -- at the old single budget (40) so the work is equivalent; the split is
    -- what makes each field's cadence independently tunable.
    FIELD_DRAIN_PER_TICK[FIELD_RESOURCE] = 10
    FIELD_DRAIN_PER_TICK[FIELD_BASE] = 10
    FIELD_DRAIN_PER_TICK[FIELD_ENEMY] = 10
    FIELD_DRAIN_PER_TICK[FIELD_PLAYER] = 10
    -- Propagate the player-derived fields every 4 ticks instead of every tick.
    FIELD_CADENCE[FIELD_PLAYER] = 4
    FIELD_CADENCE[FIELD_KAMIKAZE] = 4
    FIELD_DRAIN_PER_TICK[FIELD_KAMIKAZE] = 10
    FIELD_DRAIN_PER_TICK[FIELD_DEATH_GAIN] = 10
    FIELD_DRAIN_PER_TICK[FIELD_DEATH_LOSS] = 10
    if addProperties then
        Runtime.ensureStorage()
    end
    Initialized = true
    -- The pure SquadLogic accessors delegate to this reader when the
    -- startup flag is enabled. Keeping the hook on SquadLogic avoids a
    -- Factorio dependency in the pure decision module.
    SquadLogic.setSourceVersionedFieldReader(Runtime.readChunk)
    ChunkPropertyUtils.setSourceVersionedFieldReader(Runtime.readChunk)
end

function Runtime.isInitialized()
    return Initialized
end

function Runtime.fieldKey(fieldType)
    return tostring(fieldType)
end

-- Reusable entry tables for buildSources: the diagnostic path and every read that
-- needs a fresh source list would otherwise allocate one table per source.
function Runtime.queueSource(chunk, fieldType, oldValue, newValue)
    if not Initialized or not chunk or not chunk.id then return false end
    local fopts = FIELD_OPTS[fieldType]
    if not fopts then return false end
    local key = Runtime.fieldKey(fieldType)
    local st = relaxState(key)
    if not st.seeded or st.version ~= RELAX_VERSION then
        st.dirty = {}
        Runtime.relaxSeed(fieldType)
    end
    -- Apply THIS node now so a write is visible to a read in the same tick (one node,
    -- ~13us); neighbours arrive through the frontier over the following ticks.
    relaxNode(fieldType, key, chunk.id, fopts)
    relaxPush(key, chunk.id)
    return true
end

-- Solve one field on first use. Solving lazily keeps the first H8 tick from
-- paying for fields that are never read while still ensuring a source change
-- propagates into a complete field.
-- Chunks examined per tick while building a field's source list. The first
-- solve used to scan every tracked chunk and run a full Dijkstra in one tick:
-- measured 12.4ms on a ~1k-chunk map (and 40-50ms on the whole tick), once per
-- field, which is what produced the early-game maximum.
local REMOVAL_MAX_WORK_Per_CALL = 256
-- One pop costs ~13us on a dense field, so 256 pops was a 3.4ms slice inside a
-- single tick (measured during the periodic field rebuild). 128 halves the worst
-- slice and doubles the rebuild's spread.
-- Pop slices per rebuild call. Sized against the SIMULATION, not just the tick: a
-- full field is ~8,850 nodes, so 64 pops/tick refreshes it in 2.3s of game time,
-- while a behemoth biter (0.3 tiles/tick) needs 1.8s to cross ONE chunk. At 32 it
-- takes 4.6s - about 2.5 chunk-crossings - which is still faster than any squad can
-- outrun the gradient it is steering by. A pop costs ~13us, so this is also the
-- per-tick slice: 32 x 13us ~ 0.4ms.
local SourceBuilds = {}

-- Ticks between full reconciliations of one field. The incremental path only
-- applies increases (max-plus), so improvements skipped by the eps test and
-- values left behind by removals let the live field drift from the exact solve:
-- measured 109 of 613 chunks off by more than 10% (worst 50%) and 35 stale
-- chunks on an enemy field. Rebuilding periodically bounds that drift; the swap
-- happens only when the rebuild completes, so readers keep the old field while
-- it is being rebuilt.
-- Safety-net resync interval. The event path keeps the field correct; this only
-- catches what the event path deliberately skips (source changes under the 5%
-- throttle) and anything a bug loses. It is a TIMER, so it pays its cost whether
-- anything drifted or not: at 9,000 ticks that was a 4.6s spread rebuild every
-- 2.5 game-minutes to correct a bounded <=5% error. 15 minutes is still a backstop
-- and 6x less work. A field also rebuilds on load (no timestamp), after an id
-- compaction, when its removal heap overflows, and on demand (rebuildField).
function Runtime.ensureSolved(fieldType, opts)
    local key = Runtime.fieldKey(fieldType)
    local fopts = FIELD_OPTS[fieldType] or opts
    local st = relaxState(key)
    if not st.seeded or st.version ~= RELAX_VERSION then
        st.dirty = {}
        Runtime.relaxSeed(fieldType)
        -- Settle a slice before handing the field out, so the first read after a load
        -- sees sources rather than an empty table; the rest drains on the tick budget.
        Runtime.relaxStep(fieldType, 512, fopts)
    end
    return Universe.sourceVersionedFields[key] or {}, false
end

-- Drain ONE field's source-change queue. `budget` is that field's own
-- share of the per-tick work, so a slow field (resource) cannot starve a
-- decision-critical one (base/enemy). Collapses queued changes for a source into
-- one old -> final transition, then runs one multi-source increase propagation.
function Runtime.process(fieldType, budget, opts, workLimit)
    if not Initialized then return 0 end
    return Runtime.relaxStep(fieldType, budget, opts or FIELD_OPTS[fieldType])
end

-- Work left in a field's propagation frontier (0 = converged). A non-zero
-- frontier means the field is still partial, which reads can observe.
local FLUSH_QUEUE_DEPTH = 64
local FRONTIER_FLUSH_DEPTH = 512
local FRONTIER_FLUSH_WORK = 512

-- Fractional per-field budgets: accumulate and run when a whole unit is due.
-- This is the knob that puts the resource field on its own cadence without
-- touching the decision-critical base/enemy fields.
function Runtime.processScheduled(fieldType, opts)
    if not Initialized then return 0 end
    -- Player and kamikaze are deposited every tick (the player genererator is
    -- written by processPlayers), so they were re-propagated every tick. Their
    -- readers decide on a ~600-tick cadence, so that precision buys nothing and
    -- makes those two fields the most expensive thing in the mod: measured
    -- 0.221ms and 0.204ms average with a 0.43ms p90, while every other field
    -- cost <=0.06ms. Batch them on a cadence, draining the accumulated changes
    -- in one go; the source queue already coalesces per chunk, so this only
    -- reduces how often propagation runs.
    local cadence = FIELD_CADENCE[fieldType]
    if cadence and (game.tick % cadence) ~= 0 then
        return 0
    end
    local depth = Runtime.queueDepth(fieldType)
    if depth >= FLUSH_QUEUE_DEPTH then
        FIELD_DRAIN_ACC[fieldType] = 0
        return Runtime.process(fieldType, depth, opts)
    end
    local perTick = (FIELD_DRAIN_PER_TICK and FIELD_DRAIN_PER_TICK[fieldType] or DRAIN_PER_TICK) * (cadence or 1)
    local acc = (FIELD_DRAIN_ACC[fieldType] or 0) + perTick
    local whole = math.floor(acc)
    FIELD_DRAIN_ACC[fieldType] = acc - whole
    if whole < 1 then return 0 end
    return Runtime.process(fieldType, whole, opts)
end

-- Uniform field scheduling.
--
-- Every field used to drain on its own budget, so the per-tick total was the
-- sum of seven independent budgets (~7 x 32 pops) and activity in one field
-- could spike the tick. Here one shared quantum is spent per tick, in rotating
-- priority order, and whatever a field cannot use flows to the next one. That
-- makes the per-tick cost flat: work expands to the quantum when there is
-- backlog and collapses to near zero when the map is quiet, and no single
-- field can starve or spike the frame.
-- Measured: sourceFields cost ~0.95ms/tick at 320 pops, i.e. ~3.0us per pop,
-- and the quantum was being spent in full every tick on a live map. 96 pops
-- keeps the same flat shape at roughly a third of the cost; the backlog is
-- simply drained a little later.
local GLOBAL_FIELD_WORK_PER_TICK = 96
local FIELD_QUANTUM = 32
-- Rotated so the head of the list changes every tick; death first because
-- retreat reads it during combat, resource last because it only feeds the
-- migration decision.
local FIELD_ROTATION = {
    FIELD_DEATH_LOSS, FIELD_DEATH_GAIN,
    FIELD_PLAYER, FIELD_KAMIKAZE,
    FIELD_BASE, FIELD_ENEMY, FIELD_RESOURCE,
}
Runtime.lastFieldWork = {}
-- Flatness accounting: work spent per tick, so the report can show whether the
-- load is uniform (avg close to max) rather than bursty.
Runtime.workStats = { sum = 0, n = 0, max = 0 }

function Runtime.processAll(tick)
    if not Initialized then return 0 end
    local budget = GLOBAL_FIELD_WORK_PER_TICK
    local n = #FIELD_ROTATION
    local start = (tick % n) + 1
    local total = 0
    for i = 0, n - 1 do
        if budget <= 0 then break end
        local fType = FIELD_ROTATION[((start + i - 1) % n) + 1]
        local slice = budget < FIELD_QUANTUM and budget or FIELD_QUANTUM
        local used = Runtime.relaxStep(fType, slice, FIELD_OPTS[fType])
        Runtime.relaxAudit(fType, tick)
        Runtime.lastFieldWork[fType] = used
        budget = budget - used
        total = total + used
    end
    for i = 0, n - 1 do
        local fType = FIELD_ROTATION[(start + i - 1) % n + 1]
        if Runtime.lastFieldWork[fType] == nil then Runtime.lastFieldWork[fType] = 0 end
    end
    local stats = Runtime.workStats
    stats.sum = stats.sum + total
    stats.n = stats.n + 1
    if total > stats.max then stats.max = total end
    return total
end

-- Neighbour ids, computed on demand into a rotation of scratch arrays: eight
-- additions on the chunk's own key (one chunk in x is +2^27, one in y is +1),
-- no allocation, no cache, nothing derived to keep in storage. Multiplayer-safe
-- by construction: the list is a pure function of the stored chunk records.
--
-- The rotation (not a single array) keeps a nested call - a read that triggers a
-- solve - from clobbering the caller's list.
local CHUNK_KEY_X_STEP = MapUtils.CHUNK_KEY_X_STEP
local EMPTY_IDS = {}
-- One scratch array: the solver consumes the list immediately and never calls
-- back into neighborOf while iterating it.
local NEIGHBOR_SCRATCH = {}

function Runtime.neighborOf(chunkId)
    local chunk = Universe.chunkById[chunkId]
    if not chunk then return EMPTY_IDS end
    local map = chunk.map
    local chunks = map and map.chunks
    if not chunks then return EMPTY_IDS end
    local key = chunk.key
    if not key then
        -- Records are key-only now; this covers a hand-built chunk (tests) or one
        -- written by an older build before migrateChunkRecords ran.
        key = MapUtils.chunkKey(chunk.x or 0, chunk.y or 0)
    end
    local ids = NEIGHBOR_SCRATCH
    local n = 0
    local left = key - CHUNK_KEY_X_STEP
    local right = key + CHUNK_KEY_X_STEP
    local neighbor = chunks[left - 1]
    if neighbor then n = n + 1; ids[n] = neighbor.id end
    neighbor = chunks[left]
    if neighbor then n = n + 1; ids[n] = neighbor.id end
    neighbor = chunks[left + 1]
    if neighbor then n = n + 1; ids[n] = neighbor.id end
    neighbor = chunks[key - 1]
    if neighbor then n = n + 1; ids[n] = neighbor.id end
    neighbor = chunks[key + 1]
    if neighbor then n = n + 1; ids[n] = neighbor.id end
    neighbor = chunks[right - 1]
    if neighbor then n = n + 1; ids[n] = neighbor.id end
    neighbor = chunks[right]
    if neighbor then n = n + 1; ids[n] = neighbor.id end
    neighbor = chunks[right + 1]
    if neighbor then n = n + 1; ids[n] = neighbor.id end
    -- Return the count: callers iterate 1..n, so the 8-slot tail-clear and the
    -- ipairs() iterator setup both go away (8 neighbour lookups per pop, and pops
    -- are the dominant cost at ~13us each).
    return ids, n
end

--- One relaxation pop: recompute a chunk from its own source and its neighbours.
--- Distance is carried alongside the value so maxRadius still applies; the result
--- REPLACES the stored value (never max with it) or a settled value never falls.
-- Reusable entry tables for buildSources: the oracle rebuilds the source list on
-- every comparison, which allocated one table per source before.
local SourceScratch = {}

relaxNode = function(fieldType, key, id, opts)
    local field = Universe.sourceVersionedFields[key]
    local st = relaxState(key)
    local chunk = Universe.chunkById and Universe.chunkById[id]
    local best = chunk and Runtime.sourceValue(chunk, fieldType) or 0
    local bestDist = 0
    local decay = opts.decay or Solver.DEFAULT_DECAY
    local maxRadius = opts.maxRadius
    local dist = st.dist
    local nbrs, nbrCount = Runtime.neighborOf(id)
    if not nbrCount then nbrCount = #nbrs end
    for k = 1, nbrCount do
        local nid = nbrs[k]
        local v = field[nid]
        if v and v > 0 then
            local d = (dist[nid] or 0) + 1
            if not maxRadius or d <= maxRadius then
                local cand = v * decay
                if cand > best then best = cand; bestDist = d end
            end
        end
    end
    local old = field[id]
    local eps = opts.eps or Solver.DEFAULT_EPS
    local changed = false
    if best > eps then
        if not old or best > old + eps or best < old - eps then
            field[id] = best
            dist[id] = bestDist
            changed = true
        elseif dist[id] ~= bestDist then
            -- Same value, different hop count: the distance still feeds the radius
            -- check for this node's neighbours.
            dist[id] = bestDist
        end
    elseif old then
        if old > eps then
            field[id] = nil
            dist[id] = nil
            changed = true
        end
    end
    if changed then
        local dirty = st.dirty
        for k = 1, nbrCount do
            local nid = nbrs[k]
            if not dirty[nid] then
                dirty[nid] = true
                st.tail = st.tail + 1
                st.queue[st.tail] = nid
            end
        end
    end
    return changed
end

--- Push a chunk onto a relaxed field's frontier (membership-guarded).
relaxState = function(key)
    local st = Universe.sourceVersionedRelax[key]
    if not st then
        st = { queue = {}, head = 1, tail = 0, dirty = {}, dist = {}, seeded = false }
        Universe.sourceVersionedRelax[key] = st
    end
    return st
end

relaxPush = function(key, id)
    local st = relaxState(key)
    local dirty = st.dirty
    if dirty[id] then return end
    dirty[id] = true
    st.tail = st.tail + 1
    st.queue[st.tail] = id
end

--- Seed a relaxed field from scratch: every source is dirty, and the frontier is
--- drained by process() over the following ticks.
function Runtime.relaxSeed(fieldType)
    local key = Runtime.fieldKey(fieldType)
    local st = relaxState(key)
    st.queue = {}; st.head = 1; st.tail = 0; st.dirty = {}; st.dist = {}
    Universe.sourceVersionedFields[key] = {}
    st.seeded = true
    st.version = RELAX_VERSION
    for _, chunk in pairs(Universe.chunkById or {}) do
        if Runtime.sourceValue(chunk, fieldType) > 0 then
            relaxPush(key, chunk.id)
        end
    end
    return st.tail
end

--- Drain up to `budget` nodes of a relaxed field's frontier.
function Runtime.relaxStep(fieldType, budget, opts)
    local key = Runtime.fieldKey(fieldType)
    local st = relaxState(key)
    local popped = 0
    opts = opts or FIELD_OPTS[fieldType]
    budget = budget or RELAX_POPS_PER_TICK
    local field = Universe.sourceVersionedFields[key]
    while popped < budget and st.head <= st.tail do
        local id = st.queue[st.head]
        st.queue[st.head] = nil
        st.head = st.head + 1
        st.dirty[id] = nil
        if field[id] or (Universe.chunkById and Universe.chunkById[id]) then
            relaxNode(fieldType, key, id, opts)
        end
        popped = popped + 1
    end
    if st.head > st.tail then
        st.head = 1
        st.tail = 0
        st.queue = {}
    end
    return popped
end

--- Test/tuning hook: switch a field between relaxation and the ordered solver at
--- runtime. Used by the unit tests that pin the ordered path's removal behaviour.
function Runtime.setRelax(fieldType, enabled)
    local opts = FIELD_OPTS[fieldType]
    if not opts then return false end
    opts.relax = enabled or nil
    Universe.sourceVersionedRelax[Runtime.fieldKey(fieldType)] = nil
    return true
end

--- Self-healing invariant. No entry can exceed `largestSource * decay^dist`, because
--- a value is either a source (distance 0) or a neighbour's value decayed one hop. That
--- bound catches ghosts BELOW the field maximum too - a chunk holding 18 structures'
--- worth of pheromone when the exact solve says 1 (measured: live 9,000 vs expected 500
--- at one chunk, maxDelta 43% of peak on the enemy field).
---
--- Checking it costs one pass over the field plus one pass over the sources, so it runs
--- on a cadence. Violations are counted (Metrics "field.auditGhost") and repaired by a
--- reseed.
local AUDIT_CHECK_TICKS = 600

function Runtime.relaxAudit(fieldType, tick)
    local key = Runtime.fieldKey(fieldType)
    local st = Universe.sourceVersionedRelax[key]
    if not st or not st.seeded then return false end
    local last = st.auditTick or 0
    if tick - last < AUDIT_CHECK_TICKS then return false end
    st.auditTick = tick

    local opts = FIELD_OPTS[fieldType]
    local decay = (opts and opts.decay) or Solver.DEFAULT_DECAY
    local eps = (opts and opts.eps) or Solver.DEFAULT_EPS
    local maxSource = 0
    for _, chunk in pairs(Universe.chunkById or {}) do
        local v = Runtime.sourceValue(chunk, fieldType)
        if v > maxSource then maxSource = v end
    end
    local field = Universe.sourceVersionedFields[key] or {}
    local dist = st.dist
    local ghosts = 0
    for id, v in pairs(field) do
        local d = dist[id] or 0
        local bound = maxSource * (decay ^ d)
        if v > bound + eps then
            ghosts = ghosts + 1
            if ghosts > 8 then break end
        end
    end
    if ghosts > 0 then
        if MetricsG then MetricsG.inc("field.auditGhost", ghosts) end
        Universe.sourceVersionedFields[key] = nil
        st.seeded = false
        Runtime.ensureSolved(fieldType, opts)
        return true
    end
    return false
end

function Runtime.relaxDepth(fieldType)
    local st = Universe.sourceVersionedRelax[Runtime.fieldKey(fieldType)]
    if not st then return 0 end
    return st.tail - st.head + 1
end

--- Diagnostic: the largest entry in a field and whether its chunk still exists and
--- still supports that value.
function Runtime.fieldMax(fieldType)
    local key = Runtime.fieldKey(fieldType)
    local live = Universe.sourceVersionedFields[key] or {}
    local bestId, bestV = nil, 0
    for id, v in pairs(live) do
        if v > bestV then bestId, bestV = id, v end
    end
    if not bestId then return "empty" end
    local chunk = Universe.chunkById and Universe.chunkById[bestId]
    local own = chunk and Runtime.sourceValue(chunk, fieldType) or 0
    local st = Universe.sourceVersionedRelax and Universe.sourceVersionedRelax[key]
    return string.format("id=%d live=%.3f ownSource=%.3f inChunkById=%s dist=%s",
        bestId, bestV, own, tostring(chunk ~= nil),
        tostring(st and st.dist and st.dist[bestId]))
end

function Runtime.sourceValue(chunk, fieldType)
    if not chunk then return 0 end
    if fieldType == FIELD_DEATH_GAIN or fieldType == FIELD_DEATH_LOSS then
        local combined = ChunkPropertyUtils.getCombinedDeathGenerator(chunk)
        local magnitude = (fieldType == FIELD_DEATH_GAIN) and combined or -combined
        if magnitude <= 0 then return 0 end
        return magnitude * DEATH_SCALE
    elseif fieldType == FIELD_KAMIKAZE then
        return (chunk.playerGenerator or 0) * KAMIKAZE_SCALE
    elseif fieldType == FIELD_PLAYER then
        return chunk.playerGenerator or 0
    elseif fieldType == FIELD_BASE then
        return chunk.playerBaseGenerator or 0
    elseif fieldType == FIELD_ENEMY then
        return ChunkPropertyUtils.getEnemyStructureCount(chunk) * ENEMY_MULTIPLIER
    elseif fieldType == FIELD_RESOURCE then
        local generator = chunk.resourceGenerator or 0
        if generator > 0 then
            return MathUtils.linearInterpolation(generator, 15000, 20000)
        end
        return 0
    end
    return 0
end

function Runtime.buildSources(fieldType)
    local sources = {}
    for _, chunk in pairs(Universe.chunkById or {}) do
        local value = Runtime.sourceValue(chunk, fieldType)
        if value > 0 then
            local n = #sources + 1
            local entry = SourceScratch[fieldType] and SourceScratch[fieldType][n]
            if entry then
                entry.id = chunk.id
                entry.gen = value
                sources[n] = entry
            else
                sources[n] = { id = chunk.id, gen = value }
            end
        end
    end
    SourceScratch[fieldType] = sources
    return sources
end

local REMOVAL_SOURCE = { id = 0, gen = 0 }
local REMOVAL_OPTS = { maxWork = 0 }

--- Pool statistics, for tests and telemetry: how many queue items exist, how
--- many are idle, and how many were ever created. A healthy steady state keeps
--- `created` flat while `free` grows to the working set.
function Runtime.buildState(fieldType)
    local key = Runtime.fieldKey(fieldType)
    local st = Universe.sourceVersionedRelax and Universe.sourceVersionedRelax[key]
    return string.format("relax=%s seeded=%s frontier=%d chunks=%d fieldEntries=%d tick=%d",
        tostring(st ~= nil), tostring(st and st.seeded), Runtime.relaxDepth(fieldType),
        (function() local n = 0 for _ in pairs(Universe.chunkById or {}) do n = n + 1 end return n end)(),
        (function() local n = 0 for _ in pairs(Universe.sourceVersionedFields[key] or {}) do n = n + 1 end return n end)(),
        game.tick)
end

--- Explain one chunk's value: live, exact, and where each came from.
function Runtime.fieldDiag(fieldType, chunkId)
    local key = Runtime.fieldKey(fieldType)
    local opts = fieldOpts(fieldType, nil)
    local live = (Universe.sourceVersionedFields[key] or {})[chunkId] or 0
    local prov = FieldProvenance[key] or {}
    local sec = FieldSecond[key] or {}
    local expected = Solver.solveWithMetadata(
        Runtime.buildSources(fieldType), Runtime.neighborOf, opts)
    local chunk = MapUtils.getChunk(chunkId)
    local topId = Solver.provTop(prov, chunkId)
    local secondId = Solver.provSecond(prov, chunkId)
    local function describe(id)
        if not id then return "none" end
        local c = MapUtils.getChunk(id)
        return string.format("id=%d gen=%.2f key=%.0f", id,
            (c and Runtime.sourceValue(c, fieldType)) or 0, (c and c.key) or 0)
    end
    return string.format(
        "id=%d key=%.0f live=%.3f exp=%.3f decay=%.3f eps=%.4f radius=%s top={%s} second={%s} ownGen=%.3f",
        chunkId, (chunk and chunk.key) or 0, live, expected[chunkId] or 0,
        opts.decay or -1, opts.eps or -1, tostring(opts.maxRadius),
        describe(topId), describe(secondId),
        (chunk and Runtime.sourceValue(chunk, fieldType)) or 0)
end

--- Solve the field from scratch in ONE call. A diagnostic: the incremental build
--- (SOURCE_BUILD_PER_TICK) plus 400 ensureSolved calls is heavy enough to stall the
--- game for many seconds, so this assigns the exact solve directly and lets the
--- caller compare from a known-exact state.
function Runtime.rebuildField(fieldType)
    local key = Runtime.fieldKey(fieldType)
    Universe.sourceVersionedRelax[key] = nil
    local opts = fieldOpts(fieldType, nil)
    local sources = Runtime.buildSources(fieldType)
    local field, prov, second = Solver.solveWithMetadata(sources, Runtime.neighborOf, opts)
    Universe.sourceVersionedFields[key] = field
    FieldProvenance[key] = prov
    FieldSecond[key] = second
    SolvedFields[key] = true
    SourceBuilds[key] = nil
    local solvedAt = Universe.sourceVersionedSolvedAt
    if not solvedAt then
        solvedAt = {}
        Universe.sourceVersionedSolvedAt = solvedAt
    end
    solvedAt[key] = game.tick
    local counts = Universe.sourceVersionedSourceCount
    if not counts then
        counts = {}
        Universe.sourceVersionedSourceCount = counts
    end
    counts[key] = #sources
    return #sources
end

--- Sample chunks whose live value differs from a fresh exact solve, with the
--- state of the source that feeds that field. verifyField counts; this says why.
function Runtime.verifySamples(fieldType, sampleCount)
    local key = Runtime.fieldKey(fieldType)
    Runtime.ensureSolved(fieldType, fieldOpts(fieldType, nil))
    local guard = 0
    while Runtime.relaxDepth(fieldType) > 0 and guard < 4000 do
        guard = guard + 1
        Runtime.relaxStep(fieldType, 4096, fieldOpts(fieldType, nil))
    end
    local live = Universe.sourceVersionedFields[key] or {}
    local expected = Solver.solveWithMetadata(
        Runtime.buildSources(fieldType), Runtime.neighborOf, fieldOpts(fieldType, nil))
    local out, shown = {}, 0
    sampleCount = sampleCount or 6
    for id, expValue in pairs(expected) do
        local liveValue = live[id] or 0
        if math.abs(liveValue - expValue) > 0.001 then
            local chunk = MapUtils.getChunk(id)
            out[#out + 1] = string.format(
                "id=%d live=%.3f exp=%.3f gen=%.4f base=%s nest=%d", id, liveValue, expValue,
                (chunk and chunk.playerBaseGenerator) or 0,
                tostring(chunk and chunk.base ~= nil), (chunk and chunk.nestCount) or 0)
            shown = shown + 1
            if shown >= sampleCount then break end
        end
    end
    for id, liveValue in pairs(live) do
        if expected[id] == nil and shown < sampleCount then
            local chunk = MapUtils.getChunk(id)
            out[#out + 1] = string.format(
                "id=%d live=%.3f exp=none gen=%.4f base=%s", id, liveValue,
                (chunk and chunk.playerBaseGenerator) or 0,
                tostring(chunk and chunk.base ~= nil))
            shown = shown + 1
        end
    end
    return table.concat(out, " | ")
end

function Runtime.verifyField(fieldType, tolerance)
    local key = Runtime.fieldKey(fieldType)
    local fopts = fieldOpts(fieldType, nil)
    -- Drain the frontier to quiescence first: the comparison is only meaningful on a
    -- settled field, and the oracle is allowed to spend its own budget.
    Runtime.ensureSolved(fieldType, fopts)
    local iterations = 0
    while Runtime.relaxDepth(fieldType) > 0 and iterations < 4000 do
        iterations = iterations + 1
        Runtime.relaxStep(fieldType, 4096, fopts)
    end
    local live = Universe.sourceVersionedFields[key] or {}
    local expected = Solver.solveWithMetadata(Runtime.buildSources(fieldType),
        Runtime.neighborOf, fopts)
    tolerance = tolerance or 0.10
    local checked, mismatches, extra, maxDelta, maxRel, worstId = 0, 0, 0, 0, 0, nil
    local livePeak, expectedPeak, sampleId, sampleLive, sampleExpected = 0, 0, nil, 0, 0
    for _, v in pairs(live) do if v > livePeak then livePeak = v end end
    for _, v in pairs(expected) do if v > expectedPeak then expectedPeak = v end end
    for id, v in pairs(expected) do
        checked = checked + 1
        local liveV = live[id] or 0
        local d = liveV - v
        if d < 0 then d = -d end
        if d > maxDelta then maxDelta, worstId = d, id end
        local rel = v > 0 and (d / v) or (liveV > 0 and 1 or 0)
        if rel > maxRel then maxRel = rel end
        if rel > tolerance then
            mismatches = mismatches + 1
            if not sampleId then
                sampleId, sampleLive, sampleExpected = id, liveV, v
            end
        end
    end
    local eps = fopts and (fopts.eps or Solver.DEFAULT_EPS) or Solver.DEFAULT_EPS
    for id, liveV in pairs(live) do
        if not expected[id] and liveV > eps then
            extra = extra + 1
            if not sampleId then sampleId, sampleLive, sampleExpected = id, liveV, 0 end
        end
    end
    -- Diagnostic: entries whose carried distance exceeds the cap can only exist if the
    -- distance bookkeeping is wrong; entries the exact solve lacks but inside the cap
    -- are residuals.
    local overRadius, noDist = 0, 0
    local stR = Universe.sourceVersionedRelax[key]
    local maxR = fopts and fopts.maxRadius
    if stR then
        for id, v in pairs(live) do
            if v > eps then
                local d = stR.dist[id]
                if not d then noDist = noDist + 1
                elseif maxR and d > maxR then overRadius = overRadius + 1 end
            end
        end
    end
    local sample = ""
    if sampleId then
        sample = string.format("%d live=%.3f expected=%.3f", sampleId, sampleLive, sampleExpected)
    end
    return {
        field = key, sources = 0, checked = checked, mismatches = mismatches,
        liveOnly = extra, maxRel = string.format("%.2f%%", maxRel * 100),
        maxDelta = string.format("%.3f", maxDelta), worstChunk = tostring(worstId),
        livePeak = string.format("%.1f", livePeak),
        expectedPeak = string.format("%.1f", expectedPeak),
        sample = sample, frontier = Runtime.relaxDepth(fieldType), queued = 0,
        iterations = iterations, overRadius = overRadius, noDist = noDist,
    }
end

-- Read hook installed into SquadLogic. Returns the solved source-versioned
-- value for the chunk, or nil when the runtime is not initialized.
function Runtime.readChunk(fieldType, chunk)
    if not Initialized or not chunk or chunk.id == nil then return nil end
    local field = Runtime.ensureSolved(fieldType)
    return field[chunk.id] or 0
end

function Runtime.read(field, chunkId, tick, lastRefresh, decay)
    local value = field[chunkId] or 0
    if value == 0 then return 0 end
    local elapsed = tick - (lastRefresh or 0)
    return value * (decay ^ elapsed)
end

return Runtime
