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

-- Resolve a chunk id through the runtime's own Universe, not MapUtils: the runtime holds
-- the table it was initialised with, and the unit tests drive it without MapUtils.
local function chunkFor(id)
    return Universe.chunkById and Universe.chunkById[id]
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

-- Audit totals per field, since init: how often the sweep found a node the frontier
-- lost (ghosts: value above its justification) or a wave that stopped short (starved),
-- and how many full sweeps it completed. fieldLoad reports them.
Runtime.auditStats = {}
local function auditStat(key)
    local stats = Runtime.auditStats[key]
    if not stats then
        stats = { ghosts = 0, starved = 0, sweeps = 0, orphans = 0 }
        Runtime.auditStats[key] = stats
    end
    return stats
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
    Runtime.workStats = { sum = 0, n = 0, max = 0, byField = {} }
    Runtime.sourceReports = {}
    Runtime.auditStats = {}
    Runtime.lastFieldWork = {}
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
    Runtime.sourceReports[fieldType] = (Runtime.sourceReports[fieldType] or 0) + 1
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
-- Per-field cumulative pops and reported source changes since init. fieldLoad reports
-- them: a deep frontier that keeps receiving reports is a starved field (lag), not a
-- wrong one, and the two look identical in a before/after comparison.
Runtime.sourceReports = {}

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
        local byField = Runtime.workStats.byField
        if not byField then
            byField = {}
            Runtime.workStats.byField = byField
        end
        byField[fType] = (byField[fType] or 0) + used
        Runtime.lastFieldWork[fType] = used
        budget = budget - used
        total = total + used
    end
    for i = 0, n - 1 do
        local fType = FIELD_ROTATION[(start + i - 1) % n + 1]
        if Runtime.lastFieldWork[fType] == nil then Runtime.lastFieldWork[fType] = 0 end
    end
    -- The self-healing sweep runs OUTSIDE the pop budget: inside it, a busy tick spent the
    -- budget on the first fields of the rotation and the rest were never swept. One field
    -- per tick, in rotation, so every field is swept about every 7 x (nodes/128) ticks
    -- (~8 s on a settled map) at a flat ~0.1ms slice instead of one big pass.
    Runtime.relaxAudit(FIELD_ROTATION[((tick % n) + 1)])
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
    local distTable = st.dist
    if not chunk then
        -- No chunk record, no value. A field entry belongs to a chunk, and an id that is
        -- no longer a chunk is never read (readers resolve through chunk.id) and never
        -- revisits its neighbours. Without this rule a pop QUEUED BEFORE the record was
        -- removed recomputes the node from its neighbours and leaves a value behind that
        -- nothing will ever touch again; its cone then decays from it forever, so one
        -- removal fossilises a whole disc. Measured on a settled field: 1,052 entries at
        -- ids that are not chunks, and a rebuild (which only visits real chunks) removed
        -- every one of them.
        local old = field[id]
        if old then
            field[id] = nil
            distTable[id] = nil
            -- This cannot reach the neighbours: neighborOf resolves through the chunk
            -- record, which is exactly what is missing. dropChunk() does the dirtying at
            -- removal time, while the record is still resolvable; this branch is the
            -- safety net for an id that slipped through.
            local nbrs, nbrCount = Runtime.neighborOf(id)
            if not nbrCount then nbrCount = #nbrs end
            local dirty = st.dirty
            for k = 1, nbrCount do
                local nid = nbrs[k]
                if not dirty[nid] then
                    dirty[nid] = true
                    st.tail = st.tail + 1
                    st.queue[st.tail] = nid
                end
            end
            return true
        end
        return false
    end
    local best = Runtime.sourceValue(chunk, fieldType)
    local bestDist = 0
    local nearDist = 0
    local decay = opts.decay or Solver.DEFAULT_DECAY
    local maxRadius = opts.maxRadius
    local dist = st.dist
    local eps = opts.eps or Solver.DEFAULT_EPS
    local nbrs, nbrCount = Runtime.neighborOf(id)
    if not nbrCount then nbrCount = #nbrs end
    for k = 1, nbrCount do
        local nid = nbrs[k]
        local v = field[nid]
        if v and v > 0 then
            local d = (dist[nid] or 0) + 1
            if not maxRadius or d <= maxRadius then
                local cand = v * decay
                if cand > best or (cand == best and d < bestDist) then
                    best = cand
                    bestDist = d
                end
                -- The distance the radius cap is enforced against. Two paths whose values
                -- are within eps justify this node equally well, so take the SHORTER one:
                -- the value is what matters and the reach it buys is otherwise decided by
                -- which path happened to arrive first. The ordered solver pops in value
                -- order and so keeps the short one, which is why the two disagreed at the
                -- cap boundary (measured: 95 base chunks the exact solve fills at distance
                -- 28 and the relaxation truncated, 1.7% of peak).
                if cand >= best - eps and (nearDist == 0 or d < nearDist) then
                    nearDist = d
                end
            end
        end
    end
    if nearDist > 0 and nearDist < bestDist then bestDist = nearDist end
    local old = field[id]
    local changed = false
    if best > eps then
        if not old or best > old + eps or best < old - eps then
            field[id] = best
            dist[id] = bestDist
            changed = true
        elseif dist[id] ~= bestDist then
            -- The value did not move beyond eps but its justification did. The distance
            -- feeds the radius check for this node's neighbours, so it has to track the
            -- CURRENT best path in both directions: keeping only shorter paths leaves a
            -- too-small distance behind (measured: 421 resource chunks holding values
            -- beyond the exact solve's reach, live up to 15,438 where 5,243 was justified,
            -- carried distance 1 against a true distance of 11).
            dist[id] = bestDist
            changed = true
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

--- Remove a chunk from every relaxed field, in the right order: drop its entries and
--- dirty its neighbours while the record is STILL resolvable. A pop for an already removed
--- id cannot find its neighbours (neighborOf resolves through the record), so doing this at
--- pop time dropped the value and left its whole cone standing (measured on a real save:
--- 1,052 field entries at ids that are not chunks, and a rebuild - which only visits real
--- chunks - removed every one of them).
--- MapUtils.removeChunkFromMap calls this before it clears the record.
--- Clear every entry within `maxRadius` hops of a chunk and re-queue them, so the wave
--- refills the disc from the sources that are still there.
---
--- This is what makes a REMOVAL cheap. A fossil disc (the cone of a vanished source) is a
--- ring of mutually supporting values: each node is 0.9 of its neighbour, so a point re-pop
--- lowers the disc by one decay factor per propagation round and the peak has to be
--- re-populated ten times before the disc reaches the truth. Measured on a real save: a
--- single removed source chunk took the resource field from 73 to ~600 mismatched chunks and
--- healed by ~10% per round, which is minutes of a field reading several times too high.
--- Clearing the disc is one bounded BFS (~1,800 nodes at radius 24, ~3,200 at 32) and the
--- refill then runs in one pass from the boundary.
local function clearDisc(fieldType, centerId, opts)
    local maxRadius = opts.maxRadius
    if not maxRadius then return 0 end
    local key = Runtime.fieldKey(fieldType)
    local field = Universe.sourceVersionedFields[key]
    local st = Universe.sourceVersionedRelax[key]
    if not field or not st then return 0 end
    local dist = st.dist
    local front, tail = { centerId }, 1
    local seen = { [centerId] = 0 }
    local head, cleared = 1, 0
    while head <= tail do
        local id = front[head]
        head = head + 1
        local d = seen[id]
        if field[id] then
            field[id] = nil
            dist[id] = nil
            relaxPush(key, id)
            cleared = cleared + 1
        end
        if d < maxRadius then
            local nbrs, nbrCount = Runtime.neighborOf(id)
            if not nbrCount then nbrCount = #nbrs end
            for k = 1, nbrCount do
                local nid = nbrs[k]
                if seen[nid] == nil then
                    seen[nid] = d + 1
                    tail = tail + 1
                    front[tail] = nid
                end
            end
        end
    end
    return cleared
end

-- A removed chunk takes a disc with it, but a mass removal (an idle surface sweep of a
-- thousand chunks) would clear a thousand overlapping discs in one tick. The first ones
-- have already cleared most of the area, so after this many the single-node drop is
-- equivalent: it is the same disc, already empty.
local DISC_CLEARS_PER_TICK = 3
Runtime.dropStats = { discs = 0, clears = 0 }

function Runtime.dropChunk(chunk)
    if not Initialized or not chunk or not chunk.id then return false end
    local id = chunk.id
    local nbrs, nbrCount = Runtime.neighborOf(id)
    if not nbrCount then nbrCount = #nbrs end
    local tick = game and game.tick or 0
    local canClear = (Runtime.dropStats.tick ~= tick) or (Runtime.dropStats.discs < DISC_CLEARS_PER_TICK)
    if canClear then
        Runtime.dropStats.tick = tick
        Runtime.dropStats.discs = Runtime.dropStats.discs + 1
    end
    for fieldType, opts in pairs(FIELD_OPTS) do
        if opts.relax then
            local key = Runtime.fieldKey(fieldType)
            local field = Universe.sourceVersionedFields[key]
            local st = Universe.sourceVersionedRelax[key]
            if field and st then
                if canClear and (field[id] or 0) > 1e-9 then
                    local cleared = clearDisc(fieldType, id, opts)
                    if cleared > 0 then
                        Runtime.dropStats.clears = Runtime.dropStats.clears + cleared
                        if MetricsG then MetricsG.inc("field.dropDisc") end
                    end
                    field[id] = nil
                    if st.dist then st.dist[id] = nil end
                else
                    field[id] = nil
                    if st.dist then st.dist[id] = nil end
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
            end
        end
    end
    return nbrCount
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

--- Self-healing sweep. Every stored value must equal `max(ownSource, bestNeighbour *
--- decay)` - that is what relaxNode writes, so any node that disagrees with its own
--- recompute is one the frontier lost. Two shapes matter, and both were measured on a
--- real save after the fields looked settled (frontier 0, drained comparison still wrong):
---
---   ghost     value ABOVE its justification with no own source: a local peak holding
---             15,438 with neighbours at 13,894 and no source at all. A value like that is
---             only writable while a neighbour is high, so it means the node was never
---             re-popped when the support fell away.
---   starved   value BELOW its justification: a wave that stopped short.
---
--- The repair is a re-pop of the offending node, not a reseed: it clamps the node to what
--- its neighbours justify and dirties them, so the correction spreads exactly as far as it
--- needs to. A reseed would throw away a whole field (8,850 pops) to fix a handful of nodes.
---
--- The sweep is spread across ticks with a resumable cursor (AUDIT_NODES_PER_TICK), so it
--- costs a flat slice instead of one 1-2ms pass per cadence. It only sweeps a QUESCENT
--- field: mid-wave, a stored value legitimately lags its neighbours and every node would
--- look out of sync.
local AUDIT_NODES_PER_TICK = 64

function Runtime.relaxAudit(fieldType)
    local key = Runtime.fieldKey(fieldType)
    local st = Universe.sourceVersionedRelax[key]
    if not st or not st.seeded then return false end
    if Runtime.relaxDepth(fieldType) > 0 then return false end
    local field = Universe.sourceVersionedFields[key]
    local chunks = Universe.chunkById
    if not field or not chunks then return false end

    local opts = FIELD_OPTS[fieldType]
    local eps = (opts and opts.eps) or Solver.DEFAULT_EPS
    local dist = st.dist
    local checked, repaired = 0, 0
    -- Walk the CHUNK records, not the field: a value the frontier lost is usually a value
    -- that is still there (a ghost), but the same loss also leaves a chunk with NO value
    -- while its neighbours justify one (measured: 95 base chunks the exact solve fills at
    -- distance 28 with the live field empty). Walking the field can never see those.
    -- A whole field (10,000 chunks) is swept in ~156 ticks of this field's rotation, so
    -- every field is swept about every 18 s. The sweep is a backstop: the event path is
    -- what keeps the field right, and this only has to catch what it loses.
    -- Odd passes clean ORPHANS out of the field (ids with no chunk record), even passes
    -- check chunk records for lost values. Two cursors, so each pass stays a flat slice.
    local passField = (st.auditPass or 0) == 1
    st.auditPass = passField and 0 or 1
    if passField then
        local orphans = 0
        local fieldId = st.orphanCursor
        local scanned = 0
        while scanned < AUDIT_NODES_PER_TICK do
            local nextId = nextFrom(field, fieldId)
            if nextId == nil then
                fieldId = nil -- a finished sweep restarts from the beginning
                break
            end
            fieldId = nextId
            scanned = scanned + 1
            if chunkFor(fieldId) == nil then
                -- Leave the value: relaxNode drops it when the frontier reaches it, and
                -- pushing the id is what makes that happen.
                orphans = orphans + 1
                relaxPush(key, fieldId)
            end
        end
        st.orphanCursor = fieldId
        if orphans > 0 then
            local statsO = auditStat(key)
            statsO.orphans = (statsO.orphans or 0) + orphans
            if MetricsG then MetricsG.inc("field.auditOrphan", orphans) end
        end
        return orphans > 0
    end
    local id = st.auditCursor
    local stats = auditStat(key)
    while checked < AUDIT_NODES_PER_TICK do
        local nextId = nextFrom(chunks, id)
        if nextId == nil then
            if id ~= nil then stats.sweeps = stats.sweeps + 1 end
            st.auditCursor = nil
            break
        end
        id = nextId
        checked = checked + 1
        local value = field[id] or 0
        -- Same recompute relaxNode does, without writing: what SHOULD this node hold?
        local chunk = chunkFor(id)
        local best = chunk and Runtime.sourceValue(chunk, fieldType) or 0
        local bestDist = 0
        local decay = (opts and opts.decay) or Solver.DEFAULT_DECAY
        local maxRadius = opts and opts.maxRadius
        local nbrs, nbrCount = Runtime.neighborOf(id)
        if not nbrCount then nbrCount = #nbrs end
        for k = 1, nbrCount do
            local nid = nbrs[k]
            local v = field[nid]
            if v and v > 0 then
                local d = (dist[nid] or 0) + 1
                if not maxRadius or d <= maxRadius then
                    local cand = v * decay
                    if cand > best or (cand == best and d < bestDist) then
                        best = cand
                        bestDist = d
                    end
                end
            end
        end
        local delta = value - best
        if delta < 0 then delta = -delta end
        if delta > eps or (best > eps and dist[id] ~= bestDist) then
            repaired = repaired + 1
            if value > best + eps then
                stats.ghosts = stats.ghosts + 1
            else
                stats.starved = stats.starved + 1
            end
            relaxPush(key, id)
        end
    end
    st.auditCursor = id
    if repaired > 0 and MetricsG then MetricsG.inc("field.auditRepair", repaired) end
    return repaired > 0
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

--- One local recompute, using the same rule relaxNode uses: what should this chunk's
--- value be, given its own source and its neighbours' current values? Returns the stored
--- value, the own source, the full best (the local recompute), and BOTH parents: the
--- neighbour with the highest candidate value, and the neighbour that is one step CLOSER
--- to a source.
---
--- The descent parent has to prefer the nearer one. With a value that its neighbours do
--- not support, the highest-candidate neighbour is the node we came from, and following it
--- walks a cycle (measured: chunk 7 poked to 999 walked 7 <-> 6 until maxHops).
local function chainStep(fieldType, field, dist, id, opts)
    local chunk = chunkFor(id)
    local own = chunk and Runtime.sourceValue(chunk, fieldType) or 0
    local value = field[id] or 0
    local best = own
    local bestId = nil
    local downId = nil
    local decay = opts.decay or Solver.DEFAULT_DECAY
    local maxRadius = opts.maxRadius
    local myDist = dist[id]
    local downBest = -1
    local nbrs, count = Runtime.neighborOf(id)
    if not count then count = #nbrs end
    for k = 1, count do
        local nid = nbrs[k]
        local nv = field[nid]
        if nv and nv > 0 then
            local nDist = dist[nid]
            local d = (nDist or 0) + 1
            if not maxRadius or d <= maxRadius then
                local cand = nv * decay
                if cand > best then best = cand; bestId = nid end
                if (myDist == nil or (nDist or 0) < myDist) and cand > downBest then
                    downBest = cand
                    downId = nid
                end
            end
        end
    end
    return value, own, best, bestId, downId
end

--- Follow the live support chain from one chunk. Returns the hops (for printing), the
--- root chunk id, the verdict for the STARTING node (`consistent`, `aboveSupport`,
--- `belowSupport`, `unsupported`, or `source` when it carries its own source), and how the
--- walk ended (`source`, `unsupported`, `cycle`, `maxHops`).
---
--- Two separate answers, because they fail differently: the verdict says whether this
--- chunk's value is locally justified, and the root says which source the chain reaches.
--- A value can be locally consistent all the way down and still be wrong, if the chain
--- grounds in a source that has since decreased.
local function walkChain(fieldType, field, dist, chunkId, opts, maxHops)
    local eps = opts.eps or Solver.DEFAULT_EPS
    local hops = {}
    local seen = {}
    local id = chunkId
    local startVerdict = nil
    local terminal = "maxHops"
    for _ = 1, maxHops or 32 do
        local value, own, best, _, downId = chainStep(fieldType, field, dist, id, opts)
        local verdict
        if own > 0 and value <= own + eps then
            verdict = "source" -- carries its own source
        elseif best <= eps and value > eps then
            verdict = "unsupported" -- nothing justifies this value at all
        elseif best > value + eps then
            verdict = "belowSupport" -- live is BELOW what its neighbour justifies
        elseif best < value - eps then
            verdict = "aboveSupport" -- live is ABOVE what its neighbour justifies
        else
            verdict = "consistent"
        end
        hops[#hops + 1] = { id = id, value = value, dist = dist[id], root = verdict == "source" }
        if not startVerdict then startVerdict = verdict end
        if verdict == "source" then terminal = "source"; break end
        if not downId then terminal = "unsupported"; break end
        if seen[downId] then terminal = "cycle"; break end
        seen[id] = true
        id = downId
    end
    return hops, id, startVerdict, terminal
end

--- Render a support chain for one chunk: every hop's id, value and carried distance,
--- then the root and the verdict. This is what a relaxed field can answer and the oracle
--- cannot: whether a value is grounded in a source that is still there, grounded in a
--- source that has SINCE DECREASED, or not grounded at all. Relaxation carries no
--- provenance, so before this the only support evidence was the last exact solve's,
--- which is stale by construction.
function Runtime.liveChain(fieldType, chunkId, maxHops)
    local key = Runtime.fieldKey(fieldType)
    local opts = fieldOpts(fieldType, nil)
    local field = Universe.sourceVersionedFields[key] or {}
    local st = Universe.sourceVersionedRelax[key]
    local hops, root, verdict, terminal = walkChain(fieldType, field, (st and st.dist) or {},
        chunkId, opts, maxHops)
    local parts = {}
    for i = 1, #hops do
        local h = hops[i]
        parts[i] = string.format("%d:%.1f(d%s)%s", h.id, h.value,
            tostring(h.dist or 0), h.root and "*" or "")
    end
    return string.format("chain=%s root=%d verdict=%s grounded=%s",
        table.concat(parts, " <- "), root, verdict, terminal)
end

--- Explain one chunk's value: live, exact, and how the live value is supported.
function Runtime.fieldDiag(fieldType, chunkId)
    local key = Runtime.fieldKey(fieldType)
    local opts = fieldOpts(fieldType, nil)
    local live = (Universe.sourceVersionedFields[key] or {})[chunkId] or 0
    local expected = Solver.solveWithMetadata(
        Runtime.buildSources(fieldType), Runtime.neighborOf, opts)
    local chunk = chunkFor(chunkId)
    local head = string.format(
        "id=%d key=%.0f live=%.3f exp=%.3f ownGen=%.3f decay=%.2f eps=%s radius=%s relax=%s",
        chunkId, (chunk and chunk.key) or 0, live, expected[chunkId] or 0,
        (chunk and Runtime.sourceValue(chunk, fieldType)) or 0,
        opts.decay or Solver.DEFAULT_DECAY, tostring(opts.eps or Solver.DEFAULT_EPS),
        tostring(opts.maxRadius), tostring(opts.relax ~= nil))
    if opts.relax then
        return head .. " | " .. Runtime.liveChain(fieldType, chunkId)
    end
    -- Ordered fields do carry provenance, so report the solver's own answer.
    local prov = FieldProvenance[key] or {}
    local function describe(id)
        if not id then return "none" end
        local c = MapUtils.getChunk(id)
        return string.format("id=%d gen=%.2f key=%.0f", id,
            (c and Runtime.sourceValue(c, fieldType)) or 0, (c and c.key) or 0)
    end
    return head .. string.format(" | top={%s} second={%s}",
        describe(Solver.provTop(prov, chunkId)), describe(Solver.provSecond(prov, chunkId)))
end

--- Dump the live field around one chunk as a grid: for each cell, `live/dist/own` and
--- whether the chunk record exists. The numbers either explain a mismatch or rule out the
--- shapes a count cannot distinguish (a peak with no source, a plateau of mutually
--- supporting values, a truncation edge).
function Runtime.fieldNeighborhood(fieldType, chunkId, radius)
    local key = Runtime.fieldKey(fieldType)
    local field = Universe.sourceVersionedFields[key] or {}
    local st = Universe.sourceVersionedRelax[key]
    local dist = (st and st.dist) or {}
    local center = chunkFor(chunkId)
    if not center then return "no such chunk: " .. tostring(chunkId) end
    local cx, cy = MapUtils.chunkX(center), MapUtils.chunkY(center)
    radius = radius or 2
    local rows = {}
    for dy = -radius, radius do
        local cells = {}
        for dx = -radius, radius do
            local chunk = MapUtils.getChunkByXY(center.map, cx + dx * CHUNK_SIZE, cy + dy * CHUNK_SIZE)
            if type(chunk) == "number" or not chunk then
                cells[#cells + 1] = "--"
            else
                local own = Runtime.sourceValue(chunk, fieldType)
                cells[#cells + 1] = string.format("%d:%.0f/d%s/g%.0f", chunk.id,
                    field[chunk.id] or 0, tostring(dist[chunk.id] or "-"), own)
            end
        end
        rows[#rows + 1] = table.concat(cells, " ")
    end
    return string.format("field=%s center=%d(%d,%d) live=%.1f | %s",
        key, chunkId, cx, cy, field[chunkId] or 0, table.concat(rows, " || "))
end

--- Multi-source breadth-first distance from every chunk that carries a source. This is
--- the distance the radius cap MEANS; the relaxed field carries its own per-node `dist`
--- along the max-plus path, and the two can disagree.
local function bfsDistance(fieldType, cap)
    local dist = {}
    local queue, head, tail = {}, 1, 0
    for _, chunk in pairs(Universe.chunkById or {}) do
        if Runtime.sourceValue(chunk, fieldType) > 0 then
            dist[chunk.id] = 0
            tail = tail + 1
            queue[tail] = chunk.id
        end
    end
    while head <= tail do
        local id = queue[head]
        head = head + 1
        local d = dist[id]
        if not cap or d < cap then
            local nbrs, count = Runtime.neighborOf(id)
            if not count then count = #nbrs end
            for k = 1, count do
                local nid = nbrs[k]
                if dist[nid] == nil then
                    dist[nid] = d + 1
                    tail = tail + 1
                    queue[tail] = nid
                end
            end
        end
    end
    return dist
end

--- Diagnose a field's disagreement with an exact solve: for each mismatched chunk,
--- compare the carried distance with the true distance to the nearest source, and say
--- which of the two the mismatch blames. Answers the question verifyField can only
--- count: is this truncation at the radius, a distance error, or an unsupported value?
function Runtime.supportDiag(fieldType, sampleCount, tolerance)
    local key = Runtime.fieldKey(fieldType)
    local fopts = fieldOpts(fieldType, nil)
    Runtime.ensureSolved(fieldType, fopts)
    local guard = 0
    while Runtime.relaxDepth(fieldType) > 0 and guard < 4000 do
        guard = guard + 1
        Runtime.relaxStep(fieldType, 4096, fopts)
    end
    local live = Universe.sourceVersionedFields[key] or {}
    local st = Universe.sourceVersionedRelax[key]
    local dist = (st and st.dist) or {}
    local expected, prov = Solver.solveWithMetadata(Runtime.buildSources(fieldType),
        Runtime.neighborOf, fopts)
    local radius = fopts and fopts.maxRadius
    local bfs = bfsDistance(fieldType, radius)
    -- A carried distance is the length of the path that delivered the value, so it can
    -- never be SHORTER than the hop distance to the nearest source. When it is, the node
    -- claims support it does not have, which is exactly how values appear beyond the
    -- radius cap (measured: liveDist 1 where the true distance was 11). This count is the
    -- rigorous version of the carriedTooNear sample below: it covers the whole field.
    local uncapped = radius and bfsDistance(fieldType, nil) or bfs
    local distTooNear, distTooFar, worstShort = 0, 0, 0
    -- A field entry whose chunk record is GONE is a value nothing can ever recompute:
    -- relaxNode is only ever called for an id that is popped, and nothing pops an id that
    -- no longer exists. The entry and its cone then survive forever with no source, which
    -- is the one ghost shape a re-pop can never reach.
    local orphans = 0
    for id, _ in pairs(live) do
        if chunkFor(id) == nil then orphans = orphans + 1 end
    end
    for id, _ in pairs(live) do
        local d = dist[id]
        local trueDist = uncapped[id]
        if d and trueDist then
            if d < trueDist then
                distTooNear = distTooNear + 1
                if trueDist - d > worstShort then worstShort = trueDist - d end
            elseif d > trueDist then
                distTooFar = distTooFar + 1
            end
        end
    end
    tolerance = tolerance or 0.10
    sampleCount = sampleCount or 6
    local mismatches, liveOnly = 0, 0
    local counts = { sameDist = 0, carriedTooFar = 0, carriedTooNear = 0, noDist = 0, outOfReach = 0 }
    local worst, worstDelta = {}, {}
    -- Keep the `sampleCount` largest deltas, sorted descending, without sorting the
    -- whole mismatch set: one insertion-sorted array of that size.
    local function note(delta, line)
        local n = #worst
        if n < sampleCount then
            worst[n + 1], worstDelta[n + 1] = line, delta
        elseif delta > worstDelta[n] then
            worst[n], worstDelta[n] = line, delta
        else
            return
        end
        local i = #worst
        while i > 1 and worstDelta[i - 1] < worstDelta[i] do
            worst[i], worst[i - 1] = worst[i - 1], worst[i]
            worstDelta[i], worstDelta[i - 1] = worstDelta[i - 1], worstDelta[i]
            i = i - 1
        end
    end
    for id, ev in pairs(expected) do
        local lv = live[id] or 0
        local delta = lv - ev
        if delta < 0 then delta = -delta end
        local rel = ev > 0 and (delta / ev) or (lv > 0 and 1 or 0)
        if rel > tolerance then
            mismatches = mismatches + 1
            local ld = dist[id]
            local bd = bfs[id]
            local kind
            if not bd then kind = "outOfReach"
            elseif not ld then kind = "noDist"
            elseif ld > bd then kind = "carriedTooFar"
            elseif ld < bd then kind = "carriedTooNear"
            else kind = "sameDist" end
            counts[kind] = counts[kind] + 1
            local chunk = chunkFor(id)
            -- Whose support is it? The exact solve names the source it grounds the value
            -- in (prov), the relaxation names the root its chain walks to. If those are
            -- different sources, the LIVE value is grounded in a source the exact solve
            -- rejected, which is the difference between "truncation" and "stale value".
            local srcId = Solver.provTop(prov, id)
            local hops, root, verdict, terminal = walkChain(fieldType, live, dist, id, fopts, radius or 32)
            -- For a chunk with NO value the interesting numbers are on its neighbours:
            -- the smallest distance they carry (does the radius gate reach this node at
            -- all?) and the largest value it would inherit if it did.
            local nbrMinDist, nbrMaxVal = nil, 0
            local nbrs, nbrCount = Runtime.neighborOf(id)
            if not nbrCount then nbrCount = #nbrs end
            for k = 1, nbrCount do
                local nid = nbrs[k]
                local nv = live[nid]
                if nv and nv > 0 then
                    local nd = dist[nid]
                    if nd and (nbrMinDist == nil or nd < nbrMinDist) then nbrMinDist = nd end
                    if nv > nbrMaxVal then nbrMaxVal = nv end
                end
            end
            -- The root's own source is the load-bearing number: a chain that grounds in
            -- a chunk whose source has dropped to zero is a STALE value (an unreported
            -- decrease), not a truncation.
            local rootChunk = chunkFor(root)
            note(delta, string.format(
                "id=%d live=%.1f exp=%.1f liveDist=%s bfsDist=%s root=%d rootGen=%.1f "
                    .. "rootLive=%.1f hops=%d expSrc=%d verdict=%s grounded=%s radius=%s "
                    .. "ownGen=%.1f nbrDist=%s nbrVal=%.1f kind=%s",
                id, lv, ev, tostring(ld), tostring(bd), root,
                (rootChunk and Runtime.sourceValue(rootChunk, fieldType)) or 0,
                live[root] or 0, #hops, srcId or 0, verdict, terminal, tostring(radius),
                (chunk and Runtime.sourceValue(chunk, fieldType)) or 0,
                tostring(nbrMinDist), nbrMaxVal, kind))
        end
    end
    local eps = (fopts and fopts.eps) or Solver.DEFAULT_EPS
    for id, lv in pairs(live) do
        if not expected[id] and lv > eps then
            liveOnly = liveOnly + 1
        end
    end
    return string.format(
        "mismatches=%d liveOnly=%d sameDist=%d carriedTooFar=%d carriedTooNear=%d "
            .. "noDist=%d outOfReach=%d distTooNear=%d(worst %d) distTooFar=%d "
            .. "orphans=%d frontier=%d | %s",
        mismatches, liveOnly, counts.sameDist, counts.carriedTooFar, counts.carriedTooNear,
        counts.noDist, counts.outOfReach, distTooNear, worstShort, distTooFar, orphans,
        Runtime.relaxDepth(fieldType), table.concat(worst, " | "))
end

--- Work accounting per field: how much frontier each field is carrying, and how much
--- propagation the scheduler has given it since init. A field whose frontier stays deep
--- while its reports keep arriving is starved (its input rate exceeds its share of the
--- shared quantum), which reads exactly like "the field is wrong" in a comparison and is
--- not: it is lag.
function Runtime.fieldLoad()
    local out = {}
    local byField = Runtime.workStats.byField or {}
    for _, fieldType in ipairs({ FIELD_DEATH_GAIN, FIELD_DEATH_LOSS, FIELD_PLAYER,
        FIELD_KAMIKAZE, FIELD_BASE, FIELD_ENEMY, FIELD_RESOURCE }) do
        local key = Runtime.fieldKey(fieldType)
        local stats = (Runtime.auditStats or {})[key] or {}
        out[#out + 1] = string.format("%s depth=%d pops=%d reports=%d last=%s ghosts=%d starved=%d orphans=%d",
            key, Runtime.relaxDepth(fieldType),
            math.floor(byField[fieldType] or 0),
            math.floor((Runtime.sourceReports and Runtime.sourceReports[fieldType]) or 0),
            tostring(Runtime.lastFieldWork[fieldType]),
            math.floor(stats.ghosts or 0), math.floor(stats.starved or 0),
            math.floor(stats.orphans or 0))
    end
    return table.concat(out, " | ")
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
