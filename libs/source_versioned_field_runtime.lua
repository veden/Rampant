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
local RamState = require("libs/RamState")
local SquadLogic = require("squad_logic")
local MathUtils = require("MathUtils")

local Runtime = {}

local Universe
-- Per-field source-change queues. Each field drains on its own budget so a
-- slow field (resource) cannot starve a decision-critical one (base/enemy).
local SourceQueues = {}
local QueueIndex = {}
local DRAIN_PER_TICK = 40
local FIELD_DRAIN_PER_TICK = {}
local FIELD_DRAIN_ACC = {}
local FIELD_CADENCE = {}
local PROPAGATION_MAX_WORK_PER_TICK = Constants.PROPAGATION_MAX_WORK_PER_TICK
local BOUNDED_OPTS = { maxWork = PROPAGATION_MAX_WORK_PER_TICK }
local Initialized = false
local SolvedFields = {}
local FieldProvenance = {}
local FieldSecond = {}

-- Solved-field state is persisted in storage so a load does not re-solve. The
-- module locals alias the storage tables. `on_load` must not write storage, so
-- creation and the layout version check live in ensureStorage(), which is called
-- from init(addProperties=true) and from the tick path.
local SOLVE_VERSION = 2
local EMPTY_SOLVED = {}
local EMPTY_TOP = {}
local EMPTY_SECOND = {}
local EMPTY_SECOND_SOURCE = {}

local function bindSolvedState()
    -- The field tables MUST live in storage: multiplayer transfers storage, and
    -- a late-joining client re-executes the module code with empty module tables.
    -- The incremental solver does not reproduce a full solve exactly (measured:
    -- 21 mismatches, 606 live-only chunks), so a client that re-derived them
    -- locally would disagree with the host. Only pure derivations may live
    -- outside storage (see RamState.neighborIds).
    SolvedFields = Universe.sourceVersionedSolved
    FieldProvenance = Universe.sourceVersionedProvenance or EMPTY_TOP
    FieldSecond = Universe.sourceVersionedSecond
end

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

-- Per-field solver options. A field's reach is log(eps/source)/log(decay), so
-- leaving decay at the shared default makes every field's extent an accident of
-- its source magnitude; the kamikaze field needs its own.
local FIELD_OPTS = {}
-- Radius caps on the two fields the player writes every tick. Nominal reach at
-- decay 0.9 / eps 5e-4 is 126 hops (~4000 tiles), so one step improved most of
-- the map and the field filled every tracked chunk (991 chunks measured). 8 hops
-- is 256 tiles: far more than a hunter needs, and it bounds the work per move and
-- the field's memory to a fixed neighbourhood.
FIELD_OPTS[FIELD_PLAYER] = { maxRadius = PLAYER_FIELD_RADIUS }
-- Base, enemy and resource change rarely (structure events, chunk scans) but
-- their nominal reach was also 126 hops, so every one of those rare changes
-- re-propagated the entire tracked map and paid O(map) work in a single tick.
FIELD_OPTS[FIELD_BASE] = { maxRadius = REGIONAL_FIELD_RADIUS }
FIELD_OPTS[FIELD_ENEMY] = { maxRadius = REGIONAL_FIELD_RADIUS }
FIELD_OPTS[FIELD_RESOURCE] = { maxRadius = RESOURCE_FIELD_RADIUS }
FIELD_OPTS[FIELD_KAMIKAZE] = { decay = KAMIKAZE_DECAY, maxRadius = PLAYER_FIELD_RADIUS }
FIELD_OPTS[FIELD_DEATH_GAIN] = { decay = DEATH_DECAY, maxRadius = DEATH_RADIUS }
FIELD_OPTS[FIELD_DEATH_LOSS] = { decay = DEATH_DECAY, maxRadius = DEATH_RADIUS }

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
        bindSolvedState()
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
    U.sourceVersionedSolveVersion = SOLVE_VERSION
    SourceQueues = {}
    QueueIndex = {}
    bindSolvedState()
end

function Runtime.init(universe, addProperties)
    Universe = universe
    if addProperties then
        Universe.sourceVersionedFields = Universe.sourceVersionedFields or {}
        Universe.sourceVersionedRefresh = Universe.sourceVersionedRefresh or {}
    end
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
    bindSolvedState()
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

-- Free list for source-queue items. Every decaying generator queues one change
-- per chunk per tick (the death generator alone did this for ~83 chunks, and it
-- was the largest remaining allocator at 3.8KB/tick). Items live from enqueue to
-- drain, so they are recycled at drain time rather than reused from a monotonic
-- index.
local SourceItemFree = {}
local SourceItemFreeCount = 0
-- Diagnostics for the pool: a regression here (created growing every tick) is the
-- bug that made the death generator the largest allocator in the mod.
local SourceItemCreated = 0

local function takeSourceItem(chunk, fieldType, oldValue, newValue)
    local item
    local profile = Universe.allocProfile
    if profile then
        profile.queueNew = (profile.queueNew or 0)
        profile.queueHit = (profile.queueHit or 0)
    end
    if SourceItemFreeCount > 0 then
        if profile then profile.queueHit = profile.queueHit + 1 end
        item = SourceItemFree[SourceItemFreeCount]
        SourceItemFree[SourceItemFreeCount] = nil
        SourceItemFreeCount = SourceItemFreeCount - 1
    else
        item = {}
        SourceItemCreated = SourceItemCreated + 1
        if profile then profile.queueNew = profile.queueNew + 1 end
    end
    item.chunk = chunk
    item.fieldType = fieldType
    item.oldValue = oldValue
    item.newValue = newValue
    return item
end

local function releaseSourceItem(item)
    if not item then return end
    SourceItemFreeCount = SourceItemFreeCount + 1
    SourceItemFree[SourceItemFreeCount] = item
end

-- Depth-indexed scratch frames for the drain. The drain allocates a scratch table
-- per field per tick plus one per source change and per increase; measured at
-- 8.2KB/tick, the single largest allocator in the mod. Frames are indexed by
-- nesting depth because process() can re-enter (removeSource/ensureSolved read a
-- field), and a shared frame would be clobbered mid-iteration.
local ScratchFrames = {}
local ScratchDepth = 0

local function pushScratch()
    ScratchDepth = ScratchDepth + 1
    local frame = ScratchFrames[ScratchDepth]
    if not frame then
        frame = { sources = {}, count = 0, items = {}, increaseCount = 0 }
        ScratchFrames[ScratchDepth] = frame
    end
    frame.count = 0
    frame.increaseCount = 0
    return frame
end

local function popScratch()
    ScratchDepth = ScratchDepth - 1
end

local function allocBump(name, kb)
    local a = Universe.allocProfile
    if a then
        a[name] = (a[name] or 0) + kb * 1024
    end
end

local function takeSourceChange(frame, chunk, oldValue, newValue)
    frame.count = frame.count + 1
    local entry = frame.sources[frame.count]
    if not entry then
        entry = {}
        frame.sources[frame.count] = entry
    end
    entry.chunk = chunk
    entry.oldValue = oldValue
    entry.newValue = newValue
    return entry
end

local function takeIncreaseItem(frame)
    frame.increaseCount = frame.increaseCount + 1
    local inc = frame.items[frame.increaseCount]
    if not inc then
        inc = {}
        frame.items[frame.increaseCount] = inc
    end
    return inc
end

function Runtime.queueSource(chunk, fieldType, oldValue, newValue)
    if not Initialized or not chunk or not chunk.id then return false end

    -- Coalesce at enqueue time as well as at drain time. A source chunk
    -- may be touched several times before the queue is drained; keep only the
    -- first old value and the latest new value.
    local byChunk = QueueIndex[fieldType]
    if not byChunk then
        byChunk = {}
        QueueIndex[fieldType] = byChunk
    end
    local existing = byChunk[chunk.id]
    if existing then
        existing.newValue = newValue
        return true
    end

    local queue = SourceQueues[fieldType]
    if not queue then
        queue = { items = {}, head = 1, count = 0 }
        SourceQueues[fieldType] = queue
    end
    queue.count = queue.count + 1
    local item = takeSourceItem(chunk, fieldType, oldValue, newValue)
    queue.items[queue.count] = item
    byChunk[chunk.id] = item
    return true
end

-- Solve one field on first use. Solving lazily keeps the first H8 tick from
-- paying for fields that are never read while still ensuring a source change
-- propagates into a complete field.
-- Chunks examined per tick while building a field's source list. The first
-- solve used to scan every tracked chunk and run a full Dijkstra in one tick:
-- measured 12.4ms on a ~1k-chunk map (and 40-50ms on the whole tick), once per
-- field, which is what produced the early-game maximum.
local SOURCE_BUILD_PER_TICK = 256
-- Propagation work allowed per tick while building a field. The solve is spread
-- like the normal drain so a rebuild cannot spike the tick.
-- A single removal call never exceeds this, whatever the shared allowance says.
-- verifyField sets the allowance to 1e9, and an unbounded resume then propagated
-- for 1.16 SECONDS in one tick.
local REMOVAL_MAX_WORK_Per_CALL = 256
-- One pop costs ~13us on a dense field, so 256 pops was a 3.4ms slice inside a
-- single tick (measured during the periodic field rebuild). 128 halves the worst
-- slice and doubles the rebuild's spread.
local SOURCE_SOLVE_WORK_PER_TICK = 128
local SourceBuilds = {}

-- Ticks between full reconciliations of one field. The incremental path only
-- applies increases (max-plus), so improvements skipped by the eps test and
-- values left behind by removals let the live field drift from the exact solve:
-- measured 109 of 613 chunks off by more than 10% (worst 50%) and 35 stale
-- chunks on an enemy field. Rebuilding periodically bounds that drift; the swap
-- happens only when the rebuild completes, so readers keep the old field while
-- it is being rebuilt.
local RECONCILE_INTERVAL = 30 * 60 * 5

function Runtime.ensureSolved(fieldType, opts)
    local key = Runtime.fieldKey(fieldType)
    if SolvedFields[key] and FieldProvenance[key] then
        local solvedAt = Universe.sourceVersionedSolvedAt
        local last = solvedAt and solvedAt[key]
        if last and (game.tick - last) > RECONCILE_INTERVAL then
            -- Rebuild in place: the stored field stays readable until the fresh
            -- one is swapped in by the rebuild below.
            SolvedFields[key] = false
            SourceBuilds[key] = nil
            if TelemetryG and TelemetryG.isEnabled() then
                TelemetryG.emit("field.reconcile", { field = key, age = game.tick - last })
            end
        else
            return Universe.sourceVersionedFields[key] or {}, false
        end
    end
    -- A chunk-id compaction re-keys every chunk, so an in-progress build's
    -- iterator key and source ids belong to the old id space. The epoch makes
    -- such a build detectable: discard it and restart from the new ids.
    local epoch = Universe.chunkEpoch or 0
    local build = SourceBuilds[key]
    if build and build.epoch ~= epoch then
        SourceBuilds[key] = nil
        build = nil
    end
    if not build then
        build = { list = {}, iterator = nil, epoch = epoch }
        SourceBuilds[key] = build
    end
    local examined = 0
    local iter = build.iterator
    while examined < SOURCE_BUILD_PER_TICK do
        local chunkId, chunk = next(Universe.chunkById or {}, iter)
        if not chunk then
            build.iterator = nil
            build.done = true
            break
        end
        iter = chunkId
        examined = examined + 1
        local value = Runtime.sourceValue(chunk, fieldType)
        if value > 0 then
            build.list[#build.list + 1] = { id = chunk.id, gen = value }
        end
    end
    build.iterator = iter
    if not build.done then
        -- Still scanning: readers see the (empty) field for a few ticks rather
        -- than one tick paying for the whole map.
        return Universe.sourceVersionedFields[key] or {}, false
    end
    -- Solve incrementally too, not with one Dijkstra over the whole map. A
    -- one-shot solve cost 115-154ms on a large save (measured at ticks
    -- 13484423/39/75), and the reconcile runs about every 30 seconds, so that
    -- was the periodic spike. Seeding the resumable propagator and draining it
    -- on a per-tick budget spreads the same work; the swap happens only when
    -- the heaps empty, so readers keep the old field meanwhile.
    if not build.field then
        build.field = {}
        build.provenance = {}
        build.second = {}
        build.hTop = Solver.newIncrementalHeap()
        build.hSec = Solver.newIncrementalHeap()
    end
    -- The solve budget is per TICK, shared by every field and every read:
    -- scoring a settle reads four fields per neighbour, and a per-call budget of
    -- 256 pops let one 32-read scoring pass spend 8192 pops in a single tick
    -- (measured 151ms in squad.settleMove -> squad.score).
    if Universe.solveWorkTick ~= game.tick then
        Universe.solveWorkTick = game.tick
        Universe.solveWorkLeft = SOURCE_SOLVE_WORK_PER_TICK
    end
    local allowance = Universe.solveWorkLeft or 0
    if allowance > 0 then
        local profiler = (Universe.diagTicks and helpers) and helpers.create_profiler() or nil
        local used = Solver.propagateIncreasesWithMetadata(
            build.list, build.field, build.provenance, build.second,
            Runtime.neighborOf, { maxWork = allowance,
                decay = opts and opts.decay, eps = opts and opts.eps,
                maxRadius = opts and opts.maxRadius },
            build.hTop, build.hSec)
        Universe.solveWorkLeft = allowance - (used or 0)
        if profiler then
            log({"", "[SUB] tick=", game.tick, " name=field.solve ", profiler})
        end
    end
    if build.hTop.n > 0 or build.hSec.n > 0 then
        return Universe.sourceVersionedFields[key] or {}, false
    end
    local field, prov, second = build.field, build.provenance, build.second
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
    if TelemetryG and TelemetryG.isEnabled() then
        TelemetryG.emit("field.solved", {
            field = key, sources = #build.list, chunks = #field,
        })
    end
    return field, true
end

-- Drain ONE field's source-change queue. `budget` is that field's own
-- share of the per-tick work, so a slow field (resource) cannot starve a
-- decision-critical one (base/enemy). Collapses queued changes for a source into
-- one old -> final transition, then runs one multi-source increase propagation.
function Runtime.process(fieldType, budget, opts, workLimit)
    if not Initialized then return 0 end
    local key = Runtime.fieldKey(fieldType)
    local queue = SourceQueues[fieldType]
    local queueEmpty = (not queue) or queue.head > queue.count
    -- A frontier left by a previous tick is still work: returning early on an
    -- empty queue would strand it and the field would stay partial forever
    -- (observed as the base field's nest staying 0 in pheromone-dynamics).
    local frontier = Universe.sourceVersionedFrontier[key]
    -- A stranded removal is work even when the queue and frontier are empty: it
    -- must be resumed BEFORE the early return below, otherwise a removal with
    -- nothing else happening on that field is never completed (measured: the base
    -- field held stale values until the periodic reconcile).
    if Runtime.hasPendingRemoval(fieldType) then
        local rprof = (Universe.diagTicks and helpers) and helpers.create_profiler() or nil
        Runtime.resumeRemoval(fieldType, fieldOpts(fieldType, opts))
        if rprof then
            log({"", "[SUB] tick=", game.tick, " name=sf.resume ", rprof})
        end
    end
    if queueEmpty and not (frontier and (frontier.top.n > 0 or frontier.second.n > 0)) then
        if queue then
            queue.items = {}
            queue.head = 1
            queue.count = 0
        end
        return 0
    end
    if not budget or budget < 1 then budget = DRAIN_PER_TICK end

    -- process() handles ONE field, so the field-keyed `changes` map is
    -- vestigial: one accumulator is equivalent and drops a table plus a keyed
    -- lookup per tick.
    local prof = (Universe.diagTicks and helpers) and helpers.create_profiler() or nil
    local frame = pushScratch()
    local sources = frame.sources
    -- Reuse the frame's dedupe map too; it is keyed by chunk id.
    local byChunkId = frame.byChunkId
    if not byChunkId then
        byChunkId = {}
        frame.byChunkId = byChunkId
    end
    local processed = 0
    local workUsed = 0
    while queue and queue.head <= queue.count and processed < budget do
        local item = queue.items[queue.head]
        queue.items[queue.head] = nil
        local byChunk = QueueIndex[fieldType]
        if byChunk and byChunk[item.chunk.id] == item then
            byChunk[item.chunk.id] = nil
        end
        queue.head = queue.head + 1
        processed = processed + 1

        if item.chunk and item.chunk.id then
            local sourceChange = byChunkId[item.chunk.id]
            if not sourceChange then
                sourceChange = takeSourceChange(frame, item.chunk, item.oldValue, item.newValue)
                byChunkId[item.chunk.id] = sourceChange
            else
                sourceChange.newValue = item.newValue
            end
        end
        releaseSourceItem(item)
    end

    if prof then
        log({"", "[SUB] tick=", game.tick, " name=sf.drain ", prof})
        prof = (Universe.diagTicks and helpers) and helpers.create_profiler() or nil
    end
    local fType = fieldType
    local fOpts = fieldOpts(fType, opts)

    local kbSolve = Universe.allocProfile and collectgarbage("count") or 0
    local _, solvedNow = Runtime.ensureSolved(fType, fOpts)
    if Universe.allocProfile then
        allocBump("sf.ensureSolved", collectgarbage("count") - kbSolve)
    end
    if not SolvedFields[Runtime.fieldKey(fType)] then
        -- Source list still being built; keep the queue so nothing is lost.
        for i = 1, frame.count do
            byChunkId[sources[i].chunk.id] = nil
        end
        popScratch()
        return 0
    end
    if prof then
        log({"", "[SUB] tick=", game.tick, " name=sf.solve ", prof})
        prof = (Universe.diagTicks and helpers) and helpers.create_profiler() or nil
    end
    if not solvedNow then
        local increases = frame.increases
        if not increases then
            increases = {}
            frame.increases = increases
        end
        local removed = false
        for i = 1, frame.count do
            local sourceChange = sources[i]
            local oldValue = sourceChange.oldValue
            local newValue = Runtime.sourceValue(sourceChange.chunk, fType)
            if oldValue and newValue < oldValue then
                local kbRem = Universe.allocProfile and collectgarbage("count") or 0
                Runtime.removeSource(sourceChange.chunk, fType, oldValue, fOpts)
                if Universe.allocProfile then
                    allocBump("sf.removeSource", collectgarbage("count") - kbRem)
                end
                removed = true
                if newValue > 0 then
                    local inc = takeIncreaseItem(frame)
                    inc.id = sourceChange.chunk.id
                    inc.gen = newValue
                    increases[frame.increaseCount] = inc
                end
            elseif newValue > 0 then
                local inc = takeIncreaseItem(frame)
                inc.id = sourceChange.chunk.id
                inc.gen = newValue
                increases[frame.increaseCount] = inc
            end
        end
        -- Clear the tail so #increases reports the count we filled.
        for i = frame.increaseCount + 1, #increases do
            increases[i] = nil
        end
        -- Resume a frontier left by a previous tick even when this tick has no
        -- new changes: otherwise a single change that hit the work budget never
        -- completes, and the field stays partial forever (observed as the base
        -- field's nest staying 0 in pheromone-dynamics).
        local frontier = Universe.sourceVersionedFrontier[Runtime.fieldKey(fType)]
        if #increases > 0 or (frontier and (frontier.top.n > 0 or frontier.second.n > 0)) then
            local kbBefore = Universe.allocProfile and collectgarbage("count") or 0
            workUsed = workUsed + (Runtime.propagateIncreases(increases, fType, fOpts, removed, workLimit) or 0)
            if Universe.allocProfile then
                allocBump("sf.propagate", collectgarbage("count") - kbBefore)
            end
        end
    end

    if queue and queue.head > queue.count then
        -- Keep the array (it is reused every tick); just reset the counters.
        queue.head = 1
        queue.count = 0
    end
    for i = 1, frame.count do
        byChunkId[sources[i].chunk.id] = nil
    end
    popScratch()
    return workUsed
end

-- Work left in a field's propagation frontier (0 = converged). A non-zero
-- frontier means the field is still partial, which reads can observe.
function Runtime.frontierDepth(fieldType)
    local frontier = Universe and Universe.sourceVersionedFrontier
        and Universe.sourceVersionedFrontier[Runtime.fieldKey(fieldType)]
    if not frontier then return 0 end
    return frontier.top.n + frontier.second.n
end

-- Queued source changes not yet drained for a field.
function Runtime.queueDepth(fieldType)
    local queue = SourceQueues[fieldType]
    if not queue then return 0 end
    return queue.count - queue.head + 1
end

-- A burst (the initial scan, the evolution rescan, a mass build or destroy)
-- queues thousands of source changes at once. Draining that at
-- FIELD_DRAIN_PER_TICK leaves the field stale for hundreds of ticks -- measured
-- as a 1,500-tick tail on a 15k-chunk rescan -- during which every read sees a
-- partially updated field. Flush a burst in the tick it appears instead: the
-- cost is one-time and proportional to the burst, which is the trade the cadence
-- exists to make. Below the threshold the per-field cadence is unchanged.
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
    local perTick = (FIELD_DRAIN_PER_TICK[fieldType] or DRAIN_PER_TICK) * (cadence or 1)
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
        local used = Runtime.process(fType, DRAIN_PER_TICK, FIELD_OPTS[fType], slice)
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
    for i = n + 1, 8 do ids[i] = nil end
    return ids
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
            sources[#sources + 1] = { id = chunk.id, gen = value }
        end
    end
    return sources
end

function Runtime.solveFieldWithMetadata(fieldType, opts)
    local sources = Runtime.buildSources(fieldType)
    return Solver.solveWithMetadata(sources, Runtime.neighborOf, opts)
end

-- The optional third argument may be either the old API's `field` argument
-- or the new options table. The metadata-backed field is always local, so the
-- old `field` value is accepted and ignored for compatibility.
function Runtime.propagateIncrease(chunk, fieldType, fieldOrOpts, maybeOpts)
    local opts = maybeOpts or fieldOrOpts
    local key = Runtime.fieldKey(fieldType)
    local field = Universe.sourceVersionedFields[key]
    local source = { id = chunk.id, gen = Runtime.sourceValue(chunk, fieldType) }
    return Solver.propagateIncreaseWithMetadata(
        source, field, FieldProvenance[key], FieldSecond[key],
        Runtime.neighborOf, opts)
end

-- Scratch tables for the removal path (see removeSource).
local REMOVAL_SOURCE = { id = 0, gen = 0 }
local REMOVAL_OPTS = { maxWork = 0 }

--- Pool statistics, for tests and telemetry: how many queue items exist, how
--- many are idle, and how many were ever created. A healthy steady state keeps
--- `created` flat while `free` grows to the working set.
function Runtime.queueItemStats()
    return SourceItemCreated, SourceItemFreeCount
end

--- True when a removal for this field is still in progress (its heap hit the
--- per-tick allowance). Leftover work used to be resumed only by a later source
--- change for the same chunk, so a removal that ran out of budget stranded the
--- field too high until the periodic reconcile (measured: 1,641 chunks off by up
--- to 89% on the enemy field).
function Runtime.hasPendingRemoval(fieldType)
    local heaps = Universe.sourceVersionedRemoval
    if not heaps then return false end
    local heap = heaps[Runtime.fieldKey(fieldType)]
    return heap ~= nil and #heap > 0
end

--- Continue a removal that hit the allowance. Same budget and the same reused
--- heap as removeSource; `source` is nil because the heap is already seeded.
function Runtime.resumeRemoval(fieldType, opts)
    local key = Runtime.fieldKey(fieldType)
    local heaps = Universe.sourceVersionedRemoval
    local heap = heaps and heaps[key]
    if not heap or #heap == 0 then return 0 end
    if Universe.solveWorkTick ~= game.tick then
        Universe.solveWorkTick = game.tick
        Universe.solveWorkLeft = SOURCE_SOLVE_WORK_PER_TICK
    end
    -- Safety valve: if a removal heap outgrows any plausible field, stop growing
    -- it and let the field rebuild instead (the resumable solve spreads that).
    local REMOVAL_HEAP_CAP = 20000
    if #heap > REMOVAL_HEAP_CAP then
        heaps[key] = nil
        SolvedFields[key] = false
        SourceBuilds[key] = nil
        return 0
    end
    local allowance = Universe.solveWorkLeft or 0
    if allowance <= 0 then return 0 end
    if allowance > REMOVAL_MAX_WORK_Per_CALL then allowance = REMOVAL_MAX_WORK_Per_CALL end
    local removalOpts = REMOVAL_OPTS
    removalOpts.maxWork = allowance
    local work, leftover = Solver.removeSource(
        nil, Universe.sourceVersionedFields[key],
        FieldProvenance[key], FieldSecond[key],
        Runtime.neighborOf, removalOpts, heap)
    Universe.solveWorkLeft = allowance - (work or 0)
    heaps[key] = leftover or heap
    return work or 0
end

function Runtime.removeSource(chunk, fieldType, oldValue, opts)
    local key = Runtime.fieldKey(fieldType)
    local field = Universe.sourceVersionedFields[key]
    -- Removal is budgeted like the rest of the field work, and resumes from the
    -- leftover heap next tick: one removed source re-propagated the whole field
    -- in a single call (measured 25ms inside the drain).
    if Universe.solveWorkTick ~= game.tick then
        Universe.solveWorkTick = game.tick
        Universe.solveWorkLeft = SOURCE_SOLVE_WORK_PER_TICK
    end
    local allowance = Universe.solveWorkLeft or 0
    local heaps = Universe.sourceVersionedRemoval
    if not heaps then
        heaps = {}
        Universe.sourceVersionedRemoval = heaps
    end
    -- The heap and the options table are reused per field. Allocating them per
    -- call was 6.6KB/tick in steady state (77% of the drain's allocation):
    -- removals happen far more often than fields do.
    local heap = heaps[key]
    if not heap then
        heap = {}
        heaps[key] = heap
    end
    local hasPending = #heap > 0
    local source
    if not hasPending then
        source = REMOVAL_SOURCE
        source.id = chunk.id
        source.gen = oldValue or 0
    end
    -- Pass the field's own options through (decay/eps/rating/maxRadius differ per
    -- field); options() fills defaults in place, so only maxWork changes here.
    local removalOpts = opts or REMOVAL_OPTS
    removalOpts.maxWork = allowance
    local work, leftover = Solver.removeSource(
        source, field, FieldProvenance[key], FieldSecond[key],
        Runtime.neighborOf, removalOpts, heap)
    Universe.solveWorkLeft = allowance - (work or 0)
    heaps[key] = leftover or heap
    return work
end

-- Bounded increase propagation. A frontier left over from the previous tick
-- means the last propagation hit the work budget, so resume it. Re-seeding is
-- idempotent (a source already at its value pushes nothing), so passing
-- `sources` again is safe and the field still converges exactly.
-- `flush` drops the per-call work bound and runs to completion. A removal needs
-- it: removeSource drops every chunk that had the removed source as its top to
-- its second source, which leaves a hole only the re-propagation fills, and
-- bounded that hole persists for several ticks -- reads then see a field that
-- has lost its extent. Observed as pheromone-dynamics failing 1 run in 6 with
-- the base field 0 at the nest while the wall still read 550. A removal is a
-- one-time event, like a burst, so it pays the one-time cost.
function Runtime.propagateIncreases(sources, fieldType, opts, flush, workLimit)
    local key = Runtime.fieldKey(fieldType)
    local field = Universe.sourceVersionedFields[key]
    -- The frontier is two indexed heaps (top and second-best paths), one entry
    -- per chunk per side, arrays reused every tick. It cannot grow with activity:
    -- bounded by field size, no node tables, no dedupe map.
    local frontier = Universe.sourceVersionedFrontier[key]
    if not frontier or not frontier.top then
        frontier = { top = Solver.newIncrementalHeap(), second = Solver.newIncrementalHeap() }
        Universe.sourceVersionedFrontier[key] = frontier
    end
    local backlog = frontier.top.n + frontier.second.n
    if workLimit and workLimit > 0 then
        BOUNDED_OPTS.maxWork = workLimit
    elseif flush or backlog > FRONTIER_FLUSH_DEPTH then
        BOUNDED_OPTS.maxWork = FRONTIER_FLUSH_WORK
    else
        BOUNDED_OPTS.maxWork = PROPAGATION_MAX_WORK_PER_TICK
    end
    BOUNDED_OPTS.decay = opts and opts.decay
    BOUNDED_OPTS.eps = opts and opts.eps
    BOUNDED_OPTS.maxRadius = opts and opts.maxRadius
    local work = Solver.propagateIncreasesWithMetadata(
        sources, field, FieldProvenance[key], FieldSecond[key],
        Runtime.neighborOf, BOUNDED_OPTS, frontier.top, frontier.second)
    return work or 0
end

-- Correctness check: solve the field from its sources with the pure solver and
-- compare against the incrementally maintained live field. The incremental path
-- (queues, dedupe, indexed heap, radius caps) must converge to the same values;
-- a mismatch means a change was dropped or a cap is being applied inconsistently.
function Runtime.verifyField(fieldType, tolerance)
    local key = Runtime.fieldKey(fieldType)
    -- The comparison is only meaningful on a complete field: solve it if it has
    -- never been read, and drain any queued source changes, otherwise the live
    -- field is legitimately behind and every entry looks like a mismatch.
    if not SolvedFields[key] then
        local field, prov2, sec = Solver.solveWithMetadata(
            Runtime.buildSources(fieldType), Runtime.neighborOf, fieldOpts(fieldType, nil))
        Universe.sourceVersionedFields[key] = field
        FieldProvenance[key], FieldSecond[key] = prov2, sec
        SolvedFields[key] = true
    end
    -- Drain to convergence: process() is bounded by PROPAGATION_MAX_WORK_PER_TICK,
    -- so one call leaves a frontier and the comparison would see a partial field.
    local iterations = 0
    while iterations < 2000 do
        iterations = iterations + 1
        -- The per-tick solve allowance is already spent by the first call in this
        -- tick, so the drain would stall and the comparison would run against a
        -- PARTIAL field (observed as 0 / 1636 / 0 across consecutive calls).
        -- verifyField is an oracle, not gameplay: let it spend its own budget.
        Universe.solveWorkTick = nil
        Universe.solveWorkLeft = 1e9
        Runtime.process(fieldType, 100000, fieldOpts(fieldType, nil))
        if Runtime.frontierDepth(fieldType) == 0 and Runtime.queueDepth(fieldType) == 0 then
            break
        end
    end
    local live = Universe.sourceVersionedFields[key] or {}
    local sources = Runtime.buildSources(fieldType)
    local expected = Solver.solveWithMetadata(sources, Runtime.neighborOf, fieldOpts(fieldType, nil))
    -- Relative tolerance: absolute 0.5 is meaningless for a resource field in
    -- the thousands. A chunk is wrong when it differs from the exact solve by
    -- more than `tolerance` (default 10%) of the exact value.
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
            if not sampleId then sampleId, sampleLive, sampleExpected = id, liveV, v end
        end
    end
    for id, v in pairs(live) do
        if not expected[id] and v > 0 then extra = extra + 1 end
    end
    return {
        field = tostring(fieldType), sources = #sources, checked = checked,
        mismatches = mismatches, liveOnly = extra,
        maxDelta = string.format("%.3f", maxDelta), worstChunk = tostring(worstId),
        livePeak = string.format("%.1f", livePeak), expectedPeak = string.format("%.1f", expectedPeak),
        sample = tostring(sampleId) .. " live=" .. string.format("%.1f", sampleLive) ..
            " expected=" .. string.format("%.1f", sampleExpected),
        frontier = Runtime.frontierDepth(fieldType), queued = Runtime.queueDepth(fieldType),
        iterations = iterations,
        maxRel = string.format("%.1f%%", maxRel * 100),
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
