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
local PROPAGATION_MAX_WORK_PER_TICK = Constants.PROPAGATION_MAX_WORK_PER_TICK
local BOUNDED_OPTS = { maxWork = PROPAGATION_MAX_WORK_PER_TICK }
local Initialized = false
local SolvedFields = {}
local FieldTopSource = {}
local FieldSecond = {}
local FieldSecondSource = {}

-- Solved-field state is persisted in storage so a load does not re-solve. The
-- module locals alias the storage tables. `on_load` must not write storage, so
-- creation and the layout version check live in ensureStorage(), which is called
-- from init(addProperties=true) and from the tick path.
local SOLVE_VERSION = 1
local EMPTY_SOLVED = {}
local EMPTY_TOP = {}
local EMPTY_SECOND = {}
local EMPTY_SECOND_SOURCE = {}

local function bindSolvedState()
    SolvedFields = Universe.sourceVersionedSolved or EMPTY_SOLVED
    FieldTopSource = Universe.sourceVersionedTop or EMPTY_TOP
    FieldSecond = Universe.sourceVersionedSecond or EMPTY_SECOND
    FieldSecondSource = Universe.sourceVersionedSecondSource or EMPTY_SECOND_SOURCE
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
FIELD_OPTS[FIELD_KAMIKAZE] = { decay = KAMIKAZE_DECAY }
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
    U.sourceVersionedFrontier = U.sourceVersionedFrontier or {}
    if U.sourceVersionedSolved and U.sourceVersionedSolveVersion == SOLVE_VERSION then
        bindSolvedState()
        return
    end
    -- New save, or state from an older layout. The field values and the top-two
    -- metadata must be dropped together: the decrease path is only exact while the
    -- metadata matches the field.
    U.sourceVersionedFields = {}
    U.sourceVersionedSolved = {}
    U.sourceVersionedTop = {}
    U.sourceVersionedSecond = {}
    U.sourceVersionedSecondSource = {}
    -- A frontier from a dropped field is meaningless: the field restarts.
    U.sourceVersionedFrontier = {}
    U.sourceVersionedSolveVersion = SOLVE_VERSION
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
    local item = {
        chunk = chunk,
        fieldType = fieldType,
        oldValue = oldValue,
        newValue = newValue,
    }
    queue.items[queue.count] = item
    byChunk[chunk.id] = item
    return true
end

-- Solve one field on first use. Solving lazily keeps the first H8 tick from
-- paying for fields that are never read while still ensuring a source change
-- propagates into a complete field.
function Runtime.ensureSolved(fieldType, opts)
    local key = Runtime.fieldKey(fieldType)
    if not SolvedFields[key] or not FieldTopSource[key] then
        local field, topSource, second, secondSource =
            Runtime.solveFieldWithMetadata(fieldType, fieldOpts(fieldType, opts))
        Universe.sourceVersionedFields[key] = field
        FieldTopSource[key] = topSource
        FieldSecond[key] = second
        FieldSecondSource[key] = secondSource
        SolvedFields[key] = true
        return field, true
    end
    return Universe.sourceVersionedFields[key] or {}, false
end

-- Drain ONE field's source-change queue. `budget` is that field's own
-- share of the per-tick work, so a slow field (resource) cannot starve a
-- decision-critical one (base/enemy). Collapses queued changes for a source into
-- one old -> final transition, then runs one multi-source increase propagation.
function Runtime.process(fieldType, budget, opts)
    if not Initialized then return 0 end
    local key = Runtime.fieldKey(fieldType)
    local queue = SourceQueues[fieldType]
    local queueEmpty = (not queue) or queue.head > queue.count
    -- A frontier left by a previous tick is still work: returning early on an
    -- empty queue would strand it and the field would stay partial forever
    -- (observed as the base field's nest staying 0 in pheromone-dynamics).
    local frontier = Universe.sourceVersionedFrontier[key]
    if queueEmpty and not (frontier and #frontier > 0) then
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
    local fieldChanges = { fieldType = fieldType, sources = {} }
    local processed = 0
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
            local sourceChange = fieldChanges.sources[item.chunk.id]
            if not sourceChange then
                fieldChanges.sources[item.chunk.id] = {
                    chunk = item.chunk,
                    oldValue = item.oldValue,
                    newValue = item.newValue,
                }
            else
                sourceChange.newValue = item.newValue
            end
        end
    end

    local fType = fieldType
    local fOpts = fieldOpts(fType, opts)
    local _, solvedNow = Runtime.ensureSolved(fType, fOpts)
    if not solvedNow then
        local increases = {}
        local removed = false
        for _, sourceChange in pairs(fieldChanges.sources) do
            local oldValue = sourceChange.oldValue
            local newValue = Runtime.sourceValue(sourceChange.chunk, fType)
            if oldValue and newValue < oldValue then
                Runtime.removeSource(sourceChange.chunk, fType, oldValue, fOpts)
                removed = true
                if newValue > 0 then
                    increases[#increases + 1] = { id = sourceChange.chunk.id, gen = newValue }
                end
            elseif newValue > 0 then
                increases[#increases + 1] = { id = sourceChange.chunk.id, gen = newValue }
            end
        end
        -- Resume a frontier left by a previous tick even when this tick has no
        -- new changes: otherwise a single change that hit the work budget never
        -- completes, and the field stays partial forever (observed as the base
        -- field's nest staying 0 in pheromone-dynamics).
        local frontier = Universe.sourceVersionedFrontier[Runtime.fieldKey(fType)]
        if #increases > 0 or (frontier and #frontier > 0) then
            Runtime.propagateIncreases(increases, fType, fOpts, removed)
        end
    end

    if queue and queue.head > queue.count then
        queue.items = {}
        queue.head = 1
        queue.count = 0
    end
    return processed
end

-- Work left in a field's propagation frontier (0 = converged). A non-zero
-- frontier means the field is still partial, which reads can observe.
function Runtime.frontierDepth(fieldType)
    local frontier = Universe and Universe.sourceVersionedFrontier
        and Universe.sourceVersionedFrontier[Runtime.fieldKey(fieldType)]
    return frontier and #frontier or 0
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

-- Fractional per-field budgets: accumulate and run when a whole unit is due.
-- This is the knob that puts the resource field on its own cadence without
-- touching the decision-critical base/enemy fields.
function Runtime.processScheduled(fieldType, opts)
    if not Initialized then return 0 end
    local depth = Runtime.queueDepth(fieldType)
    if depth >= FLUSH_QUEUE_DEPTH then
        FIELD_DRAIN_ACC[fieldType] = 0
        return Runtime.process(fieldType, depth, opts)
    end
    local perTick = FIELD_DRAIN_PER_TICK[fieldType] or DRAIN_PER_TICK
    local acc = (FIELD_DRAIN_ACC[fieldType] or 0) + perTick
    local whole = math.floor(acc)
    FIELD_DRAIN_ACC[fieldType] = acc - whole
    if whole < 1 then return 0 end
    return Runtime.process(fieldType, whole, opts)
end

-- Reusable neighbour-id table. Solver consumes it immediately.
--
-- This deliberately does not use MapUtils.getNeighborChunks: that function
-- returns a single shared table. A source-versioned read can trigger a solve
-- while MapUtils.calculatePheromoneUpdates is still iterating its neighbour
-- table, which would overwrite that shared table mid-loop (observed as a
-- non-recoverable `chunk` number error at tick 0). Building the ids from
-- the chunk grid avoids the reentrancy.
function Runtime.neighborOf(chunkId)
    local chunk = MapUtils.getChunkById(chunkId, Universe)
    if not chunk then return {} end
    local ids = chunk.sourceVersionedNeighborIds
    if not ids then
        ids = {}
        local map = chunk.map
        local x = chunk.x
        local y = chunk.y
        for i = 1, 8 do
            local offset = NEIGHBOR_OFFSETS[i]
            local neighbor = MapUtils.getChunkByXY(map, x + offset[1], y + offset[2])
            if neighbor and neighbor ~= -1 then
                ids[#ids + 1] = neighbor.id
            end
        end
        chunk.sourceVersionedNeighborIds = ids
    end
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
        source, field, FieldTopSource[key], FieldSecond[key], FieldSecondSource[key],
        Runtime.neighborOf, opts)
end

function Runtime.removeSource(chunk, fieldType, oldValue, opts)
    local key = Runtime.fieldKey(fieldType)
    local field = Universe.sourceVersionedFields[key]
    local source = { id = chunk.id, gen = oldValue or 0 }
    return Solver.removeSource(
        source, field, FieldTopSource[key], FieldSecond[key], FieldSecondSource[key],
        Runtime.neighborOf, opts)
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
function Runtime.propagateIncreases(sources, fieldType, opts, flush)
    local key = Runtime.fieldKey(fieldType)
    local field = Universe.sourceVersionedFields[key]
    local frontier = Universe.sourceVersionedFrontier[key]
    if not frontier then
        frontier = {}
        Universe.sourceVersionedFrontier[key] = frontier
    end
    -- Reused scratch: the solver copies what it needs, so this allocates nothing.
    -- Same idea as the source-queue flush: the frontier is the solver's
    -- remaining heap, and it is only drained 32 nodes per tick. When new
    -- sources keep arriving (a moving player does this every tick) pushes
    -- exceed pops and the heap grows without bound -- observed as a 7559-node
    -- frontier for a 238-chunk kamikaze field, which made that one field cost
    -- 0.175ms/tick while every other field cost 0.005ms. Past the threshold,
    -- drain it completely in one tick; a bounded one-off cost beats a permanent
    -- one.
    local frontierBacklog = frontier and #frontier or 0
    -- NB: `cond and nil or x` in Lua always yields x, because `true and nil`
    -- is nil and `nil or x` is x. The unset MUST be an if/else: the previous
    -- one-liner left maxWork at 32 for every call, so neither this threshold
    -- nor the pre-existing burst/removal flush ever actually ran.
    if flush or frontierBacklog > FRONTIER_FLUSH_DEPTH then
        BOUNDED_OPTS.maxWork = nil
    else
        BOUNDED_OPTS.maxWork = PROPAGATION_MAX_WORK_PER_TICK
    end
    BOUNDED_OPTS.decay = opts and opts.decay
    BOUNDED_OPTS.eps = opts and opts.eps
    BOUNDED_OPTS.maxRadius = opts and opts.maxRadius
    local _, remaining = Solver.propagateIncreasesWithMetadata(
        sources, field, FieldTopSource[key], FieldSecond[key], FieldSecondSource[key],
        Runtime.neighborOf, BOUNDED_OPTS, frontier)
    Universe.sourceVersionedFrontier[key] = remaining
    return remaining and #remaining or 0
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
