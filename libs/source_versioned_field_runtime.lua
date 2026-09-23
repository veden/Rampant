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

local Runtime = {}

local Universe
local NeighborIds = {}
local SourceQueue = {}
local QueueHead = 1
local QueueCount = 0
local DRAIN_PER_TICK = 40
local Initialized = false
local SolvedFields = {}
local FieldTopSource = {}
local FieldSecond = {}
local FieldSecondSource = {}

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

local SOURCE_FIELDS = {
    [FIELD_PLAYER] = true,
    [FIELD_BASE] = true,
    [FIELD_ENEMY] = true,
    [FIELD_RESOURCE] = true,
}

function Runtime.init(universe)
    Universe = universe
    Universe.sourceVersionedFields = Universe.sourceVersionedFields or {}
    Universe.sourceVersionedRefresh = Universe.sourceVersionedRefresh or {}
    SourceQueue = {}
    QueueHead = 1
    QueueCount = 0
    SolvedFields = {}
    FieldTopSource = {}
    FieldSecond = {}
    FieldSecondSource = {}
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

function Runtime.ensureField(fieldType)
    local key = Runtime.fieldKey(fieldType)
    local field = Universe.sourceVersionedFields[key]
    if not field then
        field = {}
        Universe.sourceVersionedFields[key] = field
    end
    return field
end

function Runtime.queueSource(chunk, fieldType, oldValue, newValue)
    if not Initialized or not chunk or not chunk.id then return false end
    QueueCount = QueueCount + 1
    SourceQueue[QueueCount] = {
        chunk = chunk,
        fieldType = fieldType,
        oldValue = oldValue,
        newValue = newValue,
    }
    return true
end

function Runtime.solveAll(opts)
    for fieldType in pairs(SOURCE_FIELDS) do
        Runtime.ensureSolved(fieldType, opts)
    end
end

-- Solve one field on first use. Solving lazily keeps the first H8 tick from
-- paying for fields that are never read while still ensuring a source change
-- propagates into a complete field.
function Runtime.ensureSolved(fieldType, opts)
    local key = Runtime.fieldKey(fieldType)
    if not SolvedFields[key] or not FieldTopSource[key] then
        local field, topSource, second, secondSource = Runtime.solveFieldWithMetadata(fieldType, opts)
        Universe.sourceVersionedFields[key] = field
        FieldTopSource[key] = topSource
        FieldSecond[key] = second
        FieldSecondSource[key] = secondSource
        SolvedFields[key] = true
        return field, true
    end
    return Universe.sourceVersionedFields[key] or {}, false
end

function Runtime.process(opts)
    if not Initialized then return 0 end
    if QueueHead > QueueCount then
        SourceQueue = {}
        QueueHead = 1
        QueueCount = 0
        return 0
    end

    -- Collapse all queued changes for a field/source into one old -> final
    -- transition, then run one multi-source increase propagation per field.
    local changes = {}
    local processed = 0
    while QueueHead <= QueueCount and processed < DRAIN_PER_TICK do
        local item = SourceQueue[QueueHead]
        SourceQueue[QueueHead] = nil
        QueueHead = QueueHead + 1
        processed = processed + 1

        if item.chunk and item.chunk.id then
            local key = Runtime.fieldKey(item.fieldType)
            local fieldChanges = changes[key]
            if not fieldChanges then
                fieldChanges = { fieldType = item.fieldType, sources = {} }
                changes[key] = fieldChanges
            end
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

    for _, fieldChanges in pairs(changes) do
        local fieldType = fieldChanges.fieldType
        local _, solvedNow = Runtime.ensureSolved(fieldType, opts)
        if not solvedNow then
            local increases = {}
            for _, sourceChange in pairs(fieldChanges.sources) do
                local oldValue = sourceChange.oldValue
                local newValue = Runtime.sourceValue(sourceChange.chunk, fieldType)
                if oldValue and newValue < oldValue then
                    Runtime.removeSource(sourceChange.chunk, fieldType, oldValue, opts)
                    if newValue > 0 then
                        increases[#increases + 1] = { id = sourceChange.chunk.id, gen = newValue }
                    end
                elseif newValue > 0 then
                    increases[#increases + 1] = { id = sourceChange.chunk.id, gen = newValue }
                end
            end
            if #increases > 0 then
                Runtime.propagateIncreases(increases, fieldType, opts)
            end
        end
    end

    if QueueHead > QueueCount then
        SourceQueue = {}
        QueueHead = 1
        QueueCount = 0
    end
    return processed
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
    if fieldType == FIELD_PLAYER then
        return chunk.playerGenerator or 0
    elseif fieldType == FIELD_BASE then
        return chunk.playerBaseGenerator or 0
    elseif fieldType == FIELD_ENEMY then
        return ChunkPropertyUtils.getEnemyStructureCount(chunk) * ENEMY_MULTIPLIER
    elseif fieldType == FIELD_RESOURCE then
        local generator = chunk.resourceGenerator or 0
        if generator > 0 then
            return require("MathUtils").linearInterpolation(generator, 15000, 20000)
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

function Runtime.solveField(fieldType, opts)
    local field, _, _, _, work = Runtime.solveFieldWithMetadata(fieldType, opts)
    return field, work
end

function Runtime.fieldMetadata(fieldType)
    local key = Runtime.fieldKey(fieldType)
    return FieldTopSource[key], FieldSecond[key], FieldSecondSource[key]
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

function Runtime.propagateIncreases(sources, fieldType, opts)
    local key = Runtime.fieldKey(fieldType)
    local field = Universe.sourceVersionedFields[key]
    return Solver.propagateIncreasesWithMetadata(
        sources, field, FieldTopSource[key], FieldSecond[key], FieldSecondSource[key],
        Runtime.neighborOf, opts)
end

function Runtime.recompute(changedSources, fieldType, field, opts)
    local sources = Runtime.buildSources(fieldType)
    return Solver.recompute(changedSources, sources, field, Runtime.neighborOf, opts)
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
