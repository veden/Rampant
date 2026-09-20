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

local Runtime = {}

local Universe
local NeighborIds = {}

local FIELD_PLAYER = Constants.PLAYER_PHEROMONE
local FIELD_BASE = Constants.BASE_PHEROMONE
local FIELD_ENEMY = Constants.ENEMY_PHEROMONE
local FIELD_RESOURCE = Constants.RESOURCE_PHEROMONE
local ENEMY_MULTIPLIER = Constants.ENEMY_PHEROMONE_MULTIPLER

function Runtime.init(universe)
    Universe = universe
end

-- Reusable neighbour-id table. Solver consumes it immediately.
function Runtime.neighborOf(chunkId)
    local chunk = MapUtils.getChunkById(chunkId, Universe)
    if not chunk then return {} end
    local neighbors = MapUtils.getNeighborChunks(chunk.map, chunk.x, chunk.y)
    local n = 0
    for i = 1, 8 do
        local neighbor = neighbors[i]
        if neighbor and neighbor ~= -1 then
            n = n + 1
            NeighborIds[n] = neighbor.id
        end
    end
    for i = n + 1, #NeighborIds do
        NeighborIds[i] = nil
    end
    return NeighborIds
end

function Runtime.sourceValue(chunk, fieldType)
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

function Runtime.solveField(fieldType, opts)
    local sources = Runtime.buildSources(fieldType)
    return Solver.solve(sources, Runtime.neighborOf, opts)
end

function Runtime.propagateIncrease(chunk, fieldType, field, opts)
    local source = { id = chunk.id, gen = Runtime.sourceValue(chunk, fieldType) }
    return Solver.propagateIncrease(source, field, Runtime.neighborOf, opts)
end

function Runtime.recompute(changedSources, fieldType, field, opts)
    local sources = Runtime.buildSources(fieldType)
    return Solver.recompute(changedSources, sources, field, Runtime.neighborOf, opts)
end

function Runtime.read(field, chunkId, tick, lastRefresh, decay)
    local value = field[chunkId] or 0
    if value == 0 then return 0 end
    local elapsed = tick - (lastRefresh or 0)
    return value * (decay ^ elapsed)
end

return Runtime
