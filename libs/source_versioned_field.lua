-- Copyright (C) 2022  veden
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

--[[
    Source-versioned pheromone field solver.

    Pure module. No Factorio dependencies.

    The field is decomposed into:
      * static sources: solved once and never renewed;
      * dynamic sources: updated only when a source changes.

    Propagation is a max-plus Dijkstra:
      value(c) = max over sources s of source(s) * decay^distance(c, s)

    Time decay is applied by the read path, not by propagation.
]]

local Solver = {}

local DEFAULT_DECAY = 0.9
local DEFAULT_EPS = 0.0005
local DEFAULT_RATING = 1.0

local function heapPush(heap, item)
    heap[#heap + 1] = item
    local i = #heap
    while i > 1 do
        local p = math.floor(i / 2)
        if heap[p].v >= heap[i].v then break end
        heap[p], heap[i] = heap[i], heap[p]
        i = p
    end
end

local function heapPop(heap)
    local top = heap[1]
    local last = heap[#heap]
    heap[#heap] = nil
    if #heap > 0 then
        heap[1] = last
        local i = 1
        while true do
            local l, r = i * 2, i * 2 + 1
            local largest = i
            if l <= #heap and heap[l].v > heap[largest].v then largest = l end
            if r <= #heap and heap[r].v > heap[largest].v then largest = r end
            if largest == i then break end
            heap[i], heap[largest] = heap[largest], heap[i]
            i = largest
        end
    end
    return top
end

local function options(opts)
    opts = opts or {}
    return {
        decay = opts.decay or DEFAULT_DECAY,
        eps = opts.eps or DEFAULT_EPS,
        rating = opts.rating or DEFAULT_RATING,
        maxRadius = opts.maxRadius,
    }
end

-- Solve a set of sources to convergence.
-- @param sources array of { id=, gen= }
-- @param neighborOf function(id) -> array of neighbor ids
-- @param opts { decay=, eps=, rating=, maxRadius= }
-- @return field table id -> value, work count
function Solver.solve(sources, neighborOf, opts)
    local o = options(opts)
    local field = {}
    local heap = {}
    local work = 0
    for _, source in ipairs(sources) do
        local v = source.gen * o.rating
        if v > (field[source.id] or 0) then
            field[source.id] = v
            heapPush(heap, { id = source.id, v = v, d = 0 })
        end
    end
    while #heap > 0 do
        local node = heapPop(heap)
        if node.v >= (field[node.id] or 0) then
            if not (o.maxRadius and node.d >= o.maxRadius) then
                for _, nid in ipairs(neighborOf(node.id)) do
                    local cand = node.v * o.decay
                    if cand > (field[nid] or 0) + o.eps then
                        field[nid] = cand
                        heapPush(heap, { id = nid, v = cand, d = node.d + 1 })
                        work = work + 1
                    end
                end
            end
        end
    end
    return field, work
end

-- Propagate an increase from one source into an existing field.
-- Exact for increases: existing contributions from other sources remain valid.
-- @return work count
function Solver.propagateIncrease(source, field, neighborOf, opts)
    local o = options(opts)
    local heap = {}
    local work = 0
    local v = source.gen * o.rating
    if v > (field[source.id] or 0) + o.eps then
        field[source.id] = v
        heapPush(heap, { id = source.id, v = v, d = 0 })
        work = work + 1
    end
    while #heap > 0 do
        local node = heapPop(heap)
        if node.v >= (field[node.id] or 0) then
            if not (o.maxRadius and node.d >= o.maxRadius) then
                for _, nid in ipairs(neighborOf(node.id)) do
                    local cand = node.v * o.decay
                    if cand > (field[nid] or 0) + o.eps then
                        field[nid] = cand
                        heapPush(heap, { id = nid, v = cand, d = node.d + 1 })
                        work = work + 1
                    end
                end
            end
        end
    end
    return work
end

-- Decrease fallback: recompute a bounded region around changed sources.
-- For now this is a full solve; the local-window version is the next step.
function Solver.recompute(changedSources, allSources, field, neighborOf, opts)
    return Solver.solve(allSources, neighborOf, opts)
end

return Solver
