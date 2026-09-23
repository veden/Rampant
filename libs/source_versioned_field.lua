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


-- Solve while tracking the top two contributions per chunk. The second
-- contribution is what makes a source decrease locally removable without a
-- full solve. Returns field, topSource, second, secondSource, work.
function Solver.solveWithMetadata(sources, neighborOf, opts)
    local o = options(opts)
    local field = {}
    local topSource = {}
    local second = {}
    local secondSource = {}
    local heap = {}
    local work = 0

    local function relax(nid, cand, src, d)
        local top = field[nid] or 0
        local sec = second[nid] or 0
        if cand > top + o.eps then
            if src ~= topSource[nid] then
                second[nid] = top
                secondSource[nid] = topSource[nid]
            end
            field[nid] = cand
            topSource[nid] = src
            heapPush(heap, { id = nid, v = cand, d = d, src = src })
            work = work + 1
        elseif src ~= topSource[nid] and cand > sec + o.eps then
            second[nid] = cand
            secondSource[nid] = src
            heapPush(heap, { id = nid, v = cand, d = d, src = src, second = true })
            work = work + 1
        end
    end

    for _, source in ipairs(sources) do
        local v = source.gen * o.rating
        if v > (field[source.id] or 0) then
            field[source.id] = v
            topSource[source.id] = source.id
            heapPush(heap, { id = source.id, v = v, d = 0, src = source.id })
        end
    end
    while #heap > 0 do
        local node = heapPop(heap)
        if not (o.maxRadius and node.d >= o.maxRadius) then
            for _, nid in ipairs(neighborOf(node.id)) do
                relax(nid, node.v * o.decay, node.src, node.d + 1)
            end
        end
    end
    return field, topSource, second, secondSource, work
end

-- Increase propagation while maintaining the top-two metadata.
function Solver.propagateIncreaseWithMetadata(source, field, topSource, second, secondSource, neighborOf, opts)
    local o = options(opts)
    local heap = {}
    local work = 0
    local v = source.gen * o.rating
    local top = field[source.id] or 0
    local sec = second[source.id] or 0
    if v > top + o.eps then
        if source.id ~= topSource[source.id] then
            second[source.id] = top
            secondSource[source.id] = topSource[source.id]
        end
        field[source.id] = v
        topSource[source.id] = source.id
        heapPush(heap, { id = source.id, v = v, d = 0, src = source.id })
        work = work + 1
    elseif source.id ~= topSource[source.id] and v > sec + o.eps then
        second[source.id] = v
        secondSource[source.id] = source.id
        heapPush(heap, { id = source.id, v = v, d = 0, src = source.id, second = true })
        work = work + 1
    end
    while #heap > 0 do
        local node = heapPop(heap)
        local nid = node.id
        local topValue = field[nid] or 0
        local secValue = second[nid] or 0
        local stillTop = (not node.second) and node.v >= topValue
        local stillSecond = node.second and node.v >= secValue and node.src ~= topSource[nid]
        if (stillTop or stillSecond) and not (o.maxRadius and node.d >= o.maxRadius) then
            for _, neighborId in ipairs(neighborOf(nid)) do
                local cand = node.v * o.decay
                local neighborTop = field[neighborId] or 0
                local neighborSecond = second[neighborId] or 0
                if (not node.second) and cand > neighborTop + o.eps then
                    if node.src ~= topSource[neighborId] then
                        second[neighborId] = neighborTop
                        secondSource[neighborId] = topSource[neighborId]
                    end
                    field[neighborId] = cand
                    topSource[neighborId] = node.src
                    heapPush(heap, { id = neighborId, v = cand, d = node.d + 1, src = node.src })
                    work = work + 1
                elseif node.src ~= topSource[neighborId] and cand > neighborSecond + o.eps then
                    second[neighborId] = cand
                    secondSource[neighborId] = node.src
                    heapPush(heap, { id = neighborId, v = cand, d = node.d + 1, src = node.src, second = true })
                    work = work + 1
                end
            end
        end
    end
    return work
end

-- Multi-source increase propagation. Exact for a set of increases into
-- an existing field: each source is seeded once and the heap is drained once.
function Solver.propagateIncreasesWithMetadata(sources, field, topSource, second, secondSource, neighborOf, opts)
    local o = options(opts)
    local heap = {}
    local work = 0
    for _, source in ipairs(sources) do
        local id = source.id
        local v = source.gen * o.rating
        local top = field[id] or 0
        local sec = second[id] or 0
        if v > top + o.eps then
            if id ~= topSource[id] then
                second[id] = top
                secondSource[id] = topSource[id]
            end
            field[id] = v
            topSource[id] = id
            heapPush(heap, { id = id, v = v, d = 0, src = id })
            work = work + 1
        elseif id ~= topSource[id] and v > sec + o.eps then
            second[id] = v
            secondSource[id] = id
            heapPush(heap, { id = id, v = v, d = 0, src = id, second = true })
            work = work + 1
        end
    end
    while #heap > 0 do
        local node = heapPop(heap)
        local nid = node.id
        local topValue = field[nid] or 0
        local secValue = second[nid] or 0
        local stillTop = (not node.second) and node.v >= topValue
        local stillSecond = node.second and node.v >= secValue and node.src ~= topSource[nid]
        if (stillTop or stillSecond) and not (o.maxRadius and node.d >= o.maxRadius) then
            for _, neighborId in ipairs(neighborOf(nid)) do
                local cand = node.v * o.decay
                local neighborTop = field[neighborId] or 0
                local neighborSecond = second[neighborId] or 0
                if (not node.second) and cand > neighborTop + o.eps then
                    if node.src ~= topSource[neighborId] then
                        second[neighborId] = neighborTop
                        secondSource[neighborId] = topSource[neighborId]
                    end
                    field[neighborId] = cand
                    topSource[neighborId] = node.src
                    heapPush(heap, { id = neighborId, v = cand, d = node.d + 1, src = node.src })
                    work = work + 1
                elseif node.src ~= topSource[neighborId] and cand > neighborSecond + o.eps then
                    second[neighborId] = cand
                    secondSource[neighborId] = node.src
                    heapPush(heap, { id = neighborId, v = cand, d = node.d + 1, src = node.src, second = true })
                    work = work + 1
                end
            end
        end
    end
    return work
end

-- Remove an old source contribution locally. Exact when the top-two
-- metadata is exact: a source that is neither top nor second at a chunk
-- cannot become top or second farther along the decay path.
function Solver.removeSource(source, field, topSource, second, secondSource, neighborOf, opts)
    local o = options(opts)
    local heap = {}
    local v = source.gen * o.rating
    local work = 0
    if v > o.eps then
        heapPush(heap, { id = source.id, v = v, d = 0, src = source.id })
    end
    while #heap > 0 do
        local node = heapPop(heap)
        if not (o.maxRadius and node.d >= o.maxRadius) then
            for _, nid in ipairs(neighborOf(node.id)) do
                local cand = node.v * o.decay
                if cand > o.eps then
                    local isTop = topSource[nid] == node.src
                    local isSecond = secondSource[nid] == node.src
                    if isTop then
                        field[nid] = second[nid] or 0
                        topSource[nid] = secondSource[nid]
                        second[nid] = 0
                        secondSource[nid] = nil
                        heapPush(heap, { id = nid, v = cand, d = node.d + 1, src = node.src })
                        work = work + 1
                    elseif isSecond then
                        second[nid] = 0
                        secondSource[nid] = nil
                        heapPush(heap, { id = nid, v = cand, d = node.d + 1, src = node.src })
                        work = work + 1
                    end
                end
            end
        end
    end
    return work
end


return Solver
