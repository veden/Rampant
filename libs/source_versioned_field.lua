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

    There is NO time decay in these fields. A source-versioned field is maintained
    by source changes alone: a static source (ore, a structure) is solved once and
    never renewed, and a dynamic source (a base, a nest) is re-propagated when it
    changes. The H7 decay-on-read and the periodic sweep it required applied to the
    transient fields (player, death, kamikaze), which this solver does not own.

    (This comment previously read "Time decay is applied by the read path, not by
    propagation". That described the transient fields and was misread as an
    unimplemented read-time decay for these fields, which cost two rounds.)
]]

local Solver = {}

local DEFAULT_DECAY = 0.9
local DEFAULT_EPS = 0.0005
local DEFAULT_RATING = 1.0

-- Pooled heap entries. The inner loop pushes up to 8 entries per node, and a
-- table per push dominated the allocation profile (and the GC, which lands in the
-- same tick). Entries are recycled through a free list; heapPop copies the popped
-- entry into a single shared OUT record so a recycled entry can never be
-- overwritten while the caller is still reading it.
local POOL = {}
local POOL_N = 0
local OUT = {}

local function acquire(id, v, d, src, second)
    local item
    if POOL_N > 0 then
        item = POOL[POOL_N]
        POOL[POOL_N] = nil
        POOL_N = POOL_N - 1
        item.id, item.v, item.d, item.src, item.second = id, v, d, src, second
    else
        item = { id = id, v = v, d = d, src = src, second = second }
    end
    return item
end

local function heapPush(heap, id, v, d, src, second)
    local item = acquire(id, v, d, src, second)
    local n = #heap + 1
    heap[n] = item
    while n > 1 do
        local p = math.floor(n / 2)
        if heap[p].v >= heap[n].v then break end
        heap[p], heap[n] = heap[n], heap[p]
        n = p
    end
end

local function heapPop(heap)
    local top = heap[1]
    local size = #heap
    local last = heap[size]
    heap[size] = nil
    size = size - 1
    if size > 0 then
        heap[1] = last
        local i = 1
        while true do
            local l, r = i * 2, i * 2 + 1
            local largest = i
            if l <= size and heap[l].v > heap[largest].v then largest = l end
            if r <= size and heap[r].v > heap[largest].v then largest = r end
            if largest == i then break end
            heap[i], heap[largest] = heap[largest], heap[i]
            i = largest
        end
    end
    OUT.id, OUT.v, OUT.d, OUT.src, OUT.second = top.id, top.v, top.d, top.src, top.second
    -- recycle
    POOL_N = POOL_N + 1
    POOL[POOL_N] = top
    return OUT
end

-- Normalise options IN PLACE when a table is supplied. The solver is called
-- many times per tick with the same per-field options, and returning a fresh
-- defaults table each call was 4KB/tick of garbage from the removal path alone.
-- maxWork is per-call, so callers set it before each call.
local function options(opts)
    if not opts then
        return {
            decay = DEFAULT_DECAY,
            eps = DEFAULT_EPS,
            rating = DEFAULT_RATING,
        }
    end
    if opts.decay == nil then opts.decay = DEFAULT_DECAY end
    if opts.eps == nil then opts.eps = DEFAULT_EPS end
    if opts.rating == nil then opts.rating = DEFAULT_RATING end
    return opts
end

-- Provenance packing: the id of the source supplying the maximum and the id
-- supplying the runner-up share one table entry. Both are chunk ids, so
-- top * 2^25 + second is exact in a double for ids below 33.5M. This replaces two
-- tables of ~107k entries (top, secondSource) with one.
local mFloor = math.floor
-- 2^26 is the widest base that keeps the packed pair exact in a double:
-- (2^26-1)*2^26 + (2^26-1) = 4.50e15, under the 2^53 = 9.01e15 exact-integer
-- limit. Ids are sequential and never reused, so this bounds chunk creations per
-- session (67.1M), not concurrent chunks.
local PROV_SCALE = 67108864
local PROV_INV = 1 / 67108864

local function pTop(prov, id)
    local p = prov[id]
    if not p then return 0 end
    return mFloor(p * PROV_INV)
end

local function pSec(prov, id)
    local p = prov[id]
    if not p then return 0 end
    return p - mFloor(p * PROV_INV) * PROV_SCALE
end

local function pCheck(id)
    if id >= PROV_SCALE then
        error("chunk id " .. id .. " exceeds the provenance packing limit of "
            .. PROV_SCALE .. "; widen PROV_SCALE")
    end
end

local function pSetTop(prov, id, top)
    local p = prov[id]
    local second = 0
    if p then
        second = p - mFloor(p * PROV_INV) * PROV_SCALE
    end
    pCheck(top)
    pCheck(top)
    prov[id] = top * PROV_SCALE + second
end

local function pSetSec(prov, id, second)
    local p = prov[id]
    local top = 0
    if p then
        top = mFloor(p * PROV_INV)
    end
    pCheck(top)
    prov[id] = top * PROV_SCALE + second
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
            heapPush(heap, source.id, v, 0)
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
                        heapPush(heap, nid, cand, node.d + 1)
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
        heapPush(heap, source.id, v, 0)
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
                        heapPush(heap, nid, cand, node.d + 1)
                        work = work + 1
                    end
                end
            end
        end
    end
    return work
end

-- Solve while tracking the top two contributions per chunk. The second
-- contribution is what makes a source decrease locally removable without a
-- full solve. Returns field, topSource, second, secondSource, work.
function Solver.solveWithMetadata(sources, neighborOf, opts)
    local o = options(opts)
    local field = {}
    local prov = {}
    local second = {}
    local heap = {}
    local work = 0

    local function relax(nid, cand, src, d)
        local top = field[nid] or 0
        local sec = second[nid] or 0
        if cand > top + o.eps then
            if src ~= pTop(prov, nid) then
                second[nid] = top
                pSetSec(prov, nid, pTop(prov, nid))
            end
            field[nid] = cand
            pSetTop(prov, nid, src)
            heapPush(heap, nid, cand, d, src)
            work = work + 1
        elseif src ~= pTop(prov, nid) and cand > sec + o.eps then
            second[nid] = cand
            pSetSec(prov, nid, src)
            heapPush(heap, nid, cand, d, src, true)
            work = work + 1
        end
    end

    for _, source in ipairs(sources) do
        local v = source.gen * o.rating
        if v > (field[source.id] or 0) then
            field[source.id] = v
            pSetTop(prov, source.id, source.id)
            heapPush(heap, source.id, v, 0, source.id)
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
    return field, prov, second, work
end

-- Increase propagation while maintaining the top-two metadata.
function Solver.propagateIncreaseWithMetadata(source, field, prov, second, neighborOf, opts)
    local o = options(opts)
    local heap = {}
    local work = 0
    local v = source.gen * o.rating
    local top = field[source.id] or 0
    local sec = second[source.id] or 0
    if v > top + o.eps then
        if source.id ~= pTop(prov, source.id) then
            second[source.id] = top
            pSetSec(prov, source.id, pTop(prov, source.id))
        end
        field[source.id] = v
        pSetTop(prov, source.id, source.id)
        heapPush(heap, source.id, v, 0, source.id)
        work = work + 1
    elseif source.id ~= pTop(prov, source.id) and v > sec + o.eps then
        second[source.id] = v
        pSetSec(prov, source.id, source.id)
        heapPush(heap, source.id, v, 0, source.id, true)
        work = work + 1
    end
    -- Runs to completion: this path returns no resumable frontier, so bounding it
    -- would strand a partially propagated field. The multi-source
    -- propagateIncreasesWithMetadata is the bounded, resumable one.
    while #heap > 0 do
        local node = heapPop(heap)
        local nid = node.id
        local topValue = field[nid] or 0
        local secValue = second[nid] or 0
        local stillTop = (not node.second) and node.v >= topValue
        local stillSecond = node.second and node.v >= secValue and node.src ~= pTop(prov, nid)
        if (stillTop or stillSecond) and not (o.maxRadius and node.d >= o.maxRadius) then
            for _, neighborId in ipairs(neighborOf(nid)) do
                local cand = node.v * o.decay
                local neighborTop = field[neighborId] or 0
                local neighborSecond = second[neighborId] or 0
                if (not node.second) and cand > neighborTop + o.eps then
                    if node.src ~= pTop(prov, neighborId) then
                        second[neighborId] = neighborTop
                        pSetSec(prov, neighborId, pTop(prov, neighborId))
                    end
                    field[neighborId] = cand
                    pSetTop(prov, neighborId, node.src)
                    heapPush(heap, neighborId, cand, node.d + 1, node.src)
                    work = work + 1
                elseif node.src ~= pTop(prov, neighborId) and cand > neighborSecond + o.eps then
                    second[neighborId] = cand
                    pSetSec(prov, neighborId, node.src)
                    heapPush(heap, neighborId, cand, node.d + 1, node.src, true)
                    work = work + 1
                end
            end
        end
    end
    return work, heap
end

-- Multi-source increase propagation. Exact for a set of increases into an
-- existing field: each source is seeded once and the heap is drained once.
--
-- `heap` may be a frontier left over from a previous call, and `opts.maxWork`
-- bounds the relaxations this call performs. Re-seeding is idempotent: a source
-- already at its value fails the `v > top + eps` test and pushes nothing, so a
-- resumed call can re-seed safely. Returns work, heap; a non-empty heap means the
-- propagation is incomplete and must be resumed.
-- Indexed heap for the resumable propagator.
--
-- One entry per chunk per side, held in parallel arrays that are reused every
-- tick, with `pos[id]` giving the slot. An improvement updates in place; there
-- are no node tables, no pool, no duplicate entries and no frontier that can
-- grow with activity. The old lazy-deletion heap allocated one table per
-- improvement (310 per tick) and accumulated 584k entries in a live game.
local function newIndexedHeap()
    return { id = {}, v = {}, d = {}, src = {}, pos = {}, n = 0 }
end

local function ihPush(h, id, v, d, src)
    local p = h.pos[id]
    if p then
        if v <= h.v[p] then return false end
        h.v[p], h.d[p], h.src[p] = v, d, src
    else
        p = h.n + 1
        h.n = p
        h.id[p], h.v[p], h.d[p], h.src[p] = id, v, d, src
        h.pos[id] = p
    end
    while p > 1 do
        local parent = math.floor(p / 2)
        if h.v[parent] >= h.v[p] then break end
        h.id[parent], h.id[p] = h.id[p], h.id[parent]
        h.v[parent], h.v[p] = h.v[p], h.v[parent]
        h.d[parent], h.d[p] = h.d[p], h.d[parent]
        h.src[parent], h.src[p] = h.src[p], h.src[parent]
        h.pos[h.id[parent]] = parent
        h.pos[h.id[p]] = p
        p = parent
    end
    return true
end

local function ihPop(h)
    local n = h.n
    if n == 0 then return nil end
    local id, v, d, src = h.id[1], h.v[1], h.d[1], h.src[1]
    h.pos[id] = nil
    if n == 1 then
        h.id[1], h.v[1], h.d[1], h.src[1] = nil, nil, nil, nil
        h.n = 0
        return id, v, d, src
    end
    h.id[1], h.v[1], h.d[1], h.src[1] = h.id[n], h.v[n], h.d[n], h.src[n]
    h.id[n], h.v[n], h.d[n], h.src[n] = nil, nil, nil, nil
    h.n = n - 1
    h.pos[h.id[1]] = 1
    local i = 1
    local size = h.n
    while true do
        local l, r = i * 2, i * 2 + 1
        local largest = i
        if l <= size and h.v[l] > h.v[largest] then largest = l end
        if r <= size and h.v[r] > h.v[largest] then largest = r end
        if largest == i then break end
        h.id[i], h.id[largest] = h.id[largest], h.id[i]
        h.v[i], h.v[largest] = h.v[largest], h.v[i]
        h.d[i], h.d[largest] = h.d[largest], h.d[i]
        h.src[i], h.src[largest] = h.src[largest], h.src[i]
        h.pos[h.id[i]] = i
        h.pos[h.id[largest]] = largest
        i = largest
    end
    return id, v, d, src
end

function Solver.newIncrementalHeap()
    return newIndexedHeap()
end

function Solver.propagateIncreasesWithMetadata(sources, field, prov, second, neighborOf, opts, hTop, hSec)
    local o = options(opts)
    hTop = hTop or newIndexedHeap()
    hSec = hSec or newIndexedHeap()
    local maxWork = o.maxWork
    -- Hoist the option lookups and the tables the inner loop touches: the
    -- neighbour loop runs ~8 times per popped node and table indexing there was
    -- a measurable share of sourceFields' cost.
    local decay = o.decay
    local eps = o.eps
    local maxRadius = o.maxRadius
    local work = 0
    for _, source in ipairs(sources) do
        local id = source.id
        local v = source.gen * o.rating
        local top = field[id] or 0
        local sec = second[id] or 0
        if v > top + eps then
            if id ~= pTop(prov, id) then
                second[id] = top
                pSetSec(prov, id, pTop(prov, id))
            end
            field[id] = v
            pSetTop(prov, id, id)
            ihPush(hTop, id, v, 0, id)
            work = work + 1
        elseif id ~= pTop(prov, id) and v > sec + eps then
            second[id] = v
            pSetSec(prov, id, id)
            ihPush(hSec, id, v, 0, id)
            work = work + 1
        end
    end
    while hTop.n > 0 or hSec.n > 0 do
        if maxWork and work >= maxWork then break end
        local useSecond = hSec.n > 0 and (hTop.n == 0 or hSec.v[1] > hTop.v[1])
        local nid, v, d, src
        if useSecond then
            nid, v, d, src = ihPop(hSec)
        else
            nid, v, d, src = ihPop(hTop)
        end
        local topValue = field[nid] or 0
        local secValue = second[nid] or 0
        local stillTop = (not useSecond) and v >= topValue
        local stillSecond = useSecond and v >= secValue and src ~= pTop(prov, nid)
        if (stillTop or stillSecond) and not (maxRadius and d >= maxRadius) then
            for _, neighborId in ipairs(neighborOf(nid)) do
                local cand = v * decay
                local neighborTop = field[neighborId] or 0
                local neighborSecond = second[neighborId] or 0
                if (not useSecond) and cand > neighborTop + eps then
                    if src ~= pTop(prov, neighborId) then
                        second[neighborId] = neighborTop
                        pSetSec(prov, neighborId, pTop(prov, neighborId))
                    end
                    field[neighborId] = cand
                    pSetTop(prov, neighborId, src)
                    ihPush(hTop, neighborId, cand, d + 1, src)
                    work = work + 1
                elseif src ~= pTop(prov, neighborId) and cand > neighborSecond + eps then
                    second[neighborId] = cand
                    pSetSec(prov, neighborId, src)
                    ihPush(hSec, neighborId, cand, d + 1, src)
                    work = work + 1
                end
            end
        end
    end
    return work, hTop, hSec
end

-- Remove an old source contribution locally. Exact when the top-two
-- metadata is exact: a source that is neither top nor second at a chunk
-- cannot become top or second farther along the decay path.
function Solver.removeSource(source, field, prov, second, neighborOf, opts, heap)
    local o = options(opts)
    heap = heap or {}
    local work = 0
    if source then
        local v = source.gen * o.rating
        if v > o.eps then
            heapPush(heap, source.id, v, 0, source.id)
        end
    end
    while #heap > 0 do
        -- Bound removal too. An unbounded re-propagation from one removed source
        -- cost 25ms on a large field (measured inside sourceFields' drain); the
        -- leftover heap is returned so the caller can resume next tick.
        if o.maxWork and work >= o.maxWork then break end
        local node = heapPop(heap)
        work = work + 1
        if not (o.maxRadius and node.d >= o.maxRadius) then
            for _, nid in ipairs(neighborOf(node.id)) do
                local cand = node.v * o.decay
                if cand > o.eps then
                    local isTop = pTop(prov, nid) == node.src
                    local isSecond = pSec(prov, nid) == node.src
                    if isTop then
                        field[nid] = second[nid] or 0
                        pSetTop(prov, nid, pSec(prov, nid))
                        second[nid] = 0
                        pSetSec(prov, nid, 0)
                        heapPush(heap, nid, cand, node.d + 1, node.src)
                        work = work + 1
                    elseif isSecond then
                        second[nid] = 0
                        pSetSec(prov, nid, 0)
                        heapPush(heap, nid, cand, node.d + 1, node.src)
                        work = work + 1
                    end
                end
            end
        end
    end
    return work, heap
end


return Solver
