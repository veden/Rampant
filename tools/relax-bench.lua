-- Compare three ways to keep a max-plus pheromone field consistent:
--   dijkstra   the current solver: ordered propagation from sources, with a
--              top/second provenance ledger so a decrease can be undone locally
--   relax      Bellman recompute on a dirty frontier: value = max(own, max(nbr)*decay)
--              recomputed only where something changed, stopping at eps
--   wavefront  the original model: recompute every node in order, repeatedly, until
--              nothing changes (no frontier, no ledger)
-- Run: lua tools/relax-bench.lua [width] [sources] [decay] [eps]
local W       = tonumber(arg and arg[1]) or 128
local SOURCES = tonumber(arg and arg[2]) or 200
local DECAY   = tonumber(arg and arg[3]) or 0.9
local EPS     = tonumber(arg and arg[4]) or 5e-4
local N = W * W

local function key(x, y) return y * W + x end
local function neigh(i)
    local x, y = i % W, i // W
    local out, n = {}, 0
    for dy = -1, 1 do
        for dx = -1, 1 do
            if not (dx == 0 and dy == 0) then
                local nx, ny = x + dx, y + dy
                if nx >= 0 and nx < W and ny >= 0 and ny < W then
                    n = n + 1
                    out[n] = key(nx, ny)
                end
            end
        end
    end
    return out, n
end

local NEIGH, NNEIGH = {}, {}
for i = 0, N - 1 do NEIGH[i], NNEIGH[i] = neigh(i) end

local function makeSources()
    local src = {}
    for s = 1, SOURCES do
        local i = (s * 7919) % N
        src[i] = 1000 + (s * 37) % 9000
    end
    return src
end

-- ---------- dijkstra: ordered propagation with provenance ----------
local function heapPush(h, id, v, d, src)
    h[#h + 1] = { id = id, v = v, d = d, src = src }
    local i = #h
    while i > 1 do
        local p = i // 2
        if h[p].v >= h[i].v then break end
        h[p], h[i] = h[i], h[p]
        i = p
    end
end
local function heapPop(h)
    local top = h[1]
    local last = table.remove(h)
    if #h > 0 then
        h[1] = last
        local i = 1
        while true do
            local l, r, big = i * 2, i * 2 + 1, i
            if l <= #h and h[l].v > h[big].v then big = l end
            if r <= #h and h[r].v > h[big].v then big = r end
            if big == i then break end
            h[i], h[big] = h[big], h[i]
            i = big
        end
    end
    return top
end

local function dijkstraSolve(sources, pops)
    local field, prov, second = {}, {}, {}
    local h = {}
    for id, gen in pairs(sources) do
        field[id] = gen; prov[id] = id; heapPush(h, id, gen, 0, id)
    end
    while #h > 0 do
        local node = heapPop(h)
        pops.n = pops.n + 1
        if node.v >= (field[node.id] or 0) then
            for k = 1, NNEIGH[node.id] do
                local nid = NEIGH[node.id][k]
                local cand = node.v * DECAY
                if cand > (field[nid] or 0) + EPS then
                    if node.src ~= prov[nid] then
                        second[nid] = field[nid] or 0
                        prov[nid] = node.src
                    end
                    field[nid] = cand
                    heapPush(h, nid, cand, node.d + 1, node.src)
                end
            end
        end
    end
    return field, prov, second
end

local function dijkstraIncrease(sources, field, prov, second, id, gen, pops)
    local h = {}
    heapPush(h, id, gen, 0, id)
    while #h > 0 do
        local node = heapPop(h)
        pops.n = pops.n + 1
        for k = 1, NNEIGH[node.id] do
            local nid = NEIGH[node.id][k]
            local cand = node.v * DECAY
            if cand > (field[nid] or 0) + EPS then
                if node.src ~= prov[nid] then
                    second[nid] = field[nid] or 0
                    prov[nid] = node.src
                end
                field[nid] = cand
                heapPush(h, nid, cand, node.d + 1, node.src)
            end
        end
    end
end

local function dijkstraDecrease(sources, field, prov, second, id, oldGen, pops)
    -- Walk outward from the changed node, recomputing from own source + neighbours.
    -- This is what Solver.removeSource does in the mod, and it costs the affected
    -- disc (the ledger only lets it skip nodes that provably cannot change).
    local h = { { id = id, v = field[id] or 0, d = 0, src = id } }
    while #h > 0 do
        local node = heapPop(h)
        pops.n = pops.n + 1
        local best = sources[node.id] or 0
        for k = 1, NNEIGH[node.id] do
            local cand = (field[NEIGH[node.id][k]] or 0) * DECAY
            if cand > best then best = cand end
        end
        local old = field[node.id] or 0
        if best < old - EPS then
            field[node.id] = best
            prov[node.id] = nil
            for k = 1, NNEIGH[node.id] do heapPush(h, NEIGH[node.id][k], old * DECAY, node.d + 1, node.src) end
        end
    end
end

-- ---------- relax: Bellman recompute on a dirty frontier ----------
local function relaxSettle(sources, field, queue, dirty, recomputes)
    -- Explicit head/tail: nil-ing queue[head] makes #queue ambiguous (Lua may report
    -- the hole as the end), which silently truncated the wavefront to one ring.
    local head, tail = 1, #queue
    while head <= tail do
        local id = queue[head]; queue[head] = nil; head = head + 1
        dirty[id] = nil
        recomputes.n = recomputes.n + 1
        local best = sources[id] or 0
        for k = 1, NNEIGH[id] do
            local cand = (field[NEIGH[id][k]] or 0) * DECAY
            if cand > best then best = cand end
        end
        local old = field[id] or 0
        if best > old + EPS or best < old - EPS then
            field[id] = best
            for k = 1, NNEIGH[id] do
                local nid = NEIGH[id][k]
                if not dirty[nid] then
                    dirty[nid] = true
                    tail = tail + 1
                    queue[tail] = nid
                end
            end
        end
    end
    return tail
end

local function relaxFromScratch(sources)
    local field, queue, dirty, recomputes = {}, {}, {}, { n = 0 }
    -- Seed sources as DIRTY with no value yet, so the first recompute is a change
    -- and pushes its neighbours: seeding the final value leaves the frontier to
    -- quiesce immediately (the bug this bench had on its first run).
    for id in pairs(sources) do
        dirty[id] = true; queue[#queue + 1] = id
    end
    relaxSettle(sources, field, queue, dirty, recomputes)
    return field, recomputes.n
end

-- ---------- original: recompute every node, repeat until stable ----------
local function wavefrontSettle(sources, passes, recomputes)
    local field = {}
    for id, gen in pairs(sources) do field[id] = gen end
    for pass = 1, 200 do
        local changed = false
        for id = 0, N - 1 do
            recomputes.n = recomputes.n + 1
            local best = sources[id] or 0
            for k = 1, NNEIGH[id] do
                local cand = (field[NEIGH[id][k]] or 0) * DECAY
                if cand > best then best = cand end
            end
            if math.abs((field[id] or 0) - best) > EPS then field[id] = best; changed = true end
        end
        passes.n = pass
        if not changed then break end
    end
    return field
end

local clock = os.clock
local sources = makeSources()
local pops, recomputes, passes = { n = 0 }, { n = 0 }, { n = 0 }

local t0 = clock(); local dField, dProv, dSec = dijkstraSolve(sources, pops); local tDijkstra = clock() - t0
t0 = clock(); local rField, rOps = relaxFromScratch(sources); local tRelax = clock() - t0
t0 = clock(); local wField = wavefrontSettle(sources, passes, recomputes); local tWave = clock() - t0

local maxDiffRelax, maxDiffWave, maxVal = 0, 0, 0
for id = 0, N - 1 do
    local d = dField[id] or 0
    if d > maxVal then maxVal = d end
    local a = math.abs((rField[id] or 0) - d)
    local b = math.abs((wField[id] or 0) - d)
    if a > maxDiffRelax then maxDiffRelax = a end
    if b > maxDiffWave then maxDiffWave = b end
end

-- one increase and one decrease, at a source in the middle of the grid
local testId = (SOURCES // 2) * 7919 % N
local testGen = sources[testId]

sources[testId] = testGen + 5000
local p2 = { n = 0 }
t0 = clock(); dijkstraIncrease(sources, dField, dProv, dSec, testId, sources[testId], p2); local tIncDijkstra = clock() - t0

t0 = clock()
local rp = { n = 0 }
local q, dirty = { testId }, { [testId] = true }
relaxSettle(sources, rField, q, dirty, rp)
local tIncRelax = clock() - t0

sources[testId] = testGen
local p3 = { n = 0 }
t0 = clock(); dijkstraDecrease(sources, dField, dProv, dSec, testId, testGen + 5000, p3); local tDecDijkstra = clock() - t0

t0 = clock()
local rp2 = { n = 0 }
local q2, dirty2 = { testId }, { [testId] = true }
relaxSettle(sources, rField, q2, dirty2, rp2)
local tDecRelax = clock() - t0

print(string.format("grid %dx%d = %d chunks, %d sources, decay=%.2f eps=%g", W, W, N, SOURCES, DECAY, EPS))
print()
print(string.format("%-28s %10s %10s", "operation", "ms", "pops/recomputes"))
print(string.format("%-28s %10.2f %10d", "dijkstra full solve", tDijkstra * 1e3, pops.n))
print(string.format("%-28s %10.2f %10d", "relax full settle", tRelax * 1e3, rOps))
print(string.format("%-28s %10.2f %10d", "wavefront full settle", tWave * 1e3, recomputes.n))
print(string.format("%-28s %10.2f %10d", "dijkstra increase (1 source)", tIncDijkstra * 1e3, p2.n))
print(string.format("%-28s %10.2f %10d", "relax increase (1 source)", tIncRelax * 1e3, rp.n))
print(string.format("%-28s %10.2f %10d", "dijkstra decrease (1 source)", tDecDijkstra * 1e3, p3.n))
print(string.format("%-28s %10.2f %10d", "relax decrease (1 source)", tDecRelax * 1e3, rp2.n))
print()
print(string.format("agreement with dijkstra: relax max|diff|=%.3f (%.4f%% of peak %.0f), wavefront max|diff|=%.3f",
    maxDiffRelax, 100 * maxDiffRelax / maxVal, maxVal, maxDiffWave))
print(string.format("wavefront needed %d passes", passes.n))
