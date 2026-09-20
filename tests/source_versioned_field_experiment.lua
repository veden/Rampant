--[[
    Source-versioned field experiment.

    Compares:
      1. full iterative relaxation (the old sweep's numerical model),
      2. static/dynamic multi-source max-propagation (Dijkstra-like),
      3. local event propagation after a dynamic source increase.

    This is a pure Lua prototype: no Factorio dependencies.
]]

local W, H = 48, 48
local DECAY = 0.9
local EPS = 0.0005
local RATING = 1.0

local function id(x, y) return x * 1000 + y end

local chunks = {}
for x = 1, W do
    for y = 1, H do
        chunks[id(x, y)] = { x = x, y = y, gen = 0, field = 0, static = 0, dynamic = 0 }
    end
end

local staticSources = {}
local dynamicSources = {}
local function addSource(t, x, y, g)
    local c = chunks[id(x, y)]
    c.gen = c.gen + g
    t[#t + 1] = c
end

-- deterministic synthetic source placement
local seed = 12345
local function rnd()
    seed = (seed * 1103515245 + 12345) % 2147483648
    return seed / 2147483648
end
for i = 1, 300 do
    local x = 1 + math.floor(rnd() * W)
    local y = 1 + math.floor(rnd() * H)
    addSource(staticSources, x, y, 1 + math.floor(rnd() * 100))
end
for i = 1, 60 do
    local x = 1 + math.floor(rnd() * W)
    local y = 1 + math.floor(rnd() * H)
    addSource(dynamicSources, x, y, 1 + math.floor(rnd() * 100))
end

local function neighbors(x, y)
    local out = {}
    for dx = -1, 1 do
        for dy = -1, 1 do
            if not (dx == 0 and dy == 0) then
                local nx, ny = x + dx, y + dy
                if nx >= 1 and nx <= W and ny >= 1 and ny <= H then
                    out[#out + 1] = chunks[id(nx, ny)]
                end
            end
        end
    end
    return out
end

-- 1. full iterative relaxation
local function fullRelax(maxPasses, eps)
    local work = 0
    local lastDiff = 0
    for pass = 1, maxPasses do
        lastDiff = 0
        for x = 1, W do
            for y = 1, H do
                local c = chunks[id(x, y)]
                local maxN = 0
                for _, n in ipairs(neighbors(x, y)) do
                    if n.field > maxN then maxN = n.field end
                end
                local new = math.max(c.gen, maxN * DECAY) * RATING
                local d = math.abs(new - c.field)
                if d > lastDiff then lastDiff = d end
                c.field = new
                work = work + 1
            end
        end
        if lastDiff < eps then break end
    end
    return work, lastDiff
end

-- 2. multi-source max-propagation with a binary heap
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
local function maxPropagation(sources)
    local field = {}
    local heap = {}
    local work = 0
    for _, s in ipairs(sources) do
        local k = id(s.x, s.y)
        local v = s.gen * RATING
        if v > (field[k] or 0) then
            field[k] = v
            heapPush(heap, { x = s.x, y = s.y, v = v })
        end
    end
    while #heap > 0 do
        local node = heapPop(heap)
        local k = id(node.x, node.y)
        if node.v >= (field[k] or 0) then
            for _, n in ipairs(neighbors(node.x, node.y)) do
                local nk = id(n.x, n.y)
                local cand = node.v * DECAY
                if cand > (field[nk] or 0) + EPS then
                    field[nk] = cand
                    heapPush(heap, { x = n.x, y = n.y, v = cand })
                    work = work + 1
                end
            end
        end
    end
    return field, work
end

-- full reference
local fullWork, lastDiff = fullRelax(500, 1e-6)
for x = 1, W do
    for y = 1, H do
        chunks[id(x, y)].full = chunks[id(x, y)].field
        chunks[id(x, y)].field = 0
    end
end

-- static/dynamic split
local staticField, staticWork = maxPropagation(staticSources)
local dynamicField, dynamicWork = maxPropagation(dynamicSources)
local combined = {}
local maxDiff = 0
for x = 1, W do
    for y = 1, H do
        local k = id(x, y)
        local v = math.max(staticField[k], dynamicField[k])
        combined[k] = v
        local d = math.abs(v - chunks[k].full)
        if d > maxDiff then maxDiff = d end
    end
end

-- local event: increase one dynamic source
local target = dynamicSources[1]
local beforeDynamic = {}
for k, v in pairs(dynamicField) do beforeDynamic[k] = v end
target.gen = target.gen + 10
local incField, incWork = maxPropagation(dynamicSources)
local incDiff = 0
for x = 1, W do
    for y = 1, H do
        local k = id(x, y)
        local v = math.max(staticField[k], incField[k])
        local d = math.abs(v - combined[k])
        if d > incDiff then incDiff = d end
    end
end

print(string.format("grid: %dx%d = %d chunks, %d static sources, %d dynamic sources",
    W, H, W * H, #staticSources, #dynamicSources))
print(string.format("full relaxation: %d chunk-updates, final diff=%.6f", fullWork, lastDiff))
print(string.format("static+dynamic max-propagation: %d updates, max |combined-full| = %.6f",
    staticWork + dynamicWork, maxDiff))
print(string.format("dynamic source increase: %d local updates, max |new-old| = %.6f",
    incWork, incDiff))
print(string.format("one full relaxation pass = %d chunk-updates", W * H))
