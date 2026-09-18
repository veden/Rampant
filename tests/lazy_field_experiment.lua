--[[
    Read-time (lazy) field derivation vs write-time (diffusion) field.

    Same math: field(c) = max over sources s of ( gen(s) * decay^dist(c,s) * rating ).
    The write-time model iterates the diffusion to convergence (the sweep's job).
    The read-time model derives a chunk on demand with a decay-bounded flood.

    This test (a) proves the two agree, and (b) measures the WORK each does —
    the read-time cost is per READ, the write-time cost is per CHUNK per PASS.
]]

local W, H = 24, 24
local DECAY = 0.9
local EPS = 0.0005
local RATING = 1.0

local function id(x, y) return x * 1000 + y end

local chunks = {}
for x = 1, W do
    for y = 1, H do
        chunks[id(x, y)] = { gen = 0, field = 0, x = x, y = y }
    end
end

local sources = {}
local function addSource(x, y, g)
    local c = chunks[id(x, y)]
    c.gen = g
    sources[#sources + 1] = c
end
addSource(5, 5, 100)
addSource(12, 10, 60)
addSource(8, 18, 40)
addSource(20, 20, 80)

-- WRITE-TIME: the diffusion, iterated to convergence (the sweep's job)
local function writeTime(passes)
    local work = 0
    for _ = 1, passes do
        for x = 1, W do
            for y = 1, H do
                local c = chunks[id(x, y)]
                local maxN = 0
                for dx = -1, 1 do
                    for dy = -1, 1 do
                        if not (dx == 0 and dy == 0) then
                            local nx, ny = x + dx, y + dy
                            if nx >= 1 and nx <= W and ny >= 1 and ny <= H then
                                local n = chunks[id(nx, ny)]
                                if n.field > maxN then maxN = n.field end
                            end
                        end
                    end
                end
                c.field = math.max(c.gen, maxN * DECAY) * RATING
                work = work + 1
            end
        end
    end
    return work
end

-- READ-TIME: decay-bounded flood from the read chunk, pruned by EPS
local function readTime(x0, y0)
    local best = 0
    local visits = 0
    local seen = {}
    local queue = { { x = x0, y = y0, decay = 1 } }
    local head = 1
    while head <= #queue do
        local node = queue[head]
        head = head + 1
        local key = node.x * 1000 + node.y
        if not seen[key] then
            seen[key] = true
            visits = visits + 1
            local c = chunks[key]
            local v = c.gen * node.decay * RATING
            if v > best then best = v end
            for dx = -1, 1 do
                for dy = -1, 1 do
                    if not (dx == 0 and dy == 0) then
                        local nx, ny = node.x + dx, node.y + dy
                        if nx >= 1 and nx <= W and ny >= 1 and ny <= H then
                            local nd = node.decay * DECAY
                            if nd > EPS then
                                queue[#queue + 1] = { x = nx, y = ny, decay = nd }
                            end
                        end
                    end
                end
            end
        end
    end
    return best, visits
end

-- converge the write-time field
local PASSES = 40
local writeWork = writeTime(PASSES)

-- compare every chunk + measure the read-time work
local maxDiff = 0
local totalVisits = 0
local reads = 0
for x = 1, W do
    for y = 1, H do
        local rt, visits = readTime(x, y)
        reads = reads + 1
        totalVisits = totalVisits + visits
        local wt = chunks[id(x, y)].field
        local d = math.abs(rt - wt)
        if d > maxDiff then maxDiff = d end
    end
end

print(string.format("grid: %dx%d = %d chunks, %d sources, %d write-time passes",
    W, H, W * H, #sources, PASSES))
print(string.format("write-time work: %d chunk-updates (%.1f per chunk)",
    writeWork, writeWork / (W * H)))
print(string.format("read-time work:  %d chunk-visits over %d reads (%.1f per read)",
    totalVisits, reads, totalVisits / reads))
print(string.format("max |lazy - stored| = %.6f", maxDiff))
print(string.format("read-time total for %d reads vs write-time for 1 pass: %d vs %d",
    reads, totalVisits, W * H))

--[[ INDEXED read-time: visit the SOURCES within the decay range, not the grid.
     range = ceil(log(EPS)/log(DECAY)); the read is O(sources within range). ]]
local RANGE = math.ceil(math.log(EPS) / math.log(DECAY))
local function readIndexed(x0, y0)
    local best = 0
    local visits = 0
    for _, s in ipairs(sources) do
        local dx = math.abs(s.x - x0)
        local dy = math.abs(s.y - y0)
        local dist = math.max(dx, dy)   -- the 8-neighbourhood metric
        if dist <= RANGE then
            visits = visits + 1
            local v = s.gen * (DECAY ^ dist) * RATING
            if v > best then best = v end
        end
    end
    return best, visits
end

local idxDiff = 0
local idxVisits = 0
for x = 1, W do
    for y = 1, H do
        local rt, visits = readIndexed(x, y)
        idxVisits = idxVisits + visits
        local wt = chunks[id(x, y)].field
        local d = math.abs(rt - wt)
        if d > idxDiff then idxDiff = d end
    end
end
print("")
print(string.format("decay range at EPS=%.4f: %d hops", EPS, RANGE))
print(string.format("indexed read-time work: %d source-visits over %d reads (%.1f per read)",
    idxVisits, reads, idxVisits / reads))
print(string.format("indexed max |lazy - stored| = %.6f", idxDiff))
print(string.format("indexed total (%d reads) = %d  vs  write-time per pass = %d",
    reads, idxVisits, W * H))

--[[ RESULT (lua tests/lazy_field_experiment.lua):
  grid 24x24 = 576 chunks, 4 sources, 40 write-time passes
  write-time work:   23040 chunk-updates  (40.0 per chunk)
  naive flood read: 331776 chunk-visits  (576.0 per read)  -- O(grid), 14x WORSE
  indexed read:       2304 source-visits  (4.0 per read)   -- O(sources in range)
  max |lazy - stored| = 0.000000  -- EXACTLY equivalent

  The decay range at EPS=0.0005 is 73 hops (0.9^73 < 0.0005), which is why the
  naive flood covers the whole grid on every read. The indexed read visits only the
  sources within that range, so it is independent of grid size -- the scaling win.

  Not modelled (the implementation risks): source density (the real save's ore makes
  the static/dynamic split a prerequisite), the path-dependent ratings (a max-PATH
  search, not a distance kernel), and the index query cost.
]]
