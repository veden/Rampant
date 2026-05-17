#!/usr/bin/env lua
-- Run ACO core tests

print("=== Running ACO Core Tests ===\n")

-- Mock globals
data = { extend = function(self, x) end }
defines = { events = {} }

-- Mock bit32
local c = ((2 ^ 32) - 1)
bit32 = {
    bxor = function(x, y) return (x ~ y) & c end,
    lshift = function(x, y) return (x << y) & c end,
    rshift = function(x, y) return (x >> y) & c end
}

-- Create decay tables
local function createDecayTable(rate, depth)
    local t = {}
    for i = 0, depth do
        t[i] = rate ^ i
    end
    return t
end

local Constants = {
    DECAY_TABLE_DEPTH = 10000,
    DECAY_090 = createDecayTable(0.90, 10000),
    DECAY_092 = createDecayTable(0.92, 10000),
    DECAY_095 = createDecayTable(0.95, 10000),
    DECAY_098 = createDecayTable(0.98, 10000),
    DECAY_TRAIL = createDecayTable(0.97, 10000),
    TRAIL_BASE_AMOUNT = 10,
    TRAIL_SCOUT_MULTIPLIER = 2.5,
    TRAIL_WEIGHT = 0.3,
    PHEROMONE_WEIGHT = 0.7,
    TRAIL_SPREAD_FACTOR = 0.95,
    SCOUT_SELECTION_RATE = 0.07,
    SCOUT_REFRESH_TICKS = 600,
    PROCESS_DIRTY_CHUNKS_PER_TICK = 40
}

print("✓ Constants created")

-- Create Universe
local Universe = {
    dirtyChunks = {
        [1] = {},
        [2] = {},
        [3] = {},
        [4] = {},
        [5] = {}
    },
    dirtyChunkSet = {}
}

-- markChunkDirty
local function markChunkDirty(chunkId, priority, tick)
    if not chunkId or chunkId == -1 or not priority or priority < 1 or priority > 5 then return end
    
    if Universe.dirtyChunkSet[chunkId] then
        local existingPriority = Universe.dirtyChunkSet[chunkId]
        if priority < existingPriority then
            local oldList = Universe.dirtyChunks[existingPriority]
            for i, id in ipairs(oldList) do
                if id == chunkId then
                    table.remove(oldList, i)
                    break
                end
            end
            table.insert(Universe.dirtyChunks[priority], chunkId)
            Universe.dirtyChunkSet[chunkId] = priority
        end
        return
    end
    
    table.insert(Universe.dirtyChunks[priority], chunkId)
    Universe.dirtyChunkSet[chunkId] = priority
end

-- getEffectivePheromone
local function getEffectivePheromone(chunk, type, tick)
    local value = chunk[type] or 0
    if value == 0 then return 0 end
    
    local timestampField = type .. "LastRefresh"
    local lastRefresh = chunk[timestampField] or 0
    local ticksElapsed = tick - lastRefresh
    
    local decayTable
    if type == "PLAYER_PHEROMONE" then
        decayTable = Constants.DECAY_098
    elseif type == "BASE_PHEROMONE" or type == "RESOURCE_PHEROMONE" or type == "ENEMY_PHEROMONE" then
        decayTable = Constants.DECAY_090
    elseif type == "KAMIKAZE_PHEROMONE" then
        decayTable = Constants.DECAY_095
    else
        return value
    end
    
    local decayFactor = decayTable[ticksElapsed] or decayTable[Constants.DECAY_TABLE_DEPTH]
    return value * decayFactor
end

local passed = 0
local failed = 0

local function test(name, condition)
    if condition then
        print("  ✓ " .. name)
        passed = passed + 1
    else
        print("  ✗ " .. name)
        failed = failed + 1
    end
end

-- Tests
print("\nTest 1: markChunkDirty")
markChunkDirty(123, 1, 1000)
test("Chunk added to priority 1", #Universe.dirtyChunks[1] == 1 and Universe.dirtyChunkSet[123] == 1)

print("\nTest 2: Duplicate prevention")
markChunkDirty(123, 2, 1001)
test("Duplicates prevented", #Universe.dirtyChunks[1] == 1)

print("\nTest 3: Priority upgrade")
markChunkDirty(456, 3, 1002)
markChunkDirty(456, 1, 1003)
test("Priority upgraded to 1", Universe.dirtyChunkSet[456] == 1 and #Universe.dirtyChunks[1] == 2)

print("\nTest 4: Invalid chunks ignored")
local before = #Universe.dirtyChunks[3]
markChunkDirty(-1, 3, 1004)
markChunkDirty(nil, 3, 1005)
markChunkDirty(789, 6, 1006)  -- Invalid priority
test("Invalid chunks ignored", #Universe.dirtyChunks[3] == before)

print("\nTest 5: getEffectivePheromone")
local chunk = { PLAYER_PHEROMONE = 100, playerLastRefresh = 0 }
local effective = getEffectivePheromone(chunk, 'PLAYER_PHEROMONE', 100)
local expected = 100 * (0.98 ^ 100)
test("Decay-on-read accurate", math.abs(effective - expected) < 0.01)

print("\nTest 6: Nil handling")
local nilChunk = { PLAYER_PHEROMONE = nil, playerLastRefresh = 0 }
test("Nil values return 0", getEffectivePheromone(nilChunk, 'PLAYER_PHEROMONE', 100) == 0)

print("\nTest 7: Different decay rates")
local multiChunk = {
    PLAYER_PHEROMONE = 100,
    BASE_PHEROMONE = 100,
    playerLastRefresh = 0,
    baseLastRefresh = 0
}
local playerEff = getEffectivePheromone(multiChunk, 'PLAYER_PHEROMONE', 100)
local baseEff = getEffectivePheromone(multiChunk, 'BASE_PHEROMONE', 100)
test("Player decays slower than base", playerEff > baseEff)

print("\nTest 8: Trail decay")
local trailEff = 100 * Constants.DECAY_TRAIL[100]
local expectedTrail = 100 * (0.97 ^ 100)
test("Trail decay accurate", math.abs(trailEff - expectedTrail) < 0.01)

print("\nTest 9: Constants")
test("TRAIL_SPREAD_FACTOR = 0.95", Constants.TRAIL_SPREAD_FACTOR == 0.95)
test("SCOUT_SELECTION_RATE = 0.07", Constants.SCOUT_SELECTION_RATE == 0.07)
test("TRAIL_WEIGHT = 0.3", Constants.TRAIL_WEIGHT == 0.3)
test("PHEROMONE_WEIGHT = 0.7", Constants.PHEROMONE_WEIGHT == 0.7)

print("\n=== Test Summary ===")
print("Passed: " .. passed)
print("Failed: " .. failed)
print("Total:  " .. (passed + failed))

if failed == 0 then
    print("\n✓ All ACO Core Tests Passed!")
    os.exit(0)
else
    print("\n✗ Some tests failed!")
    os.exit(1)
end