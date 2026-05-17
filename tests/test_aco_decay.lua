#!/usr/bin/env lua
-- Minimal ACO decay table test

print("=== ACO Decay Table Tests ===\n")

local passed = 0
local failed = 0

local function test(name, condition, errorMsg)
    if condition then
        print("  ✓ " .. name)
        passed = passed + 1
    else
        print("  ✗ " .. name)
        if errorMsg then print("    Error: " .. errorMsg) end
        failed = failed + 1
    end
end

-- Test decay table creation function
local function createDecayTable(rate, depth)
    local decayTable = {}
    for i = 0, depth do
        decayTable[i] = rate ^ i
    end
    return decayTable
end

local DECAY_TABLE_DEPTH = 10000

print("Test 1: Decay Table Creation")
local DECAY_090 = createDecayTable(0.90, DECAY_TABLE_DEPTH)
local DECAY_092 = createDecayTable(0.92, DECAY_TABLE_DEPTH)
local DECAY_095 = createDecayTable(0.95, DECAY_TABLE_DEPTH)
local DECAY_098 = createDecayTable(0.98, DECAY_TABLE_DEPTH)
local DECAY_TRAIL = createDecayTable(0.97, DECAY_TABLE_DEPTH)

test("DECAY_090 created", DECAY_090 ~= nil)
test("DECAY_092 created", DECAY_092 ~= nil)
test("DECAY_095 created", DECAY_095 ~= nil)
test("DECAY_098 created", DECAY_098 ~= nil)
test("DECAY_TRAIL created", DECAY_TRAIL ~= nil)

print("\nTest 2: Decay Table Size")
test("DECAY_090 has 10001 entries", #DECAY_090 == 10001, "Got " .. #DECAY_090)
test("DECAY_092 has 10001 entries", #DECAY_092 == 10001)
test("DECAY_095 has 10001 entries", #DECAY_095 == 10001)
test("DECAY_098 has 10001 entries", #DECAY_098 == 10001)
test("DECAY_TRAIL has 10001 entries", #DECAY_TRAIL == 10001)

print("\nTest 3: Decay Calculations")
local decay100 = DECAY_090[100]
local expected100 = 0.90 ^ 100
test("DECAY_090[100] accurate", math.abs(decay100 - expected100) < 0.0001,
     "Got " .. decay100 .. " expected " .. expected100)

local decay500 = DECAY_098[500]
local expected500 = 0.98 ^ 500
test("DECAY_098[500] accurate", math.abs(decay500 - expected500) < 0.0001,
     "Got " .. decay500 .. " expected " .. expected500)

local trail100 = DECAY_TRAIL[100]
local expectedTrail100 = 0.97 ^ 100
test("DECAY_TRAIL[100] accurate", math.abs(trail100 - expectedTrail100) < 0.0001,
     "Got " .. trail100 .. " expected " .. expectedTrail100)

print("\nTest 4: Decay Behavior")
test("DECAY_090[0] = 1.0", math.abs(DECAY_090[0] - 1.0) < 0.0001)
test("DECAY_090[1] = 0.90", math.abs(DECAY_090[1] - 0.90) < 0.0001)
test("DECAY_090[1000] < 0.0001", DECAY_090[1000] < 0.0001)
test("DECAY_098[1000] > 0.0001", DECAY_098[1000] > 0.0001)
test("DECAY_TRAIL[500] ≈ 0.0002", math.abs(DECAY_TRAIL[500] - 0.0002) < 0.0001)

print("\nTest 5: Trail Decay Properties")
test("Trail decays faster than player", DECAY_TRAIL[100] < DECAY_098[100])
test("Trail decays slower than base", DECAY_TRAIL[100] > DECAY_090[100])
test("Trail half-life ≈ 23 ticks", math.abs(DECAY_TRAIL[23] - 0.5) < 0.05)

print("\n=== Test Summary ===")
print("Passed: " .. passed)
print("Failed: " .. failed)
print("Total:  " .. (passed + failed))

if failed == 0 then
    print("\n✓ All tests passed!")
    os.exit(0)
else
    print("\n✗ Some tests failed!")
    os.exit(1)
end