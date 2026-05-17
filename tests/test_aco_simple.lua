#!/usr/bin/env lua
-- Simple standalone test for ACO decay tables

-- Mock settings
settings = { 
    startup = { 
        ["rampant--enemySeed"] = { value = 12345 },
        ["rampant--newEnemyVariations"] = { value = false }
    }, 
    global = {} 
}

-- Mock bit32
local c = ((2 ^ 32) - 1)
bit32 = {
    bxor = function(x, y) return (x ~ y) & c end,
    lshift = function(x, y) return (x << y) & c end,
    rshift = function(x, y) return (x >> y) & c end
}

-- Load Constants
package.path = "libs/?.lua;?.lua;" .. package.path
local Constants = require("libs/Constants")

print("=== ACO Decay Table Tests ===\n")

-- Test 1: Decay table creation
print("Test 1: Decay tables created correctly")
assert(Constants.DECAY_090, "DECAY_090 table should exist")
assert(Constants.DECAY_092, "DECAY_092 table should exist")
assert(Constants.DECAY_095, "DECAY_095 table should exist")
assert(Constants.DECAY_098, "DECAY_098 table should exist")
assert(Constants.DECAY_TRAIL, "DECAY_TRAIL table should exist")
print("  ✓ All decay tables exist")

-- Test 2: Table size
print("\nTest 2: Decay table size")
assert(#Constants.DECAY_090 == 10001, "DECAY_090 should have 10001 entries")
print("  ✓ DECAY_090 has " .. #Constants.DECAY_090 .. " entries")

-- Test 3: Decay calculations
print("\nTest 3: Decay calculations")
local decay100 = Constants.DECAY_090[100]
local expected100 = 0.90 ^ 100
local error100 = math.abs(decay100 - expected100)
assert(error100 < 0.0001, "DECAY_090[100] error too large: " .. error100)
print("  ✓ DECAY_090[100] = " .. string.format("%.10f", decay100) .. " (expected " .. string.format("%.10f", expected100) .. ")")

local decay500 = Constants.DECAY_098[500]
local expected500 = 0.98 ^ 500
local error500 = math.abs(decay500 - expected500)
assert(error500 < 0.0001, "DECAY_098[500] error too large: " .. error500)
print("  ✓ DECAY_098[500] = " .. string.format("%.10f", decay500) .. " (expected " .. string.format("%.10f", expected500) .. ")")

-- Test 4: Trail decay
print("\nTest 4: Trail decay")
local trail100 = Constants.DECAY_TRAIL[100]
local expectedTrail100 = 0.97 ^ 100
local errorTrail = math.abs(trail100 - expectedTrail100)
assert(errorTrail < 0.0001, "DECAY_TRAIL[100] error too large: " .. errorTrail)
print("  ✓ DECAY_TRAIL[100] = " .. string.format("%.10f", trail100) .. " (expected " .. string.format("%.10f", expectedTrail100) .. ")")

-- Test 5: Constants
print("\nTest 5: ACO constants")
assert(Constants.TRAIL_BASE_AMOUNT == 10, "TRAIL_BASE_AMOUNT should be 10")
assert(Constants.TRAIL_SCOUT_MULTIPLIER == 2.5, "TRAIL_SCOUT_MULTIPLIER should be 2.5")
assert(Constants.TRAIL_WEIGHT == 0.3, "TRAIL_WEIGHT should be 0.3")
assert(Constants.PHEROMONE_WEIGHT == 0.7, "PHEROMONE_WEIGHT should be 0.7")
assert(Constants.TRAIL_SPREAD_FACTOR == 0.95, "TRAIL_SPREAD_FACTOR should be 0.95")
assert(Constants.SCOUT_SELECTION_RATE == 0.07, "SCOUT_SELECTION_RATE should be 0.07")
print("  ✓ All ACO constants correct")

print("\n=== All Tests Passed! ===")