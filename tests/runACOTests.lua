#!/usr/bin/env lua
-- Standalone ACO test that doesn't require full Factorio environment

print("=== ACO Implementation Tests ===\n")

-- Mock minimal globals needed

-- Capture settings from data:extend() calls (same approach as runTests.lua,
-- keeps this runner in sync with the real settings.lua shape)
local capturedSettings = {}
data = {
    extend = function(self, x)
        for _, setting in ipairs(x) do
            if setting.setting_type then
                local settingType = setting.setting_type
                if settingType == "runtime-global" then settingType = "global" end
                if not capturedSettings[settingType] then capturedSettings[settingType] = {} end
                capturedSettings[settingType][setting.name] = { value = setting.default_value }
            end
        end
    end,
}
require("settings")
settings = capturedSettings

-- Mock bit32
local c = ((2 ^ 32) - 1)
bit32 = {
    bxor = function(x, y) return (x ~ y) & c end,
    lshift = function(x, y) return (x << y) & c end,
    rshift = function(x, y) return (x >> y) & c end
}

defines = {
    wire_type = { red = "red", green = "green" },
    command = { group = "group", wander = "wander" },
    distraction = { none = "none", by_enemy = "by_enemy", by_anything = "by_anything" },
    group_state = { attacking_distraction = "attacking", attacking_target = "target", gathering = "gathering" },
    events = { on_tick = 1 }
}

-- Load Constants
package.path = "libs/?.lua;tests/?.lua;?.lua;" .. package.path
require("polyfills")
local Constants = require("libs/Constants")

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

print("Test 1: Decay Tables")
test("DECAY_090 exists", Constants.DECAY_090 ~= nil)
test("DECAY_092 exists", Constants.DECAY_092 ~= nil)
test("DECAY_095 exists", Constants.DECAY_095 ~= nil)
test("DECAY_098 exists", Constants.DECAY_098 ~= nil)
test("DECAY_TRAIL exists", Constants.DECAY_TRAIL ~= nil)
test("DECAY_090 has correct size", #Constants.DECAY_090 == Constants.DECAY_TABLE_DEPTH, "Got " .. #Constants.DECAY_090)
test("DECAY_090[100] is accurate", math.abs(Constants.DECAY_090[100] - 0.90^100) < 0.0001)
test("DECAY_098[500] is accurate", math.abs(Constants.DECAY_098[500] - 0.98^500) < 0.0001)
test("DECAY_TRAIL[100] is accurate", math.abs(Constants.DECAY_TRAIL[100] - 0.97^100) < 0.0001)

print("\nTest 2: ACO Constants")
test("TRAIL_BASE_AMOUNT = 10", Constants.TRAIL_BASE_AMOUNT == 10)
test("TRAIL_SCOUT_MULTIPLIER = 2.5", Constants.TRAIL_SCOUT_MULTIPLIER == 2.5)
test("TRAIL_WEIGHT = 0.3", Constants.TRAIL_WEIGHT == 0.3)
test("PHEROMONE_WEIGHT = 0.7", Constants.PHEROMONE_WEIGHT == 0.7)
test("TRAIL_SPREAD_FACTOR = 0.95", Constants.TRAIL_SPREAD_FACTOR == 0.95)
test("SCOUT_SELECTION_RATE = 0.07", Constants.SCOUT_SELECTION_RATE == 0.07)
test("SCOUT_REFRESH_TICKS = 600", Constants.SCOUT_REFRESH_TICKS == 600)
test("DECAY_TABLE_DEPTH = 10000", Constants.DECAY_TABLE_DEPTH == 10000)

print("\nTest 3: Decay Rate Constants")
test("DECAY_BASE = 0.90", Constants.DECAY_BASE == 0.90)
test("DECAY_GENERATOR = 0.92", Constants.DECAY_GENERATOR == 0.92)
test("DECAY_KAMIKAZE = 0.95", Constants.DECAY_KAMIKAZE == 0.95)
test("DECAY_PLAYER = 0.98", Constants.DECAY_PLAYER == 0.98)
-- DECAY_TRAIL is overwritten with its decay table at load; rate verified via DECAY_TRAIL[100]

print("\nTest 4: Refresh Intervals")
test("REFRESH_PLAYER_TICKS = 5", Constants.REFRESH_PLAYER_TICKS == 5)
test("REFRESH_STRUCTURES_TICKS = 30", Constants.REFRESH_STRUCTURES_TICKS == 30)
test("REFRESH_SPAWNERS_TICKS = 60", Constants.REFRESH_SPAWNERS_TICKS == 60)
test("REFRESH_HIVES_TICKS = 120", Constants.REFRESH_HIVES_TICKS == 120)

print("\nTest 5: Processing Budget")
test("PROCESS_DIRTY_CHUNKS_PER_TICK = 200", Constants.PROCESS_DIRTY_CHUNKS_PER_TICK == 200)

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