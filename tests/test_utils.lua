-- Unit tests for Utils module

local TestCollection = require("tests/framework")

local my_collection = TestCollection:new()

-- Load Utils (no init needed - Utils doesn't have init function)
package.loaded["libs/Utils"] = nil
local Utils = require("libs/Utils")

-- Test isRampantSetting with rampant prefix
my_collection:add('isRampantSetting_rampant_prefix', function()
    assert(Utils.isRampantSetting("rampant--settingName") == true, "Expected true for ramping prefix")
end)

-- Test isRampantSetting without ramping prefix
my_collection:add('isRampantSetting_other_prefix', function()
    assert(Utils.isRampantSetting("other--settingName") == false, "Expected false for other prefix")
end)

-- Test isRampantSetting with empty string
my_collection:add('isRampantSetting_empty', function()
    assert(Utils.isRampantSetting("") == false, "Expected false for empty string")
end)

-- TODO: Implement Utils.nextEntry, Utils.consumeEntry, Utils.isCoolingDown
-- Commented out until functions are implemented
--[[
-- Test nextEntry with empty table
my_collection:add('nextEntry_empty', function()
    local cursor, item, prev = Utils.nextEntry({}, nil)
    assert(cursor == nil, "Expected nil cursor")
    assert(item == nil, "Expected nil item")
    assert(prev == nil, "Expected nil prev")
end)

-- Test nextEntry with single element (returns nil cursor because there's no second element)
my_collection:add('nextEntry_single', function()
    local t = { key = "value" }
    local cursor, item, prev = Utils.nextEntry(t, nil)
    -- nextEntry calls next twice: once to get first key, once more to skip it
    -- So for single element, it returns nil for cursor
    assert(cursor == nil, "Expected nil cursor for single element table")
    assert(item == "value", "Expected item to be 'value'")
    assert(prev == nil, "Expected nil prev")
end)

-- Test nextEntry with cursor to existing key
my_collection:add('nextEntry_with_cursor_existing', function()
    local t = { a = 1, b = 2 }
    -- When cursor is 'a', next() returns next key after 'a'
    -- The function returns (nextKey, items[cursor], cursor)
    local cursor, item, prev = Utils.nextEntry(t, "a")
    -- After 'a', the next key is 'b' (or nil if 'a' is last)
    -- Value returned is items['a'] = 1
    assert(item == 1, "Expected item to be 1 (value at cursor)")
    assert(prev == "a", "Expected prev to be 'a' (the cursor)")
    -- Cursor may be 'b' or nil depending on iteration order
end)

-- Test nextEntry with cursor to non-existing key
my_collection:add('nextEntry_with_cursor_missing', function()
    local t = { a = 1 }
    local cursor, item, prev = Utils.nextEntry(t, "z")
    assert(cursor == nil, "Expected nil cursor for missing key")
    assert(item == nil, "Expected nil item")
    assert(prev == nil, "Expected nil prev")
end)

-- Test consumeEntry with no predicate (consumes first item that passes predicate)
my_collection:add('consumeEntry_no_predicate', function()
    local t = { a = 1, b = 2 }
    -- consumeEntry with nil predicate consumes first item
    local item = Utils.consumeEntry(t)
    -- consumeEntry returns first item from next() which is 'a'=1
    assert(item ~= nil, "Expected to get an item")
end)

-- Test consumeEntry with predicate that matches first item
my_collection:add('consumeEntry_predicate_matches', function()
    local t = { a = 1, b = 2 }
    local item = Utils.consumeEntry(t, function(x) return x > 0 end)
    assert(item ~= nil, "Expected item when predicate matches")
end)

-- Test consumeEntry with predicate that doesn't match
my_collection:add('consumeEntry_predicate_no_match', function()
    local t = { a = 1, b = 2 }
    local item = Utils.consumeEntry(t, function(x) return x > 10 end)
    assert(item == nil, "Expected nil when no item matches predicate")
end)

-- Test isCoolingDown with no previous tick (returns true - treat as cooling down)
my_collection:add('isCoolingDown_no_previous', function()
    local result = Utils.isCoolingDown(100, nil, 50)
    assert(result == true, "Expected true when no previous tick (assumed cooling down)")
end)

-- Test isCoolingDown within duration
my_collection:add('isCoolingDown_within_duration', function()
    local result = Utils.isCoolingDown(100, 60, 50)
    assert(result == true, "Expected true when within duration")
end)

-- Test isCoolingDown at exact duration
my_collection:add('isCoolingDown_at_duration', function()
    local result = Utils.isCoolingDown(110, 60, 50)
    assert(result == false, "Expected false at exact duration boundary")
end)

-- Test isCoolingDown beyond duration
my_collection:add('isCoolingDown_beyond_duration', function()
    local result = Utils.isCoolingDown(200, 60, 50)
    assert(result == false, "Expected false when beyond duration")
end)
]]--

-- Test getTimeStringFromTick
my_collection:add('getTimeStringFromTick_zero', function()
    local result = Utils.getTimeStringFromTick(0)
    assert(result == "0d 0h 0m 0s", "Expected '0d 0h 0m 0s', got " .. tostring(result))
end)

-- Test getTimeStringFromTick with seconds
my_collection:add('getTimeStringFromTick_seconds', function()
    local result = Utils.getTimeStringFromTick(125) -- 2 seconds + 5 ticks
    assert(result:find("2s") ~= nil, "Expected 2s in result")
end)

-- Test setPositionInQuery
my_collection:add('setPositionInQuery', function()
    local query = { position = { [1] = 0, [2] = 0 } }
    Utils.setPositionInQuery(query, { x = 10, y = 20 })
    assert(query.position[1] == 10, "Expected x position set")
    assert(query.position[2] == 20, "Expected y position set")
end)

-- Test setAreaInQuery
my_collection:add('setAreaInQuery', function()
    local query = { area = { { 0, 0 }, { 0, 0 } } }
    Utils.setAreaInQuery(query, { x = 5, y = 10 }, 20)
    assert(query.area[1][1] == 5, "Expected area[1][1] = 5")
    assert(query.area[1][2] == 10, "Expected area[1][2] = 10")
    assert(query.area[2][1] == 25, "Expected area[2][1] = 25")
    assert(query.area[2][2] == 30, "Expected area[2][2] = 30")
end)

-- Test setAreaYInQuery
my_collection:add('setAreaYInQuery', function()
    local query = { area = { { 0, 0 }, { 10, 10 } } }
    Utils.setAreaYInQuery(query, 5, 15)
    assert(query.area[1][2] == 5, "Expected area[1][2] = 5")
    assert(query.area[2][2] == 15, "Expected area[2][2] = 15")
end)

-- Test setAreaXInQuery
my_collection:add('setAreaXInQuery', function()
    local query = { area = { { 0, 0 }, { 10, 10 } } }
    Utils.setAreaXInQuery(query, 5, 15)
    assert(query.area[1][1] == 5, "Expected area[1][1] = 5")
    assert(query.area[2][1] == 15, "Expected area[2][1] = 15")
end)

-- Test setAreaInQueryChunkSize
my_collection:add('setAreaInQueryChunkSize', function()
    local query = { area = { { 0, 0 }, { 0, 0 } } }
    Utils.setAreaInQueryChunkSize(query, 32, 32)
    assert(query.area[1][1] == 32, "Expected area[1][1] = 32")
    assert(query.area[1][2] == 32, "Expected area[1][2] = 32")
    assert(query.area[2][1] == 64, "Expected area[2][1] = 64 (32 + 32)")
    assert(query.area[2][2] == 64, "Expected area[2][2] = 64 (32 + 32)")
end)

-- Test setPointAreaInQuery
my_collection:add('setPointAreaInQuery', function()
    local query = { area = { { 0, 0 }, { 0, 0 } } }
    Utils.setPointAreaInQuery(query, { x = 50, y = 50 }, 10)
    assert(query.area[1][1] == 40, "Expected area[1][1] = 40 (50 - 10)")
    assert(query.area[1][2] == 40, "Expected area[1][2] = 40 (50 - 10)")
    assert(query.area[2][1] == 60, "Expected area[2][1] = 60 (50 + 10)")
    assert(query.area[2][2] == 60, "Expected area[2][2] = 60 (50 + 10)")
end)

-- setPositionInCommand / setPositionInQuery: the command and query tables are
-- created as {0,0}. If a caller passes a position without coordinates, the
-- old code wrote nil over the array slots, Lua kept the initial 0, and the
-- engine was handed (0,0) with no error -- a base built at the origin. These
-- pin the contract: both forms are accepted, both forms are written, and a
-- missing position must not leave a stale zero silently.

my_collection:add('setPositionInCommand_named_form', function()
    local cmd = { destination = {0,0} }
    Utils.setPositionInCommand(cmd, { x = 500, y = -300 })
    assert(cmd.destination[1] == 500 and cmd.destination[2] == -300,
        "array slots must carry the named coordinates")
    assert(cmd.destination.x == 500 and cmd.destination.y == -300,
        "named slots must stay coherent with the array slots")
end)

my_collection:add('setPositionInCommand_array_form', function()
    local cmd = { destination = {0,0} }
    Utils.setPositionInCommand(cmd, { 7, 9 })
    assert(cmd.destination[1] == 7 and cmd.destination[2] == 9,
        "array-form input must be accepted")
    assert(cmd.destination.x == 7 and cmd.destination.y == 9,
        "array-form input must also populate the named slots")
end)

my_collection:add('setPositionInCommand_never_leaves_stale_zero', function()
    local cmd = { destination = {0,0} }
    Utils.setPositionInCommand(cmd, { x = 0, y = 0 })
    assert(cmd.destination[1] == 0 and cmd.destination[2] == 0, "explicit zero is a real position")
    Utils.setPositionInCommand(cmd, { x = -64, y = 32 })
    assert(cmd.destination[1] == -64 and cmd.destination[2] == 32, "a later set must overwrite")
end)

my_collection:add('setPositionInCommand_missing_coordinates_is_not_applied', function()
    local cmd = { destination = {0,0} }
    Utils.setPositionInCommand(cmd, { x = 40, y = 50 })
    Utils.setPositionInCommand(cmd, {})   -- no coordinates at all
    assert(cmd.destination[1] == 40 and cmd.destination[2] == 50,
        "a position without coordinates must not overwrite the previous one")
end)

my_collection:add('setPositionInCommand_nil_position_is_safe', function()
    local cmd = { destination = {0,0} }
    Utils.setPositionInCommand(cmd, nil)
    assert(cmd.destination[1] == 0 and cmd.destination[2] == 0, "nil must be safe to pass")
end)

my_collection:add('setPositionInQuery_array_and_named', function()
    local query = { position = {0,0} }
    Utils.setPositionInQuery(query, { x = 12, y = 34 })
    assert(query.position[1] == 12 and query.position[2] == 34, "query array slots updated")
    assert(query.position.x == 12 and query.position.y == 34, "query named slots updated")
end)

return my_collection
