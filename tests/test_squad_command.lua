-- Pure squad evaluation tests
-- No Factorio dependencies required

local TestCollection = require("tests/framework")
local my_collection = TestCollection:new()

my_collection:add('squad_pure_module_loads_without_factorio', function()
    local Squad = require("libs/squad_logic")
    assert(Squad ~= nil, "pure/squad should load")
    assert(type(Squad.evaluate) == "function", "should have evaluate function")
end)

my_collection:add('squad_retreat_transitions_to_raiding', function()
    local Constants = require("Constants")
    local Squad = require("libs/squad_logic")

    local squad = {
        id = 99,
        status = Constants.SQUAD_RETREATING,
        settlers = false,
    }

    local commands = Squad.evaluate(squad)

    assert(#commands == 1, "should produce one command")
    assert(commands[1].type == "DISPATCH_SQUAD", "should dispatch squad")
    assert(commands[1].status == Constants.SQUAD_RAIDING, "should transition to raiding")
    assert(commands[1].reason == "retreat_end", "should note reason")
end)

my_collection:add('squad_retreat_transitions_to_settling', function()
    local Constants = require("Constants")
    local Squad = require("libs/squad_logic")

    local squad = {
        id = 100,
        status = Constants.SQUAD_RETREATING,
        settlers = true,
    }

    local commands = Squad.evaluate(squad)

    assert(#commands == 1, "should produce one command")
    assert(commands[1].status == Constants.SQUAD_SETTLING, "should transition to settling")
    assert(commands[1].reason == "retreat_to_settle", "should note reason")
end)

my_collection:add('squad_guarding_transitions_to_raiding', function()
    local Constants = require("Constants")
    local Squad = require("libs/squad_logic")

    local squad = {
        id = 101,
        status = Constants.SQUAD_GUARDING,
        settlers = false,
    }

    local commands = Squad.evaluate(squad)

    assert(#commands == 1, "should produce one command")
    assert(commands[1].status == Constants.SQUAD_RAIDING, "should transition to raiding")
end)

my_collection:add('squad_raiding_produces_no_transition', function()
    local Constants = require("Constants")
    local Squad = require("libs/squad_logic")

    local squad = {
        id = 102,
        status = Constants.SQUAD_RAIDING,
        settlers = false,
    }

    local commands = Squad.evaluate(squad)

    assert(#commands == 0, "raiding squads produce no state transition")
end)

return my_collection
