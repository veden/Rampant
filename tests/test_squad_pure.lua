-- Pure function tests for Squad module
-- Tests for scoring and threshold calculations

local TestCollection = require("tests/framework")
local Constants = require("Constants")

local my_collection = TestCollection:new()

-- Test: calculateKamikazeSquadThreshold calculation
my_collection:add('calculateKamikazeSquadThreshold_basic', function()
    local Squad = require("libs/Squad")
    
    local threshold = Squad.calculateKamikazeSquadThreshold(100, 200, 0.5)
    -- (100 / 200) * 0.2 + (0.5 * 0.2) = 0.1 + 0.1 = 0.2
    assert(threshold == 0.2, "Should calculate correct threshold")
end)

-- Test: calculateKamikazeSquadThreshold with high evolution
my_collection:add('calculateKamikazeSquadThreshold_high_evolution', function()
    local Squad = require("libs/Squad")
    
    local threshold = Squad.calculateKamikazeSquadThreshold(100, 200, 1.0)
    -- (100 / 200) * 0.2 + (1.0 * 0.2) = 0.1 + 0.2 = 0.3
    local expected = (100 / 200) * 0.2 + (1.0 * 0.2)
    assert(math.abs(threshold - expected) < 0.0001, "Should account for high evolution, got " .. threshold)
end)

-- Test: calculateKamikazeSquadThreshold with low member count
my_collection:add('calculateKamikazeSquadThreshold_low_members', function()
    local Squad = require("libs/Squad")
    
    local threshold = Squad.calculateKamikazeSquadThreshold(50, 200, 0.5)
    -- (50 / 200) * 0.2 + (0.5 * 0.2) = 0.05 + 0.1 = 0.15
    local expected = (50 / 200) * 0.2 + (0.5 * 0.2)
    assert(math.abs(threshold - expected) < 0.0001, "Should scale with member count, got " .. threshold)
end)

-- Test: calculateKamikazeSettlerThreshold basic
my_collection:add('calculateKamikazeSettlerThreshold_basic', function()
    local Squad = require("libs/Squad")
    
    local threshold = Squad.calculateKamikazeSettlerThreshold(100, 150, 0.5)
    -- (100 / 150) * 0.2 + (0.5 * 0.2) = 0.133... + 0.1 = 0.233...
    local expected = (100 / 150) * 0.2 + (0.5 * 0.2)
    assert(math.abs(threshold - expected) < 0.0001, "Should calculate correct threshold")
end)

-- Test: scoreSettlerLocation with resource pheromone
my_collection:add('scoreSettlerLocation_resource', function()
    local Squad = require("libs/Squad")
    
    local chunk = {
        [Constants.RESOURCE_PHEROMONE] = 100,
        [Constants.PLAYER_PHEROMONE] = 10,
    }
    
    local score = Squad.scoreSettlerLocation(chunk)
    assert(score == 90, "Should prefer high resource, low player")
end)

-- Test: scoreSettlerLocation with high player pheromone
my_collection:add('scoreSettlerLocation_high_player', function()
    local Squad = require("libs/Squad")
    
    local chunk = {
        [Constants.RESOURCE_PHEROMONE] = 100,
        [Constants.PLAYER_PHEROMONE] = 80,
    }
    
    local score = Squad.scoreSettlerLocation(chunk)
    assert(score == 20, "Should penalize high player presence")
end)

-- Test: scoreSiegeSettlerLocation includes base pheromone
my_collection:add('scoreSiegeSettlerLocation_base', function()
    local Squad = require("libs/Squad")
    
    local chunk = {
        [Constants.RESOURCE_PHEROMONE] = 100,
        [Constants.BASE_PHEROMONE] = 50,
        [Constants.PLAYER_PHEROMONE] = 20,
    }
    
    local score = Squad.scoreSiegeSettlerLocation(chunk)
    assert(score == 130, "Should include base pheromone")
end)

-- Test: scoreUnitGroupLocation prefers player and base
my_collection:add('scoreUnitGroupLocation_player_base', function()
    local Squad = require("libs/Squad")
    
    local chunk = {
        [Constants.PLAYER_PHEROMONE] = 100,
        [Constants.BASE_PHEROMONE] = 50,
    }
    
    local score = Squad.scoreUnitGroupLocation(chunk)
    assert(score == 150, "Should prefer high player and base")
end)

-- Test: validUnitGroupLocation with nest
my_collection:add('validUnitGroupLocation_with_nest', function()
    local Squad = require("libs/Squad")
    
    local chunk = { nestCount = 5 }
    local valid = Squad.validUnitGroupLocation(chunk)
    assert(valid == false, "Should be invalid if nest exists")
end)

-- Test: validUnitGroupLocation without nest
my_collection:add('validUnitGroupLocation_no_nest', function()
    local Squad = require("libs/Squad")
    
    local chunk = { nestCount = nil }
    local valid = Squad.validUnitGroupLocation(chunk)
    assert(valid == true, "Should be valid if no nest")
end)

-- Test: validUnitGroupLocation with zero nest count
my_collection:add('validUnitGroupLocation_zero_nest', function()
    local Squad = require("libs/Squad")
    
    local chunk = { nestCount = 0 }
    local valid = Squad.validUnitGroupLocation(chunk)
    -- In Lua, 0 is truthy, so (not 0) = false
    assert(valid == false, "Should be invalid if nest count is 0 (0 is truthy in Lua)")
end)

return my_collection