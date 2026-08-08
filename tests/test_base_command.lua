-- Pure base evaluation tests
-- No Factorio dependencies required

local TestCollection = require("tests/framework")
local my_collection = TestCollection:new()

my_collection:add('base_pure_module_loads_without_factorio', function()
    local Base = require("libs/base_logic")
    assert(Base ~= nil, "pure/base should load")
    assert(type(Base.canAttack) == "function", "should have canAttack")
    assert(type(Base.canMigrate) == "function", "should have canMigrate")
    assert(type(Base.calculateTemperamentDelta) == "function", "should have calculateTemperamentDelta")
end)

my_collection:add('can_attack_returns_true_for_raiding_state', function()
    local Constants = require("Constants")
    local Base = require("libs/base_logic")

    assert(Base.canAttack({
        stateAI = Constants.BASE_AI_STATE_RAIDING,
        peacefulMode = false,
        aiNocturnalMode = false,
    }) == true)
end)

my_collection:add('can_attack_returns_false_for_peaceful_mode', function()
    local Constants = require("Constants")
    local Base = require("libs/base_logic")

    assert(Base.canAttack({
        stateAI = Constants.BASE_AI_STATE_RAIDING,
        peacefulMode = true,
    }) == false)
end)

my_collection:add('can_attack_checks_nocturnal_mode', function()
    local Constants = require("Constants")
    local Base = require("libs/base_logic")

    assert(Base.canAttack({
        stateAI = Constants.BASE_AI_STATE_RAIDING,
        peacefulMode = false,
        aiNocturnalMode = true,
        darkness = 0.3,  -- too bright
    }) == false)

    assert(Base.canAttack({
        stateAI = Constants.BASE_AI_STATE_RAIDING,
        peacefulMode = false,
        aiNocturnalMode = true,
        darkness = 0.8,  -- dark enough
    }) == true)
end)

my_collection:add('can_attack_requires_aggressive_slots_available', function()
    local Constants = require("Constants")
    local Base = require("libs/base_logic")

    assert(Base.canAttack({
        stateAI = Constants.BASE_AI_STATE_AGGRESSIVE,
        sentAggressiveGroups = 5,
        maxAggressiveGroups = 3,
        peacefulMode = false,
    }) == false)

    assert(Base.canAttack({
        stateAI = Constants.BASE_AI_STATE_AGGRESSIVE,
        sentAggressiveGroups = 2,
        maxAggressiveGroups = 3,
        peacefulMode = false,
    }) == true)
end)

my_collection:add('can_migrate_only_in_migrating_or_siege', function()
    local Constants = require("Constants")
    local Base = require("libs/base_logic")

    assert(Base.canMigrate({
        stateAI = Constants.BASE_AI_STATE_MIGRATING,
        expansion = true,
        peacefulMode = false,
    }) == true)

    assert(Base.canMigrate({
        stateAI = Constants.BASE_AI_STATE_SIEGE,
        expansion = true,
        peacefulMode = false,
    }) == true)

    assert(Base.canMigrate({
        stateAI = Constants.BASE_AI_STATE_RAIDING,
        expansion = true,
        peacefulMode = false,
    }) == false)
end)

my_collection:add('can_migrate_requires_expansion_enabled', function()
    local Constants = require("Constants")
    local Base = require("libs/base_logic")

    assert(Base.canMigrate({
        stateAI = Constants.BASE_AI_STATE_MIGRATING,
        expansion = false,
        peacefulMode = false,
    }) == false)
end)

my_collection:add('temperament_delta_positive_from_nests', function()
    local Base = require("libs/base_logic")

    local delta = Base.calculateTemperamentDelta({
        activeNests = 3,
        activeRaidNests = 0,
        destroyPlayerBuildings = 0,
        lostEnemyUnits = 0,
        lostEnemyBuilding = 0,
        builtEnemyBuilding = 0,
        rocketLaunched = 0,
        ionCannonBlasts = 0,
        artilleryBlasts = 0,
        temperamentScore = 0,
    }, 0.5, 1)

    -- 3 nests: +17.28, no raid nests: -3.84, no buildings: -2.78 ≈ +10.66
    assert(delta > 0, "active nests should increase temperament")
end)

my_collection:add('temperament_delta_positive_from_rocket_launch', function()
    local Base = require("libs/base_logic")

    local delta = Base.calculateTemperamentDelta({
        activeNests = 0,
        activeRaidNests = 0,
        destroyPlayerBuildings = 0,
        lostEnemyUnits = 0,
        lostEnemyBuilding = 0,
        builtEnemyBuilding = 0,
        rocketLaunched = 1,
        ionCannonBlasts = 0,
        artilleryBlasts = 0,
        temperamentScore = 0,
    }, 0.5, 1)

    -- rocket: +27.76, no nests: -5.55, no raid: -3.84, no buildings: -2.78 ≈ +15.6
    assert(delta > 10, "rocket launch should significantly increase temperament")
end)

my_collection:add('temperament_delta_negative_from_lost_units', function()
    local Base = require("libs/base_logic")

    local delta = Base.calculateTemperamentDelta({
        activeNests = 0,
        activeRaidNests = 0,
        destroyPlayerBuildings = 0,
        lostEnemyUnits = 1000,
        lostEnemyBuilding = 0,
        builtEnemyBuilding = 0,
        rocketLaunched = 0,
        ionCannonBlasts = 0,
        artilleryBlasts = 0,
        temperamentScore = 0.5,  -- positive temperament
    }, 0.5, 1)

    -- lost units with positive temp: -(0.041472 * 1000) = -41.47
    assert(delta < -30, "lost units should decrease temperament when positive")
end)

-- State machine tests

my_collection:add('selectstate_low_temperament_prefers_migration', function()
    local Constants = require("Constants")
    local Base = require("libs/base_logic")

    local state = Base.selectState({
        temperament = 0.02,
        roll = 0.1,
        enabledMigration = true,
        raidAIToggle = true,
        siegeAIToggle = true,
        peacefulAIToggle = false,
    })
    assert(state == Constants.BASE_AI_STATE_MIGRATING, "low temp + low roll → migrating")
end)

my_collection:add('selectstate_high_temperament_prefers_onslaught', function()
    local Constants = require("Constants")
    local Base = require("libs/base_logic")

    local state = Base.selectState({
        temperament = 0.99,
        roll = 0.9,
        enabledMigration = false,
        raidAIToggle = false,
        siegeAIToggle = false,
        peacefulAIToggle = false,
    })
    assert(state == Constants.BASE_AI_STATE_ONSLAUGHT, "max temp → onslaught")
end)

my_collection:add('selectstate_mid_temperament_varies_by_roll', function()
    local Constants = require("Constants")
    local Base = require("libs/base_logic")

    -- temp 0.6-0.8: roll < 0.4 → aggressive, < 0.6 → onslaught, < 0.8 → raiding
    assert(Base.selectState({ temperament = 0.7, roll = 0.2, enabledMigration = false }) == Constants.BASE_AI_STATE_AGGRESSIVE)
    assert(Base.selectState({ temperament = 0.7, roll = 0.5, enabledMigration = false }) == Constants.BASE_AI_STATE_ONSLAUGHT)
    assert(Base.selectState({ temperament = 0.7, roll = 0.7, enabledMigration = false }) == Constants.BASE_AI_STATE_RAIDING)
end)

my_collection:add('selectstate_peaceful_mode_respected', function()
    local Constants = require("Constants")
    local Base = require("libs/base_logic")

    local state = Base.selectState({
        temperament = 0.3,
        roll = 0.9,
        enabledMigration = true,
        raidAIToggle = false,
        siegeAIToggle = false,
        peacefulAIToggle = true,
    })
    assert(state == Constants.BASE_AI_STATE_PEACEFUL, "peaceful toggle should be respected")
end)

return my_collection
