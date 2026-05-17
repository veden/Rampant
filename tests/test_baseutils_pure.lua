-- Pure function tests for BaseUtils module
-- Tests for base AI decision logic

local TestCollection = require("tests/framework")

local my_collection = TestCollection:new()

-- Test: canAttackBase with aggressive state
my_collection:add('canAttackBase_aggressive', function()
    local Constants = require("Constants")
    local BaseUtils = require("libs/BaseUtils")
    local Constants = require("Constants")
    
    local canAttack = BaseUtils.canAttackBase(
        Constants.BASE_AI_STATE_AGGRESSIVE,  -- stateAI
        1,             -- sentAggressiveGroups
        5,             -- maxAggressiveGroups
        false,         -- peacefulMode
        false,         -- aiNocturnalMode
        0.5,           -- darkness
        false,         -- raidAIToggle
        0,             -- sentExpansionGroups
        3              -- maxExpansionGroups
    )
    
    assert(canAttack == true, "Should allow attack with aggressive state")
end)

-- Test: canAttackBase with raiding state
my_collection:add('canAttackBase_raiding', function()
    local Constants = require("Constants")
    local BaseUtils = require("libs/BaseUtils")
    local Constants = require("Constants")
    
    local canAttack = BaseUtils.canAttackBase(
        Constants.BASE_AI_STATE_RAIDING,     -- stateAI
        0,             -- sentAggressiveGroups
        5,             -- maxAggressiveGroups
        false,         -- peacefulMode
        false,         -- aiNocturnalMode
        0.5,           -- darkness
        false,         -- raidAIToggle
        0,             -- sentExpansionGroups
        3              -- maxExpansionGroups
    )
    
    assert(canAttack == true, "Should allow attack with raiding state")
end)

-- Test: canAttackBase with peaceful mode
my_collection:add('canAttackBase_peaceful', function()
    local Constants = require("Constants")
    local BaseUtils = require("libs/BaseUtils")
    
    local canAttack = BaseUtils.canAttackBase(
        Constants.BASE_AI_STATE_AGGRESSIVE,  -- stateAI
        1,             -- sentAggressiveGroups
        5,             -- maxAggressiveGroups
        true,          -- peacefulMode
        false,         -- aiNocturnalMode
        0.5,           -- darkness
        false,         -- raidAIToggle
        0,             -- sentExpansionGroups
        3              -- maxExpansionGroups
    )
    
    assert(canAttack == false, "Should deny attack in peaceful mode")
end)

-- Test: canAttackBase with max aggressive groups reached
my_collection:add('canAttackBase_max_groups', function()
    local Constants = require("Constants")
    local BaseUtils = require("libs/BaseUtils")
    
    local canAttack = BaseUtils.canAttackBase(
        Constants.BASE_AI_STATE_AGGRESSIVE,  -- stateAI
        5,             -- sentAggressiveGroups (at max)
        5,             -- maxAggressiveGroups
        false,         -- peacefulMode
        false,         -- aiNocturnalMode
        0.5,           -- darkness
        false,         -- raidAIToggle
        0,             -- sentExpansionGroups
        3              -- maxExpansionGroups
    )
    
    assert(canAttack == false, "Should deny attack when max groups reached")
end)

-- Test: canAttackBase with nocturnal mode and low darkness
my_collection:add('canAttackBase_nocturnal_low_darkness', function()
    local Constants = require("Constants")
    local BaseUtils = require("libs/BaseUtils")
    
    local canAttack = BaseUtils.canAttackBase(
        Constants.BASE_AI_STATE_AGGRESSIVE,  -- stateAI
        1,             -- sentAggressiveGroups
        5,             -- maxAggressiveGroups
        false,         -- peacefulMode
        true,          -- aiNocturnalMode (active)
        0.5,           -- darkness (below 0.65)
        false,         -- raidAIToggle
        0,             -- sentExpansionGroups
        3              -- maxExpansionGroups
    )
    
    assert(canAttack == false, "Should deny attack in nocturnal mode with low darkness")
end)

-- Test: canAttackBase with nocturnal mode and high darkness
my_collection:add('canAttackBase_nocturnal_high_darkness', function()
    local Constants = require("Constants")
    local BaseUtils = require("libs/BaseUtils")
    
    local canAttack = BaseUtils.canAttackBase(
        Constants.BASE_AI_STATE_AGGRESSIVE,  -- stateAI
        1,             -- sentAggressiveGroups
        5,             -- maxAggressiveGroups
        false,         -- peacefulMode
        true,          -- aiNocturnalMode (active)
        0.8,           -- darkness (above 0.65)
        false,         -- raidAIToggle
        0,             -- sentExpansionGroups
        3              -- maxExpansionGroups
    )
    
    assert(canAttack == true, "Should allow attack in nocturnal mode with high darkness")
end)

-- Test: canAttackBase with siege state and raid toggle
my_collection:add('canAttackBase_siege_raid', function()
    local Constants = require("Constants")
    local BaseUtils = require("libs/BaseUtils")
    
    local canAttack = BaseUtils.canAttackBase(
        Constants.BASE_AI_STATE_SIEGE,       -- stateAI
        0,             -- sentAggressiveGroups
        5,             -- maxAggressiveGroups
        false,         -- peacefulMode
        false,         -- aiNocturnalMode
        0.5,           -- darkness
        true,          -- raidAIToggle (enabled)
        3,             -- sentExpansionGroups (at max)
        3              -- maxExpansionGroups
    )
    
    assert(canAttack == true, "Should allow attack with siege + raid toggle")
end)

-- Test: canMigrateBase with migrating state
my_collection:add('canMigrateBase_migrating', function()
    local Constants = require("Constants")
    local BaseUtils = require("libs/BaseUtils")
    
    local canMigrate = BaseUtils.canMigrateBase(
        Constants.BASE_AI_STATE_MIGRATING,   -- stateAI
        true,          -- expansion
        false,         -- peacefulMode
        false,         -- aiNocturnalMode
        0.5            -- darkness
    )
    
    assert(canMigrate == true, "Should allow migrate with migrating state")
end)

-- Test: canMigrateBase with siege state
my_collection:add('canMigrateBase_siege', function()
    local Constants = require("Constants")
    local BaseUtils = require("libs/BaseUtils")
    
    local canMigrate = BaseUtils.canMigrateBase(
        Constants.BASE_AI_STATE_SIEGE,       -- stateAI
        true,          -- expansion
        false,         -- peacefulMode
        false,         -- aiNocturnalMode
        0.5            -- darkness
    )
    
    assert(canMigrate == true, "Should allow migrate with siege state")
end)

-- Test: canMigrateBase with raiding state (invalid)
my_collection:add('canMigrateBase_raiding', function()
    local Constants = require("Constants")
    local BaseUtils = require("libs/BaseUtils")
    
    local canMigrate = BaseUtils.canMigrateBase(
        Constants.BASE_AI_STATE_RAIDING,     -- stateAI
        true,          -- expansion
        false,         -- peacefulMode
        false,         -- aiNocturnalMode
        0.5            -- darkness
    )
    
    assert(canMigrate == false, "Should deny migrate with raiding state")
end)

-- Test: canMigrateBase with expansion disabled
my_collection:add('canMigrateBase_expansion_disabled', function()
    local Constants = require("Constants")
    local BaseUtils = require("libs/BaseUtils")
    
    local canMigrate = BaseUtils.canMigrateBase(
        Constants.BASE_AI_STATE_MIGRATING,   -- stateAI
        false,         -- expansion
        false,         -- peacefulMode
        false,         -- aiNocturnalMode
        0.5            -- darkness
    )
    
    assert(canMigrate == false, "Should deny migrate when expansion disabled")
end)

-- Test: canMigrateBase with peaceful mode
my_collection:add('canMigrateBase_peaceful', function()
    local Constants = require("Constants")
    local BaseUtils = require("libs/BaseUtils")
    
    local canMigrate = BaseUtils.canMigrateBase(
        Constants.BASE_AI_STATE_MIGRATING,   -- stateAI
        true,          -- expansion
        true,          -- peacefulMode
        false,         -- aiNocturnalMode
        0.5            -- darkness
    )
    
    assert(canMigrate == false, "Should deny migrate in peaceful mode")
end)

-- Test: canMigrateBase with nocturnal mode and low darkness
my_collection:add('canMigrateBase_nocturnal_low_darkness', function()
    local Constants = require("Constants")
    local BaseUtils = require("libs/BaseUtils")
    
local canMigrate = BaseUtils.canMigrateBase(
        Constants.BASE_AI_STATE_MIGRATING,   -- stateAI
        true,          -- expansion
        false,         -- peacefulMode
        true,          -- aiNocturnalMode (active)
        0.5            -- darkness
    )
    
    assert(canMigrate == false, "Should deny migrate in nocturnal mode with low darkness")
end)

-- Test: canMigrateBase with nocturnal mode and high darkness
my_collection:add('canMigrateBase_nocturnal_high_darkness', function()
    local Constants = require("Constants")
    local BaseUtils = require("libs/BaseUtils")
    
    local canMigrate = BaseUtils.canMigrateBase(
        Constants.BASE_AI_STATE_MIGRATING,   -- stateAI
        true,          -- expansion
        false,         -- peacefulMode
        true,          -- aiNocturnalMode (active)
        0.8            -- darkness
    )
    
    assert(canMigrate == true, "Should allow migrate in nocturnal mode with high darkness")
end)

return my_collection