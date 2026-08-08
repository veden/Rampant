-- Pure death evaluation tests
-- No Factorio dependencies required

local TestCollection = require("tests/framework")
local my_collection = TestCollection:new()

my_collection:add('death_pure_module_loads_without_factorio', function()
    local Death = require("libs/death_logic")
    assert(Death ~= nil, "pure/death should load")
    assert(type(Death.evaluateEnemy) == "function", "should have evaluateEnemy")
end)

my_collection:add('unit_death_produces_decompress_and_dirty', function()
    local Death = require("libs/death_logic")

    local commands = Death.evaluateEnemy({
        entityType = "unit",
        entityName = "small-biter",
        chunkId = 42,
        baseId = 1,
        base = { lostEnemyUnits = 0, damagedBy = {}, deathEvents = 0 },
        squadId = 10,
        damageType = "physical",
        isArtilleryCause = false,
        deathGeneratorRating = 0.5,
        retreatThreshold = 0.3,
        causeValid = true,
        rallyThreshold = 1.0,
        randomRoll = 0.5,
        lostEnemyUnits = 0,
        isDrainPylon = false,
    })

    local types = {}
    for _, c in ipairs(commands) do types[c.type] = true end

    assert(types.DECOMPRESS_SQUAD, "should decompress squad")
    assert(types.MARK_CHUNK_DIRTY, "should mark chunk dirty")
end)

my_collection:add('unit_death_triggers_retreat_when_below_threshold', function()
    local Death = require("libs/death_logic")

    local commands = Death.evaluateEnemy({
        entityType = "unit",
        entityName = "small-biter",
        chunkId = 42,
        baseId = 1,
        base = { lostEnemyUnits = 0 },
        squadId = nil,
        damageType = nil,
        isArtilleryCause = false,
        deathGeneratorRating = 0.1,  -- below threshold
        retreatThreshold = 0.3,
        causeValid = true,
        rallyThreshold = 0,
        randomRoll = 0,
        lostEnemyUnits = 0,
        isDrainPylon = false,
    })

    local hasRetreat = false
    for _, c in ipairs(commands) do
        if c.type == "RETREAT_UNITS" then
            hasRetreat = true
            assert(c.cause == "player_damage", "retreat cause should be player_damage")
        end
    end
    assert(hasRetreat, "should retreat when below threshold")
end)

my_collection:add('unit_death_no_retreat_when_above_threshold', function()
    local Death = require("libs/death_logic")

    local commands = Death.evaluateEnemy({
        entityType = "unit",
        entityName = "small-biter",
        chunkId = 42,
        baseId = 1,
        base = { lostEnemyUnits = 0 },
        squadId = nil,
        damageType = nil,
        isArtilleryCause = false,
        deathGeneratorRating = 0.8,  -- above threshold
        retreatThreshold = 0.3,
        causeValid = true,
        rallyThreshold = 0,
        randomRoll = 0,
        lostEnemyUnits = 0,
        isDrainPylon = false,
    })

    for _, c in ipairs(commands) do
        assert(c.type ~= "RETREAT_UNITS", "should not retreat when above threshold")
    end
end)

my_collection:add('spawner_death_produces_rally_and_point_cost', function()
    local Death = require("libs/death_logic")

    local commands = Death.evaluateEnemy({
        entityType = "unit-spawner",
        entityName = "biter-spawner",
        chunkId = 42,
        baseId = 1,
        base = { artilleryBlasts = 0 },
        squadId = nil,
        damageType = "explosion",
        isArtilleryCause = false,
        deathGeneratorRating = 0,
        retreatThreshold = 0,
        causeValid = true,
        rallyThreshold = 0,
        randomRoll = 0,
        lostEnemyUnits = 0,
        isDrainPylon = false,
    })

    local types = {}
    for _, c in ipairs(commands) do types[c.type] = true end

    assert(types.RALLY_SQUADS, "should rally on spawner death")
    assert(types.MODIFY_UNIT_POINTS, "should charge point cost")
end)

my_collection:add('drain_pylon_returns_empty_commands', function()
    local Death = require("libs/death_logic")

    local commands = Death.evaluateEnemy({
        isDrainPylon = true,
    })

    assert(#commands == 0, "drain pylons produce no commands")
end)

my_collection:add('unit_death_tracks_damage_type', function()
    local Death = require("libs/death_logic")

    local base = { lostEnemyUnits = 0, damagedBy = {}, deathEvents = 0 }

    Death.evaluateEnemy({
        entityType = "unit",
        entityName = "small-biter",
        chunkId = 42,
        baseId = 1,
        base = base,
        squadId = nil,
        damageType = "fire",
        isArtilleryCause = false,
        deathGeneratorRating = 1,
        retreatThreshold = 0,
        causeValid = false,
        rallyThreshold = 0,
        randomRoll = 0,
        lostEnemyUnits = 0,
        isDrainPylon = false,
    })

    assert(base.lostEnemyUnits == 1, "should increment lost units")
    assert(base.damagedBy.fire == 0.01, "should track damage type")
    assert(base.deathEvents == 1, "should increment death events")
end)

return my_collection
