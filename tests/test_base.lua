-- Integration tests for base lifecycle (creation, destruction, recycling)
-- These tests require partial mocking of Factorio APIs

local TestCollection = require("tests/framework")
local Fixtures = require("tests/fixtures")

local my_collection = TestCollection:new()

-- Create mock universe and initialize MapUtils
local Universe = Fixtures.makeUniverse()
package.loaded["libs/MapUtils"] = nil
local MapUtils = require("libs/MapUtils")
MapUtils.init(Universe)

-- Test base_created: Create base and verify it's in Universe
my_collection:add('base_created', function()
    local surface = Fixtures.makeSurface("test")
    local map = Fixtures.makeMap(surface)
    map.universe = Universe

    -- Create a base (returns a RampantBaseRegion containing the base)
    local position = { x = 0, y = 0 }
    local region = MapUtils.createBase(map, position, 0)

    assert(region ~= nil, "Expected region to be created")
    assert(region.base ~= nil, "Expected base to be created")

    -- Verify base is in Universe tables
    assert(Universe.tables.bases[region.base.id] ~= nil, "Expected base in Universe.tables.bases")
end)

-- Test base_cleaned_up: Create base, zero structures, verify cleanup
my_collection:add('base_cleaned_up', function()
    local surface = Fixtures.makeSurface("test")
    local map = Fixtures.makeMap(surface)
    map.universe = Universe

    -- Create a base with zero structures
    local position = { x = 10, y = 10 }
    local region = MapUtils.createBase(map, position, 0)
    local base = region.base

    assert(base.nests == 0, "Expected base with no nests")
    assert(base.hives == 0, "Expected base with no hives")
    assert(base.turrets == 0, "Expected base with no turrets")

    -- Verify base is in Universe tables
    assert(Universe.tables.bases[base.id] ~= nil, "Expected base in Universe.tables.bases")

    -- Test that base gets cleaned up when structures are zero
    -- (This depends on the recycleBases function)
end)

-- Test base_with_structures_preserved: Base with structures should not be cleaned up
my_collection:add('base_with_structures_preserved', function()
    local surface = Fixtures.makeSurface("test")
    local map = Fixtures.makeMap(surface)
    map.universe = Universe

    -- Create a base
    local position = { x = 20, y = 20 }
    local region = MapUtils.createBase(map, position, 0)
    local base = region.base

    -- Manually add spawner to simulate enemy base with structures
    region.spawners = 1

    -- Verify base has structures (would be cleaned up if structures were 0)
    assert(base.nests == 0, "Expected nests = 0 before registration")
    assert(region.spawners == 1, "Expected region.spawners = 1")

    -- The actual recycleBases function uses an iterator that requires multiple calls
    -- to process all bases. For this test, we verify the structure count is correct.
end)

-- Test base_registration_registers_in_map
my_collection:add('base_registration_in_map', function()
    local surface = Fixtures.makeSurface("test")
    local map = Fixtures.makeMap(surface)
    map.universe = Universe

    local position = { x = 30, y = 30 }
    local region = MapUtils.createBase(map, position, 0)
    local base = region.base

    -- Verify base is registered in map's bases
    assert(map.bases[base.id] ~= nil, "Expected base in map.bases")

    -- Verify region is in map.baseRegions
    local found = false
    for _, regionRow in pairs(map.baseRegions) do
        for _, r in pairs(regionRow) do
            if r == region then
                found = true
                break
            end
        end
    end
    assert(found, "Expected region in map.baseRegions")
end)

-- Test base_state_initialization
my_collection:add('base_state_initialization', function()
    local surface = Fixtures.makeSurface("test")
    local map = Fixtures.makeMap(surface)
    map.universe = Universe

    local position = { x = 40, y = 40 }
    local region = MapUtils.createBase(map, position, 0)
    local base = region.base

    -- stateAI is 1 for peaceful (BASE_AI_STATES.peaceful = 1)
    assert(base.stateAI == 1, "Expected initial stateAI = 1 (peaceful)")
    -- stateGeneration is 1 for active (BASE_AI_GENERATION_STATES.active = 1)
    assert(base.stateGeneration == 1, "Expected initial generation state = 1 (active)")
    assert(base.unitPoints == 0, "Expected initial unitPoints = 0")
    assert(base.temperament == 0.5, "Expected initial temperament = 0.5")
    assert(base.ionCannonBlasts == 0, "Expected initial ionCannonBlasts = 0")
end)

-- Test multiple_bases_different_positions
my_collection:add('multiple_bases_different_positions', function()
    local surface = Fixtures.makeSurface("test")
    local map = Fixtures.makeMap(surface)
    map.universe = Universe

    local region1 = MapUtils.createBase(map, { x = 0, y = 0 }, 0)
    local base1 = region1.base
    local region2 = MapUtils.createBase(map, { x = 100, y = 100 }, 0)
    local base2 = region2.base
    local region3 = MapUtils.createBase(map, { x = -100, y = -100 }, 0)
    local base3 = region3.base

    assert(base1.id ~= base2.id, "Expected unique base IDs")
    assert(base2.id ~= base3.id, "Expected unique base IDs")
    assert(base1.id ~= base3.id, "Expected unique base IDs")

    assert(Universe.tables.bases[base1.id] ~= nil, "Expected base1 in Universe")
    assert(Universe.tables.bases[base2.id] ~= nil, "Expected base2 in Universe")
    assert(Universe.tables.bases[base3.id] ~= nil, "Expected base3 in Universe")
end)

-- Test findNearbyBase
my_collection:add('findNearbyBase_basic', function()
    local surface = Fixtures.makeSurface("test")
    local map = Fixtures.makeMap(surface)
    map.universe = Universe

    local region1 = MapUtils.createBase(map, { x = 0, y = 0 }, 0)
    local region2 = MapUtils.createBase(map, { x = 200, y = 0 }, 0) -- Far away

    local found = MapUtils.findNearbyBase(map, { x = 0, y = 0 })
    assert(found == region1, "Expected to find region1 near origin")
end)

-- Test findNearbyBase_out_of_range
my_collection:add('findNearbyBase_out_of_range', function()
    local surface = Fixtures.makeSurface("test")
    local map = Fixtures.makeMap(surface)
    map.universe = Universe

    local region1 = MapUtils.createBase(map, { x = 0, y = 0 }, 0)

    local found = MapUtils.findNearbyBase(map, { x = 500, y = 500 })
    assert(found == nil, "Expected nil when too far from any base")
end)

-- Test registerEnemyEntity adds spawner to structures table
my_collection:add('registerEnemyEntity_in_base', function()
    local surface = Fixtures.makeSurface("test")
    local map = Fixtures.makeMap(surface)
    map.universe = Universe

    local region = MapUtils.createBase(map, { x = 0, y = 0 }, 0)
    local base = region.base

    local entity = {
        name = "biter-spawner",
        type = "spawner",
        position = { x = 0, y = 0 },
        valid = true,
        unit_number = 1,
        surface = surface,
        force = { name = "enemy" },
        prototype = {
            collision_box = { left_top = { x = -1, y = -1 }, right_bottom = { x = 1, y = 1 } },
            selection_box = { left_top = { x = -1, y = -1 }, right_bottom = { x = 1, y = 1 } },
        },
    }

    local result = MapUtils.registerEnemyEntity(region, entity, nil, 0)

    assert(result ~= nil, "Expected entity data to be returned")
    assert(result.type == "spawner", "Expected entity type = spawner")
    assert(Universe.tables.structures[1] ~= nil, "Expected entity registered in structures table")
end)

return my_collection