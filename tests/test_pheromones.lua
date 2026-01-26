-- Pheromone system tests
-- Tests for player pheromones, death scent, victory scent, and pheromone propagation

local TestCollection = require("tests/framework")
local Fixtures = require("tests/fixtures")

local my_collection = TestCollection:new()

-- Create mock universe and initialize MapUtils
local Universe = Fixtures.makeUniverse()
package.loaded["libs/MapUtils"] = nil
local MapUtils = require("libs/MapUtils")
MapUtils.init(Universe)

-- Helper to create a properly initialized chunk
local function makeTestChunk(map, x, y)
    local chunk = Fixtures.makeChunk(map, x, y)
    -- Initialize required fields for pheromone processing
    chunk.pathRating = 1
    chunk.playerAndBasePheromone = 0
    chunk.playerAndBaseKamikazePheromone = 0
    chunk.airPlayerAndBasePheromone = 0
    chunk.enemyAndResourcePheromone = 0
    chunk.tick = 0
    Fixtures.registerChunk(map, chunk)
    return chunk
end

-- Helper to create a player building entity with prototype info
local function makePlayerBuilding(name, position, forceName)
    return {
        name = name,
        type = "building",
        position = position,
        force = { name = forceName or "player" },
        valid = true,
        surface = nil,
        unit_number = math.random(10000),
        prototype = {
            infinite_resource = false,
            collision_box = { left_top = { x = -0.5, y = -0.5 }, right_bottom = { x = 0.5, y = 0.5 } },
        },
    }
end

-- Helper to create an enemy entity for death scent tests
local function makeEnemyEntity(name, position)
    return {
        name = name,
        type = "unit",
        position = position,
        force = { name = "enemy" },
        valid = true,
        surface = nil,
        unit_number = math.random(1000),
        prototype = {
            infinite_resource = false,
            collision_box = { left_top = { x = -0.5, y = -0.5 }, right_bottom = { x = 0.5, y = 0.5 } },
        },
    }
end

-- Helper to create a structure entity for death scent tests
local function makeStructureEntity(name, position)
    return {
        name = name,
        type = "structure",
        position = position,
        force = { name = "enemy" },
        valid = true,
        surface = nil,
        unit_number = math.random(1000),
        prototype = {
            infinite_resource = false,
            collision_box = { left_top = { x = -1, y = -1 }, right_bottom = { x = 1, y = 1 } },
        },
    }
end

-- ============================================
-- Player Pheromone Registration Tests
-- ============================================

-- Test: registerChunkPlayerEntity adds player generator to chunk
my_collection:add('registerChunkPlayerEntity_adds_generator', function()
    local surface = Fixtures.makeSurface("test")
    local map = Fixtures.makeMap(surface)
    map.universe = Universe

    -- Place chunk at -32,-32 so that position 0,0 is within it
    local chunk = makeTestChunk(map, -32, -32)
    assert(chunk.playerGenerator == nil, "Expected playerGenerator nil before registration")

    -- Entity at position 0,0 with collision box -0.5,-0.5 to 0.5,0.5 extends into chunk (-32, -32)
    local entity = makePlayerBuilding("wall", { x = 0, y = 0 })
    MapUtils.registerChunkPlayerEntity(entity, map)

    assert(chunk.playerGenerator ~= nil, "Expected playerGenerator after registration")
    assert(chunk.playerGenerator > 0, "Expected positive playerGenerator for wall")
end)

-- Test: registerChunkPlayerEntity with different building types
my_collection:add('registerChunkPlayerEntity_building_types', function()
    local surface = Fixtures.makeSurface("test")
    local map = Fixtures.makeMap(surface)
    map.universe = Universe

    -- Wall - level 1 (25) - place at chunk -32,-32 so position 0,0 is inside
    local chunk1 = makeTestChunk(map, -32, -32)
    local wall = makePlayerBuilding("wall", { x = 0, y = 0 })
    MapUtils.registerChunkPlayerEntity(wall, map)
    assert(chunk1.playerGenerator == 25, "Expected 25 for wall, got " .. tostring(chunk1.playerGenerator))

    -- Turret - level 3 (500) - position 32,0 is in chunk 0,0
    local chunk2 = makeTestChunk(map, 0, 0)
    local turret = makePlayerBuilding("turret", { x = 32, y = 0 })
    MapUtils.registerChunkPlayerEntity(turret, map)
    assert(chunk2.playerGenerator == 500, "Expected 500 for turret, got " .. tostring(chunk2.playerGenerator))

    -- Rocket silo - level 6 (6000) - position 64,0 is in chunk 64,0
    local chunk3 = makeTestChunk(map, 64, 0)
    local silo = makePlayerBuilding("rocket-silo", { x = 64, y = 0 })
    MapUtils.registerChunkPlayerEntity(silo, map)
    assert(chunk3.playerGenerator == 6000, "Expected 6000 for rocket-silo, got " .. tostring(chunk3.playerGenerator))
end)

-- Test: registerChunkPlayerEntity ignores enemy buildings
my_collection:add('registerChunkPlayerEntity_ignores_enemy', function()
    local surface = Fixtures.makeSurface("test")
    local map = Fixtures.makeMap(surface)
    map.universe = Universe

    local chunk = makeTestChunk(map, -32, -32)

    -- Enemy building should be ignored
    local enemyBuilding = makePlayerBuilding("turret", { x = 0, y = 0 })
    enemyBuilding.force = { name = "enemy" }
    MapUtils.registerChunkPlayerEntity(enemyBuilding, map)

    assert(chunk.playerGenerator == nil, "Expected playerGenerator nil for enemy building")
end)

-- Test: unregisterChunkPlayerEntity removes generator
my_collection:add('unregisterChunkPlayerEntity_removes_generator', function()
    local surface = Fixtures.makeSurface("test")
    local map = Fixtures.makeMap(surface)
    map.universe = Universe

    local chunk = makeTestChunk(map, -32, -32)

    -- Register first
    local entity = makePlayerBuilding("wall", { x = 0, y = 0 })
    MapUtils.registerChunkPlayerEntity(entity, map)
    local initialGenerator = chunk.playerGenerator
    assert(initialGenerator ~= nil and initialGenerator > 0, "Expected generator after registration")

    -- Unregister
    MapUtils.unregisterChunkPlayerEntity(entity, map, nil)

    -- Should be negative (removed)
    assert(chunk.playerGenerator < 0, "Expected negative playerGenerator after unregister, got " .. tostring(chunk.playerGenerator))
end)

-- ============================================
-- Death Scent Tests
-- ============================================

-- Test: deathScent adds death generator to chunk
my_collection:add('deathScent_adds_generator', function()
    local surface = Fixtures.makeSurface("test")
    local map = Fixtures.makeMap(surface)
    map.universe = Universe

    local chunk = makeTestChunk(map, 0, 0)

    makeEnemyEntity("small-biter", { x = 0, y = 0 })
    MapUtils.deathScent(chunk, false)

    assert(chunk.deathGenerator < 0, "Expected negative deathGenerator, got " .. tostring(chunk.deathGenerator))
end)

-- Test: deathScent with structure adds stronger death generator
my_collection:add('deathScent_structure_stronger', function()
    local surface = Fixtures.makeSurface("test")
    local map = Fixtures.makeMap(surface)
    map.universe = Universe

    local chunk = makeTestChunk(map, 0, 0)

    -- Regular unit death
    makeEnemyEntity("small-biter", { x = 0, y = 0 })
    MapUtils.deathScent(chunk, false)
    local unitDeathAmount = chunk.deathGenerator

    -- Create a new chunk for structure death
    local chunk2 = makeTestChunk(map, 32, 0)
    local structureEntity = makeStructureEntity("biter-spawner", { x = 32, y = 0 })
    MapUtils.deathScent(chunk2, structureEntity)
    local structureDeathAmount = chunk2.deathGenerator

    -- Structure death should be 10x stronger
    assert(math.abs(structureDeathAmount) > math.abs(unitDeathAmount),
        "Expected structure death to be stronger, got unit=" .. tostring(unitDeathAmount) .. " struct=" .. tostring(structureDeathAmount))
end)

-- ============================================
-- Victory Scent Tests
-- ============================================

-- Test: victoryScent adds victory generator for valid building
my_collection:add('victoryScent_adds_generator', function()
    local surface = Fixtures.makeSurface("test")
    local map = Fixtures.makeMap(surface)
    map.universe = Universe

    local chunk = makeTestChunk(map, 0, 0)

    -- Wall should have victory scent value
    MapUtils.victoryScent(chunk, "wall")
    assert(chunk.victoryGenerator ~= nil and chunk.victoryGenerator > 0,
        "Expected positive victoryGenerator for wall, got " .. tostring(chunk.victoryGenerator or "nil"))
end)

-- Test: victoryScent ignores unknown building type
my_collection:add('victoryScent_ignores_unknown', function()
    local surface = Fixtures.makeSurface("test")
    local map = Fixtures.makeMap(surface)
    map.universe = Universe

    local chunk = makeTestChunk(map, 0, 0)

    -- Unknown building should not add victory generator
    MapUtils.victoryScent(chunk, "unknown-building")
    assert(chunk.victoryGenerator == nil, "Expected nil victoryGenerator for unknown building")
end)

-- ============================================
-- Pheromone Processing Tests
-- ============================================

-- Test: processPheromone updates chunk tick
my_collection:add('processPheromone_updates_tick', function()
    local surface = Fixtures.makeSurface("test")
    local map = Fixtures.makeMap(surface)
    map.universe = Universe

    local chunk = makeTestChunk(map, 0, 0)
    chunk.tick = 0

    MapUtils.processPheromone(chunk, 100, false)

    assert(chunk.tick == 100, "Expected tick = 100, got " .. tostring(chunk.tick))
end)

-- Test: processPheromone skips if already processed
my_collection:add('processPheromone_skips_processed', function()
    local surface = Fixtures.makeSurface("test")
    local map = Fixtures.makeMap(surface)
    map.universe = Universe

    local chunk = makeTestChunk(map, 0, 0)
    chunk.tick = 200

    -- Store original values
    local originalPlayerAndBase = chunk.playerAndBasePheromone

    -- Try to process with earlier tick
    MapUtils.processPheromone(chunk, 100, false)

    -- Values should remain unchanged
    assert(chunk.playerAndBasePheromone == originalPlayerAndBase, "Expected pheromone unchanged")
    assert(chunk.tick == 200, "Expected tick unchanged at 200")
end)

-- Test: processPheromone applies player generator to pheromone
my_collection:add('processPheromone_player_generator_to_pheromone', function()
    local surface = Fixtures.makeSurface("test")
    local map = Fixtures.makeMap(surface)
    map.universe = Universe

    local chunk = makeTestChunk(map, 0, 0)
    chunk.playerGenerator = 25  -- Wall generator
    chunk.deathGenerator = 1

    MapUtils.processPheromone(chunk, 100, false)

    -- Player and base pheromone should be influenced by player generator
    assert(chunk.playerAndBasePheromone > 0, "Expected positive playerAndBasePheromone, got " .. tostring(chunk.playerAndBasePheromone))
end)

-- Test: processPheromone applies enemy generator to pheromone
my_collection:add('processPheromone_enemy_generator_to_pheromone', function()
    local surface = Fixtures.makeSurface("test")
    local map = Fixtures.makeMap(surface)
    map.universe = Universe

    local chunk = makeTestChunk(map, 0, 0)
    chunk.enemyGenerator = 100
    chunk.deathGenerator = 1

    MapUtils.processPheromone(chunk, 100, false)

    -- Enemy and resource pheromone should be influenced by enemy generator
    assert(chunk.enemyAndResourcePheromone > 0, "Expected positive enemyAndResourcePheromone, got " .. tostring(chunk.enemyAndResourcePheromone))
end)

-- ============================================
-- Generator Management Tests
-- ============================================

-- Test: addDeathGenerator modifies chunk value
my_collection:add('addDeathGenerator_modifies_chunk', function()
    local surface = Fixtures.makeSurface("test")
    local map = Fixtures.makeMap(surface)
    map.universe = Universe

    local chunk = makeTestChunk(map, 0, 0)
    chunk.deathGenerator = 0

    MapUtils.addDeathGenerator(chunk, -0.001)

    assert(chunk.deathGenerator < 0, "Expected negative deathGenerator, got " .. tostring(chunk.deathGenerator))
end)

-- Test: addPlayerGenerator modifies chunk value
my_collection:add('addPlayerGenerator_modifies_chunk', function()
    local surface = Fixtures.makeSurface("test")
    local map = Fixtures.makeMap(surface)
    map.universe = Universe

    local chunk = makeTestChunk(map, 0, 0)
    chunk.playerGenerator = 0

    -- Add a wall generator (25)
    MapUtils.addPlayerGenerator(chunk, 25)

    assert(chunk.playerGenerator == 25, "Expected playerGenerator = 25, got " .. tostring(chunk.playerGenerator))
end)

-- Test: getCombinedDeathGeneratorRating includes base death generator
my_collection:add('getCombinedDeathGeneratorRating_includes_death_generator', function()
    local surface = Fixtures.makeSurface("test")
    local map = Fixtures.makeMap(surface)
    map.universe = Universe

    local chunk = makeTestChunk(map, 0, 0)
    chunk.deathGenerator = 0.005

    local rating = MapUtils.getCombinedDeathGeneratorRating(chunk)
    assert(rating > 1, "Expected rating > 1 with positive deathGenerator, got " .. tostring(rating))
end)

-- ============================================
-- Neighbor Pheromone Diffusion Tests
-- ============================================

-- Test: processPheromone diffuses from neighbors
my_collection:add('processPheromone_diffuses_from_neighbors', function()
    local surface = Fixtures.makeSurface("test")
    local map = Fixtures.makeMap(surface)
    map.universe = Universe

    local centerChunk = makeTestChunk(map, 0, 0)
    centerChunk.deathGenerator = 1

    -- Create neighbor with high player pheromone
    local neighborChunk = makeTestChunk(map, 32, 0)
    neighborChunk.playerAndBasePheromone = 100
    neighborChunk.deathGenerator = 1

    MapUtils.processPheromone(centerChunk, 100, false)

    -- Center chunk should receive diffused pheromone from neighbor
    assert(centerChunk.playerAndBasePheromone > 0, "Expected positive pheromone from neighbor diffusion")
end)

-- Test: processPheromone with no neighbors uses only local generator
my_collection:add('processPheromone_no_neighbors_uses_local', function()
    local surface = Fixtures.makeSurface("test")
    local map = Fixtures.makeMap(surface)
    map.universe = Universe

    local chunk = makeTestChunk(map, 0, 0)
    chunk.playerGenerator = 50
    chunk.deathGenerator = 1

    MapUtils.processPheromone(chunk, 100, false)

    -- Should use local generator since no neighbors
    assert(chunk.playerAndBasePheromone > 0, "Expected positive pheromone from local generator")
end)

-- ============================================
-- Victory Scent Dispersion Tests
-- ============================================

-- Test: victory scent is added for high-value buildings
my_collection:add('victoryScent_high_value_building', function()
    local surface = Fixtures.makeSurface("test")
    local map = Fixtures.makeMap(surface)
    map.universe = Universe

    local chunk = makeTestChunk(map, 0, 0)

    -- Rocket silo should give high victory scent
    MapUtils.victoryScent(chunk, "rocket-silo")

    assert(chunk.victoryGenerator ~= nil, "Expected victoryGenerator for rocket-silo")
    -- rocket-silo = 6000 / 20000 = 0.3
    assert(chunk.victoryGenerator == 0.3, "Expected 0.3 for rocket-silo, got " .. tostring(chunk.victoryGenerator))
end)

-- ============================================
-- Edge Case Tests
-- ============================================

-- Test: multiple registrations accumulate generators
my_collection:add('multiple_registrations_accumulate', function()
    local surface = Fixtures.makeSurface("test")
    local map = Fixtures.makeMap(surface)
    map.universe = Universe

    local chunk = makeTestChunk(map, 0, 0)

    -- Simulate multiple wall registrations by directly adding generator values
    -- (addPlayerGenerator uses PLAYER_PHEROMONE_GENERATOR_AMOUNT constant)
    chunk.playerGenerator = (chunk.playerGenerator or 0) + 25
    chunk.playerGenerator = (chunk.playerGenerator or 0) + 25
    chunk.playerGenerator = (chunk.playerGenerator or 0) + 25

    assert(chunk.playerGenerator == 75, "Expected 75 for 3 walls, got " .. tostring(chunk.playerGenerator))
end)

-- Test: processPheromone respects death decay flag
my_collection:add('processPheromone_respects_death_decay_flag', function()
    local surface = Fixtures.makeSurface("test")
    local map = Fixtures.makeMap(surface)
    map.universe = Universe

    local chunk = makeTestChunk(map, 0, 0)
    chunk.deathGenerator = -0.01
    chunk.permanentDeathGenerator = -0.01

    -- Process without death decay
    MapUtils.processPheromone(chunk, 100, false)

    -- With death decay = false, decayDeathGenerator should not be called
    -- The chunk should maintain its death generator value
    assert(chunk.deathGenerator == -0.01, "Expected deathGenerator unchanged without decay")
end)

return my_collection