-- Pure function tests for pheromone calculations
-- These tests don't require global.universe or Factorio

local TestCollection = require("tests/framework")
local Constants = require("Constants")

local my_collection = TestCollection:new()

-- Helper to create mock chunk
local function createChunk(pheromones)
    return {
        [Constants.CHUNK_TICK] = 0,
        [Constants.PLAYER_PHEROMONE] = pheromones.player or 0,
        [Constants.ENEMY_PHEROMONE] = pheromones.enemy or 0,
        [Constants.BASE_PHEROMONE] = pheromones.base or 0,
        [Constants.KAMIKAZE_PHEROMONE] = pheromones.kamikaze or 0,
        [Constants.RESOURCE_PHEROMONE] = pheromones.resource or 0,
        playerGenerator = pheromones.playerGenerator or 0,
        playerBaseGenerator = pheromones.playerBaseGenerator or 0,
        resourceGenerator = pheromones.resourceGenerator or 0,
        passable = Constants.CHUNK_ALL_DIRECTIONS,
    }
end

-- Helper to create mock neighbors
local function createNeighbors(pheromoneGrid)
    -- pheromoneGrid = { [1] = {player=100, enemy=10}, [2] = -1, ... }
    local neighbors = {}
    for i = 1, 8 do
        local data = pheromoneGrid[i]
        if data and data ~= -1 then
            neighbors[i] = createChunk(data)
        else
            neighbors[i] = -1
        end
    end
    return neighbors
end

-- Test: Pure function returns correct structure
my_collection:add('calculatePheromoneUpdates_returns_correct_structure', function()
    local MapUtils = require("libs/MapUtils")
    
    local chunk = createChunk({ player = 10, enemy = 5 })
    local neighbors = createNeighbors({
        [1] = { player = 100, enemy = 10 },
        [2] = -1, [3] = -1, [4] = -1, [5] = -1, [6] = -1, [7] = -1, [8] = -1
    })
    
    local updates = MapUtils.calculatePheromoneUpdates(chunk, neighbors, 100)
    
    assert(updates.player ~= nil, "Should have player pheromone")
    assert(updates.kamikaze ~= nil, "Should have kamikaze pheromone")
    assert(updates.deathGenerator ~= nil, "Should have death generator")
    -- base/enemy/resource are solved by the source-versioned field: this pass
    -- does not compute or write them, so they must not appear in the updates.
    assert(updates.base == nil, "base must not be computed by the transient pass")
    assert(updates.enemy == nil, "enemy must not be computed by the transient pass")
    assert(updates.resource == nil, "resource must not be computed by the transient pass")
end)

-- Test: Pheromone averages with neighbors
my_collection:add('calculatePheromoneUpdates_averages_player_pheromone', function()
    local MapUtils = require("libs/MapUtils")
    
    -- Chunk with 10, neighbor with 100 → average should be ~12.22 (110/9)
    local chunk = createChunk({ player = 10 })
    local neighbors = createNeighbors({
        [1] = { player = 100 },
        [2] = -1, [3] = -1, [4] = -1, [5] = -1, [6] = -1, [7] = -1, [8] = -1
    })
    
    local updates = MapUtils.calculatePheromoneUpdates(chunk, neighbors, 100)
    
    -- Expected: (10 + 100) / 9 * 0.98 * decay ≈ 11.9 (approximate due to decay rating)
    assert(updates.player > 10, "Player pheromone should increase from neighbor")
    assert(updates.player < 100, "Player pheromone should be averaged")
end)

-- Test: No neighbors = no change (except decay)
my_collection:add('calculatePheromoneUpdates_no_neighbors', function()
    local MapUtils = require("libs/MapUtils")
    
    local chunk = createChunk({ player = 100, enemy = 50 })
    local neighbors = createNeighbors({
        [1] = -1, [2] = -1, [3] = -1, [4] = -1,
        [5] = -1, [6] = -1, [7] = -1, [8] = -1
    })
    
    local updates = MapUtils.calculatePheromoneUpdates(chunk, neighbors, 100)
    
    -- With no neighbors, values should be based on chunk's own generators
    assert(updates.player ~= nil, "Should still calculate player pheromone")
end)

-- Test: Multiple neighbors contribute
my_collection:add('calculatePheromoneUpdates_multiple_neighbors', function()
    local MapUtils = require("libs/MapUtils")
    
    local chunk = createChunk({ player = 10 })
    local neighbors = createNeighbors({
        [1] = { player = 100 },  -- West
        [3] = { player = 200 },  -- East
        [6] = { player = 150 },  -- South
        [2] = -1, [4] = -1, [5] = -1, [7] = -1, [8] = -1
    })
    
    local updates = MapUtils.calculatePheromoneUpdates(chunk, neighbors, 100)
    
    -- Should average across 4 chunks (1 + 3 neighbors)
    assert(updates.player > 10, "Should absorb from multiple neighbors")
end)

-- Test: Kamikaze pheromone is seeded from the player field
my_collection:add('calculatePheromoneUpdates_calculates_kamikaze', function()
    local MapUtils = require("libs/MapUtils")
    
    local chunk = createChunk({ player = 100 })
    local neighbors = createNeighbors({
        [1] = { player = 200 },
        [2] = -1, [3] = -1, [4] = -1, [5] = -1, [6] = -1, [7] = -1, [8] = -1
    })
    
    local updates = MapUtils.calculatePheromoneUpdates(chunk, neighbors, 100)
    
    assert(updates.kamikaze > 0, "Should calculate kamikaze pheromone")
end)

return my_collection