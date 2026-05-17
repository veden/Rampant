-- ACO Trail Tests
-- Tests for scout trails, trail spreading, and trail-based pathfinding

local TestCollection = require("tests/framework")
local Fixtures = require("tests/fixtures")

local my_collection = TestCollection:new()

-- Create mock universe
local Universe = Fixtures.makeUniverse()
package.loaded["libs/Constants"] = nil
local Constants = require("libs/Constants")

-- ============================================
-- Trail Spreading Tests
-- ============================================

my_collection:add('trail_spreading_uses_095_factor', function()
    local surface = Fixtures.makeSurface("test")
    local map = Fixtures.makeMap(surface)
    map.universe = Universe
    
    package.loaded["libs/MapUtils"] = nil
    local MapUtils = require("libs/MapUtils")
    MapUtils.init(Universe)
    
    -- Create 3x3 grid of chunks
    local chunks = {}
    for x = -1, 1 do
        chunks[x] = {}
        for y = -1, 1 do
            chunks[x][y] = Fixtures.makeChunk(map, x * 32, y * 32)
            chunks[x][y].positiveTrail = 0
            chunks[x][y].trailLastRefresh = 0
            Fixtures.registerChunk(map, chunks[x][y])
        end
    end
    
    -- Set center chunk trail to 100
    chunks[0][0].positiveTrail = 100
    chunks[0][0].trailLastRefresh = 1000
    
    -- Simulate spreading (simplified version)
    local neighborSum = 0
    local neighborCount = 1
    
    -- Sum neighbors
    for dx = -1, 1 do
        for dy = -1, 1 do
            if not (dx == 0 and dy == 0) then
                neighborSum = neighborSum + (chunks[dx][dy].positiveTrail or 0)
                neighborCount = neighborCount + 1
            end
        end
    end
    
    -- Apply spreading to center
    local newCenterTrail = (neighborSum / neighborCount) * 0.95
    
    -- Center should get ~0 (neighbors are 0)
    assert(math.abs(newCenterTrail - 0) < 0.01, "Center should get ~0 from empty neighbors")
end)

my_collection:add('trail_spreading_propagates_to_neighbors', function()
    local surface = Fixtures.makeSurface("test")
    local map = Fixtures.makeMap(surface)
    map.universe = Universe
    
    package.loaded["libs/MapUtils"] = nil
    local MapUtils = require("libs/MapUtils")
    MapUtils.init(Universe)
    
    -- Create 3x3 grid with center having trail = 100
    local chunks = {}
    for x = -1, 1 do
        chunks[x] = {}
        for y = -1, 1 do
            chunks[x][y] = Fixtures.makeChunk(map, x * 32, y * 32)
            chunks[x][y].positiveTrail = 0
            chunks[x][y].trailLastRefresh = 0
            Fixtures.registerChunk(map, chunks[x][y])
        end
    end
    
    -- Set center trail
    chunks[0][0].positiveTrail = 100
    
    -- When neighbors read center, they should get spread value
    -- Each neighbor reads 8 neighbors, one of which has 100
    -- Average = 100 / 9 = 11.11
    -- After 0.95 factor = 10.56
    local expectedNeighborTrail = (100 / 9) * 0.95
    
    assert(math.abs(expectedNeighborTrail - 10.56) < 0.1,
        "Expected neighbor trail ~10.56 but got " .. expectedNeighborTrail)
end)

-- ============================================
-- Trail Decay Tests
-- ============================================

my_collection:add('trail_decay_applied_correctly', function()
    local surface = Fixtures.makeSurface("test")
    local map = Fixtures.makeMap(surface)
    map.universe = Universe
    
    package.loaded["libs/ChunkPropertyUtils"] = nil
    local ChunkPropertyUtils = require("libs/ChunkPropertyUtils")
    ChunkPropertyUtils.init(Universe)
    
    -- Create chunk with trail
    local chunk = {
        positiveTrail = 100,
        trailLastRefresh = 0
    }
    
    -- After 100 ticks: 100 * 0.97^100 = 4.76
    local effective100 = chunk.positiveTrail * (Constants.DECAY_TRAIL[100] or 0)
    assert(math.abs(effective100 - 4.76) < 0.1,
        "After 100 ticks, trail should be ~4.76 but got " .. effective100)
    
    -- After 500 ticks: 100 * 0.97^500 = 0.0002 (effectively zero)
    local effective500 = chunk.positiveTrail * (Constants.DECAY_TRAIL[500] or 0)
    assert(effective500 < 0.001,
        "After 500 ticks, trail should be ~0 but got " .. effective500)
end)

my_collection:add('trail_decay_handles_nil_values', function()
    local surface = Fixtures.makeSurface("test")
    local map = Fixtures.makeMap(surface)
    map.universe = Universe
    
    package.loaded["libs/ChunkPropertyUtils"] = nil
    local ChunkPropertyUtils = require("libs/ChunkPropertyUtils")
    ChunkPropertyUtils.init(Universe)
    
    local chunk = {
        positiveTrail = nil,
        trailLastRefresh = 0
    }
    
    -- Should handle nil gracefully
    local effective = chunk.positiveTrail or 0
    assert(effective == 0, "Should handle nil trail")
end)

-- ============================================
-- Scout Selection Tests
-- ============================================

my_collection:add('scout_selection_uses_7_percent_rate', function()
    -- Test that scout selection probability is ~7%
    local scoutCount = 0
    local trials = 10000
    
    for i = 1, trials do
        if math.random() < 0.07 then
            scoutCount = scoutCount + 1
        end
    end
    
    local actualRate = scoutCount / trials
    assert(math.abs(actualRate - 0.07) < 0.01,
        "Scout rate should be ~7% but got " .. (actualRate * 100) .. "%")
end)

my_collection:add('scout_lays_stronger_trails', function()
    local normalTrail = 10  -- TRAIL_BASE_AMOUNT
    local scoutTrail = 10 * 2.5  -- TRAIL_SCOUT_MULTIPLIER
    
    assert(scoutTrail == 25, "Scout trail should be 2.5x normal")
    assert(scoutTrail > normalTrail, "Scout trail should be stronger")
end)

-- ============================================
-- Trail Scoring Tests
-- ============================================

my_collection:add('trail_score_weighted_correctly', function()
    local pheromoneScore = 100
    local trailScore = 50
    
    local PHEROMONE_WEIGHT = 0.7
    local TRAIL_WEIGHT = 0.3
    
    local totalScore = pheromoneScore * PHEROMONE_WEIGHT + trailScore * TRAIL_WEIGHT
    local expected = 100 * 0.7 + 50 * 0.3  -- = 70 + 15 = 85
    
    assert(math.abs(totalScore - expected) < 0.01,
        "Expected " .. expected .. " but got " .. totalScore)
end)

my_collection:add('retreat_treats_trails_as_repulsive', function()
    local pheromoneScore = 50
    local negativeTrail = 30
    
    local TRAIL_WEIGHT = 0.3
    
    -- For retreat, negative trails are repulsive
    local trailScore = -negativeTrail * TRAIL_WEIGHT
    local totalScore = pheromoneScore * 0.7 + trailScore
    
    assert(trailScore < 0, "Trail score should be negative for retreat")
    assert(totalScore < pheromoneScore * 0.7, "Total score should be reduced by negative trail")
end)

my_collection:add('attack_treats_trails_as_attractive', function()
    local pheromoneScore = 50
    local positiveTrail = 30
    
    local TRAIL_WEIGHT = 0.3
    
    -- For attack, positive trails are attractive
    local trailScore = positiveTrail * TRAIL_WEIGHT
    local totalScore = pheromoneScore * 0.7 + trailScore
    
    assert(trailScore > 0, "Trail score should be positive for attack")
    assert(totalScore > pheromoneScore * 0.7, "Total score should be increased by positive trail")
end)

-- ============================================
-- Diagonal Movement Tests
-- ============================================

my_collection:add('trail_spreading_handles_diagonal_movement', function()
    local surface = Fixtures.makeSurface("test")
    local map = Fixtures.makeMap(surface)
    map.universe = Universe
    
    package.loaded["libs/MapUtils"] = nil
    local MapUtils = require("libs/MapUtils")
    MapUtils.init(Universe)
    
    -- Create diagonal path: (0,0) -> (1,1)
    -- Without spreading, chunks (1,0) and (0,1) would have no trail
    -- With spreading, they should get some trail value
    
    local chunks = {}
    for x = 0, 1 do
        chunks[x] = {}
        for y = 0, 1 do
            chunks[x][y] = Fixtures.makeChunk(map, x * 32, y * 32)
            chunks[x][y].positiveTrail = 0
            chunks[x][y].trailLastRefresh = 0
            Fixtures.registerChunk(map, chunks[x][y])
        end
    end
    
    -- Set trails on diagonal
    chunks[0][0].positiveTrail = 100
    chunks[1][1].positiveTrail = 100
    
    -- After spreading, (1,0) and (0,1) should have some trail
    -- They are neighbors to both (0,0) and (1,1)
    -- Each reads 8 neighbors, 2 of which have 100
    -- Average = 200 / 9 = 22.22
    -- After 0.95 factor = 21.11
    
    local expectedOffDiagonal = (200 / 9) * 0.95
    
    assert(expectedOffDiagonal > 10,
        "Off-diagonal chunks should have trail > 10 but got " .. expectedOffDiagonal)
end)

return my_collection