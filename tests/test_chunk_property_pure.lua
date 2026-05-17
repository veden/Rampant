-- Pure function tests for ChunkPropertyUtils
-- Tests for decay calculations and victory generator updates

local TestCollection = require("tests/framework")

local my_collection = TestCollection:new()

-- Test: Death generator decay calculation
my_collection:add('calculateDeathGeneratorDecay_decay', function()
    local ChunkPropertyUtils = require("libs/ChunkPropertyUtils")
    
    local decayed = ChunkPropertyUtils.calculateDeathGeneratorDecay(1.0, 0.5)
    assert(decayed == 0.5, "Should decay by persistence factor")
end)

-- Test: Death generator decay to nil (below threshold)
my_collection:add('calculateDeathGeneratorDecay_below_threshold', function()
    local ChunkPropertyUtils = require("libs/ChunkPropertyUtils")
    
    local decayed = ChunkPropertyUtils.calculateDeathGeneratorDecay(0.00005, 0.5)
    assert(decayed == nil, "Should return nil when below threshold")
end)

-- Test: Death generator decay from nil
my_collection:add('calculateDeathGeneratorDecay_nil_input', function()
    local ChunkPropertyUtils = require("libs/ChunkPropertyUtils")
    
    local decayed = ChunkPropertyUtils.calculateDeathGeneratorDecay(nil, 0.5)
    assert(decayed == nil, "Should return nil for nil input")
end)

-- Test: Death generator decay negative values
my_collection:add('calculateDeathGeneratorDecay_negative', function()
    local ChunkPropertyUtils = require("libs/ChunkPropertyUtils")
    
    local decayed = ChunkPropertyUtils.calculateDeathGeneratorDecay(-1.0, 0.5)
    assert(decayed == -0.5, "Should decay negative values")
end)

-- Test: Player generator decay calculation
my_collection:add('calculatePlayerGeneratorDecay_decay', function()
    local ChunkPropertyUtils = require("libs/ChunkPropertyUtils")
    
    local decayed = ChunkPropertyUtils.calculatePlayerGeneratorDecay(100, 0.9)
    assert(decayed == 90, "Should decay by persistence factor")
end)

-- Test: Player generator decay to nil
my_collection:add('calculatePlayerGeneratorDecay_below_threshold', function()
    local ChunkPropertyUtils = require("libs/ChunkPropertyUtils")
    
    local decayed = ChunkPropertyUtils.calculatePlayerGeneratorDecay(0.00005, 0.9)
    assert(decayed == nil, "Should return nil when below threshold")
end)

-- Test: Victory generator update calculation
my_collection:add('calculateVictoryGeneratorUpdate_add', function()
    local ChunkPropertyUtils = require("libs/ChunkPropertyUtils")
    
    local update = ChunkPropertyUtils.calculateVictoryGeneratorUpdate(123, 10, 5)
    assert(update.chunkId == 123, "Should preserve chunk ID")
    assert(update.victory == 15, "Should add values")
end)

-- Test: Victory generator update from nil
my_collection:add('calculateVictoryGeneratorUpdate_from_nil', function()
    local ChunkPropertyUtils = require("libs/ChunkPropertyUtils")
    
    local update = ChunkPropertyUtils.calculateVictoryGeneratorUpdate(123, nil, 5)
    assert(update.victory == 5, "Should start from 0 when nil")
end)

-- Test: Victory generator update negative
my_collection:add('calculateVictoryGeneratorUpdate_negative', function()
    local ChunkPropertyUtils = require("libs/ChunkPropertyUtils")
    
    local update = ChunkPropertyUtils.calculateVictoryGeneratorUpdate(123, 10, -3)
    assert(update.victory == 7, "Should handle negative values")
end)

-- Test: getCombinedDeathGenerator clamps high values
my_collection:add('getCombinedDeathGenerator_clamps_high', function()
    local ChunkPropertyUtils = require("libs/ChunkPropertyUtils")
    
    local chunk = { deathGenerator = 0.8, permanentDeathGenerator = 0.5 }
    local result = ChunkPropertyUtils.getCombinedDeathGenerator(chunk)
    assert(result == 1, "Should clamp to 1")
end)

-- Test: getCombinedDeathGenerator clamps low values
my_collection:add('getCombinedDeathGenerator_clamps_low', function()
    local ChunkPropertyUtils = require("libs/ChunkPropertyUtils")
    
    local chunk = { deathGenerator = -0.8, permanentDeathGenerator = -0.5 }
    local result = ChunkPropertyUtils.getCombinedDeathGenerator(chunk)
    assert(result == -1, "Should clamp to -1")
end)

-- Test: getCombinedDeathGenerator returns actual sum
my_collection:add('getCombinedDeathGenerator_returns_sum', function()
    local ChunkPropertyUtils = require("libs/ChunkPropertyUtils")
    
    local chunk = { deathGenerator = 0.3, permanentDeathGenerator = 0.2 }
    local result = ChunkPropertyUtils.getCombinedDeathGenerator(chunk)
    assert(result == 0.5, "Should return sum")
end)

-- Test: getCombinedDeathGeneratorRating clamps high
my_collection:add('getCombinedDeathGeneratorRating_clamps_high', function()
    local ChunkPropertyUtils = require("libs/ChunkPropertyUtils")
    
    local chunk = { deathGenerator = 0.8, permanentDeathGenerator = 0.5 }
    local result = ChunkPropertyUtils.getCombinedDeathGeneratorRating(chunk)
    assert(result == 1, "Should clamp to 1")
end)

-- Test: getCombinedDeathGeneratorRating returns actual rating (within bounds)
my_collection:add('getCombinedDeathGeneratorRating_returns_rating', function()
    local ChunkPropertyUtils = require("libs/ChunkPropertyUtils")
    
    -- Sum = 0.3 + 0.2 = 0.5, 1 + 0.5 = 1.5, but clamped to 1
    local chunk = { deathGenerator = 0.3, permanentDeathGenerator = 0.2 }
    local result = ChunkPropertyUtils.getCombinedDeathGeneratorRating(chunk)
    assert(result == 1, "Should clamp to 1 when > 1")
end)

-- Test: getCombinedDeathGeneratorRating handles medium values
my_collection:add('getCombinedDeathGeneratorRating_handles_medium', function()
    local ChunkPropertyUtils = require("libs/ChunkPropertyUtils")
    
    -- Sum = 0.1 + 0.1 = 0.2, 1 + 0.2 = 1.2, but clamped to 1
    local chunk = { deathGenerator = 0.1, permanentDeathGenerator = 0.1 }
    local result = ChunkPropertyUtils.getCombinedDeathGeneratorRating(chunk)
    assert(result == 1, "Should clamp to 1 when > 1")
end)

-- Test: getCombinedDeathGeneratorRating handles low values (not clamped)
my_collection:add('getCombinedDeathGeneratorRating_handles_low', function()
    local ChunkPropertyUtils = require("libs/ChunkPropertyUtils")
    
    -- Sum = -0.5 + -0.3 = -0.8, 1 + (-0.8) = 0.2
    local chunk = { deathGenerator = -0.5, permanentDeathGenerator = -0.3 }
    local result = ChunkPropertyUtils.getCombinedDeathGeneratorRating(chunk)
    assert(math.abs(result - 0.2) < 0.0001, "Should return ~0.2 when in range (0, 1], got " .. result)
end)

-- Test: getCombinedDeathGeneratorRating handles very low values
my_collection:add('getCombinedDeathGeneratorRating_very_low', function()
    local ChunkPropertyUtils = require("libs/ChunkPropertyUtils")
    
    local chunk = { deathGenerator = -1.0, permanentDeathGenerator = -1.0 }
    local result = ChunkPropertyUtils.getCombinedDeathGeneratorRating(chunk)
    assert(result == 0.001, "Should clamp to 0.001")
end)

return my_collection