-- Pure function tests for ChunkPropertyUtils
-- Tests for decay calculations and victory generator updates

local TestCollection = require("tests/framework")

local my_collection = TestCollection:new()

-- Every test needs the module initialized: a generator write registers its chunk
-- in the universe registry. The collection runs standalone and in registration
-- order, so it cannot rely on an earlier test having initialized it.
my_collection:set_setup(function()
    local ChunkPropertyUtils = require("libs/ChunkPropertyUtils")
    ChunkPropertyUtils.init({
        deathGeneratorChunks = {},
        playerGeneratorChunks = {},
        chunkPropertyUtilsQueries = { position = {} },
    }, nil)
    ChunkPropertyUtils.setSourceVersionedFieldRuntime(nil)
end)

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

-- Test: the generator decay is time-based, so the maintenance cadence is a
-- cost knob rather than a gameplay parameter.
my_collection:add('generator_decay_is_time_based', function()
    local ChunkPropertyUtils = require("libs/ChunkPropertyUtils")

    local split = { deathGenerator = 1 }
    ChunkPropertyUtils.decayDeathGenerator(split, 0)
    ChunkPropertyUtils.decayDeathGenerator(split, 100)
    ChunkPropertyUtils.decayDeathGenerator(split, 200)

    local whole = { deathGenerator = 1 }
    ChunkPropertyUtils.decayDeathGenerator(whole, 0)
    ChunkPropertyUtils.decayDeathGenerator(whole, 200)

    assert(math.abs(split.deathGenerator - whole.deathGenerator) < 1e-9,
        "two 100-tick decays must equal one 200-tick decay")
end)

-- Test: the reference cadence reproduces the old per-pass factor, so decoupling
-- does not change the tuned behaviour at the design cadence.
my_collection:add('generator_decay_reference_cadence_matches_per_pass', function()
    local ChunkPropertyUtils = require("libs/ChunkPropertyUtils")
    local Constants = require("Constants")

    local chunk = { deathGenerator = 1 }
    ChunkPropertyUtils.decayDeathGenerator(chunk, 0)
    ChunkPropertyUtils.decayDeathGenerator(chunk, Constants.DECAY_REFERENCE_TICKS)

    assert(math.abs(chunk.deathGenerator - Constants.MOVEMENT_GENERATOR_PERSISTANCE) < 1e-9,
        "one reference pass must equal the per-pass persistence, got " .. chunk.deathGenerator)
end)

-- Test: a deposit restarts the decay interval.
my_collection:add('generator_deposit_restarts_decay_interval', function()
    local ChunkPropertyUtils = require("libs/ChunkPropertyUtils")

    local chunk = { id = 1, deathGenerator = 1 }
    ChunkPropertyUtils.decayDeathGenerator(chunk, 0)
    ChunkPropertyUtils.addDeathGenerator(chunk, 0)
    ChunkPropertyUtils.decayDeathGenerator(chunk, 5000)

    assert(chunk.deathGenerator == 1,
        "a deposit must restart the decay interval, got " .. tostring(chunk.deathGenerator))
end)

-- The player field is source-versioned, so every write to chunk.playerGenerator
-- must reach the solver. The registry exists because a generator only decays
-- while its chunk is processed, and nothing re-marks the chunk a player walked
-- away from: without it a stale source would keep the field pointing at where
-- the player used to be.

local function playerFieldHarness()
    local ChunkPropertyUtils = require("libs/ChunkPropertyUtils")
    local universe = {
        playerGeneratorChunks = {},
        chunkPropertyUtilsQueries = { position = {} },
    }
    ChunkPropertyUtils.init(universe, nil)
    local queued = {}
    ChunkPropertyUtils.setSourceVersionedFieldRuntime({
        isInitialized = function() return true end,
        queueSource = function(chunk, fieldType, oldValue, newValue)
            queued[#queued + 1] = {
                id = chunk.id, fieldType = fieldType,
                old = oldValue, new = newValue,
            }
        end,
    })
    return ChunkPropertyUtils, universe, queued
end

local function clearQueued(queued)
    for i = #queued, 1, -1 do queued[i] = nil end
end

my_collection:add('addPlayerGenerator_registers_the_chunk_and_queues_the_source', function()
    local CPU, universe, queued = playerFieldHarness()
    local Constants = require("Constants")
    local chunk = { id = 7 }

    CPU.addPlayerGenerator(chunk, 1000)

    assert(chunk.playerGenerator == Constants.PLAYER_PHEROMONE_GENERATOR_AMOUNT,
        "a deposit must set the generator, got " .. tostring(chunk.playerGenerator))
    assert(universe.playerGeneratorChunks[7] == chunk,
        "the chunk must be registered so its decay is driven")
    -- The kamikaze field shares the player's sources, so one deposit queues both.
    assert(#queued == 2, "the deposit must queue a player and a kamikaze change")
    assert(queued[1].fieldType == Constants.PLAYER_PHEROMONE, "queued for the player field")
    assert(queued[1].new == chunk.playerGenerator, "queued value must be the generator")
    assert(queued[2].fieldType == Constants.KAMIKAZE_PHEROMONE, "queued for the kamikaze field")
    assert(math.abs(queued[2].new - chunk.playerGenerator * 0.95) < 1e-9,
        "the kamikaze source is the player generator scaled 0.95")
end)

my_collection:add('decayPlayerGenerator_queues_the_decayed_value', function()
    local CPU, _, queued = playerFieldHarness()
    local Constants = require("Constants")
    local chunk = { id = 8 }
    CPU.addPlayerGenerator(chunk, 1000)
    clearQueued(queued)

    -- First call after a deposit only stamps the interval.
    CPU.decayPlayerGenerator(chunk, 0)
    assert(#queued == 0, "stamping the interval is not a change")

    -- A full reference period decays by PLAYER_GENERATOR_PERSISTANCE.
    CPU.decayPlayerGenerator(chunk, Constants.DECAY_REFERENCE_TICKS)
    local expected = Constants.PLAYER_PHEROMONE_GENERATOR_AMOUNT * Constants.PLAYER_GENERATOR_PERSISTANCE
    assert(math.abs(chunk.playerGenerator - expected) < 1e-9,
        "expected " .. expected .. ", got " .. tostring(chunk.playerGenerator))
    assert(#queued == 2, "the decay must queue both fields' source changes")
    assert(queued[1].new == chunk.playerGenerator, "queued value must be the decayed generator")
end)

my_collection:add('decayPlayerGenerator_below_cutoff_unregisters_and_clears', function()
    local CPU, universe, queued = playerFieldHarness()
    local chunk = { id = 9 }
    CPU.addPlayerGenerator(chunk, 1000)
    clearQueued(queued)

    -- Below the 1e-4 cutoff the generator clears; the source must be removed and
    -- the chunk must leave the registry, or the pass re-marks it forever.
    chunk.playerGenerator = 0.00005
    chunk.playerGeneratorDecayTick = 0
    CPU.decayPlayerGenerator(chunk, 1)

    assert(chunk.playerGenerator == nil, "the generator must clear below the cutoff")
    assert(universe.playerGeneratorChunks[9] == nil, "the chunk must be unregistered")
    assert(#queued == 2, "clearing must queue both fields")
    assert(queued[1].new == 0 and queued[2].new == 0, "clearing must queue a zero source value")
end)

return my_collection
