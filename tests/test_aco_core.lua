-- ACO Core Tests
-- Tests for decay tables, dirty chunks, and decay-on-read

local TestCollection = require("tests/framework")
local Fixtures = require("tests/fixtures")

local my_collection = TestCollection:new()

local function resetDirty(universe)
    universe.dirtyChunks = {
        priority1 = {}, priority2 = {}, priority3 = {},
        priority4 = {}, priority5 = {},
    }
    universe.dirtyChunkSet = {}
end


-- Create mock universe and initialize modules
local Universe = Fixtures.makeUniverse()
package.loaded["libs/Constants"] = nil
local Constants = require("libs/Constants")

-- ============================================
-- Decay Table Tests
-- ============================================

my_collection:add('decay_tables_created_correctly', function()
    -- Verify all decay tables exist
    assert(Constants.DECAY_090, "DECAY_090 table should exist")
    assert(Constants.DECAY_092, "DECAY_092 table should exist")
    assert(Constants.DECAY_095, "DECAY_095 table should exist")
    assert(Constants.DECAY_098, "DECAY_098 table should exist")
    assert(Constants.DECAY_TRAIL, "DECAY_TRAIL table should exist")
    
    -- Verify table size (Lua tables with index 0 have length n-1)
    assert(#Constants.DECAY_090 >= 10000, "DECAY_090 should have at least 10000 entries")
    assert(Constants.DECAY_090[0] ~= nil, "DECAY_090[0] should exist")
    assert(Constants.DECAY_090[10000] ~= nil, "DECAY_090[10000] should exist")
    
    -- Verify decay calculations
    local decay100 = Constants.DECAY_090[100]
    local expected100 = 0.90 ^ 100
    assert(math.abs(decay100 - expected100) < 0.0001, 
        "DECAY_090[100] should be " .. expected100 .. " but got " .. decay100)
    
    local decay500 = Constants.DECAY_098[500]
    local expected500 = 0.98 ^ 500
    assert(math.abs(decay500 - expected500) < 0.0001,
        "DECAY_098[500] should be " .. expected500 .. " but got " .. decay500)
end)

my_collection:add('decay_table_clamps_at_maximum', function()
    -- Access beyond table depth returns nil, caller should clamp
    local decayBeyond = Constants.DECAY_090[20000]
    local decayMax = Constants.DECAY_090[10000]
    -- The table itself doesn't clamp; usage code should handle this
    assert(decayBeyond == nil, "Access beyond depth returns nil")
    assert(decayMax ~= nil, "Max depth value exists")
    -- Verify clamping logic works when used properly
    local clampedValue = decayBeyond or decayMax
    assert(clampedValue == decayMax, "Clamping logic should use max value")
end)

-- ============================================
-- Dirty Chunk Tests
-- ============================================

my_collection:add('markChunkDirty_adds_to_priority_queue', function()
    local surface = Fixtures.makeSurface("test")
    local map = Fixtures.makeMap(surface)
    map.universe = Universe
    
    package.loaded["libs/MapUtils"] = nil
    local MapUtils = require("libs/MapUtils")
    MapUtils.init(Universe)

    resetDirty(Universe)    
    -- Mark chunk dirty
    MapUtils.markChunkDirty(123, 1, 1000)
    
    -- Verify added to priority queue
    assert(#Universe.dirtyChunks.priority1 == 1, "Should have 1 chunk in priority 1")
    assert(Universe.dirtyChunkSet[123] == 1, "Should be in chunk set with priority 1")
end)

my_collection:add('markChunkDirty_prevents_duplicates', function()
    local surface = Fixtures.makeSurface("test")
    local map = Fixtures.makeMap(surface)
    map.universe = Universe
    
    package.loaded["libs/MapUtils"] = nil
    local MapUtils = require("libs/MapUtils")
    MapUtils.init(Universe)

    resetDirty(Universe)    
    -- Mark same chunk dirty twice
    MapUtils.markChunkDirty(123, 1, 1000)
    MapUtils.markChunkDirty(123, 1, 1001)
    
    -- Should still be only one entry
    assert(#Universe.dirtyChunks.priority1 == 1, "Should still have only 1 chunk")
end)

my_collection:add('markChunkDirty_upgrades_priority', function()
    local surface = Fixtures.makeSurface("test")
    local map = Fixtures.makeMap(surface)
    map.universe = Universe
    
    package.loaded["libs/MapUtils"] = nil
    local MapUtils = require("libs/MapUtils")
    MapUtils.init(Universe)

    resetDirty(Universe)    
    -- Mark chunk with low priority, then high priority
    MapUtils.markChunkDirty(123, 3, 1000)
    MapUtils.markChunkDirty(123, 1, 1001)
    
    -- Should be upgraded to priority 1
    assert(Universe.dirtyChunkSet[123] == 1, "Should be upgraded to priority 1")
    assert(#Universe.dirtyChunks.priority1 == 1, "Should be in priority 1 list")
end)

my_collection:add('markChunkDirty_ignores_invalid_chunks', function()
    local surface = Fixtures.makeSurface("test")
    local map = Fixtures.makeMap(surface)
    map.universe = Universe
    
    package.loaded["libs/MapUtils"] = nil
    local MapUtils = require("libs/MapUtils")
    MapUtils.init(Universe)

    resetDirty(Universe)    
    -- Mark invalid chunk
    MapUtils.markChunkDirty(-1, 1, 1000)
    MapUtils.markChunkDirty(nil, 1, 1000)
    
    -- Should not add to queue
    assert(#Universe.dirtyChunks.priority1 == 0, "Should not add invalid chunks")
end)

-- ============================================
-- Decay-on-Read Tests
-- ============================================

my_collection:add('getEffectivePheromone_applies_decay', function()
    local surface = Fixtures.makeSurface("test")
    local map = Fixtures.makeMap(surface)
    map.universe = Universe
    
    package.loaded["libs/ChunkPropertyUtils"] = nil
    local ChunkPropertyUtils = require("libs/ChunkPropertyUtils")
    ChunkPropertyUtils.init(Universe)
    
    -- Create chunk with pheromone
    local chunk = {
        PLAYER_PHEROMONE = 100,
        playerLastRefresh = 0
    }
    
    -- Get effective value after 100 ticks
    local effective = ChunkPropertyUtils.getEffectivePheromone(chunk, 'PLAYER_PHEROMONE', 100)
    local expected = 100 * (0.98 ^ 100)
    
    assert(math.abs(effective - expected) < 0.01,
        "Expected " .. expected .. " but got " .. effective)
end)

my_collection:add('getEffectivePheromone_returns_zero_for_nil', function()
    local surface = Fixtures.makeSurface("test")
    local map = Fixtures.makeMap(surface)
    map.universe = Universe
    
    package.loaded["libs/ChunkPropertyUtils"] = nil
    local ChunkPropertyUtils = require("libs/ChunkPropertyUtils")
    ChunkPropertyUtils.init(Universe)
    
    local chunk = {
        PLAYER_PHEROMONE = nil,
        playerLastRefresh = 0
    }
    
    local effective = ChunkPropertyUtils.getEffectivePheromone(chunk, 'PLAYER_PHEROMONE', 100)
    assert(effective == 0, "Should return 0 for nil pheromone")
end)

my_collection:add('getEffectivePheromone_uses_correct_decay_rate', function()
    local surface = Fixtures.makeSurface("test")
    local map = Fixtures.makeMap(surface)
    map.universe = Universe
    
    package.loaded["libs/ChunkPropertyUtils"] = nil
    local ChunkPropertyUtils = require("libs/ChunkPropertyUtils")
    ChunkPropertyUtils.init(Universe)
    
    -- Test different pheromone types with different decay rates
    local chunk = {
        PLAYER_PHEROMONE = 100,
        BASE_PHEROMONE = 100,
        playerLastRefresh = 0,
        baseLastRefresh = 0
    }
    
    local playerEffective = ChunkPropertyUtils.getEffectivePheromone(chunk, 'PLAYER_PHEROMONE', 100)
    local baseEffective = ChunkPropertyUtils.getEffectivePheromone(chunk, 'BASE_PHEROMONE', 100)
    
    -- Player uses 0.98, Base uses 0.90
    assert(playerEffective > baseEffective, "Player should decay slower than base")
end)

-- ============================================
-- Process Dirty Chunks Tests
-- ============================================

my_collection:add('processDirtyChunks_processes_by_priority', function()
    local surface = Fixtures.makeSurface("test")
    local map = Fixtures.makeMap(surface)
    map.universe = Universe
    
    -- Set up Universe.maps for getChunkById (maps indexed by surface index)
    local chunksTable = {}
    Universe.maps = { [1] = chunksTable }
    
    -- Create minimal mock chunks
    local chunk1 = { id = 1, valid = true, map = map, x = 0, y = 0 }
    local chunk2 = { id = 2, valid = true, map = map, x = 1, y = 0 }
    chunksTable[0] = { [0] = chunk1 }
    chunksTable[1] = { [0] = chunk2 }
    
    package.loaded["libs/MapUtils"] = nil
    local MapUtils = require("libs/MapUtils")
    MapUtils.init(Universe)

    resetDirty(Universe)    
    package.loaded["libs/Processor"] = nil
    local Processor = require("libs/Processor")
    Processor.init(Universe)

    -- Register chunks in the reverse index (mirrors createChunk in production)
    Universe.chunkById[1] = chunk1
    Universe.chunkById[2] = chunk2
    
    -- Add chunks to different priorities
    Universe.dirtyChunks.priority1 = {1, 2}
    Universe.dirtyChunks.priority2 = {3, 4, 5}
    Universe.dirtyChunks.priority3 = {6}
    Universe.dirtyChunkSet = {[1]=1, [2]=1, [3]=2, [4]=2, [5]=2, [6]=3}
    
    -- Mock processPheromone and getNeighborChunks to avoid side effects
    local originalProcessPheromone = MapUtils.processPheromone
    local originalGetNeighborChunks = MapUtils.getNeighborChunks
    MapUtils.processPheromone = function() end
    MapUtils.getNeighborChunks = function() return {} end
    
    -- Process (should process priority 1 first, then priority 2)
    Processor.processDirtyChunks(1000, Universe)
    
    -- Restore
    MapUtils.processPheromone = originalProcessPheromone
    MapUtils.getNeighborChunks = originalGetNeighborChunks
    
    -- Priority 1 should be empty (processed), priority 2 should have chunks left
    assert(#Universe.dirtyChunks.priority1 == 0, "Priority 1 should be empty")
    assert(#Universe.dirtyChunks.priority2 < 3, "Priority 2 should have some chunks processed")
end)

my_collection:add('processDirtyChunks_respects_budget', function()
    local surface = Fixtures.makeSurface("test")
    local map = Fixtures.makeMap(surface)
    map.universe = Universe

    -- Set up Universe.maps for getChunkById (maps indexed by surface index)
    local chunksTable = {}
    Universe.maps = { [1] = chunksTable }

    -- Budget comes from the constant (retuned 40 -> 200 in recent work)
    local budget = Constants.PROCESS_DIRTY_CHUNKS_PER_TICK
    local totalChunks = budget + 100  -- exceed budget so remaining is measurable

    -- Create minimal mock chunks
    for i = 1, totalChunks do
        local chunk = { id = i, valid = true, map = map, x = i, y = 0 }
        chunksTable[i] = { [0] = chunk }
    end

    package.loaded["libs/MapUtils"] = nil
    local MapUtils = require("libs/MapUtils")
    MapUtils.init(Universe)

    resetDirty(Universe)
    package.loaded["libs/Processor"] = nil
    local Processor = require("libs/Processor")
    Processor.init(Universe)

    -- Register chunks in the reverse index (mirrors createChunk in production)
    for i = 1, totalChunks do
        Universe.chunkById[i] = chunksTable[i][0]
    end

    -- Add chunks to priority 1
    for i = 1, totalChunks do
        Universe.dirtyChunks.priority1[i] = i
        Universe.dirtyChunkSet[i] = 1
    end

    -- Mock processPheromone and getNeighborChunks to avoid complex chunk requirements
    local originalProcessPheromone = MapUtils.processPheromone
    local originalGetNeighborChunks = MapUtils.getNeighborChunks
    MapUtils.processPheromone = function() end
    MapUtils.getNeighborChunks = function() return {} end

    -- Process
    Processor.processDirtyChunks(1000, Universe)

    -- Restore
    MapUtils.processPheromone = originalProcessPheromone
    MapUtils.getNeighborChunks = originalGetNeighborChunks

    -- Should have processed exactly `budget` chunks (budget limit)
    assert(#Universe.dirtyChunks.priority1 == totalChunks - budget,
        "Should have processed " .. budget .. " chunks, remaining=" .. #Universe.dirtyChunks.priority1)
end)

return my_collection