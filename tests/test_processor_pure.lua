-- Pure function tests for Processor module
-- Tests for dirty chunk processing logic

local TestCollection = require("tests/framework")
local Constants = require("Constants")

local my_collection = TestCollection:new()

-- Helper to create mock chunk
local function createChunk(id, x, y, valid)
    return {
        id = id,
        x = x,
        y = y,
        valid = valid,
        [Constants.CHUNK_TICK] = 0,
        [Constants.PLAYER_PHEROMONE] = 0,
        [Constants.ENEMY_PHEROMONE] = 0,
        map = {},
    }
end

-- Test: processDirtyChunk processes valid chunk
my_collection:add('processDirtyChunk_processes_valid_chunk', function()
    local Processor = require("libs/Processor")
    local MapUtils = require("libs/MapUtils")
    
    -- Mock processPheromone
    local original = MapUtils.processPheromone
    MapUtils.processPheromone = function() end
    
    local chunk = createChunk(123, 0, 0, true)
    
    -- Mock getNeighborChunks to return empty
    local originalNeighbors = MapUtils.getNeighborChunks
    MapUtils.getNeighborChunks = function() return {} end
    
    local success, neighborsToMark = Processor.processDirtyChunk(chunk, 100, 1)
    
    -- Restore
    MapUtils.processPheromone = original
    MapUtils.getNeighborChunks = originalNeighbors
    
    assert(success == true, "Should process valid chunk")
    assert(type(neighborsToMark) == "table", "Should return neighbors list")
end)

-- Test: processDirtyChunk skips invalid chunk
my_collection:add('processDirtyChunk_skips_invalid_chunk', function()
    local Processor = require("libs/Processor")
    
    local chunk = createChunk(123, 0, 0, false)
    
    local success, neighborsToMark = Processor.processDirtyChunk(chunk, 100, 1)
    
    assert(success == false, "Should skip invalid chunk")
    assert(neighborsToMark == 0, "Should return 0 neighbors")
end)

-- Test: processDirtyChunk skips nil chunk
my_collection:add('processDirtyChunk_skips_nil_chunk', function()
    local Processor = require("libs/Processor")
    
    local success, neighborsToMark = Processor.processDirtyChunk(nil, 100, 1)
    
    assert(success == false, "Should skip nil chunk")
    assert(neighborsToMark == 0, "Should return 0 neighbors")
end)

-- Test: processDirtyChunk collects neighbor IDs
my_collection:add('processDirtyChunk_collects_neighbors', function()
    local Processor = require("libs/Processor")
    local MapUtils = require("libs/MapUtils")
    
    -- Mock processPheromone
    local original = MapUtils.processPheromone
    MapUtils.processPheromone = function() end
    
    local chunk = createChunk(123, 0, 0, true)
    
    -- Mock getNeighborChunks to return neighbors
    local mockNeighbors = {
        [1] = { id = 1001, x = -1, y = -1 },
        [2] = -1,
        [3] = { id = 1002, x = 1, y = -1 },
        [4] = -1, [5] = -1, [6] = -1, [7] = -1, [8] = -1
    }
    local originalNeighbors = MapUtils.getNeighborChunks
    MapUtils.getNeighborChunks = function() return mockNeighbors end
    
    local success, neighborsToMark = Processor.processDirtyChunk(chunk, 100, 1)
    
    -- Restore
    MapUtils.processPheromone = original
    MapUtils.getNeighborChunks = originalNeighbors
    
    assert(success == true, "Should process chunk")
    assert(#neighborsToMark == 2, "Should collect 2 neighbors")
    assert(neighborsToMark[1].id == 1001, "First neighbor should be 1001")
    assert(neighborsToMark[1].priority == 2, "Neighbor priority should be 2")
end)

-- Test: processDirtyChunk increments priority for neighbors
my_collection:add('processDirtyChunk_increments_priority', function()
    local Processor = require("libs/Processor")
    local MapUtils = require("libs/MapUtils")
    
    -- Mock processPheromone
    local original = MapUtils.processPheromone
    MapUtils.processPheromone = function() end
    
    local chunk = createChunk(123, 0, 0, true)
    
    -- Mock getNeighborChunks
    local mockNeighbors = {
        [1] = { id = 1001, x = -1, y = -1 },
        [2] = -1, [3] = -1, [4] = -1, [5] = -1, [6] = -1, [7] = -1, [8] = -1
    }
    local originalNeighbors = MapUtils.getNeighborChunks
    MapUtils.getNeighborChunks = function() return mockNeighbors end
    
    local success, neighborsToMark = Processor.processDirtyChunk(chunk, 100, 3)
    
    -- Restore
    MapUtils.processPheromone = original
    MapUtils.getNeighborChunks = originalNeighbors
    
    assert(neighborsToMark[1].priority == 4, "Neighbor priority should be 4 (3 + 1)")
end)

return my_collection