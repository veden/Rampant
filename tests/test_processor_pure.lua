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
    
    local success = Processor.processDirtyChunk(chunk, 100, 1)

    -- Restore
    MapUtils.processPheromone = original
    MapUtils.getNeighborChunks = originalNeighbors

    assert(success == true, "Should process valid chunk")
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

-- Test: processDirtyChunk collects neighbor IDs when pheromones changed
my_collection:add('processDirtyChunk_collects_neighbors', function()
    local Processor = require("libs/Processor")
    local MapUtils = require("libs/MapUtils")
    
    -- Mock processPheromone to produce a material change (the epsilon gate
    -- only propagates when values actually change). The gate watches the only
    -- field this pass maintains: the death generator.
    local original = MapUtils.processPheromone
    MapUtils.processPheromone = function(chunk)
        chunk.deathGenerator = 1
    end
    
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
    
    local marked = {}
    local originalMark = MapUtils.markChunkDirty
    MapUtils.markChunkDirty = function(id, priority, tick, universe)
        marked[#marked + 1] = { id = id, priority = priority }
    end

    local success = Processor.processDirtyChunk(chunk, 100, 1)

    -- Restore
    MapUtils.processPheromone = original
    MapUtils.getNeighborChunks = originalNeighbors
    MapUtils.markChunkDirty = originalMark

    assert(success == true, "Should process chunk")
    assert(#marked == 2, "Should mark 2 neighbors")
    assert(marked[1].id == 1001, "First neighbor should be 1001")
    assert(marked[1].priority == 2, "Neighbor priority should be 2")
end)

-- Test: no material change -> no propagation (epsilon gate contract)
my_collection:add('processDirtyChunk_no_change_no_propagation', function()
    local Processor = require("libs/Processor")
    local MapUtils = require("libs/MapUtils")
    
    -- Mock processPheromone as a no-op: values do not change, so the
    -- cascade must NOT propagate (prevents perpetual re-processing)
    local original = MapUtils.processPheromone
    MapUtils.processPheromone = function() end
    
    local chunk = createChunk(123, 0, 0, true)
    
    local mockNeighbors = {
        [1] = { id = 1001, x = -1, y = -1 },
        [2] = -1, [3] = -1, [4] = -1, [5] = -1, [6] = -1, [7] = -1, [8] = -1
    }
    local originalNeighbors = MapUtils.getNeighborChunks
    MapUtils.getNeighborChunks = function() return mockNeighbors end
    
    local marked = {}
    local originalMark = MapUtils.markChunkDirty
    MapUtils.markChunkDirty = function(id, priority, tick, universe)
        marked[#marked + 1] = { id = id, priority = priority }
    end

    local success = Processor.processDirtyChunk(chunk, 100, 1)

    -- Restore
    MapUtils.processPheromone = original
    MapUtils.getNeighborChunks = originalNeighbors
    MapUtils.markChunkDirty = originalMark

    assert(success == true, "Should process chunk")
    assert(#marked == 0, "No change should not propagate (no neighbor marked)")
end)

-- Test: deathGenerator decay alone must propagate (prevents death clouds
-- from freezing when the 5 pheromones have converged)
my_collection:add('processDirtyChunk_death_change_propagates', function()
    local Processor = require("libs/Processor")
    local MapUtils = require("libs/MapUtils")

    -- Mock processPheromone to only change the death generator (pheromones
    -- stay converged at 0)
    local original = MapUtils.processPheromone
    MapUtils.processPheromone = function(chunk)
        chunk.deathGenerator = (chunk.deathGenerator or 0) * 0.92
    end

    local chunk = createChunk(123, 0, 0, true)
    chunk.deathGenerator = -0.5

    local mockNeighbors = {
        [1] = { id = 1001, x = -1, y = -1 },
        [2] = -1, [3] = -1, [4] = -1, [5] = -1, [6] = -1, [7] = -1, [8] = -1
    }
    local originalNeighbors = MapUtils.getNeighborChunks
    MapUtils.getNeighborChunks = function() return mockNeighbors end

    local marked = {}
    local originalMark = MapUtils.markChunkDirty
    MapUtils.markChunkDirty = function(id, priority, tick, universe)
        marked[#marked + 1] = { id = id, priority = priority }
    end

    local success = Processor.processDirtyChunk(chunk, 100, 1)

    -- Restore
    MapUtils.processPheromone = original
    MapUtils.getNeighborChunks = originalNeighbors
    MapUtils.markChunkDirty = originalMark

    assert(success == true, "Should process chunk")
    assert(#marked == 1, "Death generator decay should propagate (one neighbor marked)")
    assert(marked[1].id == 1001, "The movable neighbor is marked")
end)

-- Test: processDirtyChunk increments priority for neighbors
my_collection:add('processDirtyChunk_increments_priority', function()
    local Processor = require("libs/Processor")
    local MapUtils = require("libs/MapUtils")
    
    -- Mock processPheromone to produce a material change. The gate watches the
    -- only field this pass maintains (the death generator); every pheromone
    -- field is solved and this pass does not write them.
    local original = MapUtils.processPheromone
    MapUtils.processPheromone = function(chunk)
        chunk.deathGenerator = 1
    end
    
    local chunk = createChunk(123, 0, 0, true)
    
    -- Mock getNeighborChunks
    local mockNeighbors = {
        [1] = { id = 1001, x = -1, y = -1 },
        [2] = -1, [3] = -1, [4] = -1, [5] = -1, [6] = -1, [7] = -1, [8] = -1
    }
    local originalNeighbors = MapUtils.getNeighborChunks
    MapUtils.getNeighborChunks = function() return mockNeighbors end
    
    local marked = {}
    local originalMark = MapUtils.markChunkDirty
    MapUtils.markChunkDirty = function(id, priority, tick, universe)
        marked[#marked + 1] = { id = id, priority = priority }
    end

    local success = Processor.processDirtyChunk(chunk, 100, 3)

    -- Restore
    MapUtils.processPheromone = original
    MapUtils.getNeighborChunks = originalNeighbors
    MapUtils.markChunkDirty = originalMark

    assert(marked[1] and marked[1].priority == 4, "Neighbor priority should be 4 (3 + 1)")
    assert(marked[1] and marked[1].id == 1001, "The movable neighbor is marked")
end)

return my_collection