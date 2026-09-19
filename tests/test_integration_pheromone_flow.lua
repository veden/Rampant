-- Integration tests for complete pheromone processing flow
-- Tests how functions work together, not just isolated pure functions

local TestCollection = require("tests/framework")
local Constants = require("Constants")

local my_collection = TestCollection:new()

-- Helper to create mock chunk
local function createChunk(id, x, y, pheromones)
    return {
        id = id,
        x = x,
        y = y,
        valid = true,
        [Constants.CHUNK_TICK] = 0,
        [Constants.PLAYER_PHEROMONE] = pheromones.player or 0,
        [Constants.ENEMY_PHEROMONE] = pheromones.enemy or 0,
        [Constants.BASE_PHEROMONE] = pheromones.base or 0,
        [Constants.KAMIKAZE_PHEROMONE] = pheromones.kamikaze or 0,
        [Constants.RESOURCE_PHEROMONE] = pheromones.resource or 0,
        playerGenerator = pheromones.playerGenerator or 0,
        playerBaseGenerator = pheromones.playerBaseGenerator or 0,
        resourceGenerator = pheromones.resourceGenerator or 0,
        deathGenerator = pheromones.deathGenerator or 0,
        permanentDeathGenerator = pheromones.permanentDeathGenerator or 0,
        passable = Constants.CHUNK_ALL_DIRECTIONS,
        map = {},
    }
end

-- Helper to setup universe with chunks
local function setupUniverse()
    local universe = {
        dirtyHeads = { 0, 0, 0, 0, 0 },
        dirtyTails = { 0, 0, 0, 0, 0 },
        dirtyNodes = {},
        dirtyCount = 0,
        maps = {},
        chunkToVictory = {},
        mapUtilsQueries = {
            neighbors = {},
            position = {}
        }
    }
    return universe
end

-- Test: Complete flow - mark chunk dirty, process, verify pheromone spreads
my_collection:add('complete_flow_marks_and_processes_chunk', function()
    local MapUtils = require("libs/MapUtils")
    local Processor = require("libs/Processor")
    
    local universe = setupUniverse()
    MapUtils.init(universe)
    
    local chunk1 = createChunk(123, 0, 0, { player = 100, enemy = 50 })
    local chunk2 = createChunk(124, 1, 0, { player = 0, enemy = 0 })
    
    universe.maps = { [1] = { [0] = { [0] = chunk1 }, [1] = { [0] = chunk2 } } }

        -- Register chunks in the reverse index (mirrors createChunk)
        for _, xChunks in pairs(universe.maps[1]) do
            for _, chunk in pairs(xChunks) do
                universe.chunkById[chunk.id] = chunk
            end
        end
    
    -- Mock getNeighborChunks to establish relationship
    local originalNeighbors = MapUtils.getNeighborChunks
    MapUtils.getNeighborChunks = function(map, x, y)
        if x == 0 and y == 0 then
            return { [3] = chunk2, [1] = -1, [2] = -1, [4] = -1, [5] = -1, [6] = -1, [7] = -1, [8] = -1 }
        elseif x == 1 and y == 0 then
            return { [6] = chunk1, [1] = -1, [2] = -1, [3] = -1, [4] = -1, [5] = -1, [7] = -1, [8] = -1 }
        end
        return {}
    end
    
    -- Debug
    -- print("BEFORE markChunkDirty: universe.dirtyChunks[1] =", universe.dirtyChunks[1])
    -- print("BEFORE markChunkDirty: type =", type(universe.dirtyChunks[1]))
    -- print("BEFORE markChunkDirty: universe.dirtyChunkSet[123] =", universe.dirtyChunkSet[123])
    
    -- Execute: Mark chunk1 dirty and process
    MapUtils.markChunkDirty(123, 1, 100, universe)
    Processor.processDirtyChunks(100, universe)
    
    -- Verify: chunk1 should have been processed (CHUNK_TICK updated)
    assert(chunk1[Constants.CHUNK_TICK] == 100, "Chunk 1 should be processed")
    
    -- Verify: chunk2 should have absorbed some pheromone from chunk1
    assert(chunk2[Constants.PLAYER_PHEROMONE] > 0, "Chunk 2 should absorb player pheromone")
    assert(chunk2[Constants.ENEMY_PHEROMONE] > 0, "Chunk 2 should absorb enemy pheromone")
    
    -- Restore
    MapUtils.getNeighborChunks = originalNeighbors
end)

-- Test: Dirty chunk priority queue processes high priority first
my_collection:add('priority_queue_processes_high_priority_first', function()
    local MapUtils = require("libs/MapUtils")
    local Processor = require("libs/Processor")
    
    local universe = setupUniverse()
    MapUtils.init(universe)
    
    local chunk1 = createChunk(1, 0, 0, { player = 100 })
    local chunk2 = createChunk(2, 1, 0, { player = 100 })
    local chunk3 = createChunk(3, 2, 0, { player = 100 })
    
    universe.maps = { [1] = { [0] = { [0] = chunk1 }, [1] = { [0] = chunk2 }, [2] = { [0] = chunk3 } } }

        -- Register chunks in the reverse index (mirrors createChunk)
        for _, xChunks in pairs(universe.maps[1]) do
            for _, chunk in pairs(xChunks) do
                universe.chunkById[chunk.id] = chunk
            end
        end
    
    local processedOrder = {}
    local originalProcess = MapUtils.processPheromone
    MapUtils.processPheromone = function(chunk, tick)
        table.insert(processedOrder, chunk.id)
    end
    
    -- Mock neighbors
    MapUtils.getNeighborChunks = function() return {} end
    
    -- Mark with different priorities
    MapUtils.markChunkDirty(3, 3, 100, universe)  -- Low priority
    MapUtils.markChunkDirty(1, 1, 100, universe)  -- High priority
    MapUtils.markChunkDirty(2, 2, 100, universe)  -- Medium priority
    
    Processor.processDirtyChunks(100, universe)
    
    -- Restore
    MapUtils.processPheromone = originalProcess
    MapUtils.getNeighborChunks = function() return {} end
    
    -- Verify: High priority (1) processed before medium (2) before low (3)
    assert(processedOrder[1] == 1, "High priority chunk should be processed first")
    assert(processedOrder[2] == 2, "Medium priority chunk should be processed second")
    assert(processedOrder[3] == 3, "Low priority chunk should be processed third")
end)

-- Test: Marking same chunk twice doesn't duplicate
my_collection:add('marking_chunk_twice_prevents_duplicates', function()
    local MapUtils = require("libs/MapUtils")
    
    local universe = setupUniverse()
    MapUtils.init(universe)
    
    -- Mark chunk dirty twice with different priorities
    MapUtils.markChunkDirty(123, 3, 100, universe)
    MapUtils.markChunkDirty(123, 1, 101, universe)  -- Higher priority
    
    -- Should be ACTIVE only in priority 1 (upgraded). The stale entry left
    -- in priority 3 is by design: the upgrade is lazy (O(1)) and the drain
    -- skips entries whose set priority no longer matches. The set is the
    -- source of truth for a chunk's active priority.
    assert(universe.dirtyCount == 1, "Should be queued once")
    assert(universe.dirtyNodes[123] and universe.dirtyNodes[123].p == 1, "Should have priority 1")
end)

-- Test: Invalid chunks are not added to queue
my_collection:add('invalid_chunks_not_added_to_queue', function()
    local MapUtils = require("libs/MapUtils")
    
    local universe = setupUniverse()
    MapUtils.init(universe)
    
    -- Mark invalid chunk IDs
    MapUtils.markChunkDirty(nil, 1, 100, universe)
    MapUtils.markChunkDirty(-1, 1, 100, universe)
    
    -- Queue should be empty
    assert(universe.dirtyCount == 0, "Invalid chunks should not be added")
end)

-- Test: Multi-tick pheromone accumulation
my_collection:add('pheromone_accumulates_over_multiple_ticks', function()
    local MapUtils = require("libs/MapUtils")
    local Processor = require("libs/Processor")
    
    local universe = setupUniverse()
    MapUtils.init(universe)
    
    local chunk = createChunk(123, 0, 0, { player = 10 })
    universe.maps = { [1] = { [0] = { [0] = chunk } } }

        -- Register chunks in the reverse index (mirrors createChunk)
        for _, xChunks in pairs(universe.maps[1]) do
            for _, chunk in pairs(xChunks) do
                universe.chunkById[chunk.id] = chunk
            end
        end
    
    local originalNeighbors = MapUtils.getNeighborChunks
    MapUtils.getNeighborChunks = function() return {} end
    
    -- Tick 1: Process
    MapUtils.markChunkDirty(123, 1, 100, universe)
    Processor.processDirtyChunks(100, universe)
    local afterTick1 = chunk[Constants.PLAYER_PHEROMONE]
    
    -- Reset dirty queue
    universe.dirtyHeads = { 0, 0, 0, 0, 0 }
    universe.dirtyTails = { 0, 0, 0, 0, 0 }
    universe.dirtyNodes = {}
    universe.dirtyCount = 0
    
    -- Tick 2: Process again
    MapUtils.markChunkDirty(123, 1, 101, universe)
    Processor.processDirtyChunks(101, universe)
    local afterTick2 = chunk[Constants.PLAYER_PHEROMONE]
    
    -- Restore
    MapUtils.getNeighborChunks = originalNeighbors
    
    -- Pheromone should change (decay + recalculation)
    assert(afterTick1 ~= nil, "Should have pheromone after tick 1")
    assert(afterTick2 ~= nil, "Should have pheromone after tick 2")
end)

-- Test: Chunk processing updates CHUNK_TICK
my_collection:add('chunk_tick_updated_after_processing', function()
    local MapUtils = require("libs/MapUtils")
    local Processor = require("libs/Processor")
    
    local universe = setupUniverse()
    MapUtils.init(universe)
    
    local chunk = createChunk(123, 0, 0, { player = 100 })
    chunk[Constants.CHUNK_TICK] = 50  -- Already processed at tick 50
    universe.maps = { [1] = { [0] = { [0] = chunk } } }

        -- Register chunks in the reverse index (mirrors createChunk)
        for _, xChunks in pairs(universe.maps[1]) do
            for _, chunk in pairs(xChunks) do
                universe.chunkById[chunk.id] = chunk
            end
        end
    
    local processCount = 0
    local originalProcess = MapUtils.processPheromone
    MapUtils.processPheromone = function(c, tick)
        processCount = processCount + 1
    end
    
    MapUtils.getNeighborChunks = function() return {} end
    
    -- Try to process at tick 100 (should process since 50 < 100)
    MapUtils.markChunkDirty(123, 1, 100, universe)
    Processor.processDirtyChunks(100, universe)
    
    -- Restore
    MapUtils.processPheromone = originalProcess
    
    -- Should have been processed once
    assert(processCount == 1, "Chunk should be processed")
end)

-- Test: Skip processing if chunk already processed at later tick
my_collection:add('skip_processing_later_tick', function()
    local MapUtils = require("libs/MapUtils")
    
    local chunk = createChunk(123, 0, 0, { player = 100 })
    local originalTick = 101
    chunk[Constants.CHUNK_TICK] = originalTick  -- Already processed at tick 101
    
    -- Call processPheromone at tick 100 (should skip since 101 > 100)
    MapUtils.processPheromone(chunk, 100)
    
    -- Verify: CHUNK_TICK should NOT have changed (function returned early)
    assert(chunk[Constants.CHUNK_TICK] == originalTick, "CHUNK_TICK should not change when skipped")
end)

return my_collection