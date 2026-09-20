-- Unit tests for the source-versioned field runtime adapter.

local TestCollection = require("tests/framework")
local Runtime = require("libs/source_versioned_field_runtime")
local MapUtils = require("MapUtils")
local Constants = require("Constants")

local my_collection = TestCollection:new()

local originalGetChunkById
local originalGetNeighborChunks
local world

local ENEMY_PHEROMONE = Constants.ENEMY_PHEROMONE
local ENEMY_MULTIPLIER = Constants.ENEMY_PHEROMONE_MULTIPLER

local function makeWorld()
    local chunks = {
        [1] = { id = 1, x = 0, y = 0, nestCount = 1 },
        [2] = { id = 2, x = 32, y = 0, nestCount = 0 },
    }
    local neighbors = {
        [1] = { [3] = chunks[2], [1] = -1, [2] = -1, [4] = -1,
                [5] = -1, [6] = -1, [7] = -1, [8] = -1 },
        [2] = { [6] = chunks[1], [1] = -1, [2] = -1, [3] = -1,
                [4] = -1, [5] = -1, [7] = -1, [8] = -1 },
    }
    local universe = { chunkById = chunks }

    originalGetChunkById = MapUtils.getChunkById
    originalGetNeighborChunks = MapUtils.getNeighborChunks
    MapUtils.getChunkById = function(chunkId)
        return chunks[chunkId]
    end
    MapUtils.getNeighborChunks = function(_, x, y)
        local chunkId = (x == 0 and y == 0) and 1 or 2
        return neighbors[chunkId] or {}
    end

    Runtime.init(universe)
    return chunks
end

local function restoreWorld()
    MapUtils.getChunkById = originalGetChunkById
    MapUtils.getNeighborChunks = originalGetNeighborChunks
end

my_collection:set_setup(function()
    world = makeWorld()
end)

my_collection:set_teardown(function()
    restoreWorld()
    world = nil
end)

my_collection:add('readChunk_solves_source_field', function()
    local value = Runtime.readChunk(ENEMY_PHEROMONE, world[1])
    assert(value == 1 * ENEMY_MULTIPLIER,
        "source chunk must read its source value")

    local neighborValue = Runtime.readChunk(ENEMY_PHEROMONE, world[2])
    assert(math.abs(neighborValue - (1 * ENEMY_MULTIPLIER * 0.9)) < 1e-9,
        "neighbor chunk must read the propagated value")
end)

my_collection:add('process_increase_propagates_locally', function()
    Runtime.readChunk(ENEMY_PHEROMONE, world[1])

    world[1].nestCount = 2
    Runtime.queueSource(world[1], ENEMY_PHEROMONE,
        1 * ENEMY_MULTIPLIER, 2 * ENEMY_MULTIPLIER)
    Runtime.process({ decay = 0.9, eps = 0.0005 })

    assert(math.abs(Runtime.readChunk(ENEMY_PHEROMONE, world[1]) - 2 * ENEMY_MULTIPLIER) < 1e-9,
        "increased source must be updated")
    assert(math.abs(Runtime.readChunk(ENEMY_PHEROMONE, world[2]) - 2 * ENEMY_MULTIPLIER * 0.9) < 1e-9,
        "increased source must propagate to its neighbor")
end)

my_collection:add('process_decrease_recomputes_field', function()
    Runtime.readChunk(ENEMY_PHEROMONE, world[1])

    world[1].nestCount = 0
    Runtime.queueSource(world[1], ENEMY_PHEROMONE,
        1 * ENEMY_MULTIPLIER, 0)
    Runtime.process({ decay = 0.9, eps = 0.0005 })

    assert(Runtime.readChunk(ENEMY_PHEROMONE, world[1]) == 0,
        "removed source must be cleared")
    assert(Runtime.readChunk(ENEMY_PHEROMONE, world[2]) == 0,
        "removed source must clear its propagated neighbor")
end)

my_collection:add('squad_logic_uses_injected_reader', function()
    local SquadLogic = require("squad_logic")
    local oldFlag = Constants.USE_SOURCE_VERSIONED_FIELD
    Constants.USE_SOURCE_VERSIONED_FIELD = true
    SquadLogic.setSourceVersionedFieldReader(function(fieldType)
        if fieldType == Constants.BASE_PHEROMONE then return 11 end
        if fieldType == Constants.ENEMY_PHEROMONE then return 22 end
        return nil
    end)

    assert(SquadLogic.basePheromone(world[1], 0) == 11,
        "base pheromone must use the injected source-versioned reader")
    assert(SquadLogic.enemyPheromone(world[1], 0) == 22,
        "enemy pheromone must use the injected source-versioned reader")

    SquadLogic.setSourceVersionedFieldReader(nil)
    Constants.USE_SOURCE_VERSIONED_FIELD = oldFlag
end)

return my_collection
