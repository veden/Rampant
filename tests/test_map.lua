-- Unit tests for MapUtils module helpers (positionToChunkXY, getNeighborChunks)

local TestCollection = require("tests/framework")
local Fixtures = require("tests/fixtures")

local my_collection = TestCollection:new()

-- Create mock universe and initialize MapUtils
local Universe = Fixtures.makeUniverse()
package.loaded["libs/MapUtils"] = nil
local MapUtils = require("libs/MapUtils")
MapUtils.init(Universe)

-- Test positionToChunkXY with positive coordinates
my_collection:add('positionToChunkXY_positive', function()
    local x, y = MapUtils.positionToChunkXY({ [1] = 50, [2] = 50 })
    assert(x == 32, "Expected x chunk boundary at 32, got " .. tostring(x))
    assert(y == 32, "Expected y chunk boundary at 32, got " .. tostring(y))
end)

-- Test positionToChunkXY at chunk boundary
my_collection:add('positionToChunkXY_at_boundary', function()
    local x, y = MapUtils.positionToChunkXY({ [1] = 32, [2] = 32 })
    assert(x == 32, "Expected x = 32 at boundary, got " .. tostring(x))
    assert(y == 32, "Expected y = 32 at boundary, got " .. tostring(y))
end)

-- Test positionToChunkXY just before boundary
my_collection:add('positionToChunkXY_before_boundary', function()
    local x, y = MapUtils.positionToChunkXY({ [1] = 31.9, [2] = 31.9 })
    assert(x == 0, "Expected x = 0 before boundary, got " .. tostring(x))
    assert(y == 0, "Expected y = 0 before boundary, got " .. tostring(y))
end)

-- Test positionToChunkXY with negative coordinates
my_collection:add('positionToChunkXY_negative', function()
    local x, y = MapUtils.positionToChunkXY({ [1] = -10, [2] = -50 })
    assert(x == -32, "Expected x = -32, got " .. tostring(x))
    assert(y == -64, "Expected y = -64, got " .. tostring(y))
end)

-- Test positionToChunkXY with zero
my_collection:add('positionToChunkXY_zero', function()
    local x, y = MapUtils.positionToChunkXY({ [1] = 0, [2] = 0 })
    assert(x == 0, "Expected x = 0, got " .. tostring(x))
    assert(y == 0, "Expected y = 0, got " .. tostring(y))
end)

-- Test getNeighborChunks with all neighbors present
my_collection:add('getNeighborChunks_all_present', function()
    local surface = Fixtures.makeSurface("test")
    local map = Fixtures.makeMap(surface)
    map.universe = Universe

    local centerChunk = Fixtures.createAndRegisterChunk(map, 0, 0)

    -- Create all 8 neighbors
    local neighbors = {
        { x = -32, y = -32 }, -- 1
        { x = 0, y = -32 },   -- 2
        { x = 32, y = -32 },  -- 3
        { x = -32, y = 0 },   -- 4
        { x = 32, y = 0 },    -- 5
        { x = -32, y = 32 },  -- 6
        { x = 0, y = 32 },    -- 7
        { x = 32, y = 32 },   -- 8
    }

    for _, pos in ipairs(neighbors) do
        Fixtures.createAndRegisterChunk(map, pos.x, pos.y)
    end

    local result = MapUtils.getNeighborChunks(map, 0, 0)

    -- Check all 8 neighbors are present
    local count = 0
    for i = 1, 8 do
        if result[i] then count = count + 1 end
    end
    assert(count == 8, "Expected all 8 neighbors, got " .. tostring(count))
end)

-- Test getNeighborChunks with no neighbors
my_collection:add('getNeighborChunks_no_neighbors', function()
    local surface = Fixtures.makeSurface("test")
    local map = Fixtures.makeMap(surface)
    map.universe = Universe

    Fixtures.createAndRegisterChunk(map, 0, 0)

    local result = MapUtils.getNeighborChunks(map, 0, 0)

    local count = 0
    for i = 1, 8 do
        if result[i] then count = count + 1 end
    end
    assert(count == 0, "Expected no neighbors, got " .. tostring(count))
end)

-- Test getNeighborChunks with partial neighbors
my_collection:add('getNeighborChunks_partial', function()
    local surface = Fixtures.makeSurface("test")
    local map = Fixtures.makeMap(surface)
    map.universe = Universe

    Fixtures.createAndRegisterChunk(map, 0, 0)

    -- Add only some neighbors
    Fixtures.createAndRegisterChunk(map, -32, 0)   -- 4
    Fixtures.createAndRegisterChunk(map, 32, 0)    -- 5
    Fixtures.createAndRegisterChunk(map, 0, 32)    -- 7

    local result = MapUtils.getNeighborChunks(map, 0, 0)

    assert(result[4] ~= nil, "Expected neighbor at position 4")
    assert(result[5] ~= nil, "Expected neighbor at position 5")
    assert(result[7] ~= nil, "Expected neighbor at position 7")
    assert(result[1] == nil, "Expected nil at position 1")
    assert(result[2] == nil, "Expected nil at position 2")
end)

-- Test getChunkByPosition
my_collection:add('getChunkByPosition_basic', function()
    local surface = Fixtures.makeSurface("test")
    local map = Fixtures.makeMap(surface)
    map.universe = Universe

    Fixtures.createAndRegisterChunk(map, 32, 32)

    local chunk = MapUtils.getChunkByPosition(map, 40, 40)
    assert(chunk ~= nil, "Expected to find chunk at position (40, 40)")
    assert(chunk.x == 32, "Expected chunk x = 32")
    assert(chunk.y == 32, "Expected chunk y = 32")
end)

-- Test getChunkByPosition with position in different chunk
my_collection:add('getChunkByPosition_different_chunk', function()
    local surface = Fixtures.makeSurface("test")
    local map = Fixtures.makeMap(surface)
    map.universe = Universe

    Fixtures.createAndRegisterChunk(map, 0, 0)

    local chunk = MapUtils.getChunkByPosition(map, 50, 50)
    assert(chunk == nil, "Expected nil for position in unregistered chunk")
end)

return my_collection