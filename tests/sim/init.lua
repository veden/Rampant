--[[
    Simulation Layer for Rampant Testing

    Provides a complete Factorio simulation environment for running
    Rampant AI code in tests without requiring a Factorio instance.
]]

-- Load adapters (sets up _G globals)
require("tests/sim.adapters.game")
require("tests/sim.adapters.script")
require("tests/sim.adapters.remote")
require("tests/sim.adapters.defines")

-- Load GameState for dependency injection
local GameState = require("libs.GameState")
_G.GameState = GameState

-- Module exports
local sim = {
    TickSystem = require("tests/sim.TickSystem"),
    Chunk = require("tests/sim.Chunk"),
    Entity = require("tests/sim.Entity"),
    Surface = require("tests/sim.Surface"),
    UnitGroup = require("tests/sim.UnitGroup"),
    WorldBuilder = require("tests/sim.WorldBuilder"),
    adapters = {
        game = require("tests/sim.adapters.game"),
        script = require("tests/sim.adapters.script"),
        remote = require("tests/sim.adapters.remote")
    }
}

-- Initialize simulation
function sim.init()
    -- Reset all systems
    sim.TickSystem.reset()
    sim.Entity.clearAll()
    sim.UnitGroup.clearAll()

    -- Clear adapters
    if _G.remote then
        _G.remote._clear()
    end
    if _G.script then
        _G.script._clear()
    end
end

-- Setup a basic world for testing
function sim.createTestWorld(options)
    options = options or {}
    local world = sim.WorldBuilder.create()
        :withSurface(options.surfaceName or "nauvis")

    -- Add chunks around origin
    world:withChunk(0, 0)
    if options.withNeighbors ~= false then
        world:withChunk(32, 0)
        world:withChunk(-32, 0)
        world:withChunk(0, 32)
        world:withChunk(0, -32)
    end

    -- Add base if specified
    if options.basePos then
        world:withBase(options.basePos)
    end

    -- Add player if specified
    if options.playerPos then
        world:withPlayer(options.playerPos)
    end

    return world:build()
end

-- Quick assert helper
function sim.assert(condition, message)
    if not condition then
        error(message or "Assertion failed", 2)
    end
end

return sim