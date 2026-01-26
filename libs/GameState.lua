--[[
    GameState - Dependency injection layer for game state

    Provides testable access to game tick, surfaces, and forces
    without requiring actual Factorio instance.

    Usage:
        -- In tests, set tick directly
        _G.GameState._tick = 100

        -- Existing code continues to use game.tick
        local tick = game.tick  -- reads from GameState when present
]]

local GameState = {
    _tick = 0,
    _surfaces = {},
    _forces = {},
    _entities = {}
}

--- Get the current game tick
-- @return number: Current tick
function GameState.getTick()
    return GameState._tick
end

--- Set the current game tick
-- @param t number: The tick value
function GameState.setTick(t)
    GameState._tick = t
end

--- Get a surface by name
-- @param name string: Surface name
-- @return table: Surface object or nil
function GameState.getSurface(name)
    return GameState._surfaces[name]
end

--- Register a surface with GameState
-- @param name string: Surface name
-- @param surface table: Surface object
function GameState.registerSurface(name, surface)
    GameState._surfaces[name] = surface
end

--- Get a force by name
-- @param name string: Force name
-- @return table: Force object or nil
function GameState.getForce(name)
    return GameState._forces[name]
end

--- Register a force with GameState
-- @param name string: Force name
-- @param force table: Force object
function GameState.registerForce(name, force)
    GameState._forces[name] = force
end

--- Get the enemy force
-- @return table: Enemy force object
function GameState.getEnemyForce()
    return GameState._forces["enemy"]
end

--- Get the player force
-- @return table: Player force object
function GameState.getPlayerForce()
    return GameState._forces["player"]
end

--- Reset GameState to initial state
function GameState.reset()
    GameState._tick = 0
    GameState._surfaces = {}
    GameState._forces = {}
    GameState._entities = {}
end

return GameState