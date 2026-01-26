--[[
    TickSystem - Time advancement and tick handling for simulation

    Provides a deterministic tick counter and event system for
    running Rampant AI code in tests.
]]

local TickSystem = {
    tick = 0,
    eventHandlers = {},
    listeners = {}
}

-- Direction constants
TickSystem.DIRECTIONS = {
    NORTH = 1,
    NORTHEAST = 2,
    EAST = 3,
    SOUTHEAST = 4,
    SOUTH = 5,
    SOUTHWEST = 6,
    WEST = 7,
    NORTHWEST = 8
}

-- Neighbor offset lookup table
TickSystem.NEIGHBOR_OFFSETS = {
    [TickSystem.DIRECTIONS.NORTH]     = {x = 0,  y = -32},
    [TickSystem.DIRECTIONS.NORTHEAST] = {x = 32, y = -32},
    [TickSystem.DIRECTIONS.EAST]      = {x = 32, y = 0},
    [TickSystem.DIRECTIONS.SOUTHEAST] = {x = 32, y = 32},
    [TickSystem.DIRECTIONS.SOUTH]     = {x = 0,  y = 32},
    [TickSystem.DIRECTIONS.SOUTHWEST] = {x = -32, y = 32},
    [TickSystem.DIRECTIONS.WEST]      = {x = -32, y = 0},
    [TickSystem.DIRECTIONS.NORTHWEST] = {x = -32, y = -32}
}

--- Register a tick handler
-- @param handler function: Function to call on each tick (receives current tick)
function TickSystem.onTick(handler)
    table.insert(TickSystem.listeners, handler)
end

--- Advance time by N ticks
-- @param ticks number: Number of ticks to advance
function TickSystem.advance(ticks)
    local startTick = TickSystem.tick
    TickSystem.tick = TickSystem.tick + ticks

    -- Fire tick events for each tick advanced
    for t = startTick + 1, TickSystem.tick do
        for _, handler in ipairs(TickSystem.listeners) do
            handler(t)
        end
    end
end

--- Get current tick
-- @return number: Current tick value
function TickSystem.now()
    return TickSystem.tick
end

--- Reset the tick system to initial state
function TickSystem.reset()
    TickSystem.tick = 0
    TickSystem.listeners = {}
end

--- Set current tick (for precise positioning)
-- @param t number: Tick value to set
function TickSystem.set(t)
    TickSystem.tick = t
end

--- Fire a custom event
-- @param eventName string: Name of the event
-- @param ... any: Arguments to pass to handlers
function TickSystem.fire(eventName, ...)
    local handlers = TickSystem.eventHandlers[eventName]
    if handlers then
        for _, handler in ipairs(handlers) do
            handler(...)
        end
    end
end

--- Register a custom event handler
-- @param eventName string: Name of the event
-- @param handler function: Handler function
function TickSystem.on(eventName, handler)
    if not TickSystem.eventHandlers[eventName] then
        TickSystem.eventHandlers[eventName] = {}
    end
    table.insert(TickSystem.eventHandlers[eventName], handler)
end

return TickSystem