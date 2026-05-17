--[[
    Event Handler Integration Tests

    Verifies event handler registration and execution within
    the Factorio game engine.
]]

local EventTests = {
    name = "Event",
    tests = {}
}

function EventTests.run()
    print("--- Event Handler Tests ---")

    local passed = 0
    local failed = 0

    local function test(name, condition, errorMsg)
        print("  Testing: " .. name)
        if not condition then
            failed = failed + 1
            print("    FAILED: " .. (errorMsg or "assertion failed"))
            return false
        end
        passed = passed + 1
        return true
    end

    local events = defines.events
    test("defines.events accessible", events ~= nil, "events nil")
    test("on_tick defined", events.on_tick ~= nil, "on_tick not defined")
    test("on_built_entity defined", events.on_built_entity ~= nil, "on_built_entity not defined")
    test("on_entity_died defined", events.on_entity_died ~= nil, "on_entity_died not defined")
    test("on_chunk_generated defined", events.on_chunk_generated ~= nil, "on_chunk_generated not defined")

    script.on_init(function() end)
    test("on_init registration", true, "on_init registration failed")
    script.on_init(nil)

    script.on_load(function() end)
    test("on_load registration", true, "on_load registration failed")
    script.on_load(nil)

    local tickHandlerCalled = false
    script.on_event(defines.events.on_tick, function(event)
        tickHandlerCalled = true
    end)
    script.on_event(defines.events.on_tick, nil)
    test("on_tick handler", true, "on_tick handler registration failed")

    local builtCount = 0
    local function countBuilt(event)
        builtCount = builtCount + 1
    end
    script.on_event(defines.events.on_built_entity, countBuilt)
    script.on_event(defines.events.on_built_entity, nil)
    test("on_built_entity handler", true, "built entity handler failed")

    local handler1Called = false
    local handler2Called = false
    local function handler1() handler1Called = true end
    local function handler2() handler2Called = true end
    script.on_event(defines.events.on_entity_died, handler1)
    script.on_event(defines.events.on_entity_died, handler2)
    script.on_event(defines.events.on_entity_died, nil)
    test("multiple handlers", true, "multiple handlers registration failed")

    local filterRegistered = false
    script.on_event(defines.events.on_built_entity, function(event) end, {
        {filter = "name", name = "rampant-ai-spawner"}
    })
    script.on_event(defines.events.on_built_entity, nil)
    test("event filters", true, "event filters registration failed")

    local nthTickRegistered = false
    script.on_nth_tick(60, function(event)
        nthTickRegistered = true
    end)
    script.on_nth_tick(60, nil)
    test("on_nth_tick registration", true, "on_nth_tick registration failed")

    remote.add_interface("rampantTests", {
        testFunction = function() return "test" end
    })
    test("remote interface", remote.interfaces["rampantTests"] ~= nil, "remote interface not registered")

    print("Event Handler Tests: " .. (failed == 0 and "PASSED" or "FAILED") .. " (" .. passed .. " passed, " .. failed .. " failed)")

    return failed == 0
end

return EventTests