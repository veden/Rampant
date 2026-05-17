--[[
    Active AI Test Scenario - RCON Controlled

    This scenario initializes Rampant and waits for RCON commands.
    Tests are run via RCON using /silent-command and remote.call().

    Usage:
    /silent-command remote.call("Rampant", "spawnEntity", "stone-wall", {0,0}, "player")
    /silent-command remote.call("Rampant", "advanceTicks", 300)
    /silent-command print(remote.call("Rampant", "getGameState").tick)
]]

local function waitForRampantReady(maxWait)
    maxWait = maxWait or 10
    local iterations = maxWait * 100
    for i = 1, iterations do
        if storage and storage._rampantReady then
            return true
        end
    end
    return false
end

script.on_init(function()
    -- Wait for Rampant to be fully initialized
    local ready = waitForRampantReady(10)
    if ready then
        print("READY_FOR_COMMANDS")
    else
        print("ERROR: Rampant initialization timed out")
    end
end)

script.on_configuration_changed(function(event)
    -- Same as on_init for fresh scenarios
    local ready = waitForRampantReady(10)
    if ready then
        print("READY_FOR_COMMANDS")
    else
        print("ERROR: Rampant initialization timed out")
    end
end)