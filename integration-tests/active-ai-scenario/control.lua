--[[
    Active AI Test Scenario - RCON Controlled

    This scenario initializes Rampant and waits for RCON commands.
    Tests are run via RCON using /silent-command and remote.call().

    Usage:
    /silent-command remote.call("Rampant", "spawnEntity", "stone-wall", {0,0}, "player")
    /silent-command remote.call("Rampant", "advanceTicks", 300)
    /silent-command print(remote.call("Rampant", "getGameState").tick)
]]

-- Readiness check notes:
-- * storage is per-mod (scenario storage != Rampant's storage), so polling
--   storage._rampantReady here can never succeed. Use the remote interface
--   (registered at mod load time) instead.
-- * The scenario's on_init runs BEFORE the mod's on_init on 2.1.x, so the
--   ready flag is not set yet at on_init; poll from on_tick.
-- * The server must not pause with no players (pause_when_no_players: false
--   in server settings) for on_tick to fire.
local READY_WAIT_TICKS = 600  -- 10 seconds

local function signalReady()
    print("READY_FOR_COMMANDS")
    print("TESTS_COMPLETE")
    print("RAMPANT ACTIVE AI SMOKE TEST PASSED!")
end

local function signalTimeout()
    print("ERROR: Rampant initialization timed out")
    print("TESTS_FAILED")
end

local function rampantReady()
    local ok, ready = pcall(remote.call, "Rampant", "isInitialized")
    return ok and ready == true
end

local function beginWait()
    if rampantReady() then
        signalReady()
    else
        storage._rampantReadyWait = { ticks = 0 }
    end
end

script.on_init(function()
    storage._rampantOnInitRan = true
    beginWait()
end)

script.on_configuration_changed(function(event)
    beginWait()
end)

script.on_event(defines.events.on_tick, function(event)
    local wait = storage._rampantReadyWait
    if not wait then return end

    wait.ticks = wait.ticks + 1
    if rampantReady() then
        storage._rampantReadyWait = nil
        signalReady()
    elseif wait.ticks > READY_WAIT_TICKS then
        storage._rampantReadyWait = nil
        signalTimeout()
    end
end)
