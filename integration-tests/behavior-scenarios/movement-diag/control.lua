--[[
    Rampant Behavior Scenario: Movement Diagnostic (CONSTRUCTED)

    Telemetry for the squad movement stall: the units move at 0.2
    tiles/tick (3 chunks in ~8 game-seconds raw), but the observed
    crossings take 7-58 game-minutes. This scenario tracks ONE aggressive
    squad across its approach, sampling the vanilla group state, the
    Rampant squad status, and the position every 5 game-seconds, and
    dumps the series plus the cleanSquads gate counters:

      squad.reachedState4 - the re-dispatch fired (group finished)
      squad.wander        - the 1-min commandTick expired -> wandered
      squad.wanderDestroy - 6 wander cycles -> squad destroyed

    The series shows where the time goes (state 2 moving / 3 attacking /
    5 pathfinding / 4 finished, position deltas).
]]

local WALL_POS = { x = 0, y = 0 }
local SPAWNER_POS = { x = 100, y = 0 }
local TURRET_POS = { x = 6, y = 0 }
local WALL_PERIMETER = 30
local TICKS_A_MINUTE = 60 * 60
local BUDGET_MINUTES = 70
local BUDGET_TICKS = BUDGET_MINUTES * TICKS_A_MINUTE
local PHASE_LIMIT_MINUTES = 60
local BASE_AI_STATE_RAIDING = 4
local SAMPLE_INTERVAL = 5 * 60  -- every 5 game-seconds
local WINDOW_SAMPLES = 600       -- 50 game-minutes of tracking

local state  -- { phase, results, stagedAt, phaseStartedAt, done }

local function rampReady()
    -- No pcall: the interface is registered at mod load; a missing
    -- interface means the mod is not enabled and the run must fail loudly.
    local ready = remote.call("Rampant", "isInitialized")
    return ready == true
end

local function gameMinutes(tick)
    return tick / TICKS_A_MINUTE
end

local function unitsNearWall(surface)
    return surface.find_entities_filtered{
        force = "enemy",
        type = "unit",
        area = {
            { WALL_POS.x - WALL_PERIMETER, WALL_POS.y - WALL_PERIMETER },
            { WALL_POS.x + WALL_PERIMETER, WALL_POS.y + WALL_PERIMETER },
        },
    }
end

local function squads(surface)
    return remote.call("Rampant", "getEnemyGroups", surface) or {}
end

local function stage(surface)
    local natural = surface.find_entities_filtered{
        force = "enemy",
        type = { "unit-spawner", "turret", "unit" },
    }
    for _, e in ipairs(natural) do
        if e.valid then
            e.destroy()
        end
    end

    for dx = -1, 1 do
        for dy = -1, 1 do
            if math.abs(dx) + math.abs(dy) == 1 then
                surface.create_entity{
                    name = "stone-wall",
                    position = { x = WALL_POS.x + dx * 2, y = WALL_POS.y + dy * 2 },
                    force = "player",
                    raise_built = true,
                }
            end
        end
    end
    surface.create_entity{
        name = "gun-turret",
        position = TURRET_POS,
        force = "player",
        raise_built = true,
    }
    local turret = surface.find_entities_filtered{ name = "gun-turret", force = "player" }[1]
    if turret then
        local ammo = turret.get_inventory(defines.inventory.turret_ammo)
        if ammo then
            ammo.clear()
        end
    end
    surface.create_entity{
        name = "neutral-biter-spawner-v1-t1-rampant",
        position = SPAWNER_POS,
        force = "enemy",
        raise_built = true,
    }
    surface.pollute(SPAWNER_POS, 50)
end

local function setPhase(phase)
    state.phase = phase
    state.phaseStartedAt = game.tick
    print("PHASE: " .. phase .. " at " ..
        string.format("%.1f", gameMinutes(game.tick - state.stagedAt)) .. "min")
end

local function finish(passed, reason)
    local r = state.results
    print("--- Behavior Scenario: Movement Diagnostic ---")
    print("Passed: " .. (passed and 1 or 0))
    print("Failed: " .. (passed and 0 or 1))
    if reason then
        print("Detail: " .. reason)
    end
    local m = remote.call("Rampant", "getMetricsSummary")
    local c = m and m.counters or {}
    print("Detail: reachedState4=" .. tostring(c["squad.reachedState4"]) ..
        " wander=" .. tostring(c["squad.wander"]) ..
        " wanderDestroy=" .. tostring(c["squad.wanderDestroy"]) ..
        " dispatch=" .. tostring(c["cmd.DISPATCH_SQUAD"]) ..
        " ticks=" .. game.tick)
    print("Execution time: " .. game.tick .. " ticks")
    if passed then
        print("TESTS_COMPLETE")
    else
        print("TESTS_FAILED")
    end
    state.done = true
    game.speed = 1
end

script.on_event(defines.events.on_tick, function()
    if not rampReady() then
        return
    end
    if not state then
        if not storage.rampantScanStable then
            local m = remote.call("Rampant", "getMetricsSummary")
            local scanned = m and m.counters and m.counters["scan.chunksScanned"] or 0
            if scanned ~= storage.rampantLastScanCount then
                storage.rampantLastScanCount = scanned
                storage.rampantScanIdle = 0
            else
                storage.rampantScanIdle = (storage.rampantScanIdle or 0) + 1
            end
            if (storage.rampantScanIdle or 0) < 30 then
                return
            end
            storage.rampantScanStable = true
            print("[INFO] initial scan complete (chunks=" .. scanned .. ")")
        end
        state = {
            phase = "setup",
            results = { samples = {} },
            stagedAt = game.tick,
            phaseStartedAt = game.tick,
            done = false,
        }
        local surface = game.surfaces[1]
        stage(surface)
        game.speed = 100
        print("STAGED: wall+turret at origin, Rampant spawner at (100,0), speed=100")
        return
    end
    if state.done then
        return
    end

    local surface = game.surfaces[1]
    local r = state.results
    local tick = game.tick
    local elapsed = tick - state.stagedAt
    local phaseElapsed = tick - state.phaseStartedAt

    if tick - (storage.rampantLastPollute or 0) > 60 * 60 then
        storage.rampantLastPollute = tick
        surface.pollute(SPAWNER_POS, 10)
    end

    if elapsed > BUDGET_TICKS then
        finish(false, "budget " .. BUDGET_MINUTES .. " game-minutes elapsed")
        return
    end

    if state.phase == "setup" then
        local base = remote.call("Rampant", "queryBaseAt", SPAWNER_POS, surface)
        if base then
            r.baseId = base.id
            print("[PASS] base formed (id=" .. base.id .. ") at " ..
                string.format("%.1f", gameMinutes(tick - state.stagedAt)) .. "min")
            -- Corridor passability analysis: the chunks between the nest
            -- (100,0) and the wall (0,0) — the pathing is gated on the
            -- scan-computed chunk.passable (canMoveChunkDirection).
            -- passable: 3=ALL_DIRECTIONS 2=NORTH_SOUTH 1=EAST_WEST 0=IMPASSABLE
            for row = -1, 1 do
                local line = {}
                for x = 0, 128, 32 do
                    local c = remote.call("Rampant", "queryChunk", { x = x, y = row * 32 }, surface)
                    if c then
                        line[#line + 1] = "(" .. c.x .. "," .. c.y .. "):p" .. c.passable ..
                            "w" .. c.waterTiles
                    end
                end
                print("[INFO] corridor y=" .. row * 32 .. ": " .. table.concat(line, " "))
            end
            remote.call("Rampant", "setBaseState", base.id, BASE_AI_STATE_RAIDING)
            remote.call("Rampant", "grantUnitPoints", base.id, 2000)
            print("[INFO] constructed: RAIDING + 2000 points")
            setPhase("track")
        elseif phaseElapsed > PHASE_LIMIT_MINUTES * TICKS_A_MINUTE then
            finish(false, "base never formed within " .. PHASE_LIMIT_MINUTES .. " game-minutes")
            return
        end

    elseif state.phase == "track" then
        -- Track the aggressive squad NEAREST the nest (the nest->wall path
        -- is the one that stalls; the first-formed squad may take a clear
        -- side path and cross fast).
        local tracked = nil
        local trackedDist = nil
        for _, s in ipairs(squads(surface)) do
            if not s.isBuilder and s.memberCount and s.memberCount >= 1 and s.position then
                local dx = s.position.x - SPAWNER_POS.x
                local dy = s.position.y - SPAWNER_POS.y
                local d = dx * dx + dy * dy
                if not trackedDist or d < trackedDist then
                    tracked = s
                    trackedDist = d
                end
            end
        end
        if not r.trackedId and tracked then
            r.trackedId = tracked.id
            r.startPos = { x = tracked.position.x, y = tracked.position.y }
            print("[INFO] tracking squad id=" .. tracked.id .. " at (" ..
                string.format("%.0f", tracked.position.x) .. "," ..
                string.format("%.0f", tracked.position.y) .. ") members=" ..
                tracked.memberCount)
        end
        if r.trackedId and tick - (r.lastSampleTick or 0) >= SAMPLE_INTERVAL then
            r.lastSampleTick = tick
            local s = nil
            for _, sq in ipairs(squads(surface)) do
                if sq.id == r.trackedId then
                    s = sq
                    break
                end
            end
            if s then
                r.samples[#r.samples + 1] = {
                    t = gameMinutes(tick - state.stagedAt),
                    x = s.position.x,
                    y = s.position.y,
                    gs = s.state,
                    st = s.status,
                    m = s.memberCount,
                }
            else
                r.samples[#r.samples + 1] = { t = gameMinutes(tick - state.stagedAt), gone = true }
            end
            local last = r.samples[#r.samples]
            if last.gone then
                print("[SAMP] t=" .. string.format("%.1f", last.t) .. " squad gone")
            else
                print("[SAMP] t=" .. string.format("%.1f", last.t) ..
                    " pos=(" .. string.format("%.0f", last.x) .. "," ..
                    string.format("%.0f", last.y) .. ")" ..
                    " gs=" .. last.gs .. " st=" .. last.st .. " m=" .. last.m)
            end
            -- Arrival: the squad reached the wall perimeter or completed the window.
            local arrival = false
            if s and s.position then
                local dx = s.position.x - WALL_POS.x
                local dy = s.position.y - WALL_POS.y
                arrival = (dx * dx + dy * dy) < WALL_PERIMETER * WALL_PERIMETER
            end
            if arrival then
                r.arrivedAt = last.t
                print("[PASS] squad reached the wall perimeter at t=" ..
                    string.format("%.1f", last.t) .. "min")
                finish(true, "movement diagnostic complete (arrival)")
                return
            end
            if #r.samples >= WINDOW_SAMPLES then
                -- Summarize the series: how much distance per state?
                local first = r.samples[1]
                local lastSample = r.samples[#r.samples]
                if lastSample.gone then
                    print("[INFO] squad vanished mid-window")
                else
                    local dx = lastSample.x - first.x
                    local dy = lastSample.y - first.y
                    local dist = math.sqrt(dx * dx + dy * dy)
                    print("[INFO] window done: net distance " .. string.format("%.1f", dist) ..
                        " tiles over " .. string.format("%.1f", lastSample.t - first.t) .. "min")
                    r.netDistance = dist
                end
                local m = remote.call("Rampant", "getMetricsSummary")
                local c = m and m.counters or {}
                print("[INFO] counters: reachedState4=" .. tostring(c["squad.reachedState4"]) ..
                    " wander=" .. tostring(c["squad.wander"]) ..
                    " wanderDestroy=" .. tostring(c["squad.wanderDestroy"]))
                finish(true, "movement diagnostic complete (window)")
                return
            end
        end
        if phaseElapsed > PHASE_LIMIT_MINUTES * TICKS_A_MINUTE then
            finish(false, "no aggressive squad tracked within " ..
                PHASE_LIMIT_MINUTES .. " game-minutes")
            return
        end
    end
end)
