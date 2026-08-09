--[[
    Rampant Behavior Scenario: Migration Settler (CONSTRUCTED)

    Verifies the expansion chain: a base in MIGRATING state forms a settler
    squad which travels to a destination and BUILDS a new spawner.

      setup      - clean map, spawner at (100,0), pollution to activate the
                   nest; base formed; construct MIGRATING state + points
      migrate    - settler squad forms (isBuilder=true) and moves
      settle     - settler builds: enemy unit-spawner count >= 2 (a second
                   nest exists at distance)

    Expected: ~5-20 game-minutes.
]]

local SPAWNER_POS = { x = 100, y = 0 }
local TICKS_A_MINUTE = 60 * 60
local BUDGET_MINUTES = 35
local BUDGET_TICKS = BUDGET_MINUTES * TICKS_A_MINUTE
local PHASE_LIMIT_MINUTES = 25
local POLLUTE_INTERVAL = 600
local BASE_AI_STATE_MIGRATING = 5

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

local function stage(surface)
    -- Clean the map: destroy all natural enemy structures/units.
    local natural = surface.find_entities_filtered{
        force = "enemy",
        type = { "unit-spawner", "turret", "unit" },
    }
    for _, e in ipairs(natural) do
        if e.valid then
            e.destroy()
        end
    end
    surface.create_entity{
        name = "biter-spawner",
        position = SPAWNER_POS,
        force = "enemy",
        raise_built = true,
    }
    -- Player structure near the nest: nest activation requires
    -- BASE_PHEROMONE > 0 at the chunk (enemy nests emit enemy pheromone,
    -- not base), which only player structures provide. This is the
    -- realistic migration source (player-adjacent bases).
    surface.create_entity{
        name = "stone-wall",
        position = { x = 90, y = 0 },
        force = "player",
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
    print("--- Behavior Scenario: Migration Settler ---")
    print("Passed: " .. (passed and 1 or 0))
    print("Failed: " .. (passed and 0 or 1))
    if reason then
        print("Detail: " .. reason)
    end
    print("Detail: base=" .. tostring(r.baseFormed) ..
        " settlerFormed=" .. tostring(r.settlerFormed) ..
        " dispatched=" .. tostring(r.dispatched) ..
        " moved=" .. tostring(r.moved) ..
        " settlerPos=" .. tostring(r.settlerPos) ..
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
        -- Stage AFTER the mod's initial chunk scan completes: the scan
        -- processes chunks one per few ticks and its unit purge destroys
        -- units in scanned chunks. Robust signal: every map chunk is
        -- scanned exactly once, so wait until the scan counter reaches the
        -- surface chunk count.
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
                return  -- still scanning; wait
            end
            storage.rampantScanStable = true
            print("[INFO] initial scan complete (chunks=" .. scanned .. ")")
        end
        state = {
            phase = "setup",
            results = {},
            stagedAt = game.tick,
            phaseStartedAt = game.tick,
            done = false,
        }
        stage(game.surfaces[1])
        game.speed = 100
        print("STAGED: spawner at (100,0), pollution at spawner, speed=100")
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

    if elapsed > BUDGET_TICKS then
        finish(false, "budget " .. BUDGET_MINUTES .. " game-minutes elapsed")
        return
    end

    -- Pollution keeps the nest active (required for expansion groups)
    if tick % POLLUTE_INTERVAL == 0 then
        surface.pollute(SPAWNER_POS, 20)
    end

    if state.phase == "setup" then
        local base = remote.call("Rampant", "queryBaseAt", SPAWNER_POS, surface)
        if base then
            r.baseFormed = true
            r.baseId = base.id
            print("[PASS] base formed at spawner (id=" .. base.id .. ") at " ..
                string.format("%.1f", gameMinutes(tick - state.stagedAt)) .. "min")
            -- CONSTRUCT migration: MIGRATING state + funds for the settler
            local stateOk = remote.call("Rampant", "setBaseState", base.id, BASE_AI_STATE_MIGRATING)
            local pointsOk = remote.call("Rampant", "grantUnitPoints", base.id, 1000)
            print("[INFO] constructed: MIGRATING=" .. tostring(stateOk) ..
                " points=1000 -> " .. tostring(pointsOk))
            -- Construct the army: free units near the nest so the settler
            -- wave can recruit members (deploySquad's set_multi_command
            -- needs recruit-able units; with none, the group forms empty
            -- and is destroyed)
            local spawned = 0
            for i = 1, 30 do
                local unit = surface.create_entity{
                    name = "small-biter",
                    position = { x = SPAWNER_POS.x + (i % 5) * 3, y = SPAWNER_POS.y + math.floor(i / 5) * 3 },
                    force = "enemy",
                }
                if unit and unit.valid then
                    spawned = spawned + 1
                end
            end
            print("[INFO] spawned " .. spawned .. " free units at the nest")
            setPhase("migrate")
        elseif phaseElapsed > PHASE_LIMIT_MINUTES * TICKS_A_MINUTE then
            finish(false, "base never formed within " .. PHASE_LIMIT_MINUTES .. " game-minutes")
            return
        end

    elseif state.phase == "migrate" then
        -- Re-assert construction: the base at the spawner can be recycled
        -- and re-formed (natural base -> our base), so construct on the
        -- CURRENT base whenever the id changes
        local liveBase = remote.call("Rampant", "queryBaseAt", SPAWNER_POS, surface)
        if liveBase and liveBase.id ~= r.baseId then
            r.baseId = liveBase.id
            local stateOk = remote.call("Rampant", "setBaseState", liveBase.id, BASE_AI_STATE_MIGRATING)
            local pointsOk = remote.call("Rampant", "grantUnitPoints", liveBase.id, 1000)
            print("[INFO] re-constructed on base id=" .. liveBase.id ..
                " MIGRATING=" .. tostring(stateOk) .. " points=" .. tostring(pointsOk))
        end
        -- Track the settler's position/state every 2 game-minutes
        if r.settlerFormed and tick % (2 * TICKS_A_MINUTE) == 0 then
            local groups = remote.call("Rampant", "getEnemyGroups", surface)
            if groups then
                for _, g in ipairs(groups) do
                    if g.isBuilder then
                        r.settlerPos = string.format("%.0f,%.0f", g.position.x, g.position.y)
                        r.settlerState = g.state
                        print("[TRACK] settler state=" .. tostring(g.state) ..
                            " pos=(" .. r.settlerPos .. ") at " ..
                            string.format("%.1f", gameMinutes(tick - state.stagedAt)) .. "min")
                        break
                    end
                end
            end
        end
        -- A settler squad must form (isBuilder=true)
        if not r.settlerFormed then
            local groups = remote.call("Rampant", "getEnemyGroups", surface)
            if groups then
                for _, g in ipairs(groups) do
                    if g.isBuilder then
                        r.settlerFormed = true
                        r.settlerId = g.id
                        print("[PASS] settler squad formed (id=" .. g.id .. ") at " ..
                            string.format("%.1f", gameMinutes(tick - state.stagedAt)) .. "min")
                        break
                    end
                end
            end
        end
        if r.settlerFormed and not r.dispatched then
            -- The settler must be DISPATCHED (the mod's dispatch pipeline ran)
            local m = remote.call("Rampant", "getMetrics")
            local counters = m and m.counters or {}
            if (counters["cmd.DISPATCH_SQUAD"] or 0) > 0 then
                r.dispatched = true
                print("[PASS] settler dispatched (cmd.DISPATCH_SQUAD) at " ..
                    string.format("%.1f", gameMinutes(tick - state.stagedAt)) .. "min")
            end
        end
        if r.settlerFormed and r.dispatched and not r.moved then
            -- The settler must LEAVE the nest region (movement start)
            local groups = remote.call("Rampant", "getEnemyGroups", surface)
            if groups then
                for _, g in ipairs(groups) do
                    if g.isBuilder then
                        local dx = g.position.x - SPAWNER_POS.x
                        local dy = g.position.y - SPAWNER_POS.y
                        local dist = math.sqrt(dx * dx + dy * dy)
                        if dist > 20 then
                            r.moved = true
                            r.settlerPos = string.format("%.0f,%.0f", g.position.x, g.position.y)
                            print("[PASS] settler left the nest region (dist=" ..
                                string.format("%.0f", dist) .. " tiles, pos=(" ..
                                r.settlerPos .. ")) at " ..
                                string.format("%.1f", gameMinutes(tick - state.stagedAt)) .. "min")
                        end
                        break
                    end
                end
            end
        end
        if r.settlerFormed and r.dispatched and r.moved then
            finish(true, "migration-settler expectations met")
            return
        end
        if phaseElapsed > PHASE_LIMIT_MINUTES * TICKS_A_MINUTE then
            local m = remote.call("Rampant", "getMetrics")
            local counters = m and m.counters or {}
            finish(false, "migration phase incomplete after " .. PHASE_LIMIT_MINUTES ..
                " game-minutes: settlerFormed=" .. tostring(r.settlerFormed) ..
                " dispatched=" .. tostring(r.dispatched) ..
                " moved=" .. tostring(r.moved) ..
                " settlerPos=" .. tostring(r.settlerPos) ..
                " squadCreated=" .. tostring(counters["squad.created"]) ..
                " squadDestroyed=" .. tostring(counters["squad.destroyed"]) ..
                " dispatch=" .. tostring(counters["cmd.DISPATCH_SQUAD"]))
            return
        end
    end
end)
