--[[
    Rampant Behavior Scenario: Base AI States (CONSTRUCTED)

    Behavioral verification of the base AI state machine. The base is
    constructed (spawner + pollution), then each state's DISTINCT behavior
    is verified in-engine:

      1. setup     - wall + ammo-less turret + Rampant spawner at (100,0);
                     base forms; evolution 0.5 (unit-point cap headroom);
                     RAIDING constructed (frozen)
      2. raid      - AGGRESSIVE squad forms (getEnemyGroups) and units
                     reach the wall perimeter (RAIDING attack behavior);
                     unitPoints net growth measured over 8 game-min
                     (the RAIDING economy baseline)
      3. onslaught - unitPoints net growth measured over 8 game-min;
                     ONSLAUGHT doubles the logic-cycle point gain
                     (processBase: points * 2) -> assert >= 1.3x the
                     RAIDING baseline; aggressive squads still form
      4. migration - BUILDER squad (settler) forms and leaves the nest
                     region (MIGRATING expansion behavior)
      5. transition- the frozen state is released (nudge); when stateAITick
                     expires the state machine rolls a new state
                     (cmd.CHANGE_BASE_STATE executes, stateAITick resets
                     into the future)

    SIEGE is not asserted separately: its settler formation (canMigrate
    allows MIGRATING and SIEGE) is identical to MIGRATING at the
    observable level; the distinct "build toward player" target selection
    is future work.

    Expected total: ~25-40 game-minutes.
]]

local WALL_POS = { x = 0, y = 0 }
local SPAWNER_POS = { x = 100, y = 0 }
local TURRET_POS = { x = 6, y = 0 }
local WALL_PERIMETER = 30
local NEST_REGION = 40
local TICKS_A_MINUTE = 60 * 60
-- The attack squads take ~50-60 game-minutes to cross the 3-chunk distance
-- (the observed movement cadence — wall-defense: 58min for the arrival).
local BUDGET_MINUTES = 150
local BUDGET_TICKS = BUDGET_MINUTES * TICKS_A_MINUTE
local PHASE_LIMIT_MINUTES = 60
local ECON_SAMPLE_INTERVAL = 30 * 60  -- 30 game-seconds
local ECON_WINDOW_SAMPLES = 16        -- 8 game-minutes per window
local ECON_MIN_RATIO = 1.3            -- onslaught/raid net-growth ratio
local BASE_AI_STATE_RAIDING = 4
local BASE_AI_STATE_MIGRATING = 5
local BASE_AI_STATE_ONSLAUGHT = 7
local EVOLUTION = 0.5

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

local function queryBase(surface)
    return remote.call("Rampant", "queryBaseAt", SPAWNER_POS, surface)
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

local function countByType(surface, isBuilder)
    local n = 0
    for _, s in ipairs(squads(surface)) do
        if s.isBuilder == isBuilder and s.memberCount and s.memberCount >= 1 then
            n = n + 1
        end
    end
    return n
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

    -- Player wall ring + gun-turret (ammo cleared: no autonomous kills —
    -- every death in this scenario must be scripted).
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
    -- Rampant spawner -> base creation (registered directly, no upgrade).
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
    print("--- Behavior Scenario: Base AI States ---")
    print("Passed: " .. (passed and 1 or 0))
    print("Failed: " .. (passed and 0 or 1))
    if reason then
        print("Detail: " .. reason)
    end
    print("Detail: base=" .. tostring(r.baseFormed) ..
        " attack=" .. tostring(r.attack) ..
        " raidDelta=" .. tostring(r.raidDelta) ..
        " onslaughtDelta=" .. tostring(r.onslaughtDelta) ..
        " migration=" .. tostring(r.migration) ..
        " rolled=" .. tostring(r.rolled) ..
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

--- Sample the economy: append the base's unitPoints every
--- ECON_SAMPLE_INTERVAL; when ECON_WINDOW_SAMPLES samples are collected
--- return the net growth (last - first), else nil.
local function econSample(base, r, tick)
    if not r.econStartTick then
        r.econStartTick = tick
        r.econLastSampleTick = tick
        r.econSamples = { base.unitPoints or 0 }
        return nil
    end
    if tick - r.econLastSampleTick < ECON_SAMPLE_INTERVAL then
        return nil
    end
    r.econLastSampleTick = tick
    r.econSamples[#r.econSamples + 1] = base.unitPoints or 0
    if #r.econSamples == ECON_WINDOW_SAMPLES then
        local delta = r.econSamples[#r.econSamples] - r.econSamples[1]
        print("[INFO] econ window done: " .. #r.econSamples .. " samples, net=" ..
            string.format("%.1f", delta) ..
            " (first=" .. string.format("%.1f", r.econSamples[1]) ..
            " last=" .. string.format("%.1f", r.econSamples[#r.econSamples]) .. ")")
        return delta
    end
    return nil
end

script.on_event(defines.events.on_tick, function()
    if not rampReady() then
        return
    end
    if not state then
        -- Stage AFTER the mod's initial chunk scan completes (the scan's
        -- unit purge destroys units in scanned chunks).
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

    -- Keep the nest active: the constructed scenario has no player factory
    -- generating pollution; the staged pollution decays within ~15
    -- game-minutes and the nest goes inactive (no spawner processing -> no
    -- units -> no squads). Periodic re-pollution simulates the player
    -- factory (the natural stimulus).
    if tick - (storage.rampantLastPollute or 0) > 60 * 60 then
        storage.rampantLastPollute = tick
        surface.pollute(SPAWNER_POS, 10)
    end

    if elapsed > BUDGET_TICKS then
        finish(false, "budget " .. BUDGET_MINUTES .. " game-minutes elapsed")
        return
    end

    if state.phase == "setup" then
        local base = queryBase(surface)
        if base then
            r.baseFormed = true
            r.baseId = base.id
            print("[PASS] base formed (id=" .. base.id .. ") at " ..
                string.format("%.1f", gameMinutes(tick - state.stagedAt)) .. "min")
            remote.call("Rampant", "setEvolution", EVOLUTION)
            local stateOk = remote.call("Rampant", "setBaseState", base.id, BASE_AI_STATE_RAIDING)
            print("[INFO] constructed: evo=" .. EVOLUTION .. " RAIDING=" .. tostring(stateOk))
            setPhase("raid")
        elseif phaseElapsed > PHASE_LIMIT_MINUTES * TICKS_A_MINUTE then
            finish(false, "base never formed within " .. PHASE_LIMIT_MINUTES .. " game-minutes")
            return
        end

    elseif state.phase == "raid" then
        -- Behavioral: an aggressive squad forms and units reach the wall.
        -- Economy: sample unitPoints for the RAIDING baseline while the
        -- attack proceeds (steady state).
        local base = queryBase(surface)
        if not r.attack and #unitsNearWall(surface) > 0 then
            r.attack = true
            r.attackSquads = countByType(surface, false)
            print("[PASS] enemy unit(s) reached wall perimeter at " ..
                string.format("%.1f", gameMinutes(tick - state.stagedAt)) .. "min" ..
                " (aggressive squads=" .. r.attackSquads .. ")")
        end
        if base and r.attack then
            local delta = econSample(base, r, tick)
            if delta then
                r.raidDelta = delta
                print("[INFO] RAIDING baseline done, net=" .. string.format("%.1f", delta))
                remote.call("Rampant", "setBaseState", r.baseId, BASE_AI_STATE_ONSLAUGHT)
                r.econStartTick = nil
                r.econSamples = nil
                r.econLastSampleTick = nil
                setPhase("onslaught")
            end
        elseif phaseElapsed > PHASE_LIMIT_MINUTES * TICKS_A_MINUTE then
            local p = remote.call("Rampant", "queryPheromones", SPAWNER_POS, surface)
            local pollution = surface.get_pollution(SPAWNER_POS)
            local active = remote.call("Rampant", "getActiveStructures", surface)
            local m = remote.call("Rampant", "getMetricsSummary")
            local c = m and m.counters or {}
            finish(false, "raid: no attack within " .. PHASE_LIMIT_MINUTES .. " game-minutes" ..
                " (squads=" .. countByType(surface, false) ..
                " basePheromone=" .. tostring(p and p.base) ..
                " pollution=" .. tostring(pollution) ..
                " structures=" .. tostring(#active) ..
                " formAttempt=" .. tostring(c["squad.formAttempt"]) ..
                " formReject.points=" .. tostring(c["squad.formReject.attacker.points"]) ..
                " formReject.pheromone=" .. tostring(c["squad.formReject.attacker.pheromone"]) ..
                " formReject.squadLimit=" .. tostring(c["squad.formReject.attacker.squadLimit"]) ..
                " formSkip.cooldown=" .. tostring(c["squad.formSkip.cooldown"]) .. ")")
            return
        end

    elseif state.phase == "onslaught" then
        -- The ONSLAUGHT economy: logic-cycle point gain is doubled
        -- (processBase: points * 2). Assert the net growth beats the
        -- RAIDING baseline by ECON_MIN_RATIO.
        local base = queryBase(surface)
        if base then
            local delta = econSample(base, r, tick)
            if delta then
                r.onslaughtDelta = delta
                local ratio = delta / math.max(r.raidDelta or 1, 1)
                if delta >= ECON_MIN_RATIO * (r.raidDelta or 0) then
                    print("[PASS] ONSLAUGHT economy: net=" .. string.format("%.1f", delta) ..
                        " vs RAIDING " .. string.format("%.1f", r.raidDelta or 0) ..
                        " (ratio=" .. string.format("%.2f", ratio) .. ") at " ..
                        string.format("%.1f", gameMinutes(tick - state.stagedAt)) .. "min")
                    r.economy = true
                else
                    finish(false, "onslaught economy not doubled: net=" ..
                        string.format("%.1f", delta) .. " vs RAIDING " ..
                        string.format("%.1f", r.raidDelta or 0) ..
                        " (ratio=" .. string.format("%.2f", ratio) .. ")")
                    return
                end
                remote.call("Rampant", "setBaseState", r.baseId, BASE_AI_STATE_MIGRATING)
                setPhase("migration")
            end
        end
        if phaseElapsed > PHASE_LIMIT_MINUTES * TICKS_A_MINUTE then
            finish(false, "onslaught: base lost or economy window incomplete")
            return
        end

    elseif state.phase == "migration" then
        -- MIGRATING: a BUILDER (settler) squad forms and leaves the nest
        -- region (the expansion behavior).
        local base = queryBase(surface)
        local builders = countByType(surface, true)
        if builders > 0 then
            local left = false
            for _, s in ipairs(squads(surface)) do
                if s.isBuilder and s.position then
                    local dx = s.position.x - SPAWNER_POS.x
                    local dy = s.position.y - SPAWNER_POS.y
                    if (dx * dx + dy * dy) > NEST_REGION * NEST_REGION then
                        left = true
                        r.settlerDist = math.floor(math.sqrt(dx * dx + dy * dy))
                        break
                    end
                end
            end
            if left then
                r.migration = true
                print("[PASS] settler left the nest region (dist=" .. r.settlerDist ..
                    " tiles, builders=" .. builders .. ") at " ..
                    string.format("%.1f", gameMinutes(tick - state.stagedAt)) .. "min")
                -- Release the frozen state machine: the next natural roll
                -- proceeds on the normal schedule.
                remote.call("Rampant", "nudgeBaseState", r.baseId, BASE_AI_STATE_MIGRATING)
                local m = remote.call("Rampant", "getMetricsSummary")
                local c = m and m.counters or {}
                r.preRollChangeCount = c["cmd.CHANGE_BASE_STATE"] or 0
                print("[INFO] state machine released; waiting for the natural roll")
                setPhase("transition")
            end
        elseif phaseElapsed > PHASE_LIMIT_MINUTES * TICKS_A_MINUTE then
            finish(false, "migration: no builder squad within " ..
                PHASE_LIMIT_MINUTES .. " game-minutes (builders=" .. builders .. ")")
            return
        end

    elseif state.phase == "transition" then
        -- The frozen state was released. When stateAITick expires, the
        -- state machine rolls a new state: cmd.CHANGE_BASE_STATE executes
        -- and stateAITick resets into the future.
        local base = queryBase(surface)
        if base and base.stateAITick and base.stateAITick <= tick then
            local m = remote.call("Rampant", "getMetricsSummary")
            local c = m and m.counters or {}
            local changeCount = c["cmd.CHANGE_BASE_STATE"] or 0
            if changeCount > (r.preRollChangeCount or 0) then
                r.rolled = true
                r.rolledState = base.state
                print("[PASS] state machine rolled: cmd.CHANGE_BASE_STATE -> " ..
                    changeCount .. ", state=" .. tostring(base.state) ..
                    ", next roll @ " .. string.format("%.1f",
                        gameMinutes((base.stateAITick or 0) - tick)) .. "min at " ..
                    string.format("%.1f", gameMinutes(tick - state.stagedAt)) .. "min")
                finish(true, "all base-AI state expectations met")
                return
            elseif phaseElapsed > PHASE_LIMIT_MINUTES * TICKS_A_MINUTE then
                finish(false, "transition: stateAITick expired but no roll applied" ..
                    " (changeCount=" .. changeCount .. ")")
                return
            end
        elseif phaseElapsed > PHASE_LIMIT_MINUTES * TICKS_A_MINUTE then
            finish(false, "transition: stateAITick never expired within " ..
                PHASE_LIMIT_MINUTES .. " game-minutes")
            return
        end
    end
end)
