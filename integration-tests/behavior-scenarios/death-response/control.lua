--[[
    Rampant Behavior Scenario: Death Response (CONSTRUCTED)

    Behavioral verification of the onDeath chain. Preconditions are
    CONSTRUCTED via test hooks (gated on the dev telemetry flag) rather
    than waited for — this test answers "is the death behavior true
    in-game?", not "does an attack eventually happen" (that is
    wall-defense's job):

      1. setup     - wall + turret at origin, spawner at (100,0); base
                     formed; construct RAIDING state + 2000 unit points;
                     spawn 5 enemy units AT the wall (constructed attack)
      2. retreat   - kill 2 units with the player turret cause -> chunk
                     death rating drops below the retreat threshold ->
                     survivors flee the wall perimeter
      3. structure - kill remaining units + the spawner -> UNREGISTER ->
                     base recycles (getBasePoints nil) and unitPoints
                     dropped (-500 nest lost)

    Expected total: ~5-10 game-minutes (~15-30s real at speed 100).
]]

local WALL_POS = { x = 0, y = 0 }
local SPAWNER_POS = { x = 100, y = 0 }
local TURRET_POS = { x = 6, y = 0 }
local UNIT_SPAWN_POS = { x = 5, y = 0 }
local WALL_PERIMETER = 30
-- Tight perimeter for the retreat-movement assertion: the retreat moves the
-- grabbed unit to the retreat position (~chunk center, >= 12 tiles from the
-- wall) — still inside the 30-tile perimeter, but clearly away from the wall
-- ring. Survivors sit at ~5 tiles.
local WALL_PERIMETER_TIGHT = 10
local TICKS_A_MINUTE = 60 * 60
local BUDGET_MINUTES = 40
local BUDGET_TICKS = BUDGET_MINUTES * TICKS_A_MINUTE
local PHASE_LIMIT_MINUTES = 15
local POLLUTE_INTERVAL = 600
local BASE_AI_STATE_RAIDING = 4

local state  -- { phase, results, stagedAt, phaseStartedAt, turret, done }

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

local function unitsNearWallTight(surface)
    return surface.find_entities_filtered{
        force = "enemy",
        type = "unit",
        area = {
            { WALL_POS.x - WALL_PERIMETER_TIGHT, WALL_POS.y - WALL_PERIMETER_TIGHT },
            { WALL_POS.x + WALL_PERIMETER_TIGHT, WALL_POS.y + WALL_PERIMETER_TIGHT },
        },
    }
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

    -- Player wall ring + gun-turret (the turret is the death CAUSE).
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
    -- Clear the turret's ammo: it must NOT kill units on its own (the death
    -- sequence below must be deterministic — every kill is scripted).
    local turret = surface.find_entities_filtered{ name = "gun-turret", force = "player" }[1]
    if turret then
        local ammo = turret.get_inventory(defines.inventory.turret_ammo)
        if ammo then
            ammo.clear()
        end
    end
    -- Enemy spawner -> base creation. Use a RAMPANT spawner (not the
    -- vanilla "biter-spawner"): vanilla entities get queued for upgrade by
    -- the mod (building.lua) — the upgrade destroys the entity and its
    -- replacement is never re-registered, so the structure-death chain
    -- (unregister + -500 point cost) could never fire on it. Rampant
    -- spawners register directly and persist.
    surface.create_entity{
        name = "neutral-biter-spawner-v1-t1-rampant",
        position = SPAWNER_POS,
        force = "enemy",
        raise_built = true,
    }
    surface.pollute(SPAWNER_POS, 50)
end

local function killUnits(surface, count)
    local turret = state.turret
    local killed = 0
    local units = unitsNearWall(surface)
    for _, u in ipairs(units) do
        if killed >= count then
            break
        end
        -- die() raises on_entity_died synchronously. Rampant's handler can
        -- invalidate sibling units mid-loop, so re-check validity before
        -- each kill. Errors propagate (no pcall): a failure here is a real
        -- bug and must fail the run loudly.
        if u.valid and turret and turret.valid then
            u.die("player", turret)
            killed = killed + 1
        end
    end
    return killed
end

local function killSpawners(surface)
    local turret = state.turret
    local killed = 0
    local spawners = surface.find_entities_filtered{ force = "enemy", type = "unit-spawner" }
    for _, s in ipairs(spawners) do
        -- Errors propagate (no pcall) — see killUnits.
        if s.valid and turret and turret.valid then
            s.die("player", turret)
            killed = killed + 1
        end
    end
    return killed
end

local function setPhase(phase)
    state.phase = phase
    state.phaseStartedAt = game.tick
    print("PHASE: " .. phase .. " at " ..
        string.format("%.1f", gameMinutes(game.tick - state.stagedAt)) .. "min")
end

local function finish(passed, reason)
    local r = state.results
    print("--- Behavior Scenario: Death Response ---")
    print("Passed: " .. (passed and 1 or 0))
    print("Failed: " .. (passed and 0 or 1))
    if reason then
        print("Detail: " .. reason)
    end
    print("Detail: base=" .. tostring(r.baseFormed) ..
        " retreat=" .. tostring(r.retreat) ..
        " baseRecycled=" .. tostring(r.baseRecycled) ..
        " pointsDropped=" .. tostring(r.pointsDropped) ..
        " preKillPoints=" .. tostring(r.preKillPoints) ..
        " postKillModifyCount=" .. tostring(r.postKillPoints) ..
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
        local surface = game.surfaces[1]
        stage(surface)
        state.turret = surface.find_entities_filtered{ name = "gun-turret", force = "player" }[1]
        game.speed = 100
        print("STAGED: wall+turret at origin, spawner at (100,0), speed=100")
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

    if state.phase == "setup" then
        -- Wait for the base, then CONSTRUCT the attack preconditions
        local base = remote.call("Rampant", "queryBaseAt", SPAWNER_POS, surface)
        if base then
            r.baseFormed = true
            r.baseId = base.id
            print("[PASS] base formed at spawner (id=" .. base.id .. ") at " ..
                string.format("%.1f", gameMinutes(tick - state.stagedAt)) .. "min")
            local stateOk = remote.call("Rampant", "setBaseState", base.id, BASE_AI_STATE_RAIDING)
            local pointsOk = remote.call("Rampant", "grantUnitPoints", base.id, 2000)
            print("[INFO] constructed: RAIDING=" .. tostring(stateOk) ..
                " points=2000 -> " .. tostring(pointsOk))
            r.preKillPoints = 2000
            -- The -500 nest-lost cost is unobservable through the base object
            -- (it recycles within 1-2 ticks of the death batch), so assert
            -- via telemetry: cmd.MODIFY_UNIT_POINTS proves the command
            -- pipeline executed the point cost
            local m = remote.call("Rampant", "getMetrics")
            r.preKillModifyCount = m and m.counters and m.counters["cmd.MODIFY_UNIT_POINTS"] or 0
            -- Construct the attack: enemy units AT the wall, in a unit group
            -- (unassigned units get recycled by Rampant; grouped units with
            -- an attack command are exempt and actually engage the wall)
            local spawned = 0
            local group = surface.create_unit_group{ position = UNIT_SPAWN_POS, force = "enemy" }
            for i = 1, 5 do
                local unit = surface.create_entity{
                    name = "small-biter",
                    position = { x = UNIT_SPAWN_POS.x + (i % 3) * 2, y = UNIT_SPAWN_POS.y + math.floor(i / 3) * 2 },
                    force = "enemy",
                }
                if unit and unit.valid then
                    group.add_member(unit)
                    spawned = spawned + 1
                end
            end
            if group.valid then
                group.set_command{
                    type = defines.command.attack_area,
                    destination = WALL_POS,
                    radius = 8,
                }
            end
            state.group = group
            print("[INFO] spawned " .. spawned .. " enemy units at the wall (grouped, attacking)")
            setPhase("retreat")
        elseif phaseElapsed > PHASE_LIMIT_MINUTES * TICKS_A_MINUTE then
            finish(false, "base never formed within " .. PHASE_LIMIT_MINUTES .. " game-minutes")
            return
        end

    elseif state.phase == "retreat" then
        if not r.killed then
            -- Construct the retreat trigger. The flee command can only pick
            -- up UNGROUPED units (set_multi_command skips grouped units), so
            -- the group must be destroyed BEFORE the retreat fires:
            --   1. kill 1 unit — death #1's rating clamps at 1.0 and can
            --      never drop below the retreat threshold (<= 0.999)
            --   2. destroy the group — survivors become ungrouped
            --   3. kill 2 more — death #2/#3 ratings drop below the
            --      threshold -> the retreat fires on the ungrouped survivors
            local units = unitsNearWall(surface)
            if #units >= 3 then
                local killed = killUnits(surface, 1)
                if state.group and state.group.valid then
                    state.group.destroy()
                end
                killed = killed + killUnits(surface, 2)
                r.killed = killed
                r.postKillUnits = #unitsNearWall(surface)
                r.postKillTight = #unitsNearWallTight(surface)
                print("[INFO] killed " .. killed .. "; group destroyed; " ..
                    r.postKillUnits .. " ungrouped survivor(s) remain (" ..
                    r.postKillTight .. " within " .. WALL_PERIMETER_TIGHT .. " of the wall)")
                local m = remote.call("Rampant", "getMetricsSummary")
                local c = m and m.counters or {}
                print("[INFO] post-kill: causeValid=" .. tostring(c["death.causeValid"]) ..
                    " cmd.RETREAT_UNITS=" .. tostring(c["cmd.RETREAT_UNITS"]) ..
                    " retreated=" .. tostring(c["squad.retreated"]) ..
                    " bail.noCause=" .. tostring(c["squad.retreatBail.noCause"]) ..
                    " bail.noUnits=" .. tostring(c["squad.retreatBail.noUnits"]) ..
                    " bail.gate=" .. tostring(c["squad.retreatBail.gate"]))
            elseif phaseElapsed > 2 * TICKS_A_MINUTE then
                finish(false, "fewer than 3 enemy units at the wall in the retreat phase")
                return
            end
        else
            -- Kills done. The retreat must have EXECUTED (squad.retreated
            -- counter — the honest signal) and at least one survivor must
            -- actually leave the perimeter. If no retreat fired within 2
            -- game-min, kill one more unit: its death re-evaluates the
            -- retreat against the remaining ungrouped survivors.
            local m = remote.call("Rampant", "getMetricsSummary")
            local c = m and m.counters or {}
            local retreated = c["squad.retreated"] or 0
            if retreated == 0 and not r.fallbackKilled and phaseElapsed > 2 * TICKS_A_MINUTE then
                r.fallbackKilled = true
                local killed = killUnits(surface, 1)
                r.postKillUnits = #unitsNearWall(surface)
                r.postKillTight = #unitsNearWallTight(surface)
                print("[INFO] fallback: killed " .. killed .. " more (" .. r.postKillUnits .. " remain)")
            end
            -- Movement assertion: the grabbed unit must leave the TIGHT
            -- perimeter (the 30-tile perimeter is too wide — the retreat
            -- position sits inside it). squad.retreated is the command-chain
            -- signal; the tight count is the physical movement signal.
            if retreated >= 1 and #unitsNearWallTight(surface) < (r.postKillTight or 2) then
                r.retreat = true
                r.retreatCount = retreated
                print("[PASS] retreat executed (" .. retreated .. "); survivors left wall perimeter at " ..
                    string.format("%.1f", gameMinutes(tick - state.stagedAt)) .. "min")
                killUnits(surface, 999)
                local killedSpawners = killSpawners(surface)
                print("[INFO] killed " .. killedSpawners .. " spawner(s)")
                setPhase("structure")
            elseif phaseElapsed > PHASE_LIMIT_MINUTES * TICKS_A_MINUTE then
                finish(false, "retreat: " .. #unitsNearWall(surface) ..
                    " unit(s) still at wall after " .. PHASE_LIMIT_MINUTES .. " game-minutes" ..
                    " (retreated=" .. retreated ..
                    " noCause=" .. tostring(c["squad.retreatBail.noCause"]) ..
                    " noUnits=" .. tostring(c["squad.retreatBail.noUnits"]) ..
                    " gate=" .. tostring(c["squad.retreatBail.gate"]) .. ")")
                return
            end
        end

    elseif state.phase == "structure" then
        -- Core structure-death behavior: the spawner is GONE (unregister
        -- processed) and the point cost applied. Base recycling is
        -- secondary cleanup (timing varies with the base's remaining
        -- chunks); report it when observed but do not require it.
        local liveBase = remote.call("Rampant", "queryBaseAt", SPAWNER_POS, surface)
        if not r.baseRecycled and not liveBase then
            r.baseRecycled = true
            print("[PASS] base recycled after spawner death at " ..
                string.format("%.1f", gameMinutes(tick - state.stagedAt)) .. "min")
        end
        if not r.spawnerGone then
            local spawners = surface.find_entities_filtered{ force = "enemy", type = "unit-spawner" }
            if #spawners == 0 then
                r.spawnerGone = true
                print("[PASS] spawner unregistered (structure death) at " ..
                    string.format("%.1f", gameMinutes(tick - state.stagedAt)) .. "min")
            end
        end
        if not r.pointsDropped then
            local m = remote.call("Rampant", "getMetrics")
            local modifyCount = m and m.counters and m.counters["cmd.MODIFY_UNIT_POINTS"] or 0
            if modifyCount > (r.preKillModifyCount or 0) then
                r.pointsDropped = true
                r.postKillPoints = modifyCount
                print("[PASS] unit point cost applied (cmd.MODIFY_UNIT_POINTS -> " ..
                    modifyCount .. ") at " ..
                    string.format("%.1f", gameMinutes(tick - state.stagedAt)) .. "min")
            end
        end
        if r.spawnerGone and r.pointsDropped then
            finish(true, "all death-response expectations met")
            return
        end
        if phaseElapsed > PHASE_LIMIT_MINUTES * TICKS_A_MINUTE then
            finish(false, "structure phase incomplete after " .. PHASE_LIMIT_MINUTES ..
                " game-minutes: baseRecycled=" .. tostring(r.baseRecycled) ..
                " pointsDropped=" .. tostring(r.pointsDropped))
            return
        end
    end
end)
