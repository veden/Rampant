--[[
    Rampant Behavior Scenario: Squad Cycle (CONSTRUCTED)

    Behavioral verification of the squad lifecycle:
      1. setup   - wall + ammo-less turret + Rampant spawner; base forms;
                   RAIDING constructed; 5 grouped units staged at the wall
      2. attack  - the constructed units are at the wall (the attack is
                   present)
      3. retreat - kill 1 (death #1 rating clamps at 1.0 — never retreats),
                   destroy the group (the flee command only picks up
                   UNGROUPED units), kill 2 more -> the retreat fires
                   (squad.retreated >= 1, a squad forms with status
                   RETREATING)
      4. re-engage - the squad state machine transitions RETREATING ->
                   RAIDING ("retreat_end", cmd.DISPATCH_SQUAD) — the squad
                   returns to the attack posture

    Known limitation (documented, not asserted): the kamikaze escalation
    (existingSquad.retreats >= 3) requires consecutive retreats on the SAME
    squad within its ~2-tick RETREATING window, but the chunk retreat
    cooldown (10 game-seconds) blocks re-fires on the same chunk — the
    escalation is unreachable in constructed scenarios. The legacy reuse
    (findNearbyRetreatingSquad) is restored for parity; the random
    kamikaze-at-creation path remains the reachable kamikaze source.

    Expected total: ~5 game-minutes.
]]

local WALL_POS = { x = 0, y = 0 }
local SPAWNER_POS = { x = 100, y = 0 }
local TURRET_POS = { x = 6, y = 0 }
local UNIT_SPAWN_POS = { x = 5, y = 0 }
local WALL_PERIMETER = 30
local TICKS_A_MINUTE = 60 * 60
local BUDGET_MINUTES = 20
local BUDGET_TICKS = BUDGET_MINUTES * TICKS_A_MINUTE
local PHASE_LIMIT_MINUTES = 8
local BASE_AI_STATE_RAIDING = 4
local SQUAD_RETREATING = 1
local SQUAD_RAIDING = 4

local state  -- { phase, results, stagedAt, phaseStartedAt, group, done }

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

    -- Player wall ring + ammo-less gun-turret (no autonomous kills).
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
    -- Rampant spawner -> base creation.
    surface.create_entity{
        name = "neutral-biter-spawner-v1-t1-rampant",
        position = SPAWNER_POS,
        force = "enemy",
        raise_built = true,
    }
    surface.pollute(SPAWNER_POS, 50)
end

local function killUnits(surface, count)
    local turret = surface.find_entities_filtered{ name = "gun-turret", force = "player" }[1]
    local killed = 0
    local units = unitsNearWall(surface)
    for _, u in ipairs(units) do
        if killed >= count then
            break
        end
        -- No pcall: die() errors (or errors inside Rampant's synchronous
        -- death handler) are real bugs and must fail the run loudly.
        if u.valid and turret and turret.valid then
            u.die("player", turret)
            killed = killed + 1
        end
    end
    return killed
end

local function spawnAttackGroup(surface)
    -- A grouped attack force at the wall (grouped units survive Rampant's
    -- recycling; ungrouped units get recycled).
    local group = surface.create_unit_group{ position = UNIT_SPAWN_POS, force = "enemy" }
    local spawned = 0
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
    return group, spawned
end

local function setPhase(phase)
    state.phase = phase
    state.phaseStartedAt = game.tick
    print("PHASE: " .. phase .. " at " ..
        string.format("%.1f", gameMinutes(game.tick - state.stagedAt)) .. "min")
end

local function finish(passed, reason)
    local r = state.results
    print("--- Behavior Scenario: Squad Cycle ---")
    print("Passed: " .. (passed and 1 or 0))
    print("Failed: " .. (passed and 0 or 1))
    if reason then
        print("Detail: " .. reason)
    end
    print("Detail: base=" .. tostring(r.baseFormed) ..
        " attack=" .. tostring(r.attack) ..
        " retreated=" .. tostring(r.retreated) ..
        " squadId=" .. tostring(r.squadId) ..
        " retreatStatus=" .. tostring(r.retreatStatus) ..
        " reEngaged=" .. tostring(r.reEngaged) ..
        " statusAfter=" .. tostring(r.statusAfter) ..
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
        -- Stage AFTER the mod's initial chunk scan completes.
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

    -- Keep the nest active (no player factory in the constructed map).
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
            r.baseFormed = true
            r.baseId = base.id
            print("[PASS] base formed (id=" .. base.id .. ") at " ..
                string.format("%.1f", gameMinutes(tick - state.stagedAt)) .. "min")
            local stateOk = remote.call("Rampant", "setBaseState", base.id, BASE_AI_STATE_RAIDING)
            local pointsOk = remote.call("Rampant", "grantUnitPoints", base.id, 2000)
            print("[INFO] constructed: RAIDING=" .. tostring(stateOk) ..
                " points=2000 -> " .. tostring(pointsOk))
            state.group, r.spawned = spawnAttackGroup(surface)
            print("[INFO] spawned " .. r.spawned .. " enemy units at the wall (grouped, attacking)")
            setPhase("attack")
        elseif phaseElapsed > PHASE_LIMIT_MINUTES * TICKS_A_MINUTE then
            finish(false, "base never formed within " .. PHASE_LIMIT_MINUTES .. " game-minutes")
            return
        end

    elseif state.phase == "attack" then
        -- The constructed attack force is present at the wall.
        if #unitsNearWall(surface) >= 3 then
            r.attack = true
            print("[PASS] attack present: " .. #unitsNearWall(surface) ..
                " unit(s) at wall at " ..
                string.format("%.1f", gameMinutes(tick - state.stagedAt)) .. "min")
            -- Trigger the retreat: kill 1 (never retreats), destroy the
            -- group (ungroup the survivors), kill 2 more (rating drops
            -- below the threshold -> retreat fires on the ungrouped).
            local killed = killUnits(surface, 1)
            if state.group and state.group.valid then
                state.group.destroy()
            end
            killed = killed + killUnits(surface, 2)
            r.killed = killed
            print("[INFO] killed " .. killed .. "; group destroyed; " ..
                #unitsNearWall(surface) .. " ungrouped survivor(s) remain")
            setPhase("retreat")
        elseif phaseElapsed > PHASE_LIMIT_MINUTES * TICKS_A_MINUTE then
            finish(false, "attack: fewer than 3 units at the wall within " ..
                PHASE_LIMIT_MINUTES .. " game-minutes")
            return
        end

    elseif state.phase == "retreat" then
        -- The retreat must have fired: squad.retreated counter + a squad
        -- exists with status RETREATING.
        local m = remote.call("Rampant", "getMetricsSummary")
        local c = m and m.counters or {}
        local retreated = c["squad.retreated"] or 0
        local retreatingSquad = nil
        for _, s in ipairs(squads(surface)) do
            if s.status == SQUAD_RETREATING then
                retreatingSquad = s
                break
            end
        end
        if not r.retreated and retreated >= 1 and retreatingSquad then
            r.retreated = true
            r.squadId = retreatingSquad.id
            r.retreatStatus = retreatingSquad.status
            print("[PASS] retreat executed (" .. retreated .. "); squad id=" ..
                r.squadId .. " status=RETREATING at " ..
                string.format("%.1f", gameMinutes(tick - state.stagedAt)) .. "min")
            setPhase("reengage")
        end
        if phaseElapsed > PHASE_LIMIT_MINUTES * TICKS_A_MINUTE then
            finish(false, "retreat: no retreating squad within " ..
                PHASE_LIMIT_MINUTES .. " game-minutes (retreated=" .. retreated .. ")")
            return
        end

    elseif state.phase == "reengage" then
        -- The squad state machine must transition RETREATING -> RAIDING
        -- (the "retreat_end" dispatch) — the squad returns to the attack
        -- posture.
        local m = remote.call("Rampant", "getMetricsSummary")
        local c = m and m.counters or {}
        local dispatchCount = c["cmd.DISPATCH_SQUAD"] or 0
        local squad = nil
        for _, s in ipairs(squads(surface)) do
            if s.id == r.squadId then
                squad = s
                break
            end
        end
        if not r.reEngaged and squad and squad.status == SQUAD_RAIDING and dispatchCount >= 1 then
            r.reEngaged = true
            r.statusAfter = squad.status
            r.dispatchCount = dispatchCount
            print("[PASS] squad re-engaged: status RETREATING -> RAIDING at " ..
                string.format("%.1f", gameMinutes(tick - state.stagedAt)) .. "min" ..
                " (cmd.DISPATCH_SQUAD=" .. dispatchCount .. ")")
            finish(true, "all squad-cycle expectations met")
            return
        end
        if phaseElapsed > PHASE_LIMIT_MINUTES * TICKS_A_MINUTE then
            finish(false, "reengage: squad " .. tostring(r.squadId) ..
                " never returned to RAIDING (status=" .. tostring(squad and squad.status) ..
                ", dispatch=" .. dispatchCount .. ")")
            return
        end
    end
end)
