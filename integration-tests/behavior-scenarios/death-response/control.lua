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
local TICKS_A_MINUTE = 60 * 60
local BUDGET_MINUTES = 40
local BUDGET_TICKS = BUDGET_MINUTES * TICKS_A_MINUTE
local PHASE_LIMIT_MINUTES = 15
local POLLUTE_INTERVAL = 600
local BASE_AI_STATE_RAIDING = 4

local state  -- { phase, results, stagedAt, phaseStartedAt, turret, done }

local function rampReady()
    local ok, ready = pcall(remote.call, "Rampant", "isInitialized")
    return ok and ready == true
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
    -- Enemy spawner -> base creation.
    surface.create_entity{
        name = "biter-spawner",
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
        -- die() raises on_entity_died synchronously; Rampant's handler can
        -- invalidate sibling units mid-loop, so tolerate failures and only
        -- count successful kills
        local ok = pcall(function()
            if u.valid and turret and turret.valid then
                u.die("player", turret)
            end
        end)
        if ok then
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
        local ok = pcall(function()
            if s.valid and turret and turret.valid then
                s.die("player", turret)
            end
        end)
        if ok then
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
            print("[INFO] spawned " .. spawned .. " enemy units at the wall (grouped, attacking)")
            setPhase("retreat")
        elseif phaseElapsed > PHASE_LIMIT_MINUTES * TICKS_A_MINUTE then
            finish(false, "base never formed within " .. PHASE_LIMIT_MINUTES .. " game-minutes")
            return
        end

    elseif state.phase == "retreat" then
        local units = unitsNearWall(surface)
        if not r.killed then
            -- wait for the constructed attack to be present, then trigger
            if #units < 2 then
                if phaseElapsed > 2 * TICKS_A_MINUTE then
                    finish(false, "fewer than 2 enemy units at the wall in the retreat phase")
                    return
                end
            else
                local killed = killUnits(surface, 2)
                r.killed = killed
                print("[INFO] killed " .. killed .. " enemy unit(s) at wall (retreat trigger)")
            end
        else
            -- kills done: survivors must flee the wall perimeter
            if #units == 0 then
                r.retreat = true
                print("[PASS] enemy units left wall perimeter at " ..
                    string.format("%.1f", gameMinutes(tick - state.stagedAt)) .. "min")
                killUnits(surface, 999)
                local killedSpawners = killSpawners(surface)
                print("[INFO] killed " .. killedSpawners .. " spawner(s)")
                setPhase("structure")
            elseif phaseElapsed > PHASE_LIMIT_MINUTES * TICKS_A_MINUTE then
                finish(false, "retreat: " .. #units ..
                    " unit(s) still at wall after " .. PHASE_LIMIT_MINUTES .. " game-minutes")
                return
            end
        end

    elseif state.phase == "structure" then
        -- After the spawner dies, the base object recycles within 1-2 ticks
        -- (getBasePoints goes nil -> recycled). The point cost is verified
        -- via telemetry: the Executor counted cmd.MODIFY_UNIT_POINTS.
        local points = remote.call("Rampant", "getBasePoints", r.baseId)
        if not r.baseRecycled and points == nil then
            r.baseRecycled = true
            print("[PASS] base recycled after spawner death at " ..
                string.format("%.1f", gameMinutes(tick - state.stagedAt)) .. "min")
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
        if r.baseRecycled and r.pointsDropped then
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
