--[[
    Rampant Behavior Scenario: Movement Test (CONSTRUCTED)

    BEHAVIOR:  A formed squad's crossing speed — member count is the
        variable; the placement is a recorded covariate.

    CODE PATHS:
        - Squad.deploySquad (~1222): the FORMATION — chunk center + 1.25
          chunks in the scored direction -> find_non_colliding_position
          ("biter-spawner", ...) -> the group position; set_multi_command
          gathers ungrouped units within 192 tiles of the group.
        - Squad.cleanSquads (~742): group.state == 4 -> squadDispatch
          (the only re-dispatch); commandTick expiry -> wander.
        - Squad.squadDispatch/attackMove (~637): the pathing step.

    CONSTRUCTION MAP:
        (a) system: base forms; the FORMATION (the mod places the group);
            the crossing (the mod dispatches, the vanilla walks).
        (b) simulated: N ungrouped units at the NEST (100,0) — the natural
            units' location — inside the deployment search radius (192)
            regardless of the scored direction.
        (c) hooks: setBaseState(RAIDING) + grantUnitPoints(2000) — stand
            in for a naturally-RAIDING base with accumulated points.

    COVARIATES (recorded, never assumed):
        - the group's formation position (the scored direction varies)
        - the gathering distance (group vs the unit spawn area)
        - the actual formed members (natural spawns pollute N)

    VARIABLE:  the formed squad's member count (small vs large).

    EXPECTED:  crossing (formation -> wall arrival) < 1 game-min for
        both; wander == 0; reachedState4 >= 3.

    FAILURE SIG (triple):
        (i)  mod bug: clean formation (small gathering distance) but slow
             crossing + wander > 0 -> the state-4 gate stalls.
        (ii) scenario: no squad forms (units outside the search radius).
        (iii) covariate: large gathering distance / far formation
             position explains the slowness — NOT the member count.
]]

local WALL_POS = { x = 0, y = 0 }
local SPAWNER_POS = { x = 100, y = 0 }
local TURRET_POS = { x = 6, y = 0 }
local NEST_SPAWN_POS = { x = 100, y = 0 }
local WALL_PERIMETER = 30
local TICKS_A_MINUTE = 60 * 60
local BUDGET_MINUTES = 40
local BUDGET_TICKS = BUDGET_MINUTES * TICKS_A_MINUTE
local PHASE_LIMIT_MINUTES = 15
local BASE_AI_STATE_RAIDING = 4
local SAMPLE_INTERVAL = 5 * 60   -- every 5 game-seconds
local N1_CROSSING_LIMIT_MIN = 5
local N12_CROSSING_LIMIT_MIN = 10

local state  -- { phase, results, stagedAt, phaseStartedAt, done }

local function rampReady()
    local ready = remote.call("Rampant", "isInitialized")
    return ready == true
end

local function gameMinutes(tick)
    return tick / TICKS_A_MINUTE
end

local function squads(surface)
    return remote.call("Rampant", "getEnemyGroups", surface) or {}
end

local function counters()
    local m = remote.call("Rampant", "getMetricsSummary")
    return (m and m.counters) or {}
end

local function counterDelta(name, before, after)
    return (after[name] or 0) - (before[name] or 0)
end

local function snapshotCounters()
    local c = counters()
    return {
        reachedState4 = c["squad.reachedState4"] or 0,
        wander = c["squad.wander"] or 0,
        wanderDestroy = c["squad.wanderDestroy"] or 0,
        squadDestroyed = c["squad.destroyed"] or 0,
    }
end

local function stage(surface)
    -- Clean the map: destroy natural enemy structures/units. STRUCTURES
    -- must die() — destroy() fires no death event, so the mod's registries
    -- (chunkToNests/chunkToActiveNest) keep the destroyed nests' chunks
    -- and the base keeps forming squads in the dead chunks (no units).
    -- die() runs the mod's own unregister chain (a system-performed step).
    local natural = surface.find_entities_filtered{
        force = "enemy",
        type = { "unit-spawner", "turret", "unit" },
    }
    for _, e in ipairs(natural) do
        if e.valid then
            if e.type == "unit" then
                e.destroy()
            else
                e.die("enemy")
            end
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
    remote.call("Rampant", "drawField", "player", true)
    remote.call("Rampant", "drawField", "base", true)
end

-- (b) simulated: the units spawn in the CLEAR nest chunk (96,0 — the
-- corridor analysis: 0 water tiles) on DISTINCT positions (the old
-- (i%3)*2 pattern repeated every 3 units — overlapping spawns blocked the
-- centroid fit and the deployment fell back into water pockets).
local function spawnUnits(surface, n)
    local spawned = 0
    for i = 1, n do
        local unit = surface.create_entity{
            name = "small-biter",
            position = { x = 96 + (i % 4) * 2, y = 2 + math.floor(i / 4) * 2 },
            force = "enemy",
        }
        if unit and unit.valid then
            spawned = spawned + 1
        end
    end
    return spawned
end

local function setPhase(phase)
    state.phase = phase
    state.phaseStartedAt = game.tick
    print("PHASE: " .. phase .. " at " ..
        string.format("%.1f", gameMinutes(game.tick - state.stagedAt)) .. "min")
end

local function finish(passed, reason)
    local r = state.results
    print("--- Behavior Scenario: Movement Test ---")
    print("Passed: " .. (passed and 1 or 0))
    print("Failed: " .. (passed and 0 or 1))
    if reason then
        print("Detail: " .. reason)
    end
    local function crossing(name)
        local x = r[name]
        if not x then return name .. "=none" end
        local crossingMin = x.crossingMin
        if crossingMin then
            crossingMin = string.format("%.2f", crossingMin) .. "min"
        else
            crossingMin = "none"
        end
        return name .. "=" .. crossingMin ..
            " members=" .. tostring(x.members) ..
            " formPos=(" .. tostring(x.formX) .. "," .. tostring(x.formY) .. ")" ..
            " gather=" .. tostring(x.gatherDist) ..
            " s4=" .. tostring(x.deltaState4) ..
            " wander=" .. tostring(x.deltaWander)
    end
    print("Detail: " .. crossing("n1") .. " | " .. crossing("n12"))
    print("Execution time: " .. game.tick .. " ticks")
    if passed then
        print("TESTS_COMPLETE")
    else
        print("TESTS_FAILED")
    end
    state.done = true
    game.speed = 1
end

--- Track a crossing: sample every SAMPLE_INTERVAL until the wall arrival
--- or the limit. Returns { arrived=true } / { arrived=false } / nil (gone).
local function trackCrossing(surface, r, tick, targetId)
    local s = nil
    for _, sq in ipairs(squads(surface)) do
        if sq.id == targetId then
            s = sq
            break
        end
    end
    if not s then
        return nil
    end
    if tick - (r.lastSampleTick or 0) >= SAMPLE_INTERVAL then
        r.lastSampleTick = tick
        r.samples[#r.samples + 1] = {
            t = gameMinutes(tick - state.stagedAt),
            x = s.position.x,
            y = s.position.y,
            gs = s.state,
            st = s.status,
        }
        local last = r.samples[#r.samples]
        print("[MOV] t=" .. string.format("%.1f", last.t) ..
            " pos=(" .. string.format("%.0f", last.x) .. "," ..
            string.format("%.0f", last.y) .. ")" ..
            " gs=" .. last.gs .. " st=" .. last.st)
    end
    local dx = s.position.x - WALL_POS.x
    local dy = s.position.y - WALL_POS.y
    if (dx * dx + dy * dy) < WALL_PERIMETER * WALL_PERIMETER then
        return { arrived = true }
    end
    return { arrived = false }
end

--- Record the formation covariates: the group position, the gathering
--- distance to the unit spawn area, the actual members.
local function recordFormation(surface, r, squad, tag)
    local rec = r[tag]
    rec.id = squad.id
    rec.members = squad.memberCount
    rec.formX = math.floor(squad.position.x)
    rec.formY = math.floor(squad.position.y)
    local dx = squad.position.x - NEST_SPAWN_POS.x
    local dy = squad.position.y - NEST_SPAWN_POS.y
    rec.gatherDist = math.floor(math.sqrt(dx * dx + dy * dy))
    rec.before = snapshotCounters()
    rec.startMin = gameMinutes(game.tick - state.stagedAt)
    r.samples = {}
    r.lastSampleTick = 0
    print("[INFO] " .. tag .. " squad formed: id=" .. rec.id ..
        " members=" .. rec.members ..
        " formPos=(" .. rec.formX .. "," .. rec.formY .. ")" ..
        " gather=" .. rec.gatherDist .. " at " ..
        string.format("%.1f", rec.startMin) .. "min")
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
        -- Upgrade-settle wait: the natural vanilla spawners get upgraded
        -- (destroy + recreate + re-register) — the stage must destroy ALL
        -- the natural nests (die() -> the mod's unregister), so wait for
        -- the spawner count to stabilize first.
        local spawnerCount = #game.surfaces[1].find_entities_filtered{ force = "enemy", type = "unit-spawner" }
        if spawnerCount ~= storage.rampantLastSpawnerCount then
            storage.rampantLastSpawnerCount = spawnerCount
            storage.rampantSpawnerIdle = 0
        else
            storage.rampantSpawnerIdle = (storage.rampantSpawnerIdle or 0) + 1
        end
        if (storage.rampantSpawnerIdle or 0) < 60 then
            return  -- upgrades still settling
        end
        print("[INFO] upgrade pipeline settled (spawners=" .. spawnerCount .. ")")
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
            -- (c) hooks: stand in for a naturally-RAIDING base with points.
            remote.call("Rampant", "setBaseState", base.id, BASE_AI_STATE_RAIDING)
            remote.call("Rampant", "grantUnitPoints", base.id, 2000)
            print("[INFO] constructed: RAIDING + 2000 points")
            -- (b) simulated: N=1 unit at the nest.
            r.n1 = { spawned = spawnUnits(surface, 1) }
            print("[INFO] N=1: spawned " .. r.n1.spawned .. " unit at the nest")
            setPhase("n1_wait")
        elseif phaseElapsed > PHASE_LIMIT_MINUTES * TICKS_A_MINUTE then
            finish(false, "base never formed within " .. PHASE_LIMIT_MINUTES .. " game-minutes")
            return
        end

    elseif state.phase == "n1_wait" then
        -- The FORMATION is (a) system-performed: wait for the mod to form
        -- a squad (the mod places the group wherever the scoring says).
        local formed = nil
        for _, s in ipairs(squads(surface)) do
            if not s.isBuilder and s.memberCount and s.memberCount >= 1 then
                formed = s
                break
            end
        end
        if formed then
            recordFormation(surface, r, formed, "n1")
            setPhase("n1_cross")
        elseif phaseElapsed > 2 * TICKS_A_MINUTE then
            finish(false, "N=1: no squad formed within 2 game-minutes (FAILURE SIG ii: construction)")
            return
        end

    elseif state.phase == "n1_cross" then
        local res = trackCrossing(surface, r, tick, r.n1.id)
        if res and res.arrived then
            r.n1.crossingMin = gameMinutes(tick - state.stagedAt) - r.n1.startMin
            r.n1.after = snapshotCounters()
            r.n1.deltaState4 = counterDelta("squad.reachedState4", r.n1.before, r.n1.after)
            r.n1.deltaWander = counterDelta("squad.wander", r.n1.before, r.n1.after)
            print("[RESULT] N=1 crossing: " .. string.format("%.2f", r.n1.crossingMin) ..
                "min members=" .. r.n1.members .. " gather=" .. r.n1.gatherDist ..
                " s4=" .. r.n1.deltaState4 .. " wander=" .. r.n1.deltaWander)
            r.n12 = { spawned = spawnUnits(surface, 12) }
            print("[INFO] N=12: spawned " .. r.n12.spawned .. " units at the nest")
            setPhase("n12_wait")
        elseif res == nil then
            finish(false, "N=1: squad " .. tostring(r.n1.id) .. " vanished mid-crossing" ..
                " (FAILURE SIG: destroyed at " .. string.format("%.1f", gameMinutes(tick - state.stagedAt)) .. "min)")
            return
        elseif phaseElapsed > N1_CROSSING_LIMIT_MIN * TICKS_A_MINUTE then
            r.n1.crossingMin = N1_CROSSING_LIMIT_MIN
            r.n1.timedOut = true
            r.n1.after = snapshotCounters()
            r.n1.deltaState4 = counterDelta("squad.reachedState4", r.n1.before, r.n1.after)
            r.n1.deltaWander = counterDelta("squad.wander", r.n1.before, r.n1.after)
            print("[RESULT] N=1 crossing TIMED OUT at " .. N1_CROSSING_LIMIT_MIN .. "min" ..
                " members=" .. r.n1.members .. " gather=" .. r.n1.gatherDist ..
                " s4=" .. r.n1.deltaState4 .. " wander=" .. r.n1.deltaWander)
            r.n12 = { spawned = spawnUnits(surface, 12) }
            print("[INFO] N=12: spawned " .. r.n12.spawned .. " units at the nest")
            setPhase("n12_wait")
        end

    elseif state.phase == "n12_wait" then
        -- The NEXT formation: a NEW squad (id != the N=1 squad's) with
        -- >= 4 members. The formation SPLITS the units into 2-5 member
        -- squads (the mod's set_multi_command gathers a few at a time) —
        -- the "large" squad is the biggest formed (~5 members).
        local formed = nil
        for _, s in ipairs(squads(surface)) do
            if not s.isBuilder and s.id ~= r.n1.id and s.memberCount and s.memberCount >= 4 then
                formed = s
                break
            end
        end
        if formed then
            recordFormation(surface, r, formed, "n12")
            setPhase("n12_cross")
        elseif phaseElapsed > 3 * TICKS_A_MINUTE then
            local c = counters()
            local active = remote.call("Rampant", "getActiveStructures", surface)
            local parts = {}
            for _, s in ipairs(active or {}) do
                parts[#parts + 1] = s.type .. "@(" .. s.x .. "," .. s.y .. ")x" .. s.count
            end
            local squadList = {}
            for _, s in ipairs(squads(surface)) do
                squadList[#squadList + 1] = "id=" .. s.id .. "m" .. s.memberCount
            end
            local c96 = remote.call("Rampant", "queryChunk", { x = 96, y = -32 }, surface)
            finish(false, "N=12: no squad formed within 3 game-minutes" ..
                " (deploy.centroid=" .. tostring(c["squad.deploy.centroid"]) ..
                " centroidFailed=" .. tostring(c["squad.deploy.centroidFailed"]) ..
                " formAttempt=" .. tostring(c["squad.formAttempt"]) ..
                " reject.pheromone=" .. tostring(c["squad.formReject.attacker.pheromone"]) ..
                " registered=" .. table.concat(parts, " ") ..
                " squads=" .. table.concat(squadList, ",") ..
                " chunk(96,-32)=" .. tostring(c96 and (c96.passable .. "w" .. c96.waterTiles .. "n" .. c96.nestCount)))
            return
        end

    elseif state.phase == "n12_cross" then
        local res = trackCrossing(surface, r, tick, r.n12.id)
        if res and res.arrived then
            r.n12.crossingMin = gameMinutes(tick - state.stagedAt) - r.n12.startMin
            r.n12.after = snapshotCounters()
            r.n12.deltaState4 = counterDelta("squad.reachedState4", r.n12.before, r.n12.after)
            r.n12.deltaWander = counterDelta("squad.wander", r.n12.before, r.n12.after)
            print("[RESULT] N=12 crossing: " .. string.format("%.2f", r.n12.crossingMin) ..
                "min members=" .. r.n12.members .. " gather=" .. r.n12.gatherDist ..
                " s4=" .. r.n12.deltaState4 .. " wander=" .. r.n12.deltaWander)
            -- Verdict: EXPECTED vs the covariates.
            local ok = true
            local problems = {}
            if (r.n1.crossingMin or 99) >= 1 then
                ok = false
                problems[#problems + 1] = "N=1 crossing " ..
                    string.format("%.2f", r.n1.crossingMin or 0) .. "min >= 1min"
            end
            if (r.n12.crossingMin or 99) >= 1 then
                ok = false
                problems[#problems + 1] = "N=12 crossing " ..
                    string.format("%.2f", r.n12.crossingMin) .. "min >= 1min"
            end
            if (r.n12.deltaWander or 0) > 0 then
                ok = false
                problems[#problems + 1] = "N=12 wander=" .. r.n12.deltaWander .. " > 0"
            end
            if ok then
                finish(true, "movement meets the requirement: both crossings < 1 game-min, wander=0")
            else
                finish(false, "movement requirement not met: " .. table.concat(problems, "; ") ..
                    " | covariates: n1 gather=" .. tostring(r.n1.gatherDist) ..
                    " n12 gather=" .. tostring(r.n12.gatherDist))
            end
            return
        elseif res == nil then
            local c = counters()
            finish(false, "N=12: squad " .. tostring(r.n12.id) .. " vanished mid-crossing" ..
                " (deploy.centroid=" .. tostring(c["squad.deploy.centroid"]) ..
                " centroidFailed=" .. tostring(c["squad.deploy.centroidFailed"]) ..
                " fallback=" .. tostring(c["squad.deploy.fallback"]) ..
                " wanderDestroy=" .. tostring(c["squad.wanderDestroy"]) ..
                " squadDestroyed=" .. tostring(c["squad.destroyed"]) .. ")")
            return
        elseif phaseElapsed > N12_CROSSING_LIMIT_MIN * TICKS_A_MINUTE then
            r.n12.crossingMin = N12_CROSSING_LIMIT_MIN
            r.n12.timedOut = true
            r.n12.after = snapshotCounters()
            r.n12.deltaState4 = counterDelta("squad.reachedState4", r.n12.before, r.n12.after)
            r.n12.deltaWander = counterDelta("squad.wander", r.n12.before, r.n12.after)
            finish(false, "N=12 timed out: " .. string.format("%.2f", r.n12.crossingMin) ..
                "min members=" .. r.n12.members .. " gather=" .. r.n12.gatherDist ..
                " s4=" .. r.n12.deltaState4 .. " wander=" .. r.n12.deltaWander)
            return
        end
    end
end)
