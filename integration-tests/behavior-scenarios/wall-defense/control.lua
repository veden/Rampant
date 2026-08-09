--[[
    Rampant Behavior Scenario: Wall Defense

    Stages a minimal conflict and asserts the AI develops it end-to-end:

      Stage:
        - player stone-wall ring at (0,0)   -> emits player-base pheromone
        - enemy biter-spawner at (100,0)    -> triggers onBuild -> base creation
        - pollution at the spawner          -> activates the nest

      Advance: game.speed = 100, run until expectations met or budget.

      Expectations (game time):
        [5 min]  base formed at the spawner
        [15 min] at least one enemy squad formed
        [30 min] enemy units reached the wall perimeter
        [45 min] wall damaged or destroyed

    Early-pass as soon as all expectations are met. Prints structured
    results + TESTS_COMPLETE/TESTS_FAILED to stdout (runner contract).
]]

local WALL_POS = { x = 0, y = 0 }
local SPAWNER_POS = { x = 100, y = 0 }
local WALL_PERIMETER = 30
local BUDGET_MINUTES = 90
local TICKS_A_MINUTE = 60 * 60
local BUDGET_TICKS = BUDGET_MINUTES * TICKS_A_MINUTE
local POLLUTE_INTERVAL = 600  -- ticks between re-pollution at the spawner

local state  -- nil until staged

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
    -- Clean the map: destroy all natural enemy structures and units so the
    -- scenario is deterministic (single base, single nest). Keep only the
    -- spawner we place next.
    local natural = surface.find_entities_filtered{
        force = "enemy",
        type = { "unit-spawner", "turret", "unit" },
    }
    for _, e in ipairs(natural) do
        if e.valid then
            e.destroy()
        end
    end

    -- Player wall ring (force "player"; raise_built fires on_built_entity so
    -- Rampant accounts the buildings and emits player-base pheromone).
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
    -- Enemy spawner (fires on_built_entity/on_biter_base_built -> base creation).
    surface.create_entity{
        name = "biter-spawner",
        position = SPAWNER_POS,
        force = "enemy",
        raise_built = true,
    }
    -- Pollution activates the nest (processNestActiveness).
    surface.pollute(SPAWNER_POS, 50)
end

local function checkExpectations()
    local r = state.results
    local tick = game.tick
    local surface = game.surfaces[1]

    if not r.baseFormed then
        local base = remote.call("Rampant", "queryBaseAt", SPAWNER_POS, surface)
        if base then
            r.baseFormed = true
            r.baseId = base.id
            print("[PASS] base formed at spawner (id=" .. base.id .. ") at " ..
                string.format("%.1f", gameMinutes(tick)) .. "min")
        end
    end

    if not r.squadFormed then
        local groups = remote.call("Rampant", "getEnemyGroups", surface)
        if groups and #groups > 0 then
            r.squadFormed = true
            r.squadCount = #groups
            r.squadInfo = {}
            for i, g in ipairs(groups) do
                table.insert(r.squadInfo, "id=" .. g.id .. " builder=" .. tostring(g.isBuilder) ..
                    " members=" .. g.memberCount)
            end
            print("[PASS] " .. #groups .. " enemy squad(s) formed at " ..
                string.format("%.1f", gameMinutes(tick)) .. "min: " .. table.concat(r.squadInfo, "; "))
        end
    end

    if not r.unitsAtWall then
        local units = surface.find_entities_filtered{
            force = "enemy",
            type = "unit",
            area = {
                { WALL_POS.x - WALL_PERIMETER, WALL_POS.y - WALL_PERIMETER },
                { WALL_POS.x + WALL_PERIMETER, WALL_POS.y + WALL_PERIMETER },
            },
        }
        if #units > 0 then
            r.unitsAtWall = true
            r.unitCountAtWall = #units
            print("[PASS] " .. #units .. " enemy unit(s) reached wall perimeter at " ..
                string.format("%.1f", gameMinutes(tick)) .. "min")
        end
    end

    if not r.wallDamaged then
        local walls = surface.find_entities_filtered{ name = "stone-wall", force = "player" }
        local alive = 0
        local damaged = 0
        for _, w in ipairs(walls) do
            if w.valid then
                alive = alive + 1
                if w.health < w.max_health then
                    damaged = damaged + 1
                end
            end
        end
        if alive < 4 or damaged > 0 then
            r.wallDamaged = true
            print("[PASS] wall damaged/destroyed (" .. alive .. " alive, " .. damaged ..
                " damaged) at " .. string.format("%.1f", gameMinutes(tick)) .. "min")
        end
    end

    return r.baseFormed and r.squadFormed and r.unitsAtWall and r.wallDamaged
end

local function diagnostics()
    local surface = game.surfaces[1]
    local wallP = remote.call("Rampant", "queryPheromones", WALL_POS, surface)
    local spawnerP = remote.call("Rampant", "queryPheromones", SPAWNER_POS, surface)
    local groups = remote.call("Rampant", "getEnemyGroups", surface)
    local lines = {}
    if wallP then
        table.insert(lines, "wall pheromone: player=" .. string.format("%.2f", wallP.player or 0) ..
            " base=" .. string.format("%.2f", wallP.base or 0))
    else
        table.insert(lines, "wall pheromone: nil (no chunk)")
    end
    if spawnerP then
        table.insert(lines, "spawner pheromone: player=" .. string.format("%.2f", spawnerP.player or 0) ..
            " base=" .. string.format("%.2f", spawnerP.base or 0) ..
            " enemy=" .. string.format("%.2f", spawnerP.enemy or 0))
    end
    if groups then
        for i, g in ipairs(groups) do
            table.insert(lines, "squad[" .. i .. "] id=" .. g.id ..
                " builder=" .. tostring(g.isBuilder) ..
                " members=" .. g.memberCount ..
                " pos=(" .. string.format("%.0f", g.position.x) .. "," .. string.format("%.0f", g.position.y) .. ")")
        end
    else
        table.insert(lines, "no squads")
    end
    return table.concat(lines, " | ")
end

local function finish(passed, reason)
    local r = state.results
    print("--- Behavior Scenario: Wall Defense ---")
    print("Passed: " .. (passed and 1 or 0))
    print("Failed: " .. (passed and 0 or 1))
    if reason then
        print("Detail: " .. reason)
    end
    print("Detail: " .. diagnostics())
    print("Detail: base=" .. tostring(r.baseFormed) ..
        " squad=" .. tostring(r.squadFormed) ..
        " unitsAtWall=" .. tostring(r.unitsAtWall) ..
        " wallDamaged=" .. tostring(r.wallDamaged) ..
        " ticks=" .. game.tick)
    -- Movement gate counters (the crossing-speed analysis)
    local m = remote.call("Rampant", "getMetricsSummary")
    local c = m and m.counters or {}
    print("Detail: reachedState4=" .. tostring(c["squad.reachedState4"]) ..
        " wander=" .. tostring(c["squad.wander"]) ..
        " wanderDestroy=" .. tostring(c["squad.wanderDestroy"]) ..
        " dispatch=" .. tostring(c["cmd.DISPATCH_SQUAD"]))
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
        state = { results = {}, stagedAt = game.tick, done = false }
        stage(game.surfaces[1])
        game.speed = 100
        print("STAGED: wall ring at (0,0), spawner at (100,0), pollution at spawner, speed=100")
        return
    end
    if state.done then
        return
    end

    if game.tick % POLLUTE_INTERVAL == 0 then
        game.surfaces[1].pollute(SPAWNER_POS, 20)
    end

    local r = state.results

    -- Movement telemetry: track the LARGEST non-builder squad (multi-member
    -- groups are the slow ones — the group-state-4 gate waits for the
    -- slowest unit). Re-target when the tracked squad vanishes.
    if r.squadFormed then
        local groups = remote.call("Rampant", "getEnemyGroups", game.surfaces[1])
        if groups then
            local trackedStillAlive = false
            if r.movementTracked then
                for _, g in ipairs(groups) do
                    if g.id == r.trackedId then
                        trackedStillAlive = true
                        break
                    end
                end
            end
            if not trackedStillAlive then
                -- (Re-)target the largest non-builder squad.
                local best = nil
                local bestMembers = 0
                for _, g in ipairs(groups) do
                    if not g.isBuilder and g.memberCount and g.memberCount > bestMembers then
                        best = g
                        bestMembers = g.memberCount
                    end
                end
                if best then
                    if r.trackedId and r.trackedId ~= best.id then
                        print("[MOV] re-targeting: squad " .. r.trackedId ..
                            " gone -> tracking id=" .. best.id .. " members=" .. best.memberCount)
                    end
                    r.movementTracked = true
                    r.trackedId = best.id
                    r.trackedMembers = best.memberCount
                    if not r.movSamples then
                        r.movSamples = {}
                        r.movLastSample = 0
                    end
                end
            end
        end
    end
    if r.movementTracked and (game.tick - (r.movLastSample or 0)) >= 30 * 60 then
        r.movLastSample = game.tick
        local groups = remote.call("Rampant", "getEnemyGroups", game.surfaces[1])
        local found = false
        if groups then
            for _, g in ipairs(groups) do
                if g.id == r.trackedId then
                    found = true
                    r.movSamples[#r.movSamples + 1] = {
                        t = gameMinutes(game.tick),
                        x = g.position.x,
                        y = g.position.y,
                        gs = g.state,
                        st = g.status,
                    }
                    local last = r.movSamples[#r.movSamples]
                    print("[MOV] t=" .. string.format("%.1f", last.t) ..
                        " pos=(" .. string.format("%.0f", last.x) .. "," ..
                        string.format("%.0f", last.y) .. ")" ..
                        " gs=" .. last.gs .. " st=" .. last.st)
                    break
                end
            end
        end
        if not found then
            r.movSamples[#r.movSamples + 1] = { t = gameMinutes(game.tick), gone = true }
            print("[MOV] t=" .. string.format("%.1f", r.movSamples[#r.movSamples].t) .. " squad gone")
        end
    end

    local elapsed = game.tick - state.stagedAt
    if elapsed > BUDGET_TICKS then
        finish(false, "budget " .. BUDGET_MINUTES .. " game-minutes elapsed")
        return
    end
    if checkExpectations() then
        finish(true, "all expectations met")
    end
end)
