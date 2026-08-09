--[[
    Rampant Behavior Scenario: Player Death Side (CONSTRUCTED)

    Verifies what happens when the ENEMY destroys PLAYER structures:
      1. Victory scent: the destroyed chunk's death generator goes
         POSITIVE (repulsion for the attackers, retreat pressure)
      2. Kill credit: the enemy base is credited for the kills
         (destroyPlayerBuildings -> temperament input)
      3. Drain-pylon mechanics are out of scope here (separate mechanic)

      setup    - wall ring + turrets + radars at origin (no ammo — the
                 enemy destroys them); spawner at (100,0); army grouped
                 attacking the origin; constructed RAIDING + points
      attack   - wait: base formed, units at origin, a player structure
                 destroyed by the enemy
      victory  - assert deathGenerator > 0 at the origin (victory scent)
                 and base.destroyPlayerBuildings > 0 (credit tracked)
]]

local ORIGIN = { x = 0, y = 0 }
local SPAWNER_POS = { x = 100, y = 0 }
local WALL_PERIMETER = 30
local TICKS_A_MINUTE = 60 * 60
local BUDGET_MINUTES = 40
local BUDGET_TICKS = BUDGET_MINUTES * TICKS_A_MINUTE
local PHASE_LIMIT_MINUTES = 20
local BASE_AI_STATE_RAIDING = 4

local state

local function rampReady()
    -- No pcall: the interface is registered at mod load; a missing
    -- interface means the mod is not enabled and the run must fail loudly.
    local ready = remote.call("Rampant", "isInitialized")
    return ready == true
end

local function gameMinutes(tick)
    return tick / TICKS_A_MINUTE
end

local function unitsAtOrigin(surface)
    return surface.find_entities_filtered{
        force = "enemy",
        type = "unit",
        area = {
            { ORIGIN.x - WALL_PERIMETER, ORIGIN.y - WALL_PERIMETER },
            { ORIGIN.x + WALL_PERIMETER, ORIGIN.y + WALL_PERIMETER },
        },
    }
end

local function playerStructureCount(surface)
    -- NOTE: the `type` filter does not match gun-turrets on 2.1.x (returns
    -- only radars); use explicit names
    return #surface.find_entities_filtered{
        force = "player",
        name = { "gun-turret", "radar" },
    }
end

local function stage(surface)
    local natural = surface.find_entities_filtered{
        force = "enemy",
        type = { "unit-spawner", "turret", "unit" },
    }
    for _, e in ipairs(natural) do
        if e.valid then e.destroy() end
    end
    -- Player structures the enemy will destroy (no ammo — biters chew them)
    for dx = -1, 1 do
        for dy = -1, 1 do
            if math.abs(dx) + math.abs(dy) == 1 then
                surface.create_entity{
                    name = "stone-wall",
                    position = { x = ORIGIN.x + dx * 2, y = ORIGIN.y + dy * 2 },
                    force = "player", raise_built = true,
                }
            end
        end
    end
    local t1 = surface.create_entity{ name = "gun-turret", position = { x = 6, y = 0 }, force = "player", raise_built = true }
    local t2 = surface.create_entity{ name = "gun-turret", position = { x = -6, y = 0 }, force = "player", raise_built = true }
    local r1 = surface.create_entity{ name = "radar", position = { x = 4, y = 4 }, force = "player", raise_built = true }
    local r2 = surface.create_entity{ name = "radar", position = { x = -4, y = -4 }, force = "player", raise_built = true }
    print("STAGE structures: turrets=" .. ((t1 and t1.valid and 1 or 0) + (t2 and t2.valid and 1 or 0)) ..
        " radars=" .. ((r1 and r1.valid and 1 or 0) + (r2 and r2.valid and 1 or 0)))
    -- Enemy spawner + pollution
    surface.create_entity{ name = "biter-spawner", position = SPAWNER_POS, force = "enemy", raise_built = true }
    surface.pollute(SPAWNER_POS, 50)
    -- Army (grouped, attacking the origin)
    local group = surface.create_unit_group{ position = { x = 10, y = 0 }, force = "enemy" }
    local spawned = 0
    for i = 1, 12 do
        local unit = surface.create_entity{
            name = "small-biter",
            position = { x = 10 + (i % 4) * 2, y = math.floor(i / 4) * 2 },
            force = "enemy",
        }
        if unit and unit.valid then
            group.add_member(unit)
            spawned = spawned + 1
        end
    end
    if group.valid then
        group.set_command{ type = defines.command.attack_area, destination = ORIGIN, radius = 10 }
    end
    print("STAGED: player base at origin, spawner at (100,0), " .. spawned .. " army units")
end

local function setPhase(phase)
    state.phase = phase
    state.phaseStartedAt = game.tick
    print("PHASE: " .. phase .. " at " ..
        string.format("%.1f", gameMinutes(game.tick - state.stagedAt)) .. "min")
end

local function finish(passed, reason)
    local r = state.results
    print("--- Behavior Scenario: Player Death Side ---")
    print("Passed: " .. (passed and 1 or 0))
    print("Failed: " .. (passed and 0 or 1))
    if reason then print("Detail: " .. reason) end
    print("Detail: base=" .. tostring(r.baseFormed) ..
        " destroyed=" .. tostring(r.structureDestroyed) ..
        " victoryScent=" .. tostring(r.victoryScent) ..
        " credit=" .. tostring(r.credit) ..
        " destroyedBuildings=" .. tostring(r.destroyedBuildings) ..
        " deathGen=" .. tostring(r.deathGen) ..
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
    if not rampReady() then return end
    if not state then
        -- Stage AFTER the mod's initial chunk scan completes: the scan
        -- processes chunks one per few ticks and its unit purge destroys
        -- units in scanned chunks. Signal: the scan counter stops climbing
        -- (30 consecutive ticks with no new scan = the pending queue
        -- drained; the surface chunk count is not a reliable target — not
        -- all counted chunks are queued for scanning).
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
        return
    end
    if state.done then return end

    local surface = game.surfaces[1]
    local r = state.results
    local tick = game.tick
    local phaseElapsed = tick - state.phaseStartedAt

    if tick - state.stagedAt > BUDGET_TICKS then
        finish(false, "budget " .. BUDGET_MINUTES .. " game-minutes elapsed")
        return
    end

    -- Watcher: print entity counts for the first 6000 ticks
    if tick - state.stagedAt < 3000 and tick % 100 == 0 then
        local m = remote.call("Rampant", "getMetricsSummary")
        local c = m and m.counters or {}
        print("[WATCH] tick=" .. (tick - state.stagedAt) .. " scans=" .. tostring(c["scan.unitsKilled"]))
    end
    if tick - state.stagedAt < 6000 and tick % 600 == 0 then
        local t = #surface.find_entities_filtered{ force = "player", name = "gun-turret" }
        local w = #surface.find_entities_filtered{ force = "player", name = "stone-wall" }
        local u = #surface.find_entities_filtered{ force = "enemy", type = "unit" }
        print("[WATCH] tick=" .. (tick - state.stagedAt) .. " turrets=" .. t .. " walls=" .. w .. " enemyUnits=" .. u)
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
            print("[INFO] constructed RAIDING=" .. tostring(stateOk) .. " points=" .. tostring(pointsOk))
            local now = playerStructureCount(surface)
            local units = unitsAtOrigin(surface)
            local all = surface.find_entities_filtered{ force = "player", type = { "turret", "radar", "stone-wall" } }
            local desc = {}
            for _, e in ipairs(all) do
                desc[#desc + 1] = e.name .. "@(" .. math.floor(e.position.x) .. "," .. math.floor(e.position.y) .. ")"
            end
            print("[INFO] post-construct: structures=" .. now .. " unitsAtOrigin=" .. #units ..
                " playerEntities=" .. #all .. " [" .. table.concat(desc, " ") .. "]")
            setPhase("attack")
        elseif phaseElapsed > PHASE_LIMIT_MINUTES * TICKS_A_MINUTE then
            finish(false, "base never formed")
            return
        end

    elseif state.phase == "attack" then
        -- Re-issue the attack command periodically: unit groups idle when
        -- their command completes, and the movement cadence stalls without
        -- a fresh push
        if tick % 600 == 0 then
            local units = surface.find_entities_filtered{ force = "enemy", type = "unit" }
            for _, u in ipairs(units) do
                if u.commandable then
                    local g = u.commandable.parent_group
                    if g and g.valid then
                        g.set_command{ type = defines.command.attack_area, destination = ORIGIN, radius = 10 }
                    end
                end
            end
        end
        -- The army destroys player structures; note the survivors then
        -- RETREAT (victory scent repels them) — that is intended behavior,
        -- so the trigger is the destruction alone, not units present.
        local remaining = playerStructureCount(surface)
        local initial = state.initialStructures or 4
        state.initialStructures = initial
        if remaining < initial then
            r.structureDestroyed = true
            r.initialStructures = initial
            r.remainingStructures = remaining
            print("[PASS] enemy destroyed a player structure (" .. remaining ..
                " of " .. initial .. " remain) at " ..
                string.format("%.1f", gameMinutes(tick - state.stagedAt)) .. "min")
            local m = remote.call("Rampant", "getMetricsSummary")
            local c = m and m.counters or {}
            print("[INFO] at destruction: onDeath=" .. tostring(c["event.onDeath"]) ..
                " mapMissing=" .. tostring(c["death.mapMissing"]) ..
                " forcePlayer=" .. tostring(c["death.force.player"]) ..
                " forceEnemy=" .. tostring(c["death.force.enemy"]) ..
                " enemyCaused=" .. tostring(c["death.enemyCaused"]))
            setPhase("victory")
        elseif phaseElapsed > PHASE_LIMIT_MINUTES * TICKS_A_MINUTE then
            finish(false, "no structure destroyed within " .. PHASE_LIMIT_MINUTES ..
                " game-minutes: structures=" .. remaining)
            return
        end

    elseif state.phase == "victory" then
        -- 1. victory scent: death generator positive at the origin
        if not r.victoryScent then
            local p = remote.call("Rampant", "queryPheromones", ORIGIN, surface)
            if p and (p.deathGenerator or 0) > 0 then
                r.victoryScent = true
                r.deathGen = p.deathGenerator
                print("[PASS] victory scent applied (deathGenerator=" ..
                    string.format("%.4f", p.deathGenerator) .. ")")
            end
        end
        -- 2. credit: the base tracked the destroyed buildings
        if not r.credit then
            local base = remote.call("Rampant", "queryBaseAt", SPAWNER_POS, surface)
            if base and (base.destroyPlayerBuildings or 0) > 0 then
                r.credit = true
                r.destroyedBuildings = base.destroyPlayerBuildings
                print("[PASS] kill credit tracked (destroyPlayerBuildings=" ..
                    base.destroyPlayerBuildings .. ")")
            end
        end
        if r.victoryScent and r.credit then
            finish(true, "player-death-side expectations met")
            return
        end
        if phaseElapsed > PHASE_LIMIT_MINUTES * TICKS_A_MINUTE then
            local m = remote.call("Rampant", "getMetricsSummary")
            local counters = m and m.counters or {}
            finish(false, "victory phase incomplete after " .. PHASE_LIMIT_MINUTES ..
                " game-minutes: scent=" .. tostring(r.victoryScent) ..
                " credit=" .. tostring(r.credit) ..
                " | mapMissing=" .. tostring(counters["death.mapMissing"]) ..
                " deathPlayer=" .. tostring(counters["death.force.player"]) ..
                " enemyCaused=" .. tostring(counters["death.enemyCaused"]) ..
                " victoryApplied=" .. tostring(counters["death.victoryScentApplied"]) ..
                " onDeath=" .. tostring(counters["event.onDeath"]))
            return
        end
    end
end)
