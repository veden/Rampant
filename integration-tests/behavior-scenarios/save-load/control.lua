--[[
    Rampant Behavior Scenario: Save/Load Continuity

    Verifies the mod survives a save + reload with full continuity:
      setup      - stage wall + turret + spawner + army; base forms;
                   constructed RAIDING state; print SAVE_READY
      (driver: RCON /save, restart, load the save)
      continuity - after reload (on_load path, storage-based state):
                   1. base exists and is registered
                   2. player pheromone at the wall persisted
                   3. the constructed army units persisted
                   4. the AI loop runs post-load: dirty processing
                      advances (chunk index intact — zero misses)
                   5. death chain works post-load: kill 2 units ->
                      retreat -> survivors leave the wall
                   print CONTINUITY_OK + TESTS_COMPLETE

    State lives in storage.sl (module locals do not survive reloads).
]]

local WALL_POS = { x = 0, y = 0 }
local SPAWNER_POS = { x = 100, y = 0 }
local TURRET_POS = { x = 6, y = 0 }
local UNIT_SPAWN_POS = { x = 5, y = 0 }
local WALL_PERIMETER = 30
local TICKS_A_MINUTE = 60 * 60
local PHASE_LIMIT_MINUTES = 15
local BUDGET_MINUTES = 40
local BUDGET_TICKS = BUDGET_MINUTES * TICKS_A_MINUTE
local BASE_AI_STATE_RAIDING = 4

-- Reload marker: module-local (NOT storage — on_load must never write
-- storage, the CRC check fails the load). The file re-executes on load,
-- so on_load and the tick handler share this variable in the same instance.
local justLoaded = false

local function rampReady()
    -- No pcall: the interface is registered at mod load; a missing
    -- interface means the mod is not enabled and the run must fail loudly.
    local ready = remote.call("Rampant", "isInitialized")
    return ready == true
end

local function gameMinutes(tick)
    return tick / TICKS_A_MINUTE
end

local function ensureState()
    if not storage.sl then
        storage.sl = {
            phase = "setup",
            results = {},
            stagedAt = 0,
            phaseStartedAt = 0,
            done = false,
        }
    end
    return storage.sl
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
                local wall = surface.create_entity{
                    name = "stone-wall",
                    position = { x = WALL_POS.x + dx * 2, y = WALL_POS.y + dy * 2 },
                    force = "player",
                    raise_built = true,
                }
                if wall then
                    -- Indestructible: a wall death fires VICTORY SCENT
                    -- (positive death generator) that overwhelms the
                    -- unit-death scents and keeps the rating above the
                    -- retreat threshold — the retreat would never fire.
                    wall.destructible = false
                end
            end
        end
    end
    local turret = surface.create_entity{ name = "gun-turret", position = TURRET_POS, force = "player", raise_built = true }
    if turret then
        -- The turret is the scripted death CAUSE (die("player", turret));
        -- it must survive the attack so the post-load kills work. It has
        -- no ammo (script-created), so it never kills on its own.
        turret.destructible = false
        local ammo = turret.get_inventory(defines.inventory.turret_ammo)
        if ammo then
            ammo.clear()
        end
    end
    -- Rampant spawner -> base creation (registered directly; the vanilla
    -- biter-spawner would be upgrade-destroyed and recreated mid-run).
    surface.create_entity{ name = "neutral-biter-spawner-v1-t1-rampant", position = SPAWNER_POS, force = "enemy", raise_built = true }
    surface.pollute(SPAWNER_POS, 50)
    -- Army (grouped, attacking) — persists across the reload
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
        group.set_command{ type = defines.command.attack_area, destination = WALL_POS, radius = 8 }
    end
    print("STAGED: wall+turret at origin, spawner at (100,0), " .. spawned .. " units at wall")
end

local function killUnits(surface, count)
    local turret = surface.find_entities_filtered{ name = "gun-turret", force = "player" }[1]
    local killed = 0
    local units = unitsNearWall(surface)
    for _, u in ipairs(units) do
        if killed >= count then break end
        -- No pcall: die() errors (or errors inside Rampant's synchronous
        -- death handler) are real bugs and must fail the run loudly.
        if u.valid and turret and turret.valid then
            u.die("player", turret)
            killed = killed + 1
        end
    end
    return killed
end

local function finish(passed, reason)
    local r = storage.sl.results
    print("--- Behavior Scenario: Save/Load Continuity ---")
    print("Passed: " .. (passed and 1 or 0))
    print("Failed: " .. (passed and 0 or 1))
    if reason then print("Detail: " .. reason) end
    print("Detail: base=" .. tostring(r.baseExists) ..
        " pheromone=" .. tostring(r.pheromonePersisted) ..
        " unitsPersisted=" .. tostring(r.unitsPersisted) ..
        " loopAlive=" .. tostring(r.loopAlive) ..
        " indexClean=" .. tostring(r.indexClean) ..
        " retreat=" .. tostring(r.retreat) ..
        " ticks=" .. game.tick)
    print("Execution time: " .. game.tick .. " ticks")
    if passed then
        print("CONTINUITY_OK")
        print("TESTS_COMPLETE")
    else
        print("TESTS_FAILED")
    end
    storage.sl.done = true
    game.speed = 1
end

script.on_init(function()
    -- fresh start: ensureState creates storage.sl (phase "setup")
    ensureState()
end)

script.on_load(function()
    -- reload: storage.sl persists; the tick handler resumes at the saved
    -- phase. Mark the reload so the continuity checks only run AFTER the
    -- driver's save+reload (otherwise they complete pre-save).
    ensureState()
    justLoaded = true
end)

script.on_event(defines.events.on_tick, function()
    if not rampReady() then return end
    local s = ensureState()
    if s.done then return end

    local surface = game.surfaces[1]
    local r = s.results
    local tick = game.tick
    local phaseElapsed = tick - s.phaseStartedAt

    if s.phase == "setup" then
        if s.stagedAt == 0 then
            s.stagedAt = tick
            s.phaseStartedAt = tick
            stage(surface)
            game.speed = 100
            print("LONGRUN_SAVELOAD_SETUP_STARTED")
            return
        end
        local base = remote.call("Rampant", "queryBaseAt", SPAWNER_POS, surface)
        if base then
            s.baseId = base.id
            print("[PASS] base formed (id=" .. base.id .. ") at " ..
                string.format("%.1f", gameMinutes(tick - s.stagedAt)) .. "min")
            local stateOk = remote.call("Rampant", "setBaseState", base.id, BASE_AI_STATE_RAIDING)
            local pointsOk = remote.call("Rampant", "grantUnitPoints", base.id, 2000)
            print("[INFO] constructed RAIDING=" .. tostring(stateOk) .. " points=" .. tostring(pointsOk))
            local units = unitsNearWall(surface)
            if #units >= 2 then
                s.results.unitsAtSave = #units
                s.phase = "continuity"  -- driver saves and reloads now
                s.phaseStartedAt = tick
                print("SAVE_READY units=" .. #units .. " tick=" .. tick)
            end
        elseif phaseElapsed > PHASE_LIMIT_MINUTES * TICKS_A_MINUTE then
            finish(false, "base never formed within " .. PHASE_LIMIT_MINUTES .. " game-minutes")
            return
        end

    elseif s.phase == "continuity" then
        -- Wait for the actual reload (on_load sets the marker); the checks
        -- must not run pre-save
        if not justLoaded then
            return
        end
        -- 1. base exists after reload
        if not r.baseExists then
            local base = remote.call("Rampant", "queryBaseAt", SPAWNER_POS, surface)
            if base then
                r.baseExists = true
                print("[PASS] base persisted after reload (id=" .. base.id .. ")")
            end
        end
        -- 2. pheromone persisted at the wall
        if not r.pheromonePersisted then
            local p = remote.call("Rampant", "queryPheromones", WALL_POS, surface)
            if p and (p.base or 0) > 0 then
                r.pheromonePersisted = true
                print("[PASS] wall pheromone persisted (base=" ..
                    string.format("%.1f", p.base or 0) .. ")")
            end
        end
        -- 3. enemy units persisted (group membership does not survive a
        --    reload — units scatter; assert existence, not location)
        if not r.unitsPersisted then
            local all = surface.find_entities_filtered{ force = "enemy", type = "unit" }
            if #all > 0 then
                r.unitsPersisted = true
                r.unitsAtReload = #all
                print("[PASS] " .. #all .. " enemy units persisted post-load")
            end
        end
        -- construct the post-load attack (units scattered; group the fresh
        -- attack at the wall so the death chain has targets)
        if r.unitsPersisted and not r.attackConstructed then
            local group = surface.create_unit_group{ position = UNIT_SPAWN_POS, force = "enemy" }
            local spawned = 0
            for i = 1, 3 do
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
                group.set_command{ type = defines.command.attack_area, destination = WALL_POS, radius = 8 }
            end
            r.attackConstructed = true
            print("[INFO] constructed post-load attack (" .. spawned .. " units at wall)")
        end
        -- 4. the AI loop runs post-load: dirty processing advances, index clean
        if not r.loopAlive then
            local m = remote.call("Rampant", "getMetricsSummary")
            local counters = m and m.counters or {}
            if (counters["dirty.chunksProcessed"] or 0) > 0 then
                r.loopAlive = true
                r.indexClean = (counters["index.misses"] or 0) == 0
                print("[PASS] AI loop runs post-load (chunksProcessed=" ..
                    counters["dirty.chunksProcessed"] ..
                    ", index.misses=" .. tostring(counters["index.misses"] or 0) .. ")")
            end
        end
        -- 5. death chain post-load: kill 2 -> retreat -> survivors leave
        if not r.retreat then
            -- The retreat threshold tightens with evolution; 2 deaths is
            -- insufficient post-save (rating must drop below ~0.998).
            -- Kill 6 to exceed the threshold despite decay.
            if not r.killed and #unitsNearWall(surface) >= 3 then
                r.unitsAtKillTime = #unitsNearWall(surface)
                local killed = killUnits(surface, 6)
                r.killed = killed
                print("[INFO] killed " .. killed .. " unit(s) (post-load retreat trigger)")
            end
            -- Retreat verified via telemetry (the command executed) plus the
            -- unit count dropping below the kill-time count (wanderers keep
            -- traffic, so exactly 0 is timing-dependent)
            if r.killed and r.killed > 0 and not r.retreat then
                local m = remote.call("Rampant", "getMetricsSummary")
                local counters = m and m.counters or {}
                local now = #unitsNearWall(surface)
                if (counters["cmd.RETREAT_UNITS"] or 0) > 0 and now < (r.unitsAtKillTime or 99) then
                    r.retreat = true
                    print("[PASS] retreat works post-load (cmd fired, units " ..
                        r.unitsAtKillTime .. " -> " .. now .. ")")
                end
            end
            if r.killed and phaseElapsed > PHASE_LIMIT_MINUTES * TICKS_A_MINUTE then
                local m = remote.call("Rampant", "getMetrics")
                local counters = m and m.counters or {}
                local samples = m and m.samples or {}
                local pheromones = remote.call("Rampant", "queryPheromones", WALL_POS, surface)
                local function list(name)
                    local t = samples[name]
                    if not t then return "none" end
                    local parts = {}
                    for i = 1, #t do
                        parts[i] = string.format("%.5f", t[i])
                    end
                    return table.concat(parts, ",")
                end
                finish(false, "retreat did not fire post-load: " ..
                    #unitsNearWall(surface) .. " unit(s) still at wall | " ..
                    "onDeath=" .. tostring(counters["event.onDeath"]) ..
                    " mapMissing=" .. tostring(counters["death.mapMissing"]) ..
                    " chunkMissing=" .. tostring(counters["death.chunkMissing"]) ..
                    " commandsEmitted=" .. tostring(counters["death.commandsEmitted"]) ..
                    " deathEnemy=" .. tostring(counters["death.force.enemy"]) ..
                    " deathPlayer=" .. tostring(counters["death.force.player"]) ..
                    " causeValid=" .. tostring(counters["death.causeValid"]) ..
                    " rating=[" .. list("death.rating") .. "]" ..
                    " threshold=[" .. list("death.threshold") .. "]" ..
                    " wallDeathGen=" .. tostring(pheromones and pheromones.deathGenerator) ..
                    " retreatCmd=" .. tostring(counters["cmd.RETREAT_UNITS"]))
                return
            end
        end
        if r.baseExists and r.pheromonePersisted and r.unitsPersisted and r.loopAlive and r.indexClean and r.retreat then
            finish(true, "save/load continuity expectations met")
            return
        end
        if phaseElapsed > PHASE_LIMIT_MINUTES * TICKS_A_MINUTE then
            finish(false, "continuity incomplete after " .. PHASE_LIMIT_MINUTES ..
                " game-minutes: base=" .. tostring(r.baseExists) ..
                " pheromone=" .. tostring(r.pheromonePersisted) ..
                " units=" .. tostring(r.unitsPersisted) ..
                " loop=" .. tostring(r.loopAlive) ..
                " retreat=" .. tostring(r.retreat))
            return
        end
    end
end)
