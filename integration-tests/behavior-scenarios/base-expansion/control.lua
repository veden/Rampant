--[[
    Rampant Behavior Scenario: Base Expansion (CONSTRUCTED)

    Behavioral verification of the expansion chain: the MIGRATING base
    dispatches a settler (builder squad), the settler travels and BUILDS a
    new base (vanilla build_base command), the new spawner enters the
    upgrade pipeline (vanilla -> QUEUE_UPGRADE -> Rampant replacement) and
    registers with the base.

      1. setup    - wall + Rampant spawner at (100,0); base forms;
                     MIGRATING constructed (frozen); periodic pollution
      2. dispatch - builder squad forms and leaves the nest region
      3. build    - a NEW enemy spawner appears (count 1 -> 2)
      4. upgrade  - the new spawner settles as a Rampant type
                     (cmd.QUEUE_UPGRADE executed, name ends "-rampant")
      5. register - the new spawner is covered by a base (queryBaseAt
                     at the new position -> base with nestCount >= 2)

    Expected total: ~20-60 game-minutes.
]]

local Lib = require("scenario-lib")

local WALL_POS = { x = 0, y = 0 }
local SPAWNER_POS = { x = 100, y = 0 }
local TURRET_POS = { x = 6, y = 0 }
local NEST_REGION = 40
local TICKS_A_MINUTE = 60 * 60
local BUDGET_MINUTES = 90
local BUDGET_TICKS = BUDGET_MINUTES * TICKS_A_MINUTE
local PHASE_LIMIT_MINUTES = 45
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

local function queryBase(surface)
    return remote.call("Rampant", "queryBaseAt", SPAWNER_POS, surface)
end

local function squads(surface)
    return remote.call("Rampant", "getEnemyGroups", surface) or {}
end

local function builderSquads(surface)
    local n = 0
    for _, s in ipairs(squads(surface)) do
        if s.isBuilder and s.memberCount and s.memberCount >= 1 then
            n = n + 1
        end
    end
    return n
end

local function spawners(surface)
    return surface.find_entities_filtered{ force = "enemy", type = "unit-spawner" }
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
    print("--- Behavior Scenario: Base Expansion ---")
    print("Passed: " .. (passed and 1 or 0))
    print("Failed: " .. (passed and 0 or 1))
    if reason then
        print("Detail: " .. reason)
    end
    print("Detail: base=" .. tostring(r.baseFormed) ..
        " dispatched=" .. tostring(r.dispatched) ..
        " built=" .. tostring(r.built) ..
        " upgraded=" .. tostring(r.upgraded) ..
        " registered=" .. tostring(r.registered) ..
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

    -- Keep the nest CONTINUOUSLY active (the shared-lib lesson: the activeness
    -- check reads the chunk CORNER and spawners absorb pollution, so a 10/min
    -- pulse leaves the nest inactive ~97% of the time and settler formation
    -- starves — observed: dispatched=nil over the whole budget). The lib
    -- streams at the corner + spawner; a small extra pulse keeps the spawner's
    -- own processing fed.
    Lib.sustainNest(surface, SPAWNER_POS, tick, storage)

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
            local stateOk = remote.call("Rampant", "setBaseState", base.id, BASE_AI_STATE_MIGRATING)
            print("[INFO] constructed: MIGRATING=" .. tostring(stateOk))
            setPhase("dispatch")
        elseif phaseElapsed > PHASE_LIMIT_MINUTES * TICKS_A_MINUTE then
            finish(false, "base never formed within " .. PHASE_LIMIT_MINUTES .. " game-minutes")
            return
        end

    elseif state.phase == "dispatch" then
        -- The builder (settler) squad must leave the nest region.
        if builderSquads(surface) > 0 then
            local left = false
            for _, s in ipairs(squads(surface)) do
                if s.isBuilder and s.position then
                    local dx = s.position.x - SPAWNER_POS.x
                    local dy = s.position.y - SPAWNER_POS.y
                    if (dx * dx + dy * dy) > NEST_REGION * NEST_REGION then
                        left = true
                        r.dispatchDist = math.floor(math.sqrt(dx * dx + dy * dy))
                        break
                    end
                end
            end
            if left then
                r.dispatched = true
                print("[PASS] settler left the nest region (dist=" .. r.dispatchDist ..
                    " tiles) at " ..
                    string.format("%.1f", gameMinutes(tick - state.stagedAt)) .. "min")
                setPhase("build")
            end
        end
        if not r.dispatched and phaseElapsed > PHASE_LIMIT_MINUTES * TICKS_A_MINUTE then
            finish(false, "dispatch: no settler left the nest within " ..
                PHASE_LIMIT_MINUTES .. " game-minutes")
            return
        end

    elseif state.phase == "build" then
        -- A NEW enemy spawner must appear (count 1 -> 2). The vanilla
        -- build_base spawner enters the upgrade pipeline, which destroys
        -- it (a gap where the count drops back to 1) and recreates the
        -- Rampant replacement — so require the count == 2 stably.
        local spawnerList = spawners(surface)
        if not r.built and #spawnerList >= 2 then
            r.built = true
            print("[PASS] new enemy spawner built (count=" .. #spawnerList .. ") at " ..
                string.format("%.1f", gameMinutes(tick - state.stagedAt)) .. "min")
            local m = remote.call("Rampant", "getMetricsSummary")
            local c = m and m.counters or {}
            r.queueUpgradeCount = c["cmd.QUEUE_UPGRADE"] or 0
            print("[INFO] cmd.QUEUE_UPGRADE=" .. r.queueUpgradeCount)
        end
        if r.built and phaseElapsed > PHASE_LIMIT_MINUTES * TICKS_A_MINUTE then
            finish(false, "build: new spawner appeared but never settled (count=" ..
                #spawners(surface) .. ")")
            return
        end
        -- The new spawner must settle: the upgrade pipeline replaces the
        -- vanilla build with a Rampant spawner (name ends "-rampant").
        if r.built and not r.upgraded then
            local spawnerList = spawners(surface)
            if #spawnerList >= 2 then
                for _, s in ipairs(spawnerList) do
                    if s.valid and s.position and
                        (math.abs(s.position.x - SPAWNER_POS.x) > 1 or
                         math.abs(s.position.y - SPAWNER_POS.y) > 1) and
                        string.find(s.name, "%-rampant$")
                    then
                        r.upgraded = true
                        r.newSpawnerPos = { x = s.position.x, y = s.position.y }
                        r.newSpawnerName = s.name
                        print("[PASS] new spawner settled as " .. s.name ..
                            " at (" .. s.position.x .. "," .. s.position.y .. ") at " ..
                            string.format("%.1f", gameMinutes(tick - state.stagedAt)) .. "min")
                        setPhase("register")
                        break
                    end
                end
            end
        end

    elseif state.phase == "register" then
        -- The new spawner must be REGISTERED: its chunk carries the nest
        -- count (chunk.nestCount — the honest registration signal) and a
        -- base covers it.
        local base = remote.call("Rampant", "queryBaseAt", r.newSpawnerPos, surface)
        if base and base.nestCount and base.nestCount >= 1 then
            r.registered = true
            r.registeredNestCount = base.nestCount
            print("[PASS] new spawner registered (base id=" .. base.id ..
                " chunkNestCount=" .. base.nestCount .. ") at " ..
                string.format("%.1f", gameMinutes(tick - state.stagedAt)) .. "min")
            finish(true, "all base-expansion expectations met")
            return
        end
        if phaseElapsed > PHASE_LIMIT_MINUTES * TICKS_A_MINUTE then
            finish(false, "register: new spawner not covered by a base within " ..
                PHASE_LIMIT_MINUTES .. " game-minutes")
            return
        end
    end
end)
