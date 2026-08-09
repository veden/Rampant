--[[
    Rampant Behavior Scenario: Multi Surface (CONSTRUCTED)

    Behavioral verification of the per-surface map handling:
      1. setup      - stage the standard construction on surface 1 (wall +
                      Rampant spawner); base forms
      2. create     - a SECOND surface is created (game.create_surface ->
                      the mod's on_surface_created preps the map) and its
                      initial chunks generate (onChunkGenerated -> pending
                      processing populates the chunk grid)
      3. construct2 - a Rampant spawner on surface 2 -> base forms there
                      (queryBaseAt on the second surface)
      4. exclude    - addExcludeSurface removes the surface's map
                      (queryBaseAt -> nil)

    Expected total: ~10-20 game-minutes.
]]

local WALL_POS = { x = 0, y = 0 }
local SPAWNER_POS = { x = 100, y = 0 }
local TURRET_POS = { x = 6, y = 0 }
local SURFACE_2_NAME = "rampant-surface-2"
local TICKS_A_MINUTE = 60 * 60
local BUDGET_MINUTES = 40
local BUDGET_TICKS = BUDGET_MINUTES * TICKS_A_MINUTE
local PHASE_LIMIT_MINUTES = 15

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

local function queryBase(surface, pos)
    return remote.call("Rampant", "queryBaseAt", pos or SPAWNER_POS, surface)
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

local function setPhase(phase)
    state.phase = phase
    state.phaseStartedAt = game.tick
    print("PHASE: " .. phase .. " at " ..
        string.format("%.1f", gameMinutes(game.tick - state.stagedAt)) .. "min")
end

local function finish(passed, reason)
    local r = state.results
    print("--- Behavior Scenario: Multi Surface ---")
    print("Passed: " .. (passed and 1 or 0))
    print("Failed: " .. (passed and 0 or 1))
    if reason then
        print("Detail: " .. reason)
    end
    print("Detail: base1=" .. tostring(r.base1) ..
        " surface2=" .. tostring(r.surface2) ..
        " base2=" .. tostring(r.base2) ..
        " excluded=" .. tostring(r.excluded) ..
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
        local base = queryBase(surface)
        if base then
            r.base1 = true
            print("[PASS] base formed on surface 1 (id=" .. base.id .. ") at " ..
                string.format("%.1f", gameMinutes(tick - state.stagedAt)) .. "min")
            -- Create the second surface: the mod's on_surface_created
            -- preps the map. Generate the initial chunks so the mod's
            -- pending-chunk processing populates the grid.
            local ok, err = game.create_surface(SURFACE_2_NAME)
            if not ok then
                finish(false, "create_surface failed: " .. tostring(err))
                return
            end
            r.surface2 = true
            local s2 = game.get_surface(SURFACE_2_NAME)
            s2.request_to_generate_chunks({ 0, 0 }, 8)
            s2.force_generate_chunk_requests()
            print("[PASS] surface 2 created + chunks generated at " ..
                string.format("%.1f", gameMinutes(tick - state.stagedAt)) .. "min")
            -- Construct the spawner on surface 2 (the map is prepped; the
            -- chunk grid populates via the pending processing).
            s2.create_entity{
                name = "neutral-biter-spawner-v1-t1-rampant",
                position = SPAWNER_POS,
                force = "enemy",
                raise_built = true,
            }
            s2.pollute(SPAWNER_POS, 50)
            print("[INFO] spawner constructed on surface 2")
            setPhase("construct2")
        elseif phaseElapsed > PHASE_LIMIT_MINUTES * TICKS_A_MINUTE then
            finish(false, "base never formed on surface 1 within " ..
                PHASE_LIMIT_MINUTES .. " game-minutes")
            return
        end

    elseif state.phase == "construct2" then
        -- The base must form on the SECOND surface (per-surface maps work).
        local s2 = game.get_surface(SURFACE_2_NAME)
        local base2 = queryBase(s2)
        if base2 then
            r.base2 = true
            r.base2Id = base2.id
            print("[PASS] base formed on surface 2 (id=" .. base2.id .. ") at " ..
                string.format("%.1f", gameMinutes(tick - state.stagedAt)) .. "min")
            remote.call("Rampant", "addExcludeSurface", SURFACE_2_NAME)
            print("[INFO] addExcludeSurface(" .. SURFACE_2_NAME .. ")")
            setPhase("exclude")
        elseif phaseElapsed > PHASE_LIMIT_MINUTES * TICKS_A_MINUTE then
            finish(false, "base never formed on surface 2 within " ..
                PHASE_LIMIT_MINUTES .. " game-minutes")
            return
        end

    elseif state.phase == "exclude" then
        -- The exclusion must remove the surface's map: queryBaseAt returns
        -- nil when the map is gone.
        local s2 = game.get_surface(SURFACE_2_NAME)
        local base2 = queryBase(s2)
        if not base2 then
            r.excluded = true
            print("[PASS] surface 2 map removed by exclusion at " ..
                string.format("%.1f", gameMinutes(tick - state.stagedAt)) .. "min")
            -- Sanity: surface 1 is unaffected.
            local base1 = queryBase(surface)
            print("[INFO] surface 1 still tracked: " .. tostring(base1 ~= nil))
            if base1 then
                finish(true, "all multi-surface expectations met")
            else
                finish(false, "exclusion removed surface 1 as well")
            end
            return
        end
        if phaseElapsed > PHASE_LIMIT_MINUTES * TICKS_A_MINUTE then
            finish(false, "exclude: surface 2 map still tracked after " ..
                PHASE_LIMIT_MINUTES .. " game-minutes")
            return
        end
    end
end)
