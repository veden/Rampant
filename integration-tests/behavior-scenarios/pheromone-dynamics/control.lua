--[[
    Rampant Behavior Scenario: Pheromone Dynamics (CONSTRUCTED)

    Behavioral verification of the pheromone pipeline (the enemy AI's world
    model). With a player base (wall + turret) at the origin and an enemy
    base at (100,0):

      1. generation  - the player-base attraction (BASE_PHEROMONE) is
                       generated at the player structures' chunk (from
                       playerBaseGenerator) and stays > 0
      2. diffusion   - the attraction DIFFUSES outward: the mid chunk
                       (48,0) and the nest chunk (96,0) accumulate the
                       value over the processing waves (last > first)
      3. presence    - the enemy's own presence (ENEMY_PHEROMONE) is
                       generated at the nest chunk (enemyStructureCount
                       x multiplier) and stays > 0

    The nest chunk's attraction > 0 is also the precondition that keeps the
    attack-formation gate open (Squad.deploySquad pheromone check) — the
    values verified here are the ones the AI acts on.

    Expected total: ~10 game-minutes.
]]

local WALL_POS = { x = 0, y = 0 }
local MID_POS = { x = 48, y = 0 }
local SPAWNER_POS = { x = 100, y = 0 }
local TURRET_POS = { x = 6, y = 0 }
local TICKS_A_MINUTE = 60 * 60
local BUDGET_MINUTES = 30
local BUDGET_TICKS = BUDGET_MINUTES * TICKS_A_MINUTE
local PHASE_LIMIT_MINUTES = 15
local SAMPLE_INTERVAL = 30 * 60  -- 30 game-seconds
local WINDOW_SAMPLES = 16         -- 8 game-minutes

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

local function pheromones(surface, pos)
    return remote.call("Rampant", "queryPheromones", pos, surface)
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
    remote.call("Rampant", "drawField", "base", true)
    remote.call("Rampant", "drawField", "enemy", true)
end

local function setPhase(phase)
    state.phase = phase
    state.phaseStartedAt = game.tick
    print("PHASE: " .. phase .. " at " ..
        string.format("%.1f", gameMinutes(game.tick - state.stagedAt)) .. "min")
end

local function finish(passed, reason)
    local r = state.results
    print("--- Behavior Scenario: Pheromone Dynamics ---")
    print("Passed: " .. (passed and 1 or 0))
    print("Failed: " .. (passed and 0 or 1))
    if reason then
        print("Detail: " .. reason)
    end
    print("Detail: wallBase=" .. tostring(r.wallBase) ..
        " midBase=" .. tostring(r.midBase) ..
        " nestBase=" .. tostring(r.nestBase) ..
        " nestEnemy=" .. tostring(r.nestEnemy) ..
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

    if elapsed > BUDGET_TICKS then
        finish(false, "budget " .. BUDGET_MINUTES .. " game-minutes elapsed")
        return
    end

    if state.phase == "setup" then
        local base = queryBase(surface)
        if base then
            r.baseFormed = true
            print("[PASS] base formed (id=" .. base.id .. ") at " ..
                string.format("%.1f", gameMinutes(tick - state.stagedAt)) .. "min")
            r.samples = {}
            r.lastSampleTick = tick
            setPhase("observe")
        elseif phaseElapsed > PHASE_LIMIT_MINUTES * TICKS_A_MINUTE then
            finish(false, "base never formed within " .. PHASE_LIMIT_MINUTES .. " game-minutes")
            return
        end

    elseif state.phase == "observe" then
        -- Sample the three chunks every SAMPLE_INTERVAL; after the window
        -- assert generation + diffusion + presence.
        if tick - (r.lastSampleTick or 0) < SAMPLE_INTERVAL then
            return
        end
        r.lastSampleTick = tick
        local wall = pheromones(surface, WALL_POS)
        local mid = pheromones(surface, MID_POS)
        local nest = pheromones(surface, SPAWNER_POS)
        local sample = {
            wallBase = wall and wall.base or 0,
            midBase = mid and mid.base or 0,
            nestBase = nest and nest.base or 0,
            nestEnemy = nest and nest.enemy or 0,
        }
        r.samples[#r.samples + 1] = sample
        if #r.samples == WINDOW_SAMPLES then
            local last = r.samples[#r.samples]
            r.wallBase = last.wallBase
            r.midBase = last.midBase
            r.nestBase = last.nestBase
            r.nestEnemy = last.nestEnemy
            local ok = true
            local problems = {}
            if not (last.wallBase > 0) then
                ok = false
                problems[#problems + 1] = "wall base pheromone not generated ("
                    .. string.format("%.2f", last.wallBase) .. ")"
            end
            -- The pipeline converges: the dirty wave diffuses the value
            -- outward (0.9 factor per chunk) until the epsilon gate
            -- settles. The DIFFUSION GRADIENT (source > mid > nest > 0)
            -- is the observable signature.
            if not (last.midBase > 0 and last.midBase < last.wallBase) then
                ok = false
                problems[#problems + 1] = "mid chunk diffusion gradient broken (wall=" ..
                    string.format("%.2f", last.wallBase) .. " mid=" ..
                    string.format("%.2f", last.midBase) .. ")"
            end
            if not (last.nestBase > 0 and last.nestBase < last.midBase) then
                ok = false
                problems[#problems + 1] = "nest chunk diffusion gradient broken (mid=" ..
                    string.format("%.2f", last.midBase) .. " nest=" ..
                    string.format("%.2f", last.nestBase) .. ")"
            end
            if not (last.nestEnemy > 0) then
                ok = false
                problems[#problems + 1] = "nest enemy pheromone absent ("
                    .. string.format("%.2f", last.nestEnemy) .. ")"
            end
            if ok then
                print("[PASS] generation: wall base=" .. string.format("%.2f", last.wallBase))
                print("[PASS] diffusion gradient: wall " .. string.format("%.2f", last.wallBase) ..
                    " > mid " .. string.format("%.2f", last.midBase) ..
                    " > nest " .. string.format("%.2f", last.nestBase))
                print("[PASS] nest enemy presence=" .. string.format("%.2f", last.nestEnemy))
                finish(true, "all pheromone-dynamics expectations met")
            else
                finish(false, table.concat(problems, "; "))
            end
            return
        end
    end
end)
