--[[
    Rampant Behavior Scenario: Upgrade Threshold (CONSTRUCTED)

    Verifies the structure-upgrade pipeline's EVOLUTION-THRESHOLD gating: the
    SAME vanilla enemy structure upgrades to a HIGHER-tier rampant variant at
    high evolution than at low evolution.

    Upgrade.queueUpgrade: evoIndex = max(distance * k, Universe.evolutionLevel)
    -> findEntityUpgrade -> tier = max(evoToTier(currentEvo, 5), evoToTier(evoIndex, 4))
    and the replacement name carries the tier
    ("<faction>-biter-spawner-v1-t<tier>-rampant").

      1. setup - scan stability; destroy natural enemies; LOW evolution (0.1);
                 stage a vanilla biter-spawner at a fixed distance from origin
      2. low   - the vanilla spawner is replaced by a rampant variant; record
                 its tier (parsed from the entity name)
      3. high  - HIGH evolution (0.9); stage a second vanilla spawner at the
                 SAME distance; its replacement's tier must EXCEED the
                 low-evolution tier

    Both spawners sit at the same distance from the origin (64 tiles), so the
    only varying input to the tier selection is evolution.

    Expected total: ~30 game-minutes.
]]

local Lib = require("scenario-lib")

local LOW_POS = { x = 64, y = 0 }
local HIGH_POS = { x = 0, y = 64 }   -- same 64-tile distance from origin
local EVO_LOW = 0.1
local EVO_HIGH = 0.9
local VANILLA_SPAWNER = "biter-spawner"
local PHASE_LIMIT_MINUTES = 20
local BUDGET_MINUTES = 60
local BUDGET_TICKS = BUDGET_MINUTES * Lib.TICKS_A_MINUTE

local state  -- { phase, results, stagedAt, phaseStartedAt, done }

local function tierOf(name)
    return tonumber(name:match("%-t(%d+)%-rampant"))
end

local function rampantSpawners(surface)
    local out = {}
    for _, e in ipairs(surface.find_entities_filtered{
        force = "enemy", type = "unit-spawner" }) do
        if e.valid and e.name:match("%-rampant$") then
            out[#out + 1] = e
        end
    end
    return out
end

local function vanillaSpawnerCount(surface)
    local count = 0
    for _, e in ipairs(surface.find_entities_filtered{
        force = "enemy", type = "unit-spawner" }) do
        if e.valid and not e.name:match("%-rampant$") then
            count = count + 1
        end
    end
    return count
end

--- Nearest rampant spawner within maxDist of pos — the upgrade replacement
-- lands at the original's position, so a proximity guard prevents matching
-- the OTHER leg's replacement (observed: the high leg matched the low leg's
-- spawner, 90 tiles away, before its own had appeared).
local function nearestRampant(surface, pos, maxDist)
    local best, bestDist
    for _, e in ipairs(rampantSpawners(surface)) do
        local d = Lib.dist(e.position, pos)
        if d <= maxDist and (not bestDist or d < bestDist) then
            best, bestDist = e, d
        end
    end
    return best
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
    print("[INFO] natural enemies cleared; vanilla spawners=" ..
        vanillaSpawnerCount(surface))
end

local function setPhase(phase)
    state.phase = phase
    state.phaseStartedAt = game.tick
    print("PHASE: " .. phase .. " at " ..
        string.format("%.1f", Lib.gameMinutes(game.tick - state.stagedAt)) .. "min")
end

local function phaseTimedOut(budgetMinutes)
    return game.tick - state.phaseStartedAt > budgetMinutes * Lib.TICKS_A_MINUTE
end

local function finish(passed, reason)
    local r = state.results
    print("--- Behavior Scenario: Upgrade Threshold ---")
    print("Passed: " .. (passed and 1 or 0))
    print("Failed: " .. (passed and 0 or 1))
    if reason then
        print("Detail: " .. reason)
    end
    print("Detail: lowTier=" .. tostring(r.lowTier) ..
        " highTier=" .. tostring(r.highTier) ..
        " lowName=" .. tostring(r.lowName) ..
        " highName=" .. tostring(r.highName) ..
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
    if not Lib.ready() then
        return
    end
    if not state then
        if not Lib.waitForScanStability(game.tick, storage) then
            return
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
        setPhase("low")
        return
    end
    if state.done then
        return
    end

    local surface = game.surfaces[1]
    local r = state.results
    local tick = game.tick

    if tick - state.stagedAt > BUDGET_TICKS then
        finish(false, "budget " .. BUDGET_MINUTES .. " game-minutes elapsed")
        return
    end

    if state.phase == "low" then
        -- Arm: set the evolution and let the mod's planning() pass (every
        -- 3rd tick) absorb it into Universe.evolutionLevel BEFORE creating
        -- the spawner — the upgrade queue reads the cached value at
        -- queueUpgrade time, so same-tick setEvolution would queue the
        -- stale evolution (observed: both legs tier 1).
        if not r.lowArmed then
            local ok = Lib.setEvolution(surface, EVO_LOW)
            r.lowArmed = true
            r.lowCreateAt = tick + 60
            print("[INFO] LOW evolution armed=" .. EVO_LOW ..
                " setEvolution=" .. tostring(ok) ..
                " engineEvo=" .. string.format("%.3f",
                    game.forces.enemy.get_evolution_factor(surface)))
            return
        end
        if tick < r.lowCreateAt then
            return
        end
        if not r.lowCreated then
            surface.create_entity{
                name = VANILLA_SPAWNER,
                position = LOW_POS,
                force = "enemy",
                raise_built = true,
            }
            r.lowCreated = true
            print("[INFO] vanilla spawner staged at (" ..
                LOW_POS.x .. "," .. LOW_POS.y .. ")")
            return
        end
        local e = nearestRampant(surface, LOW_POS, 16)
        if e then
            r.lowTier = tierOf(e.name)
            r.lowName = e.name
            if not r.lowTier then
                finish(false, "low-evolution replacement has no tier in its name: " ..
                    e.name)
                return
            end
            print("[PASS] low-evolution upgrade: " .. e.name ..
                " (tier=" .. r.lowTier .. ") at " ..
                string.format("%.1f", Lib.gameMinutes(tick - state.stagedAt)) .. "min")
            setPhase("high")
        elseif phaseTimedOut(PHASE_LIMIT_MINUTES) then
            r.lowAttempts = (r.lowAttempts or 0) + 1
            if r.lowAttempts < 3 then
                r.lowArmed = false
                r.lowCreated = false
                r.lowCreateAt = nil
                print("[RETRY] low leg attempt " .. r.lowAttempts ..
                    " timed out; re-staging")
                setPhase("low")
            else
                finish(false, "low: no rampant replacement after " ..
                    r.lowAttempts .. " attempts")
            end
            return
        end

    elseif state.phase == "high" then
        -- Arm the high evolution and wait for the planning pass.
        if not r.highArmed then
            local ok = Lib.setEvolution(surface, EVO_HIGH)
            r.highArmed = true
            r.highCreateAt = tick + 60
            print("[INFO] HIGH evolution armed=" .. EVO_HIGH ..
                " setEvolution=" .. tostring(ok) ..
                " engineEvo=" .. string.format("%.3f",
                    game.forces.enemy.get_evolution_factor(surface)))
            return
        end
        if tick < r.highCreateAt then
            return
        end
        if not r.highCreated then
            surface.create_entity{
                name = VANILLA_SPAWNER,
                position = HIGH_POS,
                force = "enemy",
                raise_built = true,
            }
            r.highCreated = true
            print("[INFO] vanilla spawner staged at (" ..
                HIGH_POS.x .. "," .. HIGH_POS.y .. ")")
            return
        end
        local e = nearestRampant(surface, HIGH_POS, 16)
        if e then
            r.highTier = tierOf(e.name)
            r.highName = e.name
            if not r.highTier then
                finish(false, "high-evolution replacement has no tier in its name: " ..
                    e.name)
                return
            end
            print("[PASS] high-evolution upgrade: " .. e.name ..
                " (tier=" .. r.highTier .. ") at " ..
                string.format("%.1f", Lib.gameMinutes(tick - state.stagedAt)) .. "min")
            -- ASSERTION: the threshold gating is visible — same distance,
            -- higher evolution -> strictly higher tier.
            if r.highTier > r.lowTier then
                finish(true, "upgrade tier scales with evolution (low=" ..
                    r.lowTier .. " -> high=" .. r.highTier .. ")")
            else
                finish(false, "high-evolution tier not above low: low=" ..
                    r.lowTier .. " high=" .. r.highTier)
            end
        elseif phaseTimedOut(PHASE_LIMIT_MINUTES) then
            r.highAttempts = (r.highAttempts or 0) + 1
            if r.highAttempts < 3 then
                r.highArmed = false
                r.highCreated = false
                r.highCreateAt = nil
                print("[RETRY] high leg attempt " .. r.highAttempts ..
                    " timed out; re-staging")
                setPhase("high")
            else
                finish(false, "high: no rampant replacement after " ..
                    r.highAttempts .. " attempts")
            end
            return
        end
    end
end)
