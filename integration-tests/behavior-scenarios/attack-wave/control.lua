--[[
    Rampant Behavior Scenario: Attack Wave (CONSTRUCTED)

    Verifies attack-wave SIZE scaling with evolution:
      Universe.attackWaveSize = attackWaveMaxSize * (evolutionLevel ^ 1.4)
      attackWaveScaling() = ceil(gaussian(attackWaveSize,
          attackWaveSize * 0.333, 1, attackWaveUpperBound))

    A formed attack squad recruits up to scaledWaveSize ungrouped units, so
    the squad's memberCount reflects the scaling when enough units are
    available. Same base, same staging, LOW vs HIGH evolution: the
    high-evolution wave must be strictly larger.

      1. setup     - scan stability; destroy natural enemies; beacon (the
                     basePheromone formation gate); Rampant nest + sustained
                     pollution; RAIDING state; LOW evolution
      2. low-arm   - evolution armed, then free units staged
      3. low-wave  - an attack squad forms; record its memberCount
      4. high-arm  - HIGH evolution armed (after the mod's planning() pass
                     absorbs it), then fresh free units staged
      5. high-wave - a NEW attack squad forms; memberCount must exceed the
                     low-evolution wave

    Expected total: ~30 game-minutes.
]]

local Lib = require("scenario-lib")

local BEACON_POS = { x = 0, y = 0 }
local SPAWNER_POS = { x = 100, y = 0 }
local SPAWNER_CANDIDATES = {
    { x = 100, y = 0 }, { x = 112, y = 0 }, { x = 96, y = 0 },
    { x = 80, y = 0 }, { x = 128, y = 0 }, { x = 64, y = 0 },
}
local WATER_SCAN_HALF = 48
local EVO_LOW = 0.1
local EVO_HIGH = 0.9
local BASE_AI_STATE_RAIDING = 4
local GRANT_POINTS = 3000
local FREE_UNIT_COUNT = 120
local ARM_DELAY = 60          -- ticks for planning() to absorb setEvolution
local PHASE_LIMIT_MINUTES = 10
local BUDGET_MINUTES = 40
local BUDGET_TICKS = BUDGET_MINUTES * Lib.TICKS_A_MINUTE

local state

local function attackSquads(surface)
    local out = {}
    for _, s in ipairs(Lib.squads(surface)) do
        if not s.isBuilder and s.memberCount and s.memberCount >= 1 then
            out[#out + 1] = s
        end
    end
    return out
end

--- New attack squad (not seen in the `seen` id set).
local function newAttackSquad(surface, seen)
    for _, s in ipairs(attackSquads(surface)) do
        if not seen[s.id] then
            return s
        end
    end
    return nil
end

local function stage(surface)
    -- Vanilla expansion off (no vanilla-built spawners mid-run). Safe at
    -- staging: the mod cached Universe.expansion=true at its init.
    game.map_settings.enemy_expansion.enabled = false

    -- Water-free nest position.
    local dry = Lib.pickDryPosition(surface, SPAWNER_CANDIDATES, WATER_SCAN_HALF)
    SPAWNER_POS.x = dry.x
    SPAWNER_POS.y = dry.y
    print("[INFO] spawner placed at (" .. SPAWNER_POS.x .. "," .. SPAWNER_POS.y .. ")")

    -- Clean the map: destroy all natural enemy structures/units.
    local natural = surface.find_entities_filtered{
        force = "enemy", type = { "unit-spawner", "turret", "unit" } }
    for _, e in ipairs(natural) do
        if e.valid then e.destroy() end
    end

    -- Beacon: the attacker formation gate needs basePheromone > 0.0001 at the
    -- nest chunk (player buildings' scent, diffused — see the sweep).
    Lib.buildBeacon(surface, BEACON_POS, {})

    -- Rampant nest -> base creation.
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
        string.format("%.1f", Lib.gameMinutes(game.tick - state.stagedAt)) .. "min")
end

local function finish(passed, reason)
    local r = state.results
    print("--- Behavior Scenario: Attack Wave ---")
    print("Passed: " .. (passed and 1 or 0))
    print("Failed: " .. (passed and 0 or 1))
    if reason then
        print("Detail: " .. reason)
    end
    print("Detail: lowCount=" .. tostring(r.lowCount) ..
        " highCount=" .. tostring(r.highCount) ..
        " lowId=" .. tostring(r.lowId) ..
        " highId=" .. tostring(r.highId) ..
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

script.on_init(function()
    -- Only the cooldowns (the mod caches these at ITS init, which runs
    -- after this scenario's on_init). Do NOT set
    -- enemy_expansion.enabled=false here: canMigrate caches it.
    game.map_settings.enemy_expansion.min_expansion_cooldown = 60 * 5
    game.map_settings.enemy_expansion.max_expansion_cooldown = 60 * 15
end)

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
            results = { seen = {} },
            stagedAt = game.tick,
            phaseStartedAt = game.tick,
            done = false,
        }
        local surface = game.surfaces[1]
        stage(surface)
        -- Keep the nest active (formation requires an active nest).
        Lib.sustainNest(surface, SPAWNER_POS, game.tick, storage)
        game.speed = 100
        setPhase("low-arm")
        return
    end
    if state.done then
        return
    end

    local surface = game.surfaces[1]
    local r = state.results
    local tick = game.tick

    -- Keep the nest active and the economy fed.
    Lib.sustainNest(surface, SPAWNER_POS, tick, storage)
    local base = Lib.queryBase(surface, SPAWNER_POS)
    if base and (base.unitPoints or 0) < 1000 then
        Lib.grantUnitPoints(surface, base.id, GRANT_POINTS)
    end

    if tick - state.stagedAt > BUDGET_TICKS then
        finish(false, "budget " .. BUDGET_MINUTES .. " game-minutes elapsed")
        return
    end

    if state.phase == "setup" then
        setPhase("low-arm")

    elseif state.phase == "low-arm" then
        local base = Lib.queryBase(surface, SPAWNER_POS)
        if not base then
            if Lib.phaseTimedOut(state, PHASE_LIMIT_MINUTES,
                "base never formed", surface) then return end
            return
        end
        r.baseId = base.id
        Lib.setBaseState(surface, base.id, BASE_AI_STATE_RAIDING)
        Lib.grantUnitPoints(surface, base.id, GRANT_POINTS)
        Lib.setEvolution(surface, EVO_LOW)
        r.lowArmedAt = tick
        print("[INFO] LOW evolution armed=" .. EVO_LOW ..
            " base=" .. base.id .. " RAIDING")
        setPhase("low-stage")

    elseif state.phase == "low-stage" then
        -- Let the mod's planning() pass absorb the evolution change BEFORE
        -- staging units (the formation reads the cached value).
        if tick - r.lowArmedAt < ARM_DELAY then
            return
        end
        r.lowStaged = Lib.stageFreeUnits(surface, SPAWNER_POS, FREE_UNIT_COUNT)
        print("[INFO] LOW free units staged=" .. r.lowStaged)
        setPhase("low-wave")

    elseif state.phase == "low-wave" then
        local s = newAttackSquad(surface, r.seen)
        if s then
            r.seen[s.id] = true
            r.lowCount = s.memberCount
            r.lowId = s.id
            print("[PASS] low-evolution attack wave formed (id=" .. s.id ..
                " members=" .. s.memberCount .. ") at " ..
                string.format("%.1f", Lib.gameMinutes(tick - state.stagedAt)) .. "min")
            Lib.setEvolution(surface, EVO_HIGH)
            r.highArmedAt = tick
            print("[INFO] HIGH evolution armed=" .. EVO_HIGH)
            setPhase("high-stage")
        elseif Lib.phaseTimedOut(state, PHASE_LIMIT_MINUTES,
            "low: no attack squad formed", surface) then
            return
        end

    elseif state.phase == "high-stage" then
        -- Arm delay again, then stage fresh units: the low squad still holds
        -- its members, so the high formation needs new free units.
        if tick - r.highArmedAt < ARM_DELAY then
            return
        end
        r.highStaged = Lib.stageFreeUnits(surface, SPAWNER_POS, FREE_UNIT_COUNT)
        print("[INFO] HIGH free units staged=" .. r.highStaged)
        setPhase("high-wave")

    elseif state.phase == "high-wave" then
        local s = newAttackSquad(surface, r.seen)
        if s then
            r.seen[s.id] = true
            r.highCount = s.memberCount
            r.highId = s.id
            print("[PASS] high-evolution attack wave formed (id=" .. s.id ..
                " members=" .. s.memberCount .. ") at " ..
                string.format("%.1f", Lib.gameMinutes(tick - state.stagedAt)) .. "min")
            -- ASSERTION: super-linear wave-size scaling is observable —
            -- same base/staging, higher evolution -> strictly larger wave.
            if r.highCount > r.lowCount then
                finish(true, "attack wave scales with evolution (low=" ..
                    r.lowCount .. " -> high=" .. r.highCount .. ")")
            else
                finish(false, "high-evolution wave not larger: low=" ..
                    r.lowCount .. " high=" .. r.highCount)
            end
        elseif Lib.phaseTimedOut(state, PHASE_LIMIT_MINUTES,
            "high: no attack squad formed", surface) then
            return
        end
    end
end)
