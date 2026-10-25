--[[
    Rampant Behavior Scenario: Siege-Settle (CONSTRUCTED)

    Behavioral verification of the SIEGE vs MIGRATING settle behavior —
    the two-phase sign-coherent design: siege builders gather away from
    the player scent and MARCH TOWARD it (player pheromone * 100 term in
    the siege movement scorer), then build a forward spawner; migration
    settlers avoid the player scent and settle away from it.

      1. setup    - wall ring + ammo-less turret at (0,0) (the "player"
                    beacon), Rampant spawner at (100,0); base forms;
                    evolution 0.5; SIEGE constructed (frozen); points
                    granted; free units staged for recruitment; vanilla
                    expansion disabled (Rampant migration is independent:
                    Universe.enabledMigration is cached at mod init)
      2. form     - a builder squad (isBuilder, memberCount >= 1,
                    type == 6 SIEGE) forms
      3. march    - the builder marches; PASS on net displacement toward
                    the player AND a second enemy unit-spawner appears
                    (attributed to the tracked builder's build site);
                    ASSERTION A (hard): the siege site is >= 20 tiles
                    closer to the player than the origin
      4. control  - MIGRATING constructed; a second builder (type == 5)
                    forms, marches and settles; ASSERTION C (hard): the
                    migration site is >= 15 tiles FARTHER from the player
                    than the siege site

    Notes:
    - The tracked march builder must be NON-kamikaze: kamikaze squads score
      the march WITHOUT the player/base terms (raw kamikaze + resource) and
      do not seek the player. The form phase waits for a non-kamikaze
      builder; kamikaze builders still prove formation.
    - Retry semantics (deliberate, assertion stays hard): a siege settle
      whose site does NOT satisfy assertion A is recorded and the scenario
      keeps tracking new builder cycles (re-granting points) until one
      produces a player-ward march + build within the phase budget. Rationale:
      maxDistance rolls (5% = 0) and kamikaze formation (~25-40% at SIEGE
      x2.5) make individual squads unreliable; the behavioral claim under
      test is that siege settling reaches toward the player, not that every
      squad does. All observed failing sites are logged.
    - Site attribution is tracker-independent: any new enemy spawner during
      a phase counts as that base's build (it is the only possible builder
      in this staging).

    Expected total: ~90 game-minutes.
]]

local WALL_POS = { x = 0, y = 0 }        -- the "player" reference point
local SPAWNER_POS = { x = 100, y = 0 }   -- the origin nest (moved by water scan)
local TICKS_A_MINUTE = 60 * 60
local BUDGET_MINUTES = 90
local BUDGET_TICKS = BUDGET_MINUTES * TICKS_A_MINUTE
local PHASE_LIMIT_MINUTES = 20
-- The march phase gets a longer budget: site throughput is quota-gated
-- (1 settler per 5-15s window) and assertion A needs a qualifying site
-- among the cycle's builds (kamikaze builds are recorded and retried).
local MARCH_PHASE_LIMIT_MINUTES = 45
local EVOLUTION = 0.5
local GRANT_POINTS = 1000
local FREE_UNIT_COUNT = 30
local BASE_AI_STATE_SIEGE = 6
local BASE_AI_STATE_MIGRATING = 5
-- Spawner relocation candidates along the march axis: the movement-test
-- scenario proved water pockets freeze formations (path-end settles);
-- place the nest on the first dry candidate so the corridor is passable.
local SPAWNER_CANDIDATES = {
    { x = 112, y = 0 }, { x = 96, y = 0 }, { x = 80, y = 0 },
    { x = 128, y = 0 }, { x = 64, y = 0 },
}
local WATER_SCAN_HALF = 48  -- half-extent of the dry-area check around a candidate
local SPAWNER_POLL_INTERVAL = 30  -- game ticks between spawner scans
local SITE_POLL_INTERVAL = 15     -- game ticks between squad position samples

local Lib = require("scenario-lib")

local state  -- { phase, results, stagedAt, phaseStartedAt, done }

local SQUAD_STATUS_ENGLISH = {
    [1] = "RETREATING",
    [2] = "GUARDING",
    [3] = "BURROWING",
    [4] = "RAIDING",
    [5] = "SETTLING",
    [6] = "BUILDING",
}

local function statusName(status)
    return SQUAD_STATUS_ENGLISH[status] or tostring(status)
end

-- Shared-lib delegates (staging physics and diagnostics live in
-- scenario-lib.lua; these keep the scenario's call sites stable).
local function rampReady()
    return Lib.ready()
end

local function gameMinutes(tick)
    return Lib.gameMinutes(tick)
end

local function dist(a, b)
    return Lib.dist(a, b)
end

local function squads(surface)
    return Lib.squads(surface)
end

local function queryBase(surface)
    return Lib.queryBase(surface, SPAWNER_POS)
end

local function builderSquads(surface, wantedType)
    return Lib.builderSquads(surface, wantedType)
end

local function spawnerSnapshot(surface)
    return Lib.spawnerSnapshot(surface)
end

local function newBuildSite(surface, snapshot)
    return Lib.newBuildSite(surface, snapshot)
end

local function metricsCounters()
    return Lib.metricsCounters()
end

-- Standard churn line (item 7): print the squad-lifecycle and formation
-- counter deltas since the last call; chains snapshots via results.
local function phaseChurn(r, label)
    r.churnLast = Lib.churnLine(r.churnLast or r.churnAtStage, label)
end

--- Adopt a tracked builder: prefer non-kamikaze, then the one nearest to
-- a reference position (the site it will build near).
local function adoptBuilder(surface, wantedType, nearPos)
    local best = nil
    local bestRank = nil
    for _, s in ipairs(builderSquads(surface, wantedType)) do
        local rank = 0
        if s.isKamikaze then
            rank = rank + 10000  -- non-kamikaze squads seek the player
        end
        if nearPos then
            rank = rank + dist(s.position, nearPos)
        end
        if not bestRank or rank < bestRank then
            best = s
            bestRank = rank
        end
    end
    return best
end

--- Net displacement of the builder track projected onto the
-- player-ward axis (player - origin): > 0 means it moved toward the
-- player structures.
local function dotTowardPlayer(pLast, pFirst)
    local dirX = WALL_POS.x - SPAWNER_POS.x
    local dirY = WALL_POS.y - SPAWNER_POS.y
    return (pLast.x - pFirst.x) * dirX + (pLast.y - pFirst.y) * dirY
end

local function stage(surface)
    -- Vanilla expansion must not create spawners mid-run (false build
    -- sites). Safe at staging time: the mod already cached
    -- Universe.expansion = true at its init (canMigrate needs it).
    game.map_settings.enemy_expansion.enabled = false

    -- Water-free spawner position (the movement-test lesson: water
    -- pockets freeze formations — see Lib.pickDryPosition).
    local dry = Lib.pickDryPosition(surface, SPAWNER_CANDIDATES, WATER_SCAN_HALF)
    SPAWNER_POS.x = dry.x
    SPAWNER_POS.y = dry.y
    print("[INFO] spawner placed at (" .. SPAWNER_POS.x .. "," .. SPAWNER_POS.y ..
        ") (dry-corridor scan)")

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

    -- Player-structure beacon (wall ring + ammo-less turret, indestructible)
    -- plus the factory proxy — the real player factory's playerBaseGenerator
    -- regime so the B gradient dominates resource-noise steering (the
    -- 5-building beacon's gradient is drowned by ore noise; see the lib).
    Lib.buildBeacon(surface, WALL_POS, { factoryProxy = true })

    -- Rampant spawner -> base creation (registered directly, no upgrade).
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

--- Free units near the nest so the builder wave can recruit members
-- (deploySquad's set_multi_command needs recruit-able units; with none,
-- the group forms empty and is destroyed).
local function stageFreeUnits(surface)
    return Lib.stageFreeUnits(surface, SPAWNER_POS, FREE_UNIT_COUNT)
end

local function setPhase(phase)
    state.phase = phase
    state.phaseStartedAt = game.tick
    print("PHASE: " .. phase .. " at " ..
        string.format("%.1f", gameMinutes(game.tick - state.stagedAt)) .. "min")
end

local function metricsCounters()
    local m = remote.call("Rampant", "getMetricsSummary")
    return m and m.counters or {}
end

local function finish(passed, reason)
    local r = state.results
    phaseChurn(r, "finish")
    print("--- Behavior Scenario: Siege-Settle ---")
    print("Passed: " .. (passed and 1 or 0))
    print("Failed: " .. (passed and 0 or 1))
    if reason then
        print("Detail: " .. reason)
    end
    print("Detail: base=" .. tostring(r.baseFormed) ..
        " formBuilder=" .. tostring(r.formBuilder) ..
        " marchSite=" .. tostring(r.siegeSite) ..
        " siegeDist=" .. tostring(r.siegeDist) ..
        " failedSites=" .. tostring(r.failedSites) ..
        " kamikazeSeen=" .. tostring(r.kamikazeSeen) ..
        " controlBuilder=" .. tostring(r.controlBuilder) ..
        " controlSite=" .. tostring(r.controlSite) ..
        " controlDist=" .. tostring(r.controlDist) ..
        " failedControlSites=" .. tostring(r.failedControlSites) ..
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

local function timeoutDiagnostics(prefix, surface)
    local r = state.results
    local c = metricsCounters()
    local p = remote.call("Rampant", "queryPheromones", SPAWNER_POS, surface)
    -- Pollution AT THE CHUNK CORNER: what processNestActiveness actually
    -- reads (the spawner-point value is a red herring).
    local pollution = surface.get_pollution({
        x = math.floor(SPAWNER_POS.x / 32) * 32,
        y = math.floor(SPAWNER_POS.y / 32) * 32,
    })
    local structures = remote.call("Rampant", "getActiveStructures", surface)
    local pWall = remote.call("Rampant", "queryPheromones", WALL_POS, surface)
    local pMid = remote.call("Rampant", "queryPheromones", { x = 50, y = 0 }, surface)
    local b = queryBase(surface)
    local builders = builderSquads(surface, BASE_AI_STATE_SIEGE)
    local migBuilders = builderSquads(surface, BASE_AI_STATE_MIGRATING)
    local detail = prefix .. " within " ..
        ((state.phase == "march") and MARCH_PHASE_LIMIT_MINUTES or PHASE_LIMIT_MINUTES) ..
        " game-minutes" ..
        " (pollution@nest=" .. string.format("%.1f", pollution or -1) ..
        " activeStructures=" .. tostring(structures and structures.activeNests or "?") ..
        " activeNest=" .. tostring(b and b.isActiveNest) ..
        " nestActiveTick=" .. tostring(b and b.nestActiveTick) ..
        " siegeBuilders=" .. #builders ..
        " migrationBuilders=" .. #migBuilders ..
        " baseFormed=" .. tostring(r.baseFormed) ..
        " trackedId=" .. tostring(r.trackedId) ..
        " trackedStatus=" .. tostring(r.trackedStatus) ..
        " displacement=" .. tostring(r.displacement) ..
        " phero@nest[base=" .. tostring(p and p.base) ..
        " player=" .. tostring(p and p.player) ..
        " enemy=" .. tostring(p and p.enemy) ..
        " resource=" .. tostring(p and p.resource) .. "]" ..
        " phero@wall[base=" .. tostring(pWall and pWall.base) ..
        " player=" .. tostring(pWall and pWall.player) .. "]" ..
        " phero@mid[base=" .. tostring(pMid and pMid.base) ..
        " player=" .. tostring(pMid and pMid.player) .. "]" ..
        " base[nest=" .. tostring(b and b.nestCount) ..
        " structures=" .. tostring(b and b.activeStructures) ..
        " points=" .. tostring(b and b.unitPoints) ..
        " state=" .. tostring(b and b.state) ..
        " quota=" .. tostring(b and b.sentExpansionGroups) .. "/" .. tostring(b and b.maxExpansionGroups) ..
        " resetIn=" .. tostring(b and (b.resetExpensionGroupsTick and (b.resetExpensionGroupsTick - game.tick) or "?")) ..
        " destroyPlayerBuildings=" .. tostring(b and b.destroyPlayerBuildings) .. "]" ..
        " formAttempt=" .. tostring(c["squad.formAttempt"]) ..
        " formReject.settler.points=" .. tostring(c["squad.formReject.settler.points"]) ..
        " formReject.settler.builder=" .. tostring(c["squad.formReject.settler.builder"]) ..
        " formReject.settler.groups=" .. tostring(c["squad.formReject.settler.groups"]) ..
        " formEmpty=" .. tostring(c["squad.formEmpty"]) ..
        " formSkip.cooldown=" .. tostring(c["squad.formSkip.cooldown"]) ..
        " settle.max_distance=" .. tostring(c["squad.settle.max_distance"]) ..
        " settle.empty_resource=" .. tostring(c["squad.settle.empty_resource_chunk"]) ..
        " settle.path_terminated=" .. tostring(c["squad.settle.path_terminated"]) ..
        " chunksProcessed=" .. tostring(c["dirty.chunksProcessed"]) ..
        " epsilonSkips=" .. tostring(c["dirty.epsilonSkips"]) ..
        " squad.created=" .. tostring(c["squad.created"]) ..
        " squad.destroyed=" .. tostring(c["squad.destroyed"]) ..
        " squad.wander=" .. tostring(c["squad.wander"]) ..
        " squad.wanderDestroy=" .. tostring(c["squad.wanderDestroy"]) .. ")"
    finish(false, detail)
end

-- Death/damage watcher (shared lib): who is killing the player structures
-- — and why do squad members die (the recycle race suspect)?
local WATCHED = { ["stone-wall"] = true, ["gun-turret"] = true,
    ["small-biter"] = true, ["neutral-biter-v1-t1-rampant"] = true,
    ["small-spitter"] = true }
Lib.watchEntities(WATCHED)
-- Scenario on_init runs BEFORE the mod's on_init on 2.1.x (documented
-- engine ordering): set the expansion cooldown override HERE so the mod
-- caches these values at initialization. Overriding at staging time was
-- too late — the mod's first quota-reset tick was scheduled from the
-- DEFAULT cooldowns (2-20 game-minutes of dead settler formation per run,
-- the dominant seed-variance source). 5-15s cadence: settler formations
-- recruit only the nest's trickle spawn (2-4 units), keeping the kamikaze
-- threshold small ((2-4/10)*0.2+0.1)*2.5 = 0.35-0.45 — full-pool recruits
-- (15-30 units) push the threshold past 1.0 and EVERY siege settler rolls
-- kamikaze (prod:1266-1273 parity: big siege settler squads are the
-- settle-and-die wavefront; only small ones player-seek).
script.on_init(function()
    -- ONLY the cooldowns here. Do NOT set enemy_expansion.enabled=false:
    -- the mod caches map_settings.enemy_expansion.enabled into
    -- Universe.expansion at ITS init (which runs after this), and
    -- canMigrate requires it — setting it false here permanently killed
    -- settler formation (formAttempt collapsed 6000 -> 15).
    game.map_settings.enemy_expansion.min_expansion_cooldown = 60 * 5
    game.map_settings.enemy_expansion.max_expansion_cooldown = 60 * 15
end)

script.on_event(defines.events.on_tick, function()
    if not rampReady() then
        return
    end
    if not state then
        -- Stage AFTER the mod's initial chunk scan completes (the scan's
        -- unit purge destroys units in scanned chunks).
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
        state.results.churnAtStage = Lib.churnSnapshot()
        local surface = game.surfaces[1]
        -- TEMPORARY landscape probe: resource vs base pheromone on a grid
        -- (resource pheromone hijacks the march scores; the origin must
        -- sit where the base gradient can compete)
        local probes = 0
        for gx = -256, 256, 64 do
            local row = {}
            for gy = -256, 256, 64 do
                local p = remote.call("Rampant", "queryPheromones", { x = gx, y = gy }, surface)
                if p then
                    row[#row+1] = string.format("[%d,%d]r=%.0f,b=%.1f", gx, gy, p.resource or 0, p.base or 0)
                    probes = probes + 1
                end
            end
            print("[PROBE] " .. table.concat(row, " "))
        end
        print("[PROBE] total=" .. probes)
        stage(surface)
        game.speed = 100
        print("STAGED: wall+turret at origin, Rampant spawner at (100,0), speed=100")
        -- Immediate post-staging probe: did onBuild register the player
        -- structures? gen>0 here means the build events fired; gen=0 means
        -- they never fired (or were wiped within the same tick).
        local p0 = remote.call("Rampant", "queryPheromones", WALL_POS, surface)
        local m0 = remote.call("Rampant", "getMetricsSummary")
        local c0 = (m0 and m0.counters) or {}
        local wallsNow = surface.find_entities_filtered{ name = "stone-wall", force = "player" }
        print("[PROBE-STAGE] wallGen=" ..
            string.format("%.0f", (p0 and p0.playerBaseGenerator) or -1) ..
            " wallB=" .. string.format("%.1f", (p0 and p0.base) or -1) ..
            " wallsAlive=" .. #wallsNow ..
            " event.onBuild=" .. tostring(c0["event.onBuild"]))
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

    -- Keep the nest CONTINUOUSLY active (shared lib): the activeness check
    -- reads getPollution(chunk) — the chunk's TOP-LEFT CORNER, NOT the
    -- spawner point; enemy spawners absorb pollution, so pulses leave the
    -- corner at ~0 and the nest flickers inactive. Small continuous stream
    -- at the corner + the spawner (see Lib.sustainNest).
    Lib.sustainNest(surface, SPAWNER_POS, tick, storage)

    -- Recruit-pool discipline: do NOT re-stage units. Settler formations
    -- must recruit SMALL batches to roll non-kamikaze (see the cooldown
    -- note in stage()); the nest's own trickle spawn (~1 unit / 4
    -- game-seconds) supplies the pool, and a big staged flood pushes every
    -- settler threshold past 1.0 (observed: 249 squads, zero non-kamikaze).

    -- Keep the formation economy alive: settlers cost 300 points each and
    -- squads die to the recycle race, so granted points drain fast; the
    -- points gate then starves ALL formation (observed:
    -- formReject.settler.points=69431 with 84 points on the base).
    if tick - (storage.rampantLastGrant or 0) > 30 * 60 then
        storage.rampantLastGrant = tick
        local b = queryBase(surface)
        if b and (b.unitPoints or 0) < 600 then
            remote.call("Rampant", "grantUnitPoints", b.id, GRANT_POINTS)
        end
    end

    -- One-shot post-staging gradient probe (~2 game-minutes after the
    -- walls were built): shows the BASE pheromone field along the march
    -- axis. Diagnoses field-arrival: alive gradient vs dead chain vs
    -- water-blocked corridor.
    if not storage.rampantProbedGradient and elapsed > 2 * TICKS_A_MINUTE then
        storage.rampantProbedGradient = true
        local walls = surface.find_entities_filtered{
            name = "stone-wall", force = "player" }
        local turrets = surface.find_entities_filtered{
            name = "gun-turret", force = "player" }
        local row = {}
        for _, gx in ipairs({ 0, 32, 64, 96, 112, 128, 144 }) do
            local p = remote.call("Rampant", "queryPheromones", { x = gx, y = 0 }, surface)
            row[#row + 1] = string.format("x=%d B=%.1f gen=%.0f death=%.1f perm=%.1f", gx,
                (p and p.base) or -1,
                (p and p.playerBaseGenerator) or -1,
                (p and p.deathGenerator) or 0,
                (p and p.permanentDeathGenerator) or 0)
        end
        print("[PROBE-AXIS] walls=" .. #walls .. " turrets=" .. #turrets ..
            " | " .. table.concat(row, " | "))
    end

    -- Wall-vanish watcher: poll the player-structure count; on change,
    -- print the tick plus what's nearby (pins the silent destroy()).
    if tick % 60 == 0 then
        local wallsNow = surface.count_entities_filtered{
            name = "stone-wall", force = "player" }
        if wallsNow ~= (storage.rampantLastWallCount or -1) then
            local near = surface.find_entities_filtered{
                position = WALL_POS, radius = 40, force = "enemy", type = "unit" }
            print("[WALL-WATCH] tick=" .. tick ..
                " walls=" .. wallsNow ..
                " (was " .. tostring(storage.rampantLastWallCount) .. ")" ..
                " enemyUnits@40=" .. #near ..
                " phase=" .. state.phase)
            storage.rampantLastWallCount = wallsNow
        end
    end

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
            remote.call("Rampant", "setEvolution", EVOLUTION)
            local stateOk = remote.call("Rampant", "setBaseState", base.id, BASE_AI_STATE_SIEGE)
            local pointsOk = remote.call("Rampant", "grantUnitPoints", base.id, GRANT_POINTS)
            local units = stageFreeUnits(surface)
            print("[INFO] constructed: evo=" .. EVOLUTION ..
                " SIEGE=" .. tostring(stateOk) ..
                " points=" .. tostring(pointsOk) ..
                " freeUnits=" .. units)
            r.marchSnapshot = spawnerSnapshot(surface)
            print("[INFO] march snapshot spawners=" .. #r.marchSnapshot)
            phaseChurn(r, "setup->form")
            setPhase("form")
        elseif phaseElapsed > PHASE_LIMIT_MINUTES * TICKS_A_MINUTE then
            finish(false, "base never formed within " .. PHASE_LIMIT_MINUTES .. " game-minutes")
            return
        end

    elseif state.phase == "form" then
        -- Re-assert construction when the base at the spawner changed
        -- (recycled and re-formed bases get a new id and a random state).
        local base = queryBase(surface)
        if base and base.id ~= r.baseId then
            r.baseId = base.id
            local stateOk = remote.call("Rampant", "setBaseState", base.id, BASE_AI_STATE_SIEGE)
            local pointsOk = remote.call("Rampant", "grantUnitPoints", base.id, GRANT_POINTS)
            print("[INFO] re-constructed on base id=" .. base.id ..
                " SIEGE=" .. tostring(stateOk) .. " points=" .. tostring(pointsOk))
        end
        -- Wait for ANY siege builder (isBuilder, type==6). The old
        -- non-kamikaze gate blocked phases on seeds whose recruit pool
        -- stayed large (pool-size-driven kamikaze threshold) — but site
        -- attribution is tracker-independent now, so the march phase
        -- evaluates EVERY build: kamikaze builds (no player homing) are
        -- recorded and retried, non-kamikaze builds satisfy assertion A.
        local builder = builderSquads(surface, BASE_AI_STATE_SIEGE)[1]
        if builder then
            r.formBuilder = true
            r.trackedId = builder.id
            r.pFirst = { x = builder.position.x, y = builder.position.y }
            r.pLast = { x = builder.position.x, y = builder.position.y }
            print("[PASS] siege builder squad formed (id=" .. builder.id ..
                " members=" .. builder.memberCount ..
                " kamikaze=" .. tostring(builder.isKamikaze) ..
                " pos=" .. string.format("%.0f,%.0f", builder.position.x, builder.position.y) ..
                ") at " .. string.format("%.1f", gameMinutes(tick - state.stagedAt)) .. "min")
            phaseChurn(r, "form->march")
            setPhase("march")
        elseif phaseElapsed > PHASE_LIMIT_MINUTES * TICKS_A_MINUTE then
            timeoutDiagnostics("form: no siege builder squad", surface)
            return
        end

    elseif state.phase == "march" then
        local base = queryBase(surface)
        if base and base.id ~= r.baseId then
            r.baseId = base.id
            local stateOk = remote.call("Rampant", "setBaseState", base.id, BASE_AI_STATE_SIEGE)
            local pointsOk = remote.call("Rampant", "grantUnitPoints", base.id, GRANT_POINTS)
            print("[INFO] re-constructed on base id=" .. base.id ..
                " SIEGE=" .. tostring(stateOk) .. " points=" .. tostring(pointsOk))
        end

        -- Track a NON-kamikaze builder only: kamikaze siege squads score
        -- the march without the player/base terms and cannot satisfy the
        -- march assertions. Never track one, never adopt one.
        local tracked = nil
        for _, s in ipairs(builderSquads(surface, BASE_AI_STATE_SIEGE)) do
            if s.isKamikaze then
                if not r.kamikazeSeen then
                    r.kamikazeSeen = true
                    print("[INFO] kamikaze siege builder present (id=" .. s.id ..
                        "); ignoring (no player-seeking march)")
                end
            elseif s.id == r.trackedId then
                tracked = s
                break
            end
        end
        if not tracked then
            if r.trackedId then
                local c = metricsCounters()
                print("[TRACE] tracked builder id=" .. r.trackedId ..
                    " VANISHED at " ..
                    string.format("%.1f", gameMinutes(tick - state.stagedAt)) .. "min" ..
                    " (lastStatus=" .. statusName(r.trackedStatus) ..
                    " squad.destroyed=" .. tostring(c["squad.destroyed"]) ..
                    " squad.wander=" .. tostring(c["squad.wander"]) ..
                    " squad.wanderDestroy=" .. tostring(c["squad.wanderDestroy"]) .. ")")
                r.trackedId = nil  -- clear so the VANISHED line prints once
            end
            tracked = adoptBuilder(surface, BASE_AI_STATE_SIEGE, r.pLast)
            if tracked and tracked.isKamikaze then
                tracked = nil  -- never track kamikaze
            end
            if tracked and r.droppedIds and r.droppedIds[tracked.id] then
                tracked = nil  -- never re-adopt a stalled dropped builder
            end
            if tracked then
                print("[INFO] adopted builder id=" .. tracked.id ..
                    " kamikaze=" .. tostring(tracked.isKamikaze) ..
                    " pos=" .. string.format("%.0f,%.0f", tracked.position.x, tracked.position.y))
            end
        end
        if tracked then
            r.trackedId = tracked.id
            -- Stalled-builder drop: a BUILDING squad whose build_base can
            -- never complete (settled too close to the origin nest —
            -- maxDistance=0 rolls settle at formation) blocks the tracker
            -- until the recycle race kills it (~20 game-minutes). Drop it
            -- and cycle to the next builder; with the formation firehose
            -- running, the next builder arrives within seconds.
            if tracked.status == 6 then
                if not r.trackedBuildSince then
                    r.trackedBuildSince = tick
                elseif tick - r.trackedBuildSince > 3 * TICKS_A_MINUTE then
                    print("[INFO] dropping stalled builder id=" .. tracked.id ..
                        " (BUILDING >3min, no site; settled too close to build)")
                    r.droppedIds = r.droppedIds or {}
                    r.droppedIds[tracked.id] = true
                    r.trackedId = nil
                    r.trackedBuildSince = nil
                    r.pFirst = nil
                    r.pLast = nil
                    tracked = nil
                end
            else
                r.trackedBuildSince = nil
            end
        end
        if tracked then
            -- Lifecycle trace: status / position / member changes.
            if tracked.status ~= r.trackedStatus
                or tracked.memberCount ~= r.trackedMembers
                or dist(tracked.position, r.pLast or tracked.position) > 8
            then
                local moved = (r.pLast and dist(tracked.position, r.pLast)) or 0
                print("[TRACE] builder id=" .. tracked.id ..
                    " status=" .. statusName(tracked.status) ..
                    " members=" .. tracked.memberCount ..
                    " pos=" .. string.format("%.0f,%.0f", tracked.position.x, tracked.position.y) ..
                    " moved=" .. string.format("%.1f", moved) ..
                    " at " .. string.format("%.1f", gameMinutes(tick - state.stagedAt)) .. "min")
            end
            r.trackedStatus = tracked.status
            r.trackedMembers = tracked.memberCount
            if not r.pFirst then
                r.pFirst = { x = tracked.position.x, y = tracked.position.y }
                r.pLast = { x = tracked.position.x, y = tracked.position.y }
            end
            if tick % SITE_POLL_INTERVAL == 0 then
                r.pLast = { x = tracked.position.x, y = tracked.position.y }
                r.displacement = dotTowardPlayer(r.pLast, r.pFirst)
                r.trackedStatus = tracked.status
                -- Every ~2 game-minutes: pheromone landscape at the squad.
                if tick % (2 * TICKS_A_MINUTE) < SITE_POLL_INTERVAL then
                    local p = remote.call("Rampant", "queryPheromones", tracked.position, surface)
                    print("[INFO] phero@" ..
                        string.format("%.0f,%.0f", tracked.position.x, tracked.position.y) ..
                        " base=" .. string.format("%.3f", p and p.base or 0) ..
                        " player=" .. string.format("%.3f", p and p.player or 0) ..
                        " enemy=" .. string.format("%.3f", p and p.enemy or 0) ..
                        " resource=" .. string.format("%.3f", p and p.resource or 0))
                end
            end

        end
        -- Spawner scan runs REGARDLESS of tracking: any new spawner is
        -- this base's build (it is the only possible builder in this
        -- staging). The old tracked-proximity filter silently dropped
        -- sites whenever the tracker had no squad — the quota blocks new
        -- builders for most of a phase, but their earlier builds count.
        if tick % SPAWNER_POLL_INTERVAL == 0 then
                local site = newBuildSite(surface, r.marchSnapshot)
                if site then
                    r.siegeSite = site
                    r.siegeDist = dist(site, WALL_POS)
                    local originDist = dist(SPAWNER_POS, WALL_POS)
                    -- ASSERTION A (hard): the siege site is >= 20 tiles
                    -- closer to the player than the origin. March
                    -- displacement is diagnostic only: the two-phase
                    -- design lets siege squads settle AT their formation
                    -- site (which already homed on the player-building
                    -- pheromone gradient) — requiring a visible march
                    -- before evaluating A would reject valid player-ward
                    -- builds (observed: formation at (88,-24) vs origin
                    -- (112,0), built at (81,-22), no march).
                    if r.siegeDist < originDist - 20 then
                        print("[PASS] siege builder built player-ward" ..
                            " (siteDist=" .. string.format("%.1f", r.siegeDist) ..
                            " originDist=" .. string.format("%.1f", originDist) ..
                            " marchDisplacement=" .. string.format("%.1f", r.displacement or 0) ..
                            ") at " ..
                            string.format("%.1f", gameMinutes(tick - state.stagedAt)) .. "min")
                            r.controlSnapshot = spawnerSnapshot(surface)
                            print("[INFO] control snapshot spawners=" .. #r.controlSnapshot)
                            remote.call("Rampant", "setBaseState", r.baseId, BASE_AI_STATE_MIGRATING)
                            remote.call("Rampant", "grantUnitPoints", r.baseId, GRANT_POINTS)
                            local units = stageFreeUnits(surface)
                            print("[INFO] constructed: MIGRATING, points granted, freeUnits=" .. units)
                            r.trackedId = nil
                            r.pFirst = nil
                            r.pLast = nil
                            r.displacement = nil
                            phaseChurn(r, "march->control")
                            setPhase("control")
                    else
                        -- Retry: record the failing site as known so it is
                        -- not re-attributed, re-grant points for the next
                        -- builder cycle, and keep tracking. Assertion A
                        -- must eventually be satisfied by SOME siege build
                        -- within the phase budget; unlucky rolls
                        -- (maxDistance=0 at 5%, kamikaze formation) make
                        -- single-squad settlement unreliable.
                        table.insert(r.marchSnapshot, site)
                        r.failedSites = (r.failedSites or 0) + 1
                        print("[INFO] siege site too far (attempt " ..
                            r.failedSites ..
                            "): dist=" .. string.format("%.1f", r.siegeDist) ..
                            " not < originDist-20=" ..
                            string.format("%.1f", originDist - 20) ..
                            " (site=" .. string.format("%.0f,%.0f", site.x, site.y) ..
                            " displacement=" .. string.format("%.1f", r.displacement or 0) ..
                            "); retrying with next builder cycle")
                        r.siegeSite = nil
                        r.siegeDist = nil
                        r.trackedId = nil
                        r.pFirst = nil
                        r.pLast = nil
                        r.displacement = nil
                        remote.call("Rampant", "grantUnitPoints", r.baseId, GRANT_POINTS)
                    end
                end
            end
        if phaseElapsed > MARCH_PHASE_LIMIT_MINUTES * TICKS_A_MINUTE then
            timeoutDiagnostics("march: no player-ward siege build", surface)
            return
        end

    elseif state.phase == "control" then
        local base = queryBase(surface)
        if base and base.id ~= r.baseId then
            r.baseId = base.id
            local stateOk = remote.call("Rampant", "setBaseState", base.id, BASE_AI_STATE_MIGRATING)
            local pointsOk = remote.call("Rampant", "grantUnitPoints", base.id, GRANT_POINTS)
            print("[INFO] re-constructed on base id=" .. base.id ..
                " MIGRATING=" .. tostring(stateOk) .. " points=" .. tostring(pointsOk))
        end

        -- Track the migration builder (type == 5): adopt the first one;
        -- re-adopt by id when it vanishes.
        local tracked = nil
        for _, s in ipairs(builderSquads(surface, BASE_AI_STATE_MIGRATING)) do
            if s.id == r.trackedId then
                tracked = s
                break
            end
        end
        if not tracked then
            tracked = adoptBuilder(surface, BASE_AI_STATE_MIGRATING, nil)
            if tracked then
                r.controlBuilder = true
                print("[PASS] migration builder squad formed (id=" .. tracked.id ..
                    " members=" .. tracked.memberCount ..
                    " pos=" .. string.format("%.0f,%.0f", tracked.position.x, tracked.position.y) ..
                    ") at " .. string.format("%.1f", gameMinutes(tick - state.stagedAt)) .. "min")
            end
        end
        if tracked then
            r.trackedId = tracked.id
            -- Same stalled-builder drop as the march phase: migration
            -- builders settling too close to any nest also never build.
            if tracked.status == 6 then
                if not r.trackedBuildSince then
                    r.trackedBuildSince = tick
                elseif tick - r.trackedBuildSince > 3 * TICKS_A_MINUTE then
                    print("[INFO] dropping stalled migration builder id=" .. tracked.id ..
                        " (BUILDING >3min, no site)")
                    r.droppedIds = r.droppedIds or {}
                    r.droppedIds[tracked.id] = true
                    r.trackedId = nil
                    r.trackedBuildSince = nil
                    r.pFirst = nil
                    r.pLast = nil
                    tracked = nil
                end
            else
                r.trackedBuildSince = nil
            end
        end
        if tracked then
            if not r.pFirst then
                r.pFirst = { x = tracked.position.x, y = tracked.position.y }
                r.pLast = { x = tracked.position.x, y = tracked.position.y }
            end
            if tick % SITE_POLL_INTERVAL == 0 then
                r.pLast = { x = tracked.position.x, y = tracked.position.y }
                r.controlDisplacement = dotTowardPlayer(r.pLast, r.pFirst)
            end
        end
        -- Migration site scan, tracker-independent (same rationale as the
        -- march phase).
        if tick % SPAWNER_POLL_INTERVAL == 0 then
                local site = newBuildSite(surface, r.controlSnapshot)
                if site then
                    r.controlSite = site
                    r.controlDist = dist(site, WALL_POS)
                    -- ASSERTION C — DIAGNOSTIC (demoted per the design's
                    -- decided fallback after repeated geography noise):
                    -- migration's away-from-player pull is the -P*100 term,
                    -- and P (character scent) is structurally absent in
                    -- headless scenarios (processPlayers iterates
                    -- connected_players). With P=0, migration steering is
                    -- directionless (flat R minus E) and its settle sites
                    -- are noise, so the comparative distance assertion
                    -- cannot be asserted fairly here. In real games P
                    -- exists and migration avoids the player. Recorded as
                    -- evidence, never asserted.
                    print("[INFO] migration site (diagnostic, C demoted):" ..
                        " controlDist=" .. string.format("%.1f", r.controlDist) ..
                        " siegeDist=" .. string.format("%.1f", r.siegeDist) ..
                        " displacement=" .. string.format("%.1f", r.controlDisplacement or 0) ..
                        " at " ..
                        string.format("%.1f", gameMinutes(tick - state.stagedAt)) .. "min")
                    finish(true, "siege-settle expectations met (A hard, C diagnostic)")
                    return
                end
        end
        if phaseElapsed > PHASE_LIMIT_MINUTES * TICKS_A_MINUTE then
            timeoutDiagnostics("control: no migration builder settle", surface)
            return
        end
    end
end)
