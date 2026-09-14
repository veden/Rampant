--[[
    Rampant Behavior Scenario Library

    Shared scaffolding for the constructed behavior scenarios. Encodes the
    staging physics learned in engine-debugging sessions so every scenario
    is correct-by-construction instead of re-learning it per scenario:

    - Nest activity needs CONTINUOUS pollution at the CHUNK CORNER: the
      activeness check reads getPollution(chunk) = the chunk's top-left
      corner, and enemy spawners ABSORB pollution (~4/tick) — any finite
      pulse at the spawner point leaves the corner at ~0 and the nest
      flickers inactive (formation starves to ~1 attempt/2 minutes).
    - Player-structure beacons need destructible=false: squad units converge
      on them and chew 4 walls down within ~1 game-minute, wiping
      playerBaseGenerator and killing the gradient the march needs.
    - Recruit-pool size drives the kamikaze threshold: a full pool (15-30
      units) pushes the siege settler threshold past 1.0 (every settler
      rolls kamikaze — prod parity: big siege settler squads are the
      settle-and-die wavefront); small batches (2-4) roll non-kamikaze
      ~60-70% of the time.
    - The expansion-group quota's FIRST reset is scheduled at mod init from
      the DEFAULT cooldowns — scenarios needing fast builder cadence must
      override min/max_expansion_cooldown in script.on_init (scenario on_init
      runs BEFORE the mod's on_init on 2.1.x). NEVER set
      enemy_expansion.enabled=false there: the mod caches it into
      Universe.expansion and canMigrate requires it — settler formation
      dies permanently.
    - The initial chunk scan must be STABLE before staging (the scan's unit
      purge destroys units in scanned chunks).

    Scenario-side only: no mod storage access, no pcall (policy), all mod
    interaction through remote.call. The save bundles this file next to
    control.lua (make-saves); scenarios require() it as "scenario-lib".
]]

local Lib = {}

-- ============================================================
-- Generic utils
-- ============================================================

Lib.TICKS_A_MINUTE = 60 * 60

function Lib.gameMinutes(tick)
    return tick / Lib.TICKS_A_MINUTE
end

function Lib.dist(a, b)
    local dx = a.x - b.x
    local dy = a.y - b.y
    return math.sqrt(dx * dx + dy * dy)
end

function Lib.formatPos(pos)
    return string.format("%.0f,%.0f", pos.x, pos.y)
end

-- ============================================================
-- Remote-interface wrappers (the Rampant mod surface)
-- ============================================================

--- No pcall: a missing interface means the mod is not enabled and the run
-- must fail loudly.
function Lib.ready()
    return remote.call("Rampant", "isInitialized") == true
end

function Lib.metricsCounters()
    local m = remote.call("Rampant", "getMetricsSummary")
    return (m and m.counters) or {}
end

function Lib.queryBase(surface, pos)
    return remote.call("Rampant", "queryBaseAt", pos, surface)
end

function Lib.squads(surface)
    return remote.call("Rampant", "getEnemyGroups", surface) or {}
end

function Lib.setBaseState(_surface, baseId, state)
    return remote.call("Rampant", "setBaseState", baseId, state)
end

function Lib.nudgeBaseState(_surface, baseId, state)
    return remote.call("Rampant", "nudgeBaseState", baseId, state)
end

function Lib.grantUnitPoints(_surface, baseId, amount)
    return remote.call("Rampant", "grantUnitPoints", baseId, amount)
end

function Lib.setEvolution(_surface, value)
    return remote.call("Rampant", "setEvolution", value)
end

function Lib.queryPheromones(surface, pos)
    return remote.call("Rampant", "queryPheromones", pos, surface)
end

--- Builders of a wanted base-AI-state type with at least one member.
function Lib.builderSquads(surface, wantedType)
    local out = {}
    for _, s in ipairs(Lib.squads(surface)) do
        if s.isBuilder and s.memberCount and s.memberCount >= 1
            and (s.type or 0) == wantedType
        then
            out[#out + 1] = s
        end
    end
    return out
end

--- Snapshot every enemy unit-spawner position on the surface.
function Lib.spawnerSnapshot(surface)
    local out = {}
    for _, e in ipairs(surface.find_entities_filtered{
        force = "enemy", type = "unit-spawner" }) do
        if e.valid then
            out[#out + 1] = { x = e.position.x, y = e.position.y }
        end
    end
    return out
end

--- First enemy spawner NOT at any snapshotted position (a new build site).
-- Tracker-independent: during a constructed run the staged base is the
-- only possible builder, so any new spawner is its build. (The tracked-
-- squad proximity filter silently dropped sites whenever the tracker had
-- no squad — the expansion quota blocks new builders for most of a phase,
-- but their earlier builds still count.)
function Lib.newBuildSite(surface, snapshot)
    for _, e in ipairs(surface.find_entities_filtered{
        force = "enemy", type = "unit-spawner" }) do
        if e.valid then
            local known = false
            for _, k in ipairs(snapshot) do
                if Lib.dist(e.position, k) <= 16 then
                    known = true
                    break
                end
            end
            if not known then
                return { x = e.position.x, y = e.position.y }
            end
        end
    end
    return nil
end

-- ============================================================
-- Staging
-- ============================================================

--- Wait for the mod's initial chunk scan to go idle (the scan's unit purge
-- destroys units in scanned chunks — staging must happen AFTER it).
-- Call every tick until it returns true; prints the completion line once.
function Lib.waitForScanStability(_tick, cadence)
    if cadence.scanStable then
        return true
    end
    local scanned = Lib.metricsCounters()["scan.chunksScanned"] or 0
    if scanned ~= (cadence.lastScanCount or -1) then
        cadence.lastScanCount = scanned
        cadence.scanIdle = 0
    else
        cadence.scanIdle = (cadence.scanIdle or 0) + 1
    end
    if (cadence.scanIdle or 0) < 30 then
        return false  -- still scanning; wait
    end
    cadence.scanStable = true
    print("[INFO] initial scan complete (chunks=" .. scanned .. ")")
    return true
end

-- Keep the surface parameter on query/setBaseState wrappers for call-site
-- symmetry with the remote interface (the mod reads it; the wrappers pass
-- it through). luacheck: the pass-through makes some args unused.

--- First water-free position among the candidates. The movement-test
-- scenario proved water pockets freeze formations (the squad's 4-step
-- search path terminates -> path-end settle in place); scan each
-- candidate's surrounding area for water tiles.
function Lib.pickDryPosition(surface, candidates, halfExtent)
    for _, candidate in ipairs(candidates) do
        local water = surface.count_tiles_filtered{
            name = { "water", "deepwater", "water-green" },
            area = {
                { candidate.x - halfExtent, candidate.y - halfExtent },
                { candidate.x + halfExtent, candidate.y + halfExtent },
            },
            limit = 1,
        }
        if water == 0 then
            return candidate
        end
    end
    return candidates[1]  -- all wet: use the first (diagnostic will show)
end

--- Player-structure beacon at pos: wall ring + ammo-less gun-turret, all
-- indestructible (squad units chew a destructible beacon down within ~1
-- game-minute, wiping playerBaseGenerator — see file header). Options:
--   factoryProxy = true: dense assembler block behind the ring — a real
--     player factory's playerBaseGenerator regime (tens of thousands) so
--     the B gradient dominates resource-noise steering, as in real play.
--   wallCount / turretAmmo: tuning (defaults: plus-ring of 4, cleared).
-- Returns the created entities' count summary.
function Lib.buildBeacon(surface, pos, opts)
    opts = opts or {}
    local beacon = {}
    local ring = opts.wallRing or 2
    for dx = -1, 1 do
        for dy = -1, 1 do
            if math.abs(dx) + math.abs(dy) == 1 then
                beacon[#beacon + 1] = surface.create_entity{
                    name = "stone-wall",
                    position = { x = pos.x + dx * ring, y = pos.y + dy * ring },
                    force = "player",
                    raise_built = true,
                }
            end
        end
    end
    local turret = surface.create_entity{
        name = "gun-turret",
        position = { x = pos.x + 6, y = pos.y },
        force = "player",
        raise_built = true,
    }
    beacon[#beacon + 1] = turret
    for _, e in ipairs(beacon) do
        if e and e.valid then
            e.destructible = false
        end
    end
    if turret and turret.valid then
        local ammo = turret.get_inventory(defines.inventory.turret_ammo)
        if ammo then
            ammo.clear()
        end
    end
    local assemblers = 0
    if opts.factoryProxy then
        -- 100+ assemblers (LEVEL_3 = 500 each) reproduce the real player
        -- factory's B regime: a 5-building beacon's gradient (~40 per
        -- chunk-step) is drowned by resource-density noise (±2000); a
        -- factory's (thousands per chunk-step) dominates, as in real play.
        for ax = -15, 15, 3 do
            for ay = -15, 15, 3 do
                if math.abs(ax) > 4 or math.abs(ay) > 4 then
                    local e = surface.create_entity{
                        name = "assembling-machine-1",
                        position = { x = pos.x + ax, y = pos.y + ay },
                        force = "player",
                        raise_built = true,
                    }
                    if e and e.valid then
                        e.destructible = false
                        assemblers = assemblers + 1
                    end
                end
            end
        end
    end
    print("[INFO] beacon built at (" .. pos.x .. "," .. pos.y .. "): " ..
        (#beacon - 1) .. " walls + 1 turret" ..
        (opts.factoryProxy and (" + " .. assemblers .. " assemblers") or ""))
    return { walls = #beacon - 1, turret = turret ~= nil, assemblers = assemblers }
end

--- Stage free enemy units near pos so builder formations can recruit
-- (deploySquad's set_multi_command needs recruit-able units; with none,
-- the group forms empty and is destroyed). Returns the spawned count.
function Lib.stageFreeUnits(surface, pos, count)
    local spawned = 0
    for i = 1, count do
        local unit = surface.create_entity{
            name = "small-biter",
            position = {
                x = pos.x + (i % 5) * 3,
                y = pos.y + math.floor(i / 5) * 3,
            },
            force = "enemy",
        }
        if unit and unit.valid then
            spawned = spawned + 1
        end
    end
    return spawned
end

--- Keep the nest CONTINUOUSLY active: call every on_tick. Streams a small
-- pollution amount at the CHUNK CORNER (what processNestActiveness reads —
-- getPollution(chunk) is the top-left corner, NOT the spawner point) plus
-- a trickle at the spawner. cadence is a scenario-held table (cadence
-- state must survive across ticks; pass the scenario's storage or a
-- state table).
function Lib.sustainNest(surface, pos, tick, _cadence)
    if tick % 10 ~= 0 then
        return
    end
    local chunkCorner = {
        x = math.floor(pos.x / 32) * 32,
        y = math.floor(pos.y / 32) * 32,
    }
    surface.pollute(chunkCorner, 30)
    surface.pollute(pos, 2)
end

-- ============================================================
-- Diagnostics
-- ============================================================

--- Register death/damage watchers for the given entity names (prints
-- [WATCH-DIE]/[WATCH-DMG] lines with the killing cause). Registers the
-- engine events ONCE — a scenario must NOT register its own
-- on_entity_died/on_entity_damaged handlers after calling this (the last
-- registration wins).
-- 2.x-safe: entity-level e.max_health (LuaEntityPrototype is strict —
-- prototype.max_health errors on access).
function Lib.watchEntities(names, label)
    script.on_event(defines.events.on_entity_died, function(event)
        local e = event.entity
        if e and e.valid and names[e.name] then
            print("[WATCH-DIE]" .. (label or "") .. " " .. e.name .. " @" ..
                Lib.formatPos(e.position) ..
                " cause=" .. tostring(event.cause and event.cause.name or "none") ..
                " atTick=" .. game.tick)
        end
    end)
    script.on_event(defines.events.on_entity_damaged, function(event)
        local e = event.entity
        if e and e.valid and names[e.name] then
            print("[WATCH-DMG]" .. (label or "") .. " " .. e.name .. " @" ..
                Lib.formatPos(e.position) ..
                " cause=" .. tostring(event.cause and event.cause.name or "none") ..
                " final=%" .. string.format("%.0f", (e.health or 0) / (e.max_health or 1) * 100) ..
                " damage=" .. string.format("%.0f", event.damage and event.damage.amount or 0) ..
                " atTick=" .. game.tick)
        end
    end)
end

--- Sample the churn counters (squad lifecycle + formation gates + field
-- maintenance) for later Lib.churnLine comparison.
local CHURN_KEYS = {
    "squad.created",
    "squad.destroyed",
    "squad.retreated",
    "squad.settle.max_distance",
    "squad.settle.empty_resource_chunk",
    "squad.settle.path_terminated",
    "squad.formEmpty",
    "squad.formSkip.cooldown",
    "squad.formReject.settler.points",
    "squad.formReject.settler.groups",
    "squad.formReject.attacker.points",
    "squad.wanderDestroy",
    "dirty.sweepMarks",
    "dirty.chunksProcessed",
}

function Lib.churnSnapshot()
    local c = Lib.metricsCounters()
    local snap = {}
    for _, key in ipairs(CHURN_KEYS) do
        snap[key] = c[key] or 0
    end
    return snap
end

--- Item-7 standard churn line: prints the counter DELTAS since the
-- snapshot (formation throughput, squad churn, settle-rule mix, field
-- maintenance) — the recycle race and formation starvation become visible
-- in every run instead of on grep.
function Lib.churnLine(before, label)
    local after = Lib.churnSnapshot()
    local parts = {}
    for _, key in ipairs(CHURN_KEYS) do
        local delta = after[key] - before[key]
        if delta ~= 0 then
            parts[#parts + 1] = key .. "=+" .. delta
        end
    end
    print("[CHURN]" .. (label and (" " .. label) or "") ..
        (#parts > 0 and (" " .. table.concat(parts, " ")) or " (quiet)"))
    return after
end

--- Pheromone gradient probe along a position list (the [PROBE-AXIS]
-- pattern): shows the computed pheromone field AND the source generators
-- along the march corridor — discriminates dead-gradient vs never-written
-- vs water-blocked.
function Lib.pheromoneGradient(surface, positions, label)
    local row = {}
    for _, pos in ipairs(positions) do
        local p = Lib.queryPheromones(surface, pos)
        row[#row + 1] = string.format("%d,%d B=%.1f gen=%.0f",
            pos.x, pos.y,
            (p and p.base) or -1,
            (p and p.playerBaseGenerator) or -1)
    end
    print("[PROBE-GRADIENT]" .. (label and (" " .. label) or "") ..
        " " .. table.concat(row, " | "))
end

-- ============================================================
-- Phase machine
-- ============================================================

--- Print the phase transition (call after updating state.phase/
-- state.phaseStartedAt).
function Lib.setPhase(state, phase)
    state.phase = phase
    state.phaseStartedAt = game.tick
    print("PHASE: " .. phase .. " at " ..
        string.format("%.1f", Lib.gameMinutes(game.tick - state.stagedAt)) .. "min")
end

--- Print the scenario result line block and flip state.done. ALWAYS ends
-- with TESTS_COMPLETE or TESTS_FAILED — the runner polls for these.
function Lib.finish(state, passed, reason)
    print("--- Behavior Scenario Result ---")
    print("Passed: " .. (passed and 1 or 0))
    print("Failed: " .. (passed and 0 or 1))
    if reason then
        print("Detail: " .. reason)
    end
    print("Execution time: " .. game.tick .. " ticks")
    if passed then
        print("TESTS_COMPLETE")
    else
        print("TESTS_FAILED")
    end
    state.done = true
    game.speed = 1
end

--- Budget check for the current phase; returns true and prints the
-- timeout detail when the phase budget expired.
function Lib.phaseTimedOut(state, budgetMinutes, message, surface)
    if game.tick - state.phaseStartedAt <= budgetMinutes * Lib.TICKS_A_MINUTE then
        return false
    end
    Lib.finish(state, false, message .. " within " .. budgetMinutes ..
        " game-minutes")
    return true
end

return Lib
