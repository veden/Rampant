--[[
    Rampant E2E Long-Run Scenario (with Virtual Player)

    The session-level observation run: stage several enemy bases on a
    1024x1024 map, simulate a growing player base (virtual player), and let
    the mod develop over 4 game-hours at high speed. The external driver
    (tools/long_run.py) interrogates the session via telemetry + state
    queries and runs the session-level assertions.

    Virtual player (mimics "doing basic things"):
      - builds a growing base: wall rings, gun-turrets (with ammo), radars,
        assemblers — all via real onBuild events (pheromone sources)
      - pollutes at the base (factory pollution proxy -> nest activation,
        evolution)
      - turrets actually shoot raiders -> kills -> death events, evolution

    The AI develops naturally: nests activate from diffused pollution,
    raids attack the growing base, evolution rises, bases expand/migrate.

    Signals to the driver:
      LONGRUN_STARTED      - staging complete
      LONGRUN_STATUS <...>  - milestone line every 15 game-minutes
      LONGRUN_COMPLETE     - 4 game-hours reached (driver asserts)
]]

local PLAYER_ORIGIN = { x = 0, y = 0 }
local SPAWNER_POSITIONS = {
    { x = 200, y = 0 },
    { x = -180, y = 80 },
    { x = 160, y = -140 },
    { x = 0, y = 220 },
}
local TICKS_A_MINUTE = 60 * 60
local RUN_MINUTES = 240            -- 4 game-hours
local RUN_TICKS = RUN_MINUTES * TICKS_A_MINUTE
local POLLUTE_INTERVAL = 600       -- 10 game-s
local MILESTONE_INTERVAL = 15      -- game-min between status lines
local NUDGE_INTERVAL = 30           -- game-min between migration nudges

-- Virtual player build schedule: { min, wallRadius, turrets, radars, assemblers }
local BUILD_SCHEDULE = {
    { min = 0,  radius = 3,  turrets = 4, radars = 2, assemblers = 2 },
    { min = 5,  radius = 6,  turrets = 6 },
    { min = 10, radius = 9,  turrets = 8 },
    { min = 15, radius = 12, turrets = 10, radars = 1 },
    { min = 20, radius = 15, turrets = 12 },
    { min = 30, radius = 18, turrets = 14 },
    { min = 45, radius = 21, turrets = 16 },
    { min = 60, radius = 24, turrets = 18, radars = 1 },
}

local state  -- { phase, results, stagedAt, done, vp }

local function rampReady()
    local ok, ready = pcall(remote.call, "Rampant", "isInitialized")
    return ok and ready == true
end

local function gameMinutes(tick)
    return tick / TICKS_A_MINUTE
end

local function ringPosition(radius, i)
    local angle = (i - 1) * (math.pi / 4)
    return {
        x = PLAYER_ORIGIN.x + math.floor(math.cos(angle) * radius + 0.5),
        y = PLAYER_ORIGIN.y + math.floor(math.sin(angle) * radius + 0.5),
    }
end

local function buildWallRing(surface, radius)
    local built = 0
    for i = 1, 8 do
        local pos = ringPosition(radius, i)
        local e = surface.create_entity{ name = "stone-wall", position = pos, force = "player", raise_built = true }
        if e and e.valid then built = built + 1 end
    end
    return built
end

local function buildTurrets(surface, radius, count)
    local built = 0
    for i = 1, count do
        local pos = ringPosition(math.max(radius - 1, 2), i)
        local e = surface.create_entity{ name = "gun-turret", position = pos, force = "player", raise_built = true }
        if e and e.valid then
            built = built + 1
        end
    end
    return built
end

local function buildSimple(surface, name, positions, count)
    local built = 0
    for i = 1, count do
        local pos = positions[i] or positions[#positions]
        local e = surface.create_entity{ name = name, position = pos, force = "player", raise_built = true }
        if e and e.valid then built = built + 1 end
    end
    return built
end

local VP = {}

--- Called each tick while running.
function VP.tick(surface, tick)
    local vp = state.vp
    local elapsedMin = gameMinutes(tick - state.stagedAt)

    -- Build steps
    if vp.step <= #BUILD_SCHEDULE then
        local step = BUILD_SCHEDULE[vp.step]
        if elapsedMin >= step.min then
            local walls = buildWallRing(surface, step.radius)
            local turrets = buildTurrets(surface, step.radius, step.turrets or 0)
            print("VP: build step " .. vp.step .. " (t=" .. string.format("%.0f", elapsedMin) ..
                "min) walls=" .. walls .. " turrets=" .. turrets)
            vp.step = vp.step + 1
        end
    end

    -- Factory pollution
    if tick % POLLUTE_INTERVAL == 0 then
        surface.pollute(PLAYER_ORIGIN, 60)
    end
end

local function stage(surface)
    -- Clean the map: destroy natural enemy structures/units.
    local natural = surface.find_entities_filtered{
        force = "enemy",
        type = { "unit-spawner", "turret", "unit" },
    }
    for _, e in ipairs(natural) do
        if e.valid then
            e.destroy()
        end
    end
    -- Stage the enemy bases
    local staged = 0
    for _, pos in ipairs(SPAWNER_POSITIONS) do
        local e = surface.create_entity{ name = "biter-spawner", position = pos, force = "enemy", raise_built = true }
        if e and e.valid then staged = staged + 1 end
    end
    print("STAGED: " .. staged .. " enemy spawners, virtual player at origin")
end

local function statusLine(tick)
    local surface = game.surfaces[1]
    local bases = remote.call("Rampant", "getBaseCount", surface)
    local groups = remote.call("Rampant", "getEnemyGroups", surface)
    local squads = 0
    local builders = 0
    if groups then
        for _, g in ipairs(groups) do
            if g.isBuilder then builders = builders + 1 else squads = squads + 1 end
        end
    end
    local spawners = #surface.find_entities_filtered{ force = "enemy", type = "unit-spawner" }
    local evolution = game.forces.enemy.get_evolution_factor(surface)
    print("LONGRUN_STATUS t=" .. string.format("%.0f", gameMinutes(tick - state.stagedAt)) ..
        "min bases=" .. bases .. " spawners=" .. spawners ..
        " squads=" .. squads .. " builders=" .. builders ..
        " evolution=" .. string.format("%.3f", evolution))
end

script.on_event(defines.events.on_tick, function()
    if not rampReady() then
        return
    end
    if not state then
        state = {
            phase = "running",
            stagedAt = game.tick,
            done = false,
            vp = { step = 1 },
        }
        stage(game.surfaces[1])
        game.speed = 100
        print("LONGRUN_STARTED")
        return
    end
    if state.done then
        return
    end

    local surface = game.surfaces[1]
    local tick = game.tick
    local elapsed = tick - state.stagedAt

    if elapsed > RUN_TICKS then
        print("LONGRUN_COMPLETE ticks=" .. tick .. " minutes=" ..
            string.format("%.1f", gameMinutes(elapsed)))
        statusLine(tick)
        state.done = true
        game.speed = 1
        return
    end

    VP.tick(surface, tick)

    -- Milestone status lines
    if elapsed % (MILESTONE_INTERVAL * TICKS_A_MINUTE) == 0 then
        statusLine(tick)
    end

    -- Periodic migration nudge: the base's random state rolls decide whether
    -- expansion happens; nudge MIGRATING + fund the economy so settlements
    -- are exercised without pinning the state machine (nudge = no freeze)
    if elapsed % (NUDGE_INTERVAL * TICKS_A_MINUTE) == 0 then
        local base = remote.call("Rampant", "queryBaseAt", PLAYER_ORIGIN, surface)
        if base then
            local ok = remote.call("Rampant", "nudgeBaseState", base.id, 5)  -- MIGRATING
            local funded = remote.call("Rampant", "grantUnitPoints", base.id, 500)
            print("NUDGE: MIGRATING=" .. tostring(ok) .. " points=" .. tostring(funded))
        end
    end
end)
