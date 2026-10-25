-- Copyright (C) 2022  veden

-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <https://www.gnu.org/licenses/>

if Observability then
    return Observability
end

local Universe
local Observability = {}

function Observability.init(universe)
    Universe = universe
end

local mFloor = math.floor

local Constants = require("libs/Constants")
local DEATH_PHEROMONE = Constants.DEATH_PHEROMONE
local VICTORY_PHEROMONE = Constants.VICTORY_PHEROMONE
local CHUNK_TICK = Constants.CHUNK_TICK

local ChunkPropertyUtils = require("libs/ChunkPropertyUtils")
local MapUtils = require("libs/MapUtils")
local SquadLogic = require("libs/squad_logic")
local SourceVersionedFieldRuntime = require("libs/source_versioned_field_runtime")
local Runtime = { fieldKey = SourceVersionedFieldRuntime.fieldKey }

local findNearbyBase = ChunkPropertyUtils.findNearbyBase
local getChunkByPosition = MapUtils.getChunkByPosition
local getPassable = ChunkPropertyUtils.getPassable

function Observability.queryPheromones(position, surface)
    if not Universe or not Universe.awake then
        return nil
    end

    local map = Universe.maps[surface.index]
    if not map then
        return nil
    end

    local chunk = getChunkByPosition(map, position)
    if chunk == -1 then
        return nil
    end

    return {
        player = SquadLogic.playerPheromone(chunk, game.tick),
        enemy = SquadLogic.enemyPheromone(chunk, game.tick),
        base = SquadLogic.basePheromone(chunk, game.tick),
        -- real death state (named fields, not numeric chunk indices)
        deathGenerator = chunk.deathGenerator or 0,
        permanentDeathGenerator = chunk.permanentDeathGenerator or 0,
        kamikaze = SquadLogic.kamikazePheromone(chunk, game.tick),
        resource = SquadLogic.resourcePheromone(chunk, game.tick),
        -- persistent source generators (diagnostics: a computed pheromone
        -- of 0 with a live generator means the chunk is not being
        -- re-processed; a dead generator means the source never registered)
        playerGenerator = chunk.playerGenerator or 0,
        playerBaseGenerator = chunk.playerBaseGenerator or 0,
    }
end

--- Query the mod's chunk state at a position: the scan-computed
--- passability (the pathing gate), the terrain water tile count, and the
--- structure counts. Diagnostic telemetry for movement analysis.
function Observability.queryChunk(position, surface)
    if not Universe or not Universe.awake then
        return nil
    end

    local map = Universe.maps[surface.index]
    if not map then
        return nil
    end

    local chunk = getChunkByPosition(map, position)
    if chunk == -1 then
        return nil
    end

    local waterTiles = 0
    -- No pcall: a bug here is a real bug; fail loudly.
    local count = surface.count_tiles_filtered{
        area = { { chunk.x, chunk.y }, { chunk.x + 31, chunk.y + 31 } },
        name = { "water", "deepwater" },
    }
    waterTiles = count

    return {
        x = chunk.x,
        y = chunk.y,
        passable = getPassable(chunk),
        waterTiles = waterTiles,
        nestCount = chunk.nestCount or 0,
        turretCount = chunk.turretCount or 0,
    }
end

function Observability.queryBaseAt(position, surface)
    if not Universe or not Universe.awake then
        return nil
    end

    local map = Universe.maps[surface.index]
    if not map then
        return nil
    end

    local chunk = getChunkByPosition(map, position)
    if chunk == -1 then
        return nil
    end

    local base = findNearbyBase(chunk)
    if not base then
        return nil
    end

    return {
        id = base.id,
        x = base.x,
        y = base.y,
        state = base.stateAI,
        stateAITick = base.stateAITick,
        points = base.points,
        unitPoints = base.unitPoints,
        destroyPlayerBuildings = base.destroyPlayerBuildings or 0,
        -- The structure counts live on the CHUNK (chunk.nestCount), not the
        -- base object — report the chunk at the query position.
        nestCount = chunk.nestCount or 0,
        activeStructures = base.activeStructureCount or 0,
        attackThreshold = base.attackThreshold or 0,
        retreatThreshold = base.retreatThreshold or 0,
        -- Expansion-quota state (settler formation diagnostics): the
        -- settler gate rejects while sentExpansionGroups >= maxExpansion-
        -- Groups; the quota clears when resetExpensionGroupsTick expires.
        sentExpansionGroups = base.sentExpansionGroups or 0,
        maxExpansionGroups = base.maxExpansionGroups or 0,
        resetExpensionGroupsTick = base.resetExpensionGroupsTick or 0,
        -- Nest activeness state (formation-cadence diagnostics): the
        -- active-registry membership and the re-check deadline that gates
        -- processNestActiveness.
        isActiveNest = Universe.chunkToActiveNest and (Universe.chunkToActiveNest[chunk.id] ~= nil) or false,
        nestActiveTick = chunk.activeTick or 0,
    }
end

function Observability.getEnemyUnits(surface)
    if not Universe or not Universe.awake then
        return {}
    end

    return surface.find_entities_filtered({
        force = "enemy",
        type = "unit"
    })
end

function Observability.getEnemyGroups(surface)
    if not Universe or not Universe.awake then
        return {}
    end

    local groups = {}
    local squadTable = Universe.groupNumberToSquad

    if squadTable then
        for _, squad in pairs(squadTable) do
            if squad.group and squad.group.valid then
                local memberCount = 0
                local members = {}
                for unitNumber, _ in pairs(squad.group.members) do
                    memberCount = memberCount + 1
                    members[#members + 1] = unitNumber
                end
                table.insert(groups, {
                    id = squad.group.unique_id,
                    position = squad.group.position,
                    state = squad.group.state,
                    memberCount = memberCount,
                    members = members,
                    isBuilder = squad.settlers or false,
                    isKamikaze = squad.kamikaze or false,
                    retreats = squad.retreats or 0,
                    -- Rampant squad status (1=RETREATING, 4=RAIDING, 5=SETTLING)
                    status = squad.status or 0,
                    -- Base AI state snapshot at squad creation
                    -- (1=PEACEFUL..5=MIGRATING, 6=SIEGE, 7=ONSLAUGHT)
                    type = squad.type or 0,
                    -- Settle-decision geometry (scenario diagnostics):
                    -- which settle rule should fire at the squad's chunk
                    maxDistance = squad.maxDistance or 0,
                    originX = squad.originPosition and squad.originPosition.x or 0,
                    originY = squad.originPosition and squad.originPosition.y or 0,
                })
            end
        end
    end

    return groups
end

function Observability.getBasePoints(baseId)
    if not Universe or not Universe.awake then
        return nil
    end
    local base = Universe.bases[baseId]
    if not base then
        return nil
    end
    return base.unitPoints
end

function Observability.getBaseCount(surface)
    if not Universe or not Universe.awake then
        return 0
    end

    local map = Universe.maps[surface.index]
    if not map then
        return 0
    end

    local count = 0
    for _, base in pairs(map.bases or {}) do
        if (base.activeNests or 0) > 0 then
            count = count + 1
        end
    end
    return count
end

function Observability.getActiveStructures(surface)
    if not Universe or not Universe.awake then
        return {}
    end

    local map = Universe.maps[surface.index]
    if not map then
        return {}
    end

    -- The structure registries live on the chunks (nestIds/nestCount,
    -- turretIds/turretCount, ...) with the chunk id -> chunk maps in the
    -- Universe (chunkToNests, chunkToUtilities, ...). The entities are not
    -- stored (only unit_number -> true), so report the registered chunks.
    local structures = {}
    local registries = {
        { map = Universe.chunkToNests, type = "nest" },
        { map = Universe.chunkToUtilities, type = "turret" },
        { map = Universe.chunkToHives, type = "hive" },
    }
    for _, reg in ipairs(registries) do
        for _, chunk in pairs(reg.map or {}) do
            if chunk and chunk.map == map then
                local count = (reg.type == "nest" and chunk.nestCount)
                    or (reg.type == "turret" and chunk.turretCount)
                    or (reg.type == "hive" and chunk.hiveCount)
                    or 0
                table.insert(structures, {
                    type = reg.type,
                    count = count,
                    x = chunk.x,
                    y = chunk.y,
                })
            end
        end
    end
    return structures
end

-- RCON Test Helpers
-- In-world pheromone overlay.
--
-- The field numbers are only meaningful compared across neighbouring chunks, so
-- a log line cannot be watched. This draws one rectangle per chunk, coloured by
-- the field value relative to that field's peak, refreshed every few ticks.
-- Toggled with remote Rampant.drawField(name[, enabled]).
local FIELD_COLORS = {
    player = { r = 0.2, g = 0.4, b = 1.0 },
    kamikaze = { r = 0.7, g = 0.2, b = 1.0 },
    enemy = { r = 1.0, g = 0.2, b = 0.2 },
    base = { r = 1.0, g = 0.9, b = 0.2 },
    resource = { r = 0.2, g = 1.0, b = 0.4 },
    deathGain = { r = 1.0, g = 1.0, b = 1.0 },
    deathLoss = { r = 0.1, g = 0.1, b = 0.1 },
}
local FIELD_TYPES = {
    player = Constants.PLAYER_PHEROMONE,
    kamikaze = Constants.KAMIKAZE_PHEROMONE,
    enemy = Constants.ENEMY_PHEROMONE,
    base = Constants.BASE_PHEROMONE,
    resource = Constants.RESOURCE_PHEROMONE,
    deathGain = Constants.DEATH_GAIN_PHEROMONE,
    deathLoss = Constants.DEATH_LOSS_PHEROMONE,
}
local Visualizers = {}
local VISUALIZE_EVERY = 15

function Observability.toggleField(name, enabled, showNumbers)
    if enabled == nil then enabled = not Visualizers[name] end
    if not enabled then
        Visualizers[name] = nil
        -- LuaRendering has no per-object destroy in 2.1; clear this mod's
        -- objects and let the tick redraw whichever fields are still enabled.
        rendering.clear("Rampant")
        return name .. ": off"
    end
    local fieldType = FIELD_TYPES[name]
    if not fieldType then return "unknown field: " .. tostring(name) end
    Visualizers[name] = { fieldType = fieldType, color = FIELD_COLORS[name], numbers = showNumbers }
    local legend = "[Rampant] " .. name .. " pheromone: brighter = stronger. " ..
        "It is strongest at the source (a nest, a spawner, the player) and fades one step per chunk outward."
    if showNumbers then legend = legend .. " Chunk values are drawn as text." end
    game.print(legend)
    return name .. ": on"
end

function Observability.updateVisualizers(tick)
    if tick % VISUALIZE_EVERY ~= 0 then return end
    local any = false
    for _ in pairs(Visualizers) do any = true break end
    if not any then return end
    rendering.clear("Rampant")
    for _, v in pairs(Visualizers) do
        local field = Universe.sourceVersionedFields[Runtime.fieldKey(v.fieldType)] or {}
        local peak = 0
        for _, value in pairs(field) do
            if value > peak then peak = value end
        end
        if peak > 0 then
            for chunkId, value in pairs(field) do
                local chunk = MapUtils.getChunkById(chunkId, Universe)
                if chunk and chunk.map and chunk.map.surface and chunk.map.surface.valid then
                    local t = value / peak
                    rendering.draw_rectangle{
                        surface = chunk.map.surface,
                        left_top = { chunk.x, chunk.y },
                        right_bottom = { chunk.x + 32, chunk.y + 32 },
                        color = { r = v.color.r, g = v.color.g, b = v.color.b, a = 0.10 + 0.45 * t },
                        filled = true,
                        only_during_screenshot = false,
                    }
                    if v.numbers and value >= 1 then
                        rendering.draw_text{
                            surface = chunk.map.surface,
                            text = string.format("%.0f", value),
                            target = { chunk.x + 16, chunk.y + 16 },
                            color = { r = 1, g = 1, b = 1, a = 0.85 },
                            scale = 1.1,
                            alignment = "center",
                            only_during_screenshot = false,
                        }
                    end
                end
            end
        end
    end
end

function Observability.getGameState()
    return {
        tick = game.tick,
        speed = game.speed,
        universeAwake = Universe and Universe.awake or false,
        mapCount = Universe and Universe.maps and #Universe.maps or 0,
    }
end

function Observability.setGameSpeed(speed)
    game.speed = speed or 1
    return "Speed set to " .. tostring(game.speed)
end

function Observability.spawnEntity(name, position, forceName)
    local surface = game.surfaces[1]
    if not surface then
        return "No surface available"
    end
    local entity = surface.create_entity({
        name = name,
        position = position,
        force = forceName or "player"
    })
    if entity then
        return "Spawned " .. name .. " at " .. position.x .. "," .. position.y
    else
        return "Failed to spawn " .. name
    end
end

function Observability.countEntitiesByForce(forceName)
    local surface = game.surfaces[1]
    if not surface then
        return 0
    end
    local entities = surface.find_entities_filtered({force = forceName})
    return #entities
end

-- UDP server for external tooling (Python client, etc.)
-- Launch Factorio with: --enable-lua-udp 5200

local function sendResponse(sourcePort, data)
    if sourcePort and sourcePort > 0 then
        helpers.send_udp(sourcePort, helpers.table_to_json(data))
    end
end

function Observability.onUdpPacket(event)
    local payload = event.payload
    local port = event.source_port

    -- Simple command protocol: "cmd" or "cmd:arg"
    if payload == "ping" then
        return sendResponse(port, {ok = true, tick = game.tick})
    end

    -- debug:on / debug:off - toggle entity event logging
    if payload == "debug:on" or payload == "debug:off" then
        if Universe then
            Universe.DEBUG_ENTITY_EVENTS = (payload == "debug:on")
            return sendResponse(port, {ok = true, debug = Universe.DEBUG_ENTITY_EVENTS})
        end
        return sendResponse(port, {error = "Universe not initialized"})
    end

    if payload == "debug" then
        return sendResponse(port, {debug = Universe and Universe.DEBUG_ENTITY_EVENTS or false})
    end

    if payload == "squads" then
        local squads = {}
        if Universe and Universe.tables and Universe.tables.groupNumberToSquad then
            for _, squad in pairs(Universe.tables.groupNumberToSquad) do
                if squad.group and squad.group.valid then
                    local count = 0
                    for _ in pairs(squad.group.members) do count = count + 1 end
                    table.insert(squads, {
                        id = squad.group.group_number,
                        x = squad.group.position.x,
                        y = squad.group.position.y,
                        members = count,
                        builder = squad.builder or false,
                    })
                end
            end
        end
        return sendResponse(port, {squads = squads, count = #squads})
    end

    if payload == "bases" then
        local bases = {}
        if Universe and Universe.bases then
            for _, base in pairs(Universe.bases) do
                table.insert(bases, {
                    id = base.id,
                    x = base.x,
                    y = base.y,
                    tier = base.tier,
                    alignment = base.alignment,
                })
            end
        end
        return sendResponse(port, {bases = bases, count = #bases})
    end

    if payload == "state" or payload:match("^state:") then
        -- Stream all chunks as multiple UDP packets tagged with a request ID.
        -- Each packet: {id: req_id, seq: N, total: M, chunks: [...]}
        -- First packet also includes tick. Last packet has seq == total.
        local req_id = "state_" .. port .. "_" .. game.tick
        local all_chunks = {}

        local function to_number(v)
            return type(v) == "number" and v or 0
        end

        local function collect_one(xChunks)
            for _, chunk in pairs(xChunks) do
                if type(chunk) == "table" and chunk.id then
                    -- base/enemy/resource come from the source-versioned solver;
                    -- their chunk slots are no longer written.
                    table.insert(all_chunks, {
                        x = to_number(chunk.x), y = to_number(chunk.y),
                        b = to_number(SquadLogic.basePheromone(chunk, game.tick)),
                        p = to_number(SquadLogic.playerPheromone(chunk, game.tick)),
                        r = to_number(SquadLogic.resourcePheromone(chunk, game.tick)),
                        e = to_number(SquadLogic.enemyPheromone(chunk, game.tick)),
                        k = to_number(SquadLogic.kamikazePheromone(chunk, game.tick)),
                        sqs = to_number(chunk.squads), tick = to_number(chunk[CHUNK_TICK]),
                    })
                end
            end
        end

        local function collect_chunks()
            if not Universe or not Universe.maps then return end
            for _, mapObj in pairs(Universe.maps) do
                if type(mapObj) == "table" then
                    for _, xChunks in pairs(mapObj) do
                        if type(xChunks) == "table" then
                            collect_one(xChunks)
                        end
                    end
                end
            end
        end

        -- No pcall: a bug while collecting chunk state is a real bug and must
        -- fail loudly (the UDP handler is event-driven; the error surfaces in
        -- the log and the caller sees a dropped response).
        collect_chunks()

        -- Multi-packet streaming: split into batches for large maps
        -- Each chunk ~213 bytes. helpers.send_udp ceiling ~60KB, target 32KB for safety.
        local total = #all_chunks
        local max_batch_size = 150  -- ~32KB per packet
        local total_packets = math.ceil(total / max_batch_size)
        -- streaming: ~56KB/packet, 263 chunks each

        for seq = 1, total_packets do
            local batch_start = (seq - 1) * max_batch_size + 1
            local batch_end = math.min(batch_start + max_batch_size - 1, total)
            local batch = {}
            for i = batch_start, batch_end do
                table.insert(batch, all_chunks[i])
            end
            local pkt = {
                id = req_id,
                seq = seq,
                total = total_packets,
                chunks = batch,
            }
            if seq == 1 then
                pkt.tick = game.tick
            end
            local json_str = helpers.table_to_json(pkt)
            helpers.send_udp(port, json_str)
        end
    end

    if payload == "info" then
        return sendResponse(port, {
            tick = game.tick,
            awake = Universe and Universe.awake or false,
            evolution = (game.forces.enemy and game.surfaces[1] and game.forces.enemy.get_evolution_factor(game.surfaces[1])) or 0,
            surfaces = #game.surfaces,
        })
    end

    -- exec:LUA_CODE — execute arbitrary Lua and return JSON-serializable result
    -- DELIBERATE pcall (policy-sanctioned): this is the RCON/debug interface.
    -- Its contract is reporting the executed code's error back to the caller
    -- over UDP — not swallowing it. Mod simulation logic never uses pcall.
    if payload:match("^exec:") then
        local code = payload:sub(6)  -- strip "exec:" prefix
        local ok, result = pcall(function()
            local env = setmetatable({
                game = game,
                script = script,
                settings = settings,
                defines = defines,
                remote = remote,
                storage = storage,
                helpers = helpers,
                math = math,
                string = string,
                table = table,
                type = type,
                pairs = pairs,
                ipairs = ipairs,
                next = next,
                tonumber = tonumber,
                tostring = tostring,
                print = print,
                pcall = pcall,
                Universe = Universe,
            }, {__index = _G})
            local fn = load(code, "udp-exec", "t", env)
            if not fn then return {error = "load failed"} end
            return fn()
        end)
        if not ok then
            return sendResponse(port, {error = tostring(result)})
        end
        return sendResponse(port, {ok = true, result = result})
    end

    -- Unknown command
    return sendResponse(port, {error = "unknown command: " .. payload})
end

return Observability