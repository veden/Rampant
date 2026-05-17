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
local PLAYER_PHEROMONE = Constants.PLAYER_PHEROMONE
local ENEMY_PHEROMONE = Constants.ENEMY_PHEROMONE
local BASE_PHEROMONE = Constants.BASE_PHEROMONE
local DEATH_PHEROMONE = Constants.DEATH_PHEROMONE
local VICTORY_PHEROMONE = Constants.VICTORY_PHEROMONE
local KAMIKAZE_PHEROMONE = Constants.KAMIKAZE_PHEROMONE
local RESOURCE_PHEROMONE = Constants.RESOURCE_PHEROMONE
local CHUNK_TICK = Constants.CHUNK_TICK

local ChunkPropertyUtils = require("libs/ChunkPropertyUtils")
local MapUtils = require("libs/MapUtils")

local findNearbyBase = ChunkPropertyUtils.findNearbyBase
local getChunkByPosition = MapUtils.getChunkByPosition

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
        player = chunk[PLAYER_PHEROMONE] or 0,
        enemy = chunk[ENEMY_PHEROMONE] or 0,
        base = chunk[BASE_PHEROMONE] or 0,
        death = chunk[DEATH_PHEROMONE] or 0,
        victory = chunk[VICTORY_PHEROMONE] or 0,
        kamikaze = chunk[KAMIKAZE_PHEROMONE] or 0,
        resource = chunk[RESOURCE_PHEROMONE] or 0,
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
        position = base.position,
        state = base.stateAI,
        points = base.points,
        nestCount = base.nestCount or 0,
        activeStructures = base.activeStructureCount or 0,
        attackThreshold = base.attackThreshold or 0,
        retreatThreshold = base.retreatThreshold or 0,
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
    local squadTable = Universe.tables.groupNumberToSquad

    if squadTable then
        for _, squad in pairs(squadTable) do
            if squad.group and squad.group.valid then
                local memberCount = 0
                for _ in pairs(squad.group.members) do
                    memberCount = memberCount + 1
                end
                table.insert(groups, {
                    id = squad.group.group_number,
                    position = squad.group.position,
                    state = squad.group.state,
                    memberCount = memberCount,
                    isBuilder = squad.builder or false,
                })
            end
        end
    end

    return groups
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
        if base.activeStructureCount and base.activeStructureCount > 0 then
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

    local structures = {}
    for _, entity in pairs(map.structures or {}) do
        if entity.valid then
            table.insert(structures, {
                name = entity.name,
                position = entity.position,
                type = entity.type,
            })
        end
    end
    return structures
end

-- RCON Test Helpers
function Observability.getGameState()
    return {
        tick = game.tick,
        speed = game.speed,
        universeAwake = Universe and Universe.awake or false,
        mapCount = Universe and Universe.maps and #Universe.maps or 0,
    }
end

function Observability.advanceTicks(n)
    n = n or 1
    for i = 1, n do
        game.tick = game.tick + 1
    end
    return n .. " ticks advanced"
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
                    x = base.cx,
                    y = base.cy,
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
                    table.insert(all_chunks, {
                        x = to_number(chunk.x), y = to_number(chunk.y),
                        b = to_number(chunk[BASE_PHEROMONE]),
                        p = to_number(chunk[PLAYER_PHEROMONE]),
                        r = to_number(chunk[RESOURCE_PHEROMONE]),
                        e = to_number(chunk[ENEMY_PHEROMONE]),
                        k = to_number(chunk[KAMIKAZE_PHEROMONE]),
                        sqs = to_number(chunk.squads), tick = to_number(chunk[CHUNK_TICK]),
                    })
                end
            end
        end

        local function collect_chunks()
            if not Universe or not Universe.maps then return end
            for _, mapObj in pairs(Universe.maps) do
                if type(mapObj) ~= "table" then end
                for _, xChunks in pairs(mapObj) do
                    if type(xChunks) == "table" then
                        pcall(collect_one, xChunks)
                    end
                end
            end
        end

        local ok, err = pcall(collect_chunks)
        if not ok then
            return sendResponse(port, {error = tostring(err), chunks = {}, count = 0})
        end

        -- Multi-packet streaming: split into batches for large maps
        -- Each chunk ~213 bytes. helpers.send_udp ceiling ~60KB, target 56KB for margin.
        local total = #all_chunks
        local max_batch_size = 263  -- ~56KB per packet
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