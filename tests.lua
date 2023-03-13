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
-- along with this program.  If not, see <https://www.gnu.org/licenses/>.


local tests = {}

local constants = require("libs/Constants")
local chunkUtils = require("libs/ChunkUtils")
local chunkPropertyUtils = require("libs/ChunkPropertyUtils")

function tests.chunkCount()
    local count = 0
    for _,map in pairs(global.universe.maps) do
        count = count + #map.processQueue
    end
    print(count)
end

function tests.fillableDirtTest()
    local playerPosition = game.players[1].position
    local chunkX = math.floor(playerPosition.x * 0.03125) * 32
    local chunkY = math.floor(playerPosition.y * 0.03125) * 32
    game.get_surface(global.natives.activeSurface).set_tiles({{name="fillableDirt", position={chunkX-1, chunkY-1}},
            {name="fillableDirt", position={chunkX, chunkY-1}},
            {name="fillableDirt", position={chunkX-1, chunkY}},
            {name="fillableDirt", position={chunkX, chunkY}}},
        false)
end

function tests.tunnelTest()
    local playerPosition = game.players[1].position
    local chunkX = math.floor(playerPosition.x * 0.03125) * 32
    local chunkY = math.floor(playerPosition.y * 0.03125) * 32
    game.get_surface(global.natives.activeSurface).create_entity({name="tunnel-entrance-rampant", position={chunkX, chunkY}})
end

function tests.reveal (size)
    local pos = game.player.character.position
    game.player.force.chart(game.player.surface,
                            {{x=-size+pos.x, y=-size+pos.y}, {x=size+pos.x, y=size+pos.y}})
end

function tests.showBaseGrid(time)
    local map = global.universe.maps[game.player.surface.index]
    local chunks = map.chunkToBase
    for chunk in pairs(chunks) do
        local count = chunkPropertyUtils.getEnemyStructureCount(map, chunk)
        chunkUtils.mapScanEnemyChunk(chunk, map, game.tick)
        local newCount = chunkPropertyUtils.getEnemyStructureCount(map, chunk)
        if newCount ~= count then
            constants.gpsDebug(chunk.x+16,chunk.y+16, "f2:" .. tostring(count) .. "/" .. tostring(newCount))
            chunkUtils.colorChunk(chunk, game.player.surface.index, {0.3, 0.1, 0.1, 0.6}, time and tonumber(time))
        else
            chunkUtils.colorChunk(chunk, game.player.surface.index, nil, time and tonumber(time))
        end
    end
end

-- function tests.showMovementGrid()
--     local chunks = global.map.processQueue
--     for i=1,#chunks do
--         local chunk = chunks[i]
--         local color = "concrete"
--         if (chunkPropertyUtils.getPassable(global.map, chunk) == constants.CHUNK_ALL_DIRECTIONS) then
--             color = "hazard-concrete-left"
--         elseif (chunkPropertyUtils.getPassable(global.map, chunk) == constants.CHUNK_NORTH_SOUTH) then
--             color = "concrete"
--         elseif (chunkPropertyUtils.getPassable(global.map, chunk) == constants.CHUNK_EAST_WEST) then
--             color = "stone-path"
--         end
--         chunkUtils.colorChunk(chunk.x, chunk.y, color, game.get_surface(global.natives.activeSurface))
--     end
-- end

-- function tests.colorResourcePoints()
--     local chunks = global.map.processQueue
--     for i=1,#chunks do
--         local chunk = chunks[i]
--         local color = "concrete"
--         if (chunk[constants.RESOURCE_GENERATOR] ~= 0) and (chunk[constants.NEST_COUNT] ~= 0) then
--             color = "hazard-concrete-left"
--         elseif (chunk[constants.RESOURCE_GENERATOR] ~= 0) then
--             color = "deepwater"
--         elseif (chunk[constants.NEST_COUNT] ~= 0) then
--             color = "stone-path"
--         end
--         chunkUtils.colorChunk(chunk.x, chunk.y, color, game.get_surface(global.natives.activeSurface))
--     end
-- end

function tests.entityStats(name, d)
    local playerPosition = game.players[1].position
    local chunkX = math.floor(playerPosition.x * 0.03125) * 32
    local chunkY = math.floor(playerPosition.y * 0.03125) * 32
    local a = game.get_surface(global.natives.activeSurface).create_entity({name=name, position={chunkX, chunkY}})
    if d then
        a['direction'] = d
    end
    print(serpent.dump(a))
    a.destroy()
end

local function lookupIndexFaction(targetFaction)
    for i=1,#constants.FACTION_SET do
        if constants.FACTION_SET[i].type == targetFaction then
            return i
        end
    end
    return 0
end

local function scoreResourceLocationKamikaze(_, neighborChunk)
    local settle = neighborChunk[constants.RESOURCE_PHEROMONE]
    return settle
        - (neighborChunk[constants.PLAYER_PHEROMONE] * constants.PLAYER_PHEROMONE_MULTIPLER)
        - neighborChunk[constants.ENEMY_PHEROMONE]
end

local function scoreSiegeLocationKamikaze(_, neighborChunk)
    local settle = neighborChunk[constants.BASE_PHEROMONE]
        + neighborChunk[constants.RESOURCE_PHEROMONE] * 0.5
        + (neighborChunk[constants.PLAYER_PHEROMONE] * constants.PLAYER_PHEROMONE_MULTIPLER)
        - neighborChunk[constants.ENEMY_PHEROMONE]

    return settle
end

local function scoreResourceLocation(map, neighborChunk)
    local settle = (neighborChunk[constants.RESOURCE_PHEROMONE])
    return settle
        - (neighborChunk[constants.PLAYER_PHEROMONE] * constants.PLAYER_PHEROMONE_MULTIPLER)
        - neighborChunk[constants.ENEMY_PHEROMONE]
end

local function scoreSiegeLocation(map, neighborChunk)
    local settle = neighborChunk[constants.BASE_PHEROMONE]
        + neighborChunk[constants.RESOURCE_PHEROMONE] * 0.5
        + (neighborChunk[constants.PLAYER_PHEROMONE] * constants.PLAYER_PHEROMONE_MULTIPLER)
        - neighborChunk[constants.ENEMY_PHEROMONE]

    return settle
end

local function scoreAttackLocation(map, neighborChunk)
    local damage = neighborChunk[constants.BASE_PHEROMONE] +
        (neighborChunk[constants.PLAYER_PHEROMONE] * constants.PLAYER_PHEROMONE_MULTIPLER)
    return damage
end

local function scoreAttackKamikazeLocation(_, neighborChunk)
    local damage = neighborChunk[constants.BASE_PHEROMONE] + (neighborChunk[constants.PLAYER_PHEROMONE] * constants.PLAYER_PHEROMONE_MULTIPLER)
    return damage
end

function tests.exportAiState()

    local printState = function ()
        local map = global.universe.maps[game.players[1].surface.index]
        local chunks = map.processQueue
        local s = ""
        for i=1,#chunks do
            local chunk = chunks[i]

            local base = chunk.base
            local alignmentCount = 0

            if base then
                if (#base.alignment == 2) then
                    alignmentCount = (math.abs(base.x) * 10000) + (math.abs(base.y) * 10000) + (lookupIndexFaction(base.alignment[1]) * 100) + lookupIndexFaction(base.alignment[2])
                else
                    alignmentCount = (math.abs(base.x) * 10000) + (math.abs(base.y) * 10000) + lookupIndexFaction(base.alignment[1])
                end
            end

            s = s .. table.concat({chunk.x,
                                   chunk.y,
                                   chunkPropertyUtils.getCombinedDeathGeneratorRating(chunk),
                                   chunk[constants.BASE_PHEROMONE],
                                   chunk[constants.PLAYER_PHEROMONE],
                                   chunk[constants.RESOURCE_PHEROMONE],
                                   chunk[constants.ENEMY_PHEROMONE],
                                   chunkPropertyUtils.getPassable(chunk),
                                   chunk[constants.CHUNK_TICK],
                                   chunkPropertyUtils.getPathRating(chunk),
                                   chunk.nestCount or 0,
                                   chunk.turretCount or 0,
                                   chunkPropertyUtils.getRallyTick(chunk) or 0,
                                   chunkPropertyUtils.getRetreatTick(chunk) or 0,
                                   chunk.resourceGenerator or 0,
                                   chunk.playerBaseGenerator or 0,
                                   chunkPropertyUtils.getCombinedDeathGenerator(chunk),
                                   scoreResourceLocationKamikaze(map, chunk),
                                   scoreResourceLocation(map, chunk),
                                   scoreSiegeLocationKamikaze(map, chunk),
                                   scoreSiegeLocation(map, chunk),
                                   scoreAttackKamikazeLocation(map, chunk),
                                   scoreAttackLocation(map, chunk),
                                   game.get_surface(game.players[1].surface.index).get_pollution(chunk),
                                   (chunkPropertyUtils.isActiveNest(chunk) and 1) or 0,
                                   (chunkPropertyUtils.isActiveRaidNest(chunk) and 1) or 0,
                                   table_size(chunk.squads or {}),
                                   alignmentCount,
                                   chunk.hiveCount or 0,
                                   chunk.trapCount or 0,
                                   chunk.utilityCount or 0,
                                   global.universe.chunkToVictory[chunk.id] or 0
                                  }, ",") .. "\n"
        end
        game.write_file("rampantState.txt", s, false)
    end

    return function(interval)
        if not interval then
            interval = 0
        else
            interval = tonumber(interval)
        end

        printState()

        if (interval > 0) then
            script.on_nth_tick(interval, printState)
        end
    end
end

function tests.dumpEnvironment(x)
    print (serpent.dump(global[x]))
end

return tests
