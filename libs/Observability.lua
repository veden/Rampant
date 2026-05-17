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

local Observability = {}

local mFloor = math.floor

local Constants = require("libs/Constants")
local PLAYER_PHEROMONE = Constants.PLAYER_PHEROMONE
local ENEMY_PHEROMONE = Constants.ENEMY_PHEROMONE
local BASE_PHEROMONE = Constants.BASE_PHEROMONE
local DEATH_PHEROMONE = Constants.DEATH_PHEROMONE
local VICTORY_PHEROMONE = Constants.VICTORY_PHEROMONE
local KAMIKAZE_PHEROMONE = Constants.KAMIKAZE_PHEROMONE
local RESOURCE_PHEROMONE = Constants.RESOURCE_PHEROMONE

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

return Observability