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


if aiDefenseG then
    return aiDefenseG
end
local aiDefense = {}

-- imports

local constants = require("Constants")
local mapUtils = require("MapUtils")
local unitGroupUtils = require("UnitGroupUtils")
local movementUtils = require("MovementUtils")
local chunkPropertyUtils = require("ChunkPropertyUtils")

-- constants

local PLAYER_PHEROMONE = constants.PLAYER_PHEROMONE
local BASE_PHEROMONE = constants.BASE_PHEROMONE

local PLAYER_PHEROMONE_MULTIPLER = constants.PLAYER_PHEROMONE_MULTIPLER

local SQUAD_RETREATING = constants.SQUAD_RETREATING

local COOLDOWN_RETREAT = constants.COOLDOWN_RETREAT

-- imported functions

local findNearbyBase = chunkPropertyUtils.findNearbyBase

local addSquadToChunk = chunkPropertyUtils.addSquadToChunk

local positionFromDirectionAndFlat = mapUtils.positionFromDirectionAndFlat
local getNeighborChunks = mapUtils.getNeighborChunks
local findNearbyRetreatingSquad = unitGroupUtils.findNearbyRetreatingSquad
local createSquad = unitGroupUtils.createSquad
local scoreNeighborsForRetreat = movementUtils.scoreNeighborsForRetreat
local findMovementPosition = movementUtils.findMovementPosition

local getRetreatTick = chunkPropertyUtils.getRetreatTick
local getPlayerBaseGenerator = chunkPropertyUtils.getPlayerBaseGenerator
local setRetreatTick = chunkPropertyUtils.setRetreatTick
local getDeathGeneratorRating = chunkPropertyUtils.getDeathGeneratorRating
local getEnemyStructureCount = chunkPropertyUtils.getEnemyStructureCount

-- module code

local function scoreRetreatLocation(map, neighborChunk)
    return (-neighborChunk[BASE_PHEROMONE] +
            -(neighborChunk[PLAYER_PHEROMONE] * PLAYER_PHEROMONE_MULTIPLER) +
            -(getPlayerBaseGenerator(map, neighborChunk) * 1000)) *
        getDeathGeneratorRating(map, neighborChunk)
end

function aiDefense.retreatUnits(chunk, cause, map, tick, radius)
    if (tick - getRetreatTick(map, chunk) > COOLDOWN_RETREAT) and (getEnemyStructureCount(map, chunk) == 0) then

        setRetreatTick(map, chunk, tick)
        local exitPath,exitDirection,
            nextExitPath,nextExitDirection  = scoreNeighborsForRetreat(chunk,
                                                                       getNeighborChunks(map,
                                                                                         chunk.x,
                                                                                         chunk.y),
                                                                       scoreRetreatLocation,
                                                                       map)
        local universe = map.universe
        local position = {
            x = chunk.x + 16,
            y = chunk.y + 16
        }
        local retreatPosition
        local surface = map.surface
        if (exitPath == -1) then
            return
        elseif (nextExitPath ~= -1) then
            retreatPosition = findMovementPosition(
                surface,
                positionFromDirectionAndFlat(
                    nextExitDirection,
                    positionFromDirectionAndFlat(
                        exitDirection,
                        position
                    )
                )
            )
            exitPath = nextExitPath
        else
            retreatPosition = findMovementPosition(
                surface,
                positionFromDirectionAndFlat(
                    exitDirection,
                    position
                )
            )
        end

        if retreatPosition then
            position.x = retreatPosition.x
            position.y = retreatPosition.y
        else
            return
        end

        local newSquad = findNearbyRetreatingSquad(map, exitPath)
        local created = false

        if not newSquad then
            if (universe.squadCount < universe.AI_MAX_SQUAD_COUNT) then
                created = true
                local base = findNearbyBase(map, chunk)
                if not base then
                    return
                end
                newSquad = createSquad(position, map, nil, false, base)
            else
                return
            end
        end

        universe.fleeCommand.from = cause
        universe.retreatCommand.group = newSquad.group

        universe.formRetreatCommand.unit_search_distance = radius

        local foundUnits = surface.set_multi_command(universe.formRetreatCommand)

        if (foundUnits == 0) then
            if created then
                newSquad.group.destroy()
            end
            return
        end

        if created then
            universe.groupNumberToSquad[newSquad.groupNumber] = newSquad
            universe.squadCount = universe.squadCount + 1
        end

        newSquad.status = SQUAD_RETREATING

        addSquadToChunk(map, chunk, newSquad)

        newSquad.frenzy = true
        local squadPosition = newSquad.group.position
        newSquad.frenzyPosition.x = squadPosition.x
        newSquad.frenzyPosition.y = squadPosition.y
    end
end

aiDefenseG = aiDefense
return aiDefense
