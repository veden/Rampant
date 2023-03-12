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


if AiDefenseG then
    return AiDefenseG
end
local AiDefense = {}

--

local Universe

-- imports

local Constants = require("Constants")
local MapUtils = require("MapUtils")
local UnitGroupUtils = require("UnitGroupUtils")
local MovementUtils = require("MovementUtils")
local ChunkPropertyUtils = require("ChunkPropertyUtils")

-- Constants

local PLAYER_PHEROMONE = Constants.PLAYER_PHEROMONE
local BASE_PHEROMONE = Constants.BASE_PHEROMONE

local PLAYER_PHEROMONE_MULTIPLER = Constants.PLAYER_PHEROMONE_MULTIPLER

local SQUAD_RETREATING = Constants.SQUAD_RETREATING

local COOLDOWN_RETREAT = Constants.COOLDOWN_RETREAT

-- imported functions

local findNearbyBase = ChunkPropertyUtils.findNearbyBase

local addSquadToChunk = ChunkPropertyUtils.addSquadToChunk

local positionFromDirectionAndFlat = MapUtils.positionFromDirectionAndFlat
local getNeighborChunks = MapUtils.getNeighborChunks
local findNearbyRetreatingSquad = UnitGroupUtils.findNearbyRetreatingSquad
local createSquad = UnitGroupUtils.createSquad
local scoreNeighborsForRetreat = MovementUtils.scoreNeighborsForRetreat
local findMovementPosition = MovementUtils.findMovementPosition

local getRetreatTick = ChunkPropertyUtils.getRetreatTick
local setRetreatTick = ChunkPropertyUtils.setRetreatTick
local getEnemyStructureCount = ChunkPropertyUtils.getEnemyStructureCount

-- module code

local function scoreRetreatLocation(map, neighborChunk)
    return (-neighborChunk[BASE_PHEROMONE] +
            -(neighborChunk[PLAYER_PHEROMONE] * PLAYER_PHEROMONE_MULTIPLER) +
            -((neighborChunk.playerBaseGenerator or 0) * 1000))
end

function AiDefense.retreatUnits(chunk, cause, map, tick, radius)
    if (tick - getRetreatTick(chunk) > COOLDOWN_RETREAT) and (getEnemyStructureCount(chunk) == 0) then

        setRetreatTick(chunk, tick)
        local exitPath,exitDirection,
            nextExitPath,nextExitDirection  = scoreNeighborsForRetreat(chunk,
                                                                       getNeighborChunks(map,
                                                                                         chunk.x,
                                                                                         chunk.y),
                                                                       scoreRetreatLocation,
                                                                       map)
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
            if (Universe.squadCount < Universe.AI_MAX_SQUAD_COUNT) then
                created = true
                local base = findNearbyBase(chunk)
                if not base then
                    return
                end
                newSquad = createSquad(position, map, nil, false, base)
            else
                return
            end
        end

        Universe.fleeCommand.from = cause
        Universe.retreatCommand.group = newSquad.group

        Universe.formRetreatCommand.unit_search_distance = radius

        local foundUnits = surface.set_multi_command(Universe.formRetreatCommand)

        if (foundUnits == 0) then
            if created then
                newSquad.group.destroy()
            end
            return
        end

        if created then
            Universe.groupNumberToSquad[newSquad.groupNumber] = newSquad
            Universe.squadCount = Universe.squadCount + 1
        end

        newSquad.status = SQUAD_RETREATING

        addSquadToChunk(chunk, newSquad)

        newSquad.frenzy = true
        local squadPosition = newSquad.group.position
        newSquad.frenzyPosition.x = squadPosition.x
        newSquad.frenzyPosition.y = squadPosition.y
    end
end

function AiDefense.init(universe)
    Universe = universe
end

AiDefenseG = AiDefense
return AiDefense
