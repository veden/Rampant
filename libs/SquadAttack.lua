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


if (squadAttackG) then
    return squadAttackG
end
local squadAttack = {}

-- imports

local constants = require("Constants")
local mapUtils = require("MapUtils")
local movementUtils = require("MovementUtils")
local mathUtils = require("MathUtils")
local chunkPropertyUtils = require("ChunkPropertyUtils")
local queryUtils = require("QueryUtils")

-- constants

local COMMAND_TIMEOUT = constants.COMMAND_TIMEOUT
local PLAYER_PHEROMONE = constants.PLAYER_PHEROMONE
local BASE_PHEROMONE = constants.BASE_PHEROMONE
local RESOURCE_PHEROMONE = constants.RESOURCE_PHEROMONE

local FIVE_DEATH_PHEROMONE_GENERATOR_AMOUNT = constants.FIVE_DEATH_PHEROMONE_GENERATOR_AMOUNT

local SQUAD_BUILDING = constants.SQUAD_BUILDING

local SQUAD_RAIDING = constants.SQUAD_RAIDING
local SQUAD_SETTLING = constants.SQUAD_SETTLING
local SQUAD_GUARDING = constants.SQUAD_GUARDING
local SQUAD_RETREATING = constants.SQUAD_RETREATING

local AI_STATE_SIEGE = constants.AI_STATE_SIEGE

local PLAYER_PHEROMONE_MULTIPLER = constants.PLAYER_PHEROMONE_MULTIPLER

local DEFINES_DISTRACTION_NONE = defines.distraction.none
local DEFINES_DISTRACTION_BY_ENEMY = defines.distraction.by_enemy
local DEFINES_DISTRACTION_BY_ANYTHING = defines.distraction.by_anything

-- imported functions

local setPositionInCommand = queryUtils.setPositionInCommand

local euclideanDistancePoints = mathUtils.euclideanDistancePoints

local findMovementPosition = movementUtils.findMovementPosition

local removeSquadFromChunk = chunkPropertyUtils.removeSquadFromChunk
local addDeathGenerator = chunkPropertyUtils.addDeathGenerator
local getDeathGenerator = chunkPropertyUtils.getDeathGenerator

local getNestCount = chunkPropertyUtils.getNestCount

local getNeighborChunks = mapUtils.getNeighborChunks
local addSquadToChunk = chunkPropertyUtils.addSquadToChunk
local getChunkByXY = mapUtils.getChunkByXY
local positionToChunkXY = mapUtils.positionToChunkXY
local addMovementPenalty = movementUtils.addMovementPenalty
local positionFromDirectionAndFlat = mapUtils.positionFromDirectionAndFlat

local euclideanDistanceNamed = mathUtils.euclideanDistanceNamed

local getPlayerBaseGenerator = chunkPropertyUtils.getPlayerBaseGenerator
local getResourceGenerator = chunkPropertyUtils.getResourceGenerator

local scoreNeighborsForAttack = movementUtils.scoreNeighborsForAttack
local scoreNeighborsForSettling = movementUtils.scoreNeighborsForSettling

-- module code

local function scoreResourceLocationKamikaze(_, neighborChunk)
    local settle = neighborChunk[RESOURCE_PHEROMONE]
    return settle - (neighborChunk[PLAYER_PHEROMONE] * PLAYER_PHEROMONE_MULTIPLER)
end

local function scoreSiegeLocationKamikaze(_, neighborChunk)
    local settle = neighborChunk[BASE_PHEROMONE] +
        neighborChunk[RESOURCE_PHEROMONE] + (neighborChunk[PLAYER_PHEROMONE] * PLAYER_PHEROMONE_MULTIPLER)

    return settle
end

local function scoreResourceLocation(map, neighborChunk)
    local settle = -getDeathGenerator(map, neighborChunk) + neighborChunk[RESOURCE_PHEROMONE]
    return settle - (neighborChunk[PLAYER_PHEROMONE] * PLAYER_PHEROMONE_MULTIPLER)
end

local function scoreSiegeLocation(map, neighborChunk)
    local settle = -getDeathGenerator(map, neighborChunk) + neighborChunk[BASE_PHEROMONE] +
        neighborChunk[RESOURCE_PHEROMONE] + (neighborChunk[PLAYER_PHEROMONE] * PLAYER_PHEROMONE_MULTIPLER)

    return settle
end

local function scoreAttackLocation(map, neighborChunk)
    local damage = -getDeathGenerator(map, neighborChunk) + neighborChunk[BASE_PHEROMONE] +
        (neighborChunk[PLAYER_PHEROMONE] * PLAYER_PHEROMONE_MULTIPLER)
    return damage
end

local function scoreAttackKamikazeLocation(_, neighborChunk)
    local damage = neighborChunk[BASE_PHEROMONE] + (neighborChunk[PLAYER_PHEROMONE] * PLAYER_PHEROMONE_MULTIPLER)
    return damage
end

local function settleMove(map, squad)
    local universe = map.universe
    local group = squad.group
    local targetPosition = {x=0,y=0}

    local groupPosition = group.position
    local x, y = positionToChunkXY(groupPosition)
    local chunk = getChunkByXY(map, x, y)
    local scoreFunction = scoreResourceLocation
    if (map.state == AI_STATE_SIEGE) then
        if squad.kamikaze then
            scoreFunction = scoreSiegeLocationKamikaze
        else
            scoreFunction = scoreSiegeLocation
        end
    elseif squad.kamikaze then
        scoreFunction = scoreResourceLocationKamikaze
    end
    local squadChunk = squad.chunk
    if squadChunk ~= -1 then
        addDeathGenerator(map, squadChunk, FIVE_DEATH_PHEROMONE_GENERATOR_AMOUNT)
    end
    if chunk ~= -1 then
        addSquadToChunk(map, chunk, squad)
        addMovementPenalty(squad, chunk)
        if not squad.group.valid then
            return
        end
    end
    local distance = euclideanDistancePoints(groupPosition.x,
                                             groupPosition.y,
                                             squad.originPosition.x,
                                             squad.originPosition.y)
    local cmd
    local position
    local surface = map.surface

    if (chunk ~= -1) and
        ((distance >= squad.maxDistance) or ((getResourceGenerator(map, chunk) ~= 0) and (getNestCount(map, chunk) == 0)))
    then
        position = findMovementPosition(surface, groupPosition)

        if not position then
            position = groupPosition
        end

        cmd = universe.settleCommand
        if squad.kamikaze then
            cmd.distraction = DEFINES_DISTRACTION_NONE
        else
            cmd.distraction = DEFINES_DISTRACTION_BY_ENEMY
        end

        setPositionInCommand(cmd, position)

        squad.status = SQUAD_BUILDING

        group.set_command(cmd)
    else
        local attackChunk,
            attackDirection,
            nextAttackChunk,
            nextAttackDirection = scoreNeighborsForSettling(map,
                                                            chunk,
                                                            getNeighborChunks(map, x, y),
                                                            scoreFunction)

        if (attackChunk == -1) then
            cmd = universe.wanderCommand
            group.set_command(cmd)
            return
        elseif (attackDirection ~= 0) then
            local attackPlayerThreshold = universe.attackPlayerThreshold

            if (nextAttackChunk ~= -1) then
                if (getPlayerBaseGenerator(map,nextAttackChunk) == 0) or (map.state ~= AI_STATE_SIEGE) then
                    attackChunk = nextAttackChunk
                    position = findMovementPosition(
                        surface,
                        positionFromDirectionAndFlat(
                            nextAttackDirection,
                            positionFromDirectionAndFlat(
                                attackDirection,
                                groupPosition
                            )
                        )
                    )
                else
                    position = findMovementPosition(
                        surface,
                        positionFromDirectionAndFlat(
                            nextAttackDirection,
                            groupPosition,
                            1.3
                        )
                    )
                    if not position then
                        position = findMovementPosition(
                            surface,
                            positionFromDirectionAndFlat(
                                attackDirection,
                                groupPosition,
                                1.3
                            )
                        )
                    end
                end
            else
                position = findMovementPosition(
                    surface,
                    positionFromDirectionAndFlat(
                        attackDirection,
                        groupPosition
                    )
                )
            end

            if position then
                targetPosition.x = position.x
                targetPosition.y = position.y
                if nextAttackChunk ~= -1 then
                    addDeathGenerator(map, nextAttackChunk, FIVE_DEATH_PHEROMONE_GENERATOR_AMOUNT)
                else
                    addDeathGenerator(map, attackChunk, FIVE_DEATH_PHEROMONE_GENERATOR_AMOUNT)
                end
            else
                cmd = universe.wanderCommand
                group.set_command(cmd)
                return
            end

            if (nextAttackChunk ~= -1) and (map.state == AI_STATE_SIEGE) and (getPlayerBaseGenerator(map, nextAttackChunk) ~= 0) then
                cmd = universe.settleCommand
                squad.status = SQUAD_BUILDING
                if squad.kamikaze then
                    cmd.distraction = DEFINES_DISTRACTION_NONE
                else
                    cmd.distraction = DEFINES_DISTRACTION_BY_ENEMY
                end
            elseif (getPlayerBaseGenerator(map, attackChunk) ~= 0) or
                (attackChunk[PLAYER_PHEROMONE] >= attackPlayerThreshold)
            then
                cmd = universe.attackCommand

                if not squad.rabid then
                    squad.frenzy = true
                    squad.frenzyPosition.x = groupPosition.x
                    squad.frenzyPosition.y = groupPosition.y
                end
            else
                cmd = universe.moveCommand
                if squad.rabid or squad.kamikaze then
                    cmd.distraction = DEFINES_DISTRACTION_NONE
                else
                    cmd.distraction = DEFINES_DISTRACTION_BY_ENEMY
                end
            end
        else
            cmd = universe.settleCommand
            targetPosition.x = groupPosition.x
            targetPosition.y = groupPosition.y

            if squad.kamikaze then
                cmd.distraction = DEFINES_DISTRACTION_NONE
            else
                cmd.distraction = DEFINES_DISTRACTION_BY_ENEMY
            end

            squad.status = SQUAD_BUILDING
        end

        setPositionInCommand(cmd, targetPosition)

        group.set_command(cmd)
    end
end

local function attackMove(map, squad)

    local universe = map.universe
    local targetPosition = {0,0}

    local group = squad.group

    local surface = map.surface
    local position
    local groupPosition = group.position
    local x, y = positionToChunkXY(groupPosition)
    local chunk = getChunkByXY(map, x, y)
    local attackScorer = scoreAttackLocation
    if squad.kamikaze then
        attackScorer = scoreAttackKamikazeLocation
    end
    local squadChunk = squad.chunk
    if squadChunk ~= -1 then
        addDeathGenerator(map, squadChunk, FIVE_DEATH_PHEROMONE_GENERATOR_AMOUNT)
    end
    if chunk ~= -1 then
        addSquadToChunk(map, chunk, squad)
        addMovementPenalty(squad, chunk)
        if not squad.group.valid then
            return
        end
    end
    squad.frenzy = (squad.frenzy and (euclideanDistanceNamed(groupPosition, squad.frenzyPosition) < 100))
    local attackChunk, attackDirection,
        nextAttackChunk, nextAttackDirection = scoreNeighborsForAttack(map,
                                                                       chunk,
                                                                       getNeighborChunks(map, x, y),
                                                                       attackScorer)
    local cmd
    if (attackChunk == -1) then
        cmd = universe.wanderCommand
        group.set_command(cmd)
        return
    end

    if (nextAttackChunk ~= -1) then
        attackChunk = nextAttackChunk
        position = findMovementPosition(
            surface,
            positionFromDirectionAndFlat(
                nextAttackDirection,
                positionFromDirectionAndFlat(
                    attackDirection,
                    groupPosition
                )
            )
        )
    else
        position = findMovementPosition(
            surface,
            positionFromDirectionAndFlat(
                attackDirection,
                groupPosition
            )
        )
    end

    if not position then
        cmd = universe.wanderCommand
        group.set_command(cmd)
        return
    else
        targetPosition.x = position.x
        targetPosition.y = position.y
        if (nextAttackChunk ~= -1) then
            addDeathGenerator(map, nextAttackChunk, FIVE_DEATH_PHEROMONE_GENERATOR_AMOUNT)
        else
            addDeathGenerator(map, attackChunk, FIVE_DEATH_PHEROMONE_GENERATOR_AMOUNT)
        end
    end

    if (getPlayerBaseGenerator(map, attackChunk) ~= 0) and
        (attackChunk[PLAYER_PHEROMONE] >= universe.attackPlayerThreshold)
    then
        cmd = universe.attackCommand

        if not squad.rabid then
            squad.frenzy = true
            squad.frenzyPosition.x = groupPosition.x
            squad.frenzyPosition.y = groupPosition.y
        end
    else
        cmd = universe.moveCommand
        if squad.rabid or squad.frenzy then
            cmd.distraction = DEFINES_DISTRACTION_BY_ANYTHING
        else
            cmd.distraction = DEFINES_DISTRACTION_BY_ENEMY
        end
    end
    setPositionInCommand(cmd, targetPosition)

    group.set_command(cmd)
end

local function buildMove(map, squad)
    local group = squad.group
    local universe = map.universe
    local groupPosition = group.position
    local newGroupPosition = findMovementPosition(map.surface, groupPosition)

    if not newGroupPosition then
        setPositionInCommand(universe.settleCommand, groupPosition)
    else
        setPositionInCommand(universe.settleCommand, newGroupPosition)
    end

    group.set_command(universe.compoundSettleCommand)
end

function squadAttack.cleanSquads(universe, tick)
    local squads = universe.groupNumberToSquad
    local groupId = universe.squadIterator
    local squad
    if not groupId then
        groupId, squad = next(squads, groupId)
    else
        squad = squads[groupId]
    end
    if not groupId then
        universe.squadIterator = nil
        if (table_size(squads) == 0) then
            -- this is needed as the next command remembers the max length a table has been
            universe.groupNumberToSquad = {}
        end
    else
        universe.squadIterator = next(squads, groupId)
        local group = squad.group
        if not group.valid then
            if squad.chunk ~= -1 then
                addDeathGenerator(squad.map, squad.chunk, FIVE_DEATH_PHEROMONE_GENERATOR_AMOUNT)
            end
            removeSquadFromChunk(squad.map, squad)
            if squad.settlers then
                universe.builderCount = universe.builderCount - 1
            else
                universe.squadCount = universe.squadCount - 1
            end
            squads[groupId] = nil
        elseif (group.state == 4) then
            squad.wanders = 0
            squadAttack.squadDispatch(squad.map, squad, tick)
        elseif (squad.commandTick and (squad.commandTick < tick)) then
            if squad.wanders > 5 then
                squad.group.destroy()
            else
                squad.wanders = squad.wanders + 1
                local cmd = universe.wander2Command
                squad.commandTick = tick + COMMAND_TIMEOUT
                group.set_command(cmd)
                group.start_moving()
            end
        end
    end
end

function squadAttack.squadDispatch(map, squad, tick)
    local group = squad.group
    if group and group.valid then
        local status = squad.status
        if (status == SQUAD_RAIDING) then
            squad.commandTick = tick + COMMAND_TIMEOUT
            attackMove(map, squad)
        elseif (status == SQUAD_SETTLING) then
            squad.commandTick = tick + COMMAND_TIMEOUT
            settleMove(map, squad)
        elseif (status == SQUAD_RETREATING) then
            squad.commandTick = tick + COMMAND_TIMEOUT
            if squad.settlers then
                squad.status = SQUAD_SETTLING
                settleMove(map, squad)
            else
                squad.status = SQUAD_RAIDING
                attackMove(map, squad)
            end
        elseif (status == SQUAD_BUILDING) then
            squad.commandTick = tick + COMMAND_TIMEOUT
            removeSquadFromChunk(map, squad)
            buildMove(map, squad)
        elseif (status == SQUAD_GUARDING) then
            squad.commandTick = tick + COMMAND_TIMEOUT
            if squad.settlers then
                squad.status = SQUAD_SETTLING
                settleMove(map, squad)
            else
                squad.status = SQUAD_RAIDING
                attackMove(map, squad)
            end
        end
    end
end

squadAttackG = squadAttack
return squadAttack
