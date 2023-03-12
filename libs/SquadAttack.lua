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


if SquadAttackG then
    return SquadAttackG
end
local SquadAttack = {}

--

local Universe

-- imports

local Constants = require("Constants")
local MapUtils = require("MapUtils")
local MovementUtils = require("MovementUtils")
local MathUtils = require("MathUtils")
local ChunkPropertyUtils = require("ChunkPropertyUtils")
local QueryUtils = require("QueryUtils")

-- Constants

local PLAYER_PHEROMONE_GENERATOR_THRESHOLD = Constants.PLAYER_PHEROMONE_GENERATOR_THRESHOLD
local COMMAND_TIMEOUT = Constants.COMMAND_TIMEOUT
local PLAYER_PHEROMONE = Constants.PLAYER_PHEROMONE
local BASE_PHEROMONE = Constants.BASE_PHEROMONE
local ENEMY_PHEROMONE = Constants.ENEMY_PHEROMONE
local RESOURCE_PHEROMONE = Constants.RESOURCE_PHEROMONE

local FIVE_DEATH_PHEROMONE_GENERATOR_AMOUNT = Constants.FIVE_DEATH_PHEROMONE_GENERATOR_AMOUNT

local SQUAD_BUILDING = Constants.SQUAD_BUILDING

local SQUAD_RAIDING = Constants.SQUAD_RAIDING
local SQUAD_SETTLING = Constants.SQUAD_SETTLING
local SQUAD_GUARDING = Constants.SQUAD_GUARDING
local SQUAD_RETREATING = Constants.SQUAD_RETREATING

local BASE_AI_STATE_SIEGE = Constants.BASE_AI_STATE_SIEGE
local BASE_AI_STATE_AGGRESSIVE = Constants.BASE_AI_STATE_AGGRESSIVE

local PLAYER_PHEROMONE_MULTIPLER = Constants.PLAYER_PHEROMONE_MULTIPLER

local DEFINES_DISTRACTION_NONE = defines.distraction.none
local DEFINES_DISTRACTION_BY_ENEMY = defines.distraction.by_enemy
local DEFINES_DISTRACTION_BY_ANYTHING = defines.distraction.by_anything

-- imported functions

local tableSize = table_size
local setPositionInCommand = QueryUtils.setPositionInCommand

local euclideanDistancePoints = MathUtils.euclideanDistancePoints

local findMovementPosition = MovementUtils.findMovementPosition

local removeSquadFromChunk = ChunkPropertyUtils.removeSquadFromChunk
local addDeathGenerator = ChunkPropertyUtils.addDeathGenerator

local getNeighborChunks = MapUtils.getNeighborChunks
local addSquadToChunk = ChunkPropertyUtils.addSquadToChunk
local getChunkByXY = MapUtils.getChunkByXY
local positionToChunkXY = MapUtils.positionToChunkXY
local addMovementPenalty = MovementUtils.addMovementPenalty
local positionFromDirectionAndFlat = MapUtils.positionFromDirectionAndFlat

local euclideanDistanceNamed = MathUtils.euclideanDistanceNamed

local scoreNeighborsForAttack = MovementUtils.scoreNeighborsForAttack
local scoreNeighborsForSettling = MovementUtils.scoreNeighborsForSettling

-- module code
local function scoreResourceLocation(map, neighborChunk)
    return neighborChunk[RESOURCE_PHEROMONE]
        - (neighborChunk[PLAYER_PHEROMONE] * PLAYER_PHEROMONE_MULTIPLER)
        - neighborChunk[ENEMY_PHEROMONE]
end

local function scoreSiegeLocation(map, neighborChunk)
    local settle = neighborChunk[BASE_PHEROMONE]
        + neighborChunk[RESOURCE_PHEROMONE] * 0.5
        + (neighborChunk[PLAYER_PHEROMONE] * PLAYER_PHEROMONE_MULTIPLER)

    return settle - neighborChunk[ENEMY_PHEROMONE]
end

local function scoreAttackLocation(map, neighborChunk)
    local damage = neighborChunk[BASE_PHEROMONE] +
        (neighborChunk[PLAYER_PHEROMONE] * PLAYER_PHEROMONE_MULTIPLER)
    return damage
end

local function settleMove(map, squad)
    local group = squad.group
    local targetPosition = {x=0,y=0}

    local groupPosition = group.position
    local x, y = positionToChunkXY(groupPosition)
    local chunk = getChunkByXY(map, x, y)
    local scoreFunction = scoreResourceLocation
    if (squad.type == BASE_AI_STATE_SIEGE) then
        scoreFunction = scoreSiegeLocation
    end
    local squadChunk = squad.chunk
    if squadChunk ~= -1 then
        addDeathGenerator(squadChunk, -FIVE_DEATH_PHEROMONE_GENERATOR_AMOUNT)
    end
    if chunk ~= -1 then
        addSquadToChunk(chunk, squad)
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
        (
            (distance >= squad.maxDistance) or
            (
                chunk.resourceGenerator and (not chunk.nestCount) and (not chunk.hiveCount)
            )
        )
    then
        position = findMovementPosition(surface, groupPosition)

        if not position then
            position = groupPosition
        end

        cmd = Universe.settleCommand
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
            cmd = Universe.wanderCommand
            group.set_command(cmd)
            return
        elseif (attackDirection ~= 0) then
            local attackPlayerThreshold = Universe.attackPlayerThreshold

            if (nextAttackChunk ~= -1) then
                if (not nextAttackChunk.playerBaseGenerator)
                    and ((nextAttackChunk.playerGenerator or 0) < PLAYER_PHEROMONE_GENERATOR_THRESHOLD)
                then
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
                    position = groupPosition
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
                    addDeathGenerator(nextAttackChunk, -FIVE_DEATH_PHEROMONE_GENERATOR_AMOUNT)
                else
                    addDeathGenerator(attackChunk, -FIVE_DEATH_PHEROMONE_GENERATOR_AMOUNT)
                end
            else
                cmd = Universe.wanderCommand
                group.set_command(cmd)
                return
            end

            if (nextAttackChunk ~= -1)
                and (
                    nextAttackChunk.playerBaseGenerator
                    or ((nextAttackChunk.playerBaseGenerator or 0) >= PLAYER_PHEROMONE_GENERATOR_THRESHOLD)
                )
            then
                cmd = Universe.settleCommand
                squad.status = SQUAD_BUILDING
                if squad.kamikaze then
                    cmd.distraction = DEFINES_DISTRACTION_NONE
                else
                    cmd.distraction = DEFINES_DISTRACTION_BY_ENEMY
                end
            elseif attackChunk.playerBaseGenerator
                or (attackChunk[PLAYER_PHEROMONE] >= attackPlayerThreshold)
            then
                cmd = Universe.attackCommand

                if not squad.rabid then
                    squad.frenzy = true
                    squad.frenzyPosition.x = groupPosition.x
                    squad.frenzyPosition.y = groupPosition.y
                end
            else
                cmd = Universe.moveCommand
                if squad.rabid or squad.kamikaze then
                    cmd.distraction = DEFINES_DISTRACTION_NONE
                else
                    cmd.distraction = DEFINES_DISTRACTION_BY_ENEMY
                end
            end
        else
            cmd = Universe.settleCommand
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
    local targetPosition = {0,0}

    local group = squad.group

    local surface = map.surface
    local position
    local groupPosition = group.position
    local x, y = positionToChunkXY(groupPosition)
    local chunk = getChunkByXY(map, x, y)
    local attackScorer = scoreAttackLocation
    local squadChunk = squad.chunk
    if squadChunk ~= -1 then
        addDeathGenerator(squadChunk, -FIVE_DEATH_PHEROMONE_GENERATOR_AMOUNT)
    end
    if chunk ~= -1 then
        addSquadToChunk(chunk, squad)
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
        cmd = Universe.wanderCommand
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
        cmd = Universe.wanderCommand
        group.set_command(cmd)
        return
    else
        targetPosition.x = position.x
        targetPosition.y = position.y
        if (nextAttackChunk ~= -1) then
            addDeathGenerator(nextAttackChunk, -FIVE_DEATH_PHEROMONE_GENERATOR_AMOUNT)
        else
            addDeathGenerator(attackChunk, -FIVE_DEATH_PHEROMONE_GENERATOR_AMOUNT)
        end
    end

    if attackChunk.playerBaseGenerator and
        (attackChunk[PLAYER_PHEROMONE] >= Universe.attackPlayerThreshold)
    then
        cmd = Universe.attackCommand

        if not squad.rabid then
            squad.frenzy = true
            squad.frenzyPosition.x = groupPosition.x
            squad.frenzyPosition.y = groupPosition.y
        end
    else
        cmd = Universe.moveCommand
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
    local groupPosition = group.position
    local newGroupPosition = findMovementPosition(map.surface, groupPosition)

    if not newGroupPosition then
        setPositionInCommand(Universe.settleCommand, groupPosition)
    else
        setPositionInCommand(Universe.settleCommand, newGroupPosition)
    end

    group.set_command(Universe.compoundSettleCommand)
end

function SquadAttack.cleanSquads(tick)
    local squads = Universe.groupNumberToSquad
    local groupId = Universe.squadIterator
    local squad
    if not groupId then
        groupId, squad = next(squads, groupId)
    else
        squad = squads[groupId]
    end
    if not groupId then
        Universe.squadIterator = nil
        if (tableSize(squads) == 0) then
            -- this is needed as the next command remembers the max length a table has been
            Universe.groupNumberToSquad = {}
        end
    else
        Universe.squadIterator = next(squads, groupId)
        local group = squad.group
        if not group.valid then
            if squad.chunk ~= -1 then
                addDeathGenerator(squad.chunk, -FIVE_DEATH_PHEROMONE_GENERATOR_AMOUNT)
            end
            removeSquadFromChunk(squad)
            if squad.settlers then
                Universe.builderCount = Universe.builderCount - 1
                if Universe.builderCount < 0 then
                    Universe.builderCount = 0
                end
            else
                Universe.squadCount = Universe.squadCount - 1
                if Universe.squadCount < 0 then
                    Universe.squadCount = 0
                end
                if squad.type == BASE_AI_STATE_AGGRESSIVE then
                    local base = squad.base
                    base.sentAggressiveGroups = base.sentAggressiveGroups - 1
                    if base.sentAggressiveGroups < 0 then
                        base.sentAggressiveGroups = 0
                    end
                end
            end
            squads[groupId] = nil
        elseif (group.state == 4) then
            squad.wanders = 0
            SquadAttack.squadDispatch(squad.map, squad, tick)
        elseif (squad.commandTick and (squad.commandTick < tick)) then
            if squad.wanders > 5 then
                squad.group.destroy()
            else
                squad.wanders = squad.wanders + 1
                local cmd = Universe.wander2Command
                squad.commandTick = tick + COMMAND_TIMEOUT
                group.set_command(cmd)
                group.start_moving()
            end
        end
    end
end

function SquadAttack.squadDispatch(map, squad, tick)
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
            removeSquadFromChunk(squad)
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

function SquadAttack.init(universe)
    Universe = universe
end

SquadAttackG = SquadAttack
return SquadAttack
