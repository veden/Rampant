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


if UnitGroupUtilsG then
    return UnitGroupUtilsG
end
local UnitGroupUtils = {}

--

local Universe

-- imports

local MapUtils = require("MapUtils")
local Constants = require("Constants")
local ChunkPropertyUtils = require("ChunkPropertyUtils")
local MathUtils = require("MathUtils")

-- Constants

local MINIMUM_EXPANSION_DISTANCE = Constants.MINIMUM_EXPANSION_DISTANCE
local SQUAD_RETREATING = Constants.SQUAD_RETREATING
local SQUAD_GUARDING = Constants.SQUAD_GUARDING

-- imported functions

local gaussianRandomRangeRG = MathUtils.gaussianRandomRangeRG

local getSquadsOnChunk = ChunkPropertyUtils.getSquadsOnChunk

local getNeighborChunks = MapUtils.getNeighborChunks

-- module code

function UnitGroupUtils.findNearbyRetreatingSquad(map, chunk)

    for _,squad in pairs(getSquadsOnChunk(map, chunk)) do
        local unitGroup = squad.group
        if (squad.status == SQUAD_RETREATING) and unitGroup and unitGroup.valid then
            return squad
        end
    end

    local neighbors = getNeighborChunks(map, chunk.x, chunk.y)

    for i=1,#neighbors do
        local neighbor = neighbors[i]
        if neighbor ~= -1 then
            for _,squad in pairs(getSquadsOnChunk(map, neighbor)) do
                local unitGroup = squad.group
                if (squad.status == SQUAD_RETREATING) and unitGroup and unitGroup.valid then
                    return squad
                end
            end
        end
    end
    return nil
end

function UnitGroupUtils.findNearbySquad(map, chunk)

    for _,squad in pairs(getSquadsOnChunk(map, chunk)) do
        local unitGroup = squad.group
        if unitGroup and unitGroup.valid then
            return squad
        end
    end

    local neighbors = getNeighborChunks(map, chunk.x, chunk.y)

    for i=1,#neighbors do
        local neighbor = neighbors[i]
        if neighbor ~= -1 then
            for _,squad in pairs(getSquadsOnChunk(map, neighbor)) do
                local unitGroup = squad.group
                if unitGroup and unitGroup.valid then
                    return squad
                end
            end
        end
    end

    return nil
end

function UnitGroupUtils.calculateSettlerMaxDistance()
    local targetDistance
    local distanceRoll = Universe.random()
    if distanceRoll < 0.05 then
        return 0
    elseif distanceRoll < 0.30 then
        targetDistance = Universe.expansionLowTargetDistance
    elseif distanceRoll < 0.70 then
        targetDistance = Universe.expansionMediumTargetDistance
    elseif distanceRoll < 0.95 then
        targetDistance = Universe.expansionHighTargetDistance
    else
        return Universe.expansionMaxDistance
    end
    return gaussianRandomRangeRG(targetDistance,
                                 Universe.expansionDistanceDeviation,
                                 MINIMUM_EXPANSION_DISTANCE,
                                 Universe.expansionMaxDistance,
                                 Universe.random)
end

function UnitGroupUtils.createSquad(position, map, group, settlers, base)
    local unitGroup = group or map.surface.create_unit_group({position=position})

    local squad = {
        group = unitGroup,
        status = SQUAD_GUARDING,
        rabid = false,
        penalties = {},
        base = base,
        type = base.stateAI,
        frenzy = false,
        map = map,
        wanders = 0,
        settlers = settlers or false,
        kamikaze = false,
        frenzyPosition = {x = 0,
                          y = 0},
        maxDistance = 0,
        groupNumber = unitGroup.group_number,
        originPosition = {x = 0,
                          y = 0},
        commandTick = nil,
        chunk = -1
    }

    if settlers then
        squad.maxDistance = UnitGroupUtils.calculateSettlerMaxDistance()
    end

    if position then
        squad.originPosition.x = position.x
        squad.originPosition.y = position.y
    elseif group then
        squad.originPosition.x = group.position.x
        squad.originPosition.y = group.position.y
    end

    return squad
end

function UnitGroupUtils.calculateKamikazeSquadThreshold(memberCount)
    local threshold = (memberCount / Universe.attackWaveMaxSize) * 0.2 + (Universe.evolutionLevel * 0.2)
    return threshold
end

function UnitGroupUtils.calculateKamikazeSettlerThreshold(memberCount)
    local threshold = (memberCount / Universe.expansionMaxSize) * 0.2 + (Universe.evolutionLevel * 0.2)
    return threshold
end

function UnitGroupUtils.init(universe)
    Universe = universe
end

UnitGroupUtilsG = UnitGroupUtils
return UnitGroupUtils
