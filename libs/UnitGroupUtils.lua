if unitGroupUtilsG then
    return unitGroupUtilsG
end
local unitGroupUtils = {}

-- imports

local mapUtils = require("MapUtils")
local constants = require("Constants")
local chunkPropertyUtils = require("ChunkPropertyUtils")
local mathUtils = require("MathUtils")

-- constants

local MINIMUM_EXPANSION_DISTANCE = constants.MINIMUM_EXPANSION_DISTANCE
local SQUAD_RETREATING = constants.SQUAD_RETREATING
local SQUAD_GUARDING = constants.SQUAD_GUARDING

-- imported functions

local gaussianRandomRangeRG = mathUtils.gaussianRandomRangeRG

local getSquadsOnChunk = chunkPropertyUtils.getSquadsOnChunk

local getNeighborChunks = mapUtils.getNeighborChunks

-- module code

function unitGroupUtils.findNearbyRetreatingSquad(map, chunk)

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

function unitGroupUtils.findNearbySquad(map, chunk)

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

function unitGroupUtils.calculateSettlerMaxDistance(universe)
    local targetDistance
    local distanceRoll = universe.random()
    if distanceRoll < 0.05 then
        return 0
    elseif distanceRoll < 0.30 then
        targetDistance = universe.expansionLowTargetDistance
    elseif distanceRoll < 0.70 then
        targetDistance = universe.expansionMediumTargetDistance
    elseif distanceRoll < 0.95 then
        targetDistance = universe.expansionHighTargetDistance
    else
        return universe.expansionMaxDistance
    end
    return gaussianRandomRangeRG(targetDistance,
                                 universe.expansionDistanceDeviation,
                                 MINIMUM_EXPANSION_DISTANCE,
                                 universe.expansionMaxDistance,
                                 universe.random)
end

function unitGroupUtils.createSquad(position, map, group, settlers)
    local unitGroup = group or map.surface.create_unit_group({position=position})

    local squad = {
        group = unitGroup,
        status = SQUAD_GUARDING,
        rabid = false,
        penalties = {},
        base = nil,
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
        squad.maxDistance = unitGroupUtils.calculateSettlerMaxDistance(map.universe)
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

function unitGroupUtils.calculateKamikazeSquadThreshold(memberCount, universe)
    local threshold = (memberCount / universe.attackWaveMaxSize) * 0.2 + (universe.evolutionLevel * 0.2)
    return threshold
end

function unitGroupUtils.calculateKamikazeSettlerThreshold(memberCount, universe)
    local threshold = (memberCount / universe.expansionMaxSize) * 0.2 + (universe.evolutionLevel * 0.2)
    return threshold
end

unitGroupUtilsG = unitGroupUtils
return unitGroupUtils
