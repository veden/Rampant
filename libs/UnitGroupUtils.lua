if unitGroupUtilsG then
    return unitGroupUtilsG
end
local unitGroupUtils = {}

-- imports

local mapUtils = require("MapUtils")
local constants = require("Constants")
local chunkPropertyUtils = require("ChunkPropertyUtils")

-- constants

local SQUAD_RETREATING = constants.SQUAD_RETREATING
local SQUAD_GUARDING = constants.SQUAD_GUARDING

-- imported functions

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

function unitGroupUtils.createSquad(position, surface, group, settlers)
    local unitGroup = group or surface.create_unit_group({position=position})

    local squad = {
        group = unitGroup,
        status = SQUAD_GUARDING,
        rabid = false,
        penalties = {},
        frenzy = false,
        settlers = settlers or false,
        kamikaze = false,
        frenzyPosition = {x = 0,
                          y = 0},
        maxDistance = 0,
        groupNumber = unitGroup.group_number,
        originPosition = {x = 0,
                          y = 0},
        chunk = -1
    }

    if position then
        squad.originPosition.x = position.x
        squad.originPosition.y = position.y
    elseif group then
        squad.originPosition.x = group.position.x
        squad.originPosition.y = group.position.y
    end

    return squad
end

function unitGroupUtils.calculateKamikazeThreshold(memberCount, map)
    local threshold = (memberCount / map.attackWaveMaxSize) * 0.2 + (map.evolutionLevel * 0.2)
    return threshold
end

unitGroupUtilsG = unitGroupUtils
return unitGroupUtils
