if unitGroupUtilsG then
    return unitGroupUtilsG
end
local unitGroupUtils = {}

-- imports

local mapUtils = require("MapUtils")
local constants = require("Constants")
local chunkPropertyUtils = require("ChunkPropertyUtils")
local chunkUtils = require("ChunkUtils")
local movementUtils = require("MovementUtils")

-- constants

local TEN_DEATH_PHEROMONE_GENERATOR_AMOUNT = constants.TEN_DEATH_PHEROMONE_GENERATOR_AMOUNT

local DIVISOR_DEATH_TRAIL_TABLE = constants.DIVISOR_DEATH_TRAIL_TABLE
local SQUAD_QUEUE_SIZE = constants.SQUAD_QUEUE_SIZE

local DEFINES_GROUP_STATE_ATTACKING_TARGET = defines.group_state.attacking_target
local DEFINES_GROUP_STATE_ATTACKING_DISTRACTION = defines.group_state.attacking_distraction

local SQUAD_RETREATING = constants.SQUAD_RETREATING
local SQUAD_GUARDING = constants.SQUAD_GUARDING

local AI_MAX_BITER_GROUP_SIZE = constants.AI_MAX_BITER_GROUP_SIZE
local AI_SQUAD_MERGE_THRESHOLD = constants.AI_SQUAD_MERGE_THRESHOLD

-- imported functions

local tRemove = table.remove

local mRandom = math.random

local findMovementPosition = movementUtils.findMovementPosition
local removeSquadFromChunk = chunkPropertyUtils.removeSquadFromChunk
local addDeathGenerator = chunkPropertyUtils.addDeathGenerator
local getDeathGenerator = chunkPropertyUtils.getDeathGenerator

local next = next
local table_size = table_size

local mLog = math.log10

local mMin = math.min

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

function unitGroupUtils.calculateKamikazeThreshold(memberCount, natives)
    local threshold = (memberCount / natives.attackWaveMaxSize) * 0.2 + (natives.evolutionLevel * 0.2)
    return threshold
end

unitGroupUtilsG = unitGroupUtils
return unitGroupUtils
