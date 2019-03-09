if unitGroupUtilsG then
    return unitGroupUtilsG
end
local unitGroupUtils = {}

-- imports

local mapUtils = require("MapUtils")
-- local mathUtils = require("MathUtils")
local constants = require("Constants")
local chunkPropertyUtils = require("ChunkPropertyUtils")

-- constants

-- local HALF_CHUNK_SIZE = constants.HALF_CHUNK_SIZE

local SQUAD_QUEUE_SIZE = constants.SQUAD_QUEUE_SIZE

-- local DEFINES_GROUP_STATE_FINISHED = defines.group_state.finished
local DEFINES_GROUP_STATE_ATTACKING_TARGET = defines.group_state.attacking_target
local DEFINES_GROUP_STATE_ATTACKING_DISTRACTION = defines.group_state.attacking_distraction

local SQUAD_RETREATING = constants.SQUAD_RETREATING
local SQUAD_GUARDING = constants.SQUAD_GUARDING
-- local SQUAD_SETTLING = constants.SQUAD_SETTLING
-- local SQUAD_BUILDING = constants.SQUAD_BUILDING
-- local GROUP_MERGE_DISTANCE = constants.GROUP_MERGE_DISTANCE

-- local RETREAT_FILTER = constants.RETREAT_FILTER

local NO_RETREAT_SQUAD_SIZE_BONUS_MAX = constants.NO_RETREAT_SQUAD_SIZE_BONUS_MAX

local AI_MAX_OVERFLOW_POINTS = constants.AI_MAX_OVERFLOW_POINTS

local AI_MAX_BITER_GROUP_SIZE = constants.AI_MAX_BITER_GROUP_SIZE
local AI_SQUAD_MERGE_THRESHOLD = constants.AI_SQUAD_MERGE_THRESHOLD

local SENTINEL_IMPASSABLE_CHUNK = constants.SENTINEL_IMPASSABLE_CHUNK

-- imported functions

local tRemove = table.remove

local mRandom = math.random

local mLog = math.log10

-- local removeSquadFromChunk = chunkPropertyUtils.removeSquadFromChunk

local mMin = math.min

local getSquadsOnChunk = chunkPropertyUtils.getSquadsOnChunk
-- local removeSquadFromChunk = chunkPropertyUtils.removeSquadFromChunk

local getNeighborChunks = mapUtils.getNeighborChunks

-- local euclideanDistanceNamed = mathUtils.euclideanDistanceNamed

-- module code

function unitGroupUtils.findNearbySquadFiltered(map, chunk)

    local squads = getSquadsOnChunk(map, chunk)
    for i=1,#squads do
        local squad = squads[i]
	local unitGroup = squad.group
	if unitGroup and unitGroup.valid and (squad.status == SQUAD_RETREATING) then
            return squad
        end
    end

    local neighbors = getNeighborChunks(map, chunk.x, chunk.y)

    for i=1,#neighbors do
        local neighbor = neighbors[i]
        if neighbor ~= SENTINEL_IMPASSABLE_CHUNK then
            squads = getSquadsOnChunk(map, neighbor)
            for squadIndex=1,#squads do
                local squad = squads[squadIndex]
                local unitGroup = squad.group
                if unitGroup and unitGroup.valid and (squad.status == SQUAD_RETREATING) then
                    return squad
                end
            end
        end
    end
    return nil
end

function unitGroupUtils.findNearbySquad(map, chunk)

    local squads = getSquadsOnChunk(map, chunk)
    for i=1,#squads do
        local squad = squads[i]
        local unitGroup = squad.group
        if unitGroup and unitGroup.valid then           
            return squad            
        end
    end
    

    local neighbors = getNeighborChunks(map, chunk.x, chunk.y)

    for i=1,#neighbors do
        local neighbor = neighbors[i]
        if neighbor ~= SENTINEL_IMPASSABLE_CHUNK then
            squads = getSquadsOnChunk(map, neighbor)
            for squadIndex=1,#squads do
                local squad = squads[squadIndex]
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
        penalties = {},
        rabid = false,
        frenzy = false,
        settlers = settlers or false,
        kamikaze = false,
        frenzyPosition = {x = 0,
                          y = 0},
        cycles = 0,
        maxDistance = 0,
        attackScoreFunction = 1,
        originPosition = {x = 0,
                          y = 0},
        chunk = nil
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

function unitGroupUtils.membersToSquad(cmd, members, overwriteGroup)
    if (members ~= nil) then
        for i=1,#members do
            local member = members[i]
            if member.valid and (overwriteGroup or (not overwriteGroup and not member.unit_group)) then
                member.set_command(cmd)
            end
        end
    end
end

function unitGroupUtils.convertUnitGroupToSquad(natives, unitGroup)
    if not unitGroup then
        return nil
    end
    local squads = natives.squads
    for i=1,#squads do
        local squad = squads[i]
        if (squad.group == unitGroup) then
            return squad
        end
    end
    local squad = unitGroupUtils.createSquad(nil,nil,unitGroup)
    squad.kamikaze = mRandom() < unitGroupUtils.calculateKamikazeThreshold(#unitGroup.members, natives)
    return squad
end

function unitGroupUtils.calculateKamikazeThreshold(memberCount, natives)
    local squadSizeBonus = mLog((memberCount / natives.attackWaveMaxSize) + 0.1) + 1
    return natives.kamikazeThreshold + (NO_RETREAT_SQUAD_SIZE_BONUS_MAX * squadSizeBonus)
end

function unitGroupUtils.recycleBiters(natives, biters)
    local unitCount = #biters
    for i=1,unitCount do
        biters[i].destroy()
    end
    natives.points = natives.points + (unitCount * natives.unitRefundAmount)

    if (natives.points > AI_MAX_OVERFLOW_POINTS) then
        natives.points = AI_MAX_OVERFLOW_POINTS
    end
end

function unitGroupUtils.cleanBuilders(natives)
    local squads = natives.building
    local squadCount = #squads

    local startIndex = natives.cleanBuildingIndex

    local maxSquadIndex = mMin(startIndex + SQUAD_QUEUE_SIZE, squadCount)
    for i=maxSquadIndex,startIndex,-1 do
        local squad = squads[i]
        local group = squad.group
        if not (group and group.valid) then
            tRemove(squads, i)
        end
    end

    if (maxSquadIndex >= squadCount) then
        natives.cleanBuildingIndex = 1
    else
        natives.cleanBuildingIndex = maxSquadIndex + 1
    end
end

local function isAttacking(group)
    local state = group.state
    return (state == DEFINES_GROUP_STATE_ATTACKING_TARGET) or (state == DEFINES_GROUP_STATE_ATTACKING_DISTRACTION)
end

function unitGroupUtils.regroupSquads(natives, map)
    local squads = natives.squads
    local squadCount = #squads

    local startIndex = natives.regroupIndex

    local maxSquadIndex = mMin(startIndex + SQUAD_QUEUE_SIZE, squadCount)
    for i=startIndex,maxSquadIndex do
        local squad = squads[i]
        local group = squad.group
        if group and group.valid and not isAttacking(group) then
            local memberCount = #group.members
            if (memberCount < AI_SQUAD_MERGE_THRESHOLD) then
                local status = squad.status
                local chunk = squad.chunk

                if chunk then
                    local chunkSquads = getSquadsOnChunk(map, chunk)
                    for p=1,#chunkSquads do
                        local mergeSquad = chunkSquads[p]
                        if (mergeSquad ~= squad) then
                            local mergeGroup = mergeSquad.group
                            if mergeGroup and
                                mergeGroup.valid and                
                                (mergeSquad.status == status) and
                                not isAttacking(mergeGroup)
                            then
                                local mergeMembers = mergeGroup.members
                                local mergeCount = #mergeMembers
                                if ((mergeCount + memberCount) < AI_MAX_BITER_GROUP_SIZE) then
                                    for memberIndex=1, mergeCount do
                                        group.add_member(mergeMembers[memberIndex])
                                    end                                    
                                    mergeGroup.destroy()
                                end
                                squad.status = SQUAD_GUARDING
                                memberCount = memberCount + mergeCount
                                if (memberCount > AI_SQUAD_MERGE_THRESHOLD) then
                                    break
                                end
                            end
                        end                        
                    end                    
                end
            end
        end
    end

    if (maxSquadIndex >= squadCount) then
        natives.regroupIndex = 1
    else
        natives.regroupIndex = maxSquadIndex + 1
    end
end

unitGroupUtilsG = unitGroupUtils
return unitGroupUtils
