local unitGroupUtils = {}

-- imports

local mapUtils = require("MapUtils")
local mathUtils = require("MathUtils")
local constants = require("Constants")
local chunkPropertyUtils = require("ChunkPropertyUtils")

-- constants

local HALF_CHUNK_SIZE = constants.HALF_CHUNK_SIZE

local SQUAD_QUEUE_SIZE = constants.SQUAD_QUEUE_SIZE

local DEFINES_GROUP_STATE_FINISHED = defines.group_state.finished
local DEFINES_GROUP_STATE_ATTACKING_TARGET = defines.group_state.attacking_target
local DEFINES_GROUP_STATE_ATTACKING_DISTRACTION = defines.group_state.attacking_distraction

local SQUAD_RETREATING = constants.SQUAD_RETREATING
local SQUAD_GUARDING = constants.SQUAD_GUARDING
local SQUAD_SETTLING = constants.SQUAD_SETTLING
local GROUP_MERGE_DISTANCE = constants.GROUP_MERGE_DISTANCE

local RETREAT_FILTER = constants.RETREAT_FILTER

local NO_RETREAT_SQUAD_SIZE_BONUS_MAX = constants.NO_RETREAT_SQUAD_SIZE_BONUS_MAX

local AI_MAX_OVERFLOW_POINTS = constants.AI_MAX_OVERFLOW_POINTS

local AI_MAX_BITER_GROUP_SIZE = constants.AI_MAX_BITER_GROUP_SIZE
local AI_SQUAD_MERGE_THRESHOLD = constants.AI_SQUAD_MERGE_THRESHOLD

-- imported functions

local mRandom = math.random

local mLog = math.log10

local mMin = math.min

local getSquadsOnChunk = chunkPropertyUtils.getSquadsOnChunk
local removeSquadFromChunk = chunkPropertyUtils.removeSquadFromChunk

local getNeighborChunks = mapUtils.getNeighborChunks

local euclideanDistanceNamed = mathUtils.euclideanDistanceNamed

-- module code

function unitGroupUtils.findNearbySquadFiltered(map, chunk, position)
    for _,squad in pairs(getSquadsOnChunk(map, chunk)) do
	local unitGroup = squad.group
	if unitGroup and unitGroup.valid and RETREAT_FILTER[squad.status] then
	    if (euclideanDistanceNamed(unitGroup.position, position) <= HALF_CHUNK_SIZE) then
		return squad
	    end
	end
    end

    local neighbors = getNeighborChunks(map, chunk.x, chunk.y)

    for i=1,#neighbors do
	for _,squad in pairs(getSquadsOnChunk(map, neighbors[i])) do
	    local unitGroup = squad.group
	    if unitGroup and unitGroup.valid and RETREAT_FILTER[squad.status] then
		if (euclideanDistanceNamed(unitGroup.position, position) <= HALF_CHUNK_SIZE) then
		    return squad
		end
	    end
	end
    end
    return nil
end

function unitGroupUtils.findNearbySquad(map, chunk, position)

    for _,squad in pairs(getSquadsOnChunk(map, chunk)) do
	local unitGroup = squad.group
	if unitGroup and unitGroup.valid then
	    if (euclideanDistanceNamed(unitGroup.position, position) <= HALF_CHUNK_SIZE) then
		return squad
	    end
	end
    end

    local neighbors = getNeighborChunks(map, chunk.x, chunk.y)

    for i=1,#neighbors do
	for _,squad in pairs(getSquadsOnChunk(map, neighbors[i])) do
	    local unitGroup = squad.group
	    if unitGroup and unitGroup.valid then
		if (euclideanDistanceNamed(unitGroup.position, position) <= HALF_CHUNK_SIZE) then
		    return squad
		end
	    end
	end
    end
    
    return nil
end

function unitGroupUtils.createSquad(position, surface, natives, group)
    local unitGroup = group or surface.create_unit_group({position=position})
    
    local squad = {
	group = unitGroup, 
	status = SQUAD_GUARDING,
	penalties = {},
	settlers = false,
	rabid = false,
	frenzy = false,
	kamikaze = false,
	frenzyPosition = {x = 0,
			  y = 0},
	cycles = 0,
	maxDistance = 0,
	originPosition = {x = 0,
			  y = 0},
	chunk = nil
	
    }
    natives.squads[#natives.squads+1] = squad
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
    local squad = unitGroupUtils.createSquad(nil,nil,natives,unitGroup)
    squad.kamikaze = mRandom() < unitGroupUtils.calculateKamikazeThreshold(#unitGroup.members, natives)
    return squad
end

function unitGroupUtils.calculateKamikazeThreshold(memberCount, natives)
    local squadSizeBonus = mLog((memberCount / natives.attackWaveMaxSize) + 0.1) + 1
    return natives.kamikazeThreshold + (NO_RETREAT_SQUAD_SIZE_BONUS_MAX * squadSizeBonus)
end

local function isAttacking(group)
    local state = group.state
    return (state == DEFINES_GROUP_STATE_ATTACKING_TARGET) or (state == DEFINES_GROUP_STATE_ATTACKING_DISTRACTION)
end

function unitGroupUtils.cleanSquads(natives, map)
    local squads = natives.squads
    local squadCount = #squads

    local cleanSquads = {}
    for i=1, squadCount do
	local squad = squads[i]
	local group = squad.group
	if group and group.valid then
	    local memberCount = #group.members
	    if (memberCount == 0) then
		removeSquadFromChunk(map, squad)
		group.destroy()
	    elseif (memberCount > AI_MAX_BITER_GROUP_SIZE) then
		local members = group.members
		unitGroupUtils.recycleBiters(natives, members)
		removeSquadFromChunk(map, squad)
		group.destroy()
	    else
		local status = squad.status
		local cycles = squad.cycles
		if (status == SQUAD_RETREATING) and (cycles == 0) then
		    squad.status = SQUAD_GUARDING
		    squad.frenzy = true
		    local squadPosition = group.position
		    squad.frenzyPosition.x = squadPosition.x
		    squad.frenzyPosition.y = squadPosition.y
		elseif (group.state == DEFINES_GROUP_STATE_FINISHED) and not squad.settlers then
		    squad.status = SQUAD_GUARDING
		elseif (cycles > 0) then
		    squad.cycles = cycles - 1
		end

		cleanSquads[#cleanSquads+1] = squad
	    end
	else
	    removeSquadFromChunk(map, squad)
	end
    end
    
    natives.squads = cleanSquads
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

local function mergeGroups(squads, squad, group, status, position, memberCount)
    local merge = false
    local maxed = false
    for _,mergeSquad in pairs(squads) do
	if (mergeSquad ~= squad) then
	    local mergeGroup = mergeSquad.group
	    if mergeGroup and mergeGroup.valid and (euclideanDistanceNamed(position, mergeGroup.position) < GROUP_MERGE_DISTANCE) and (mergeSquad.status == status) and not isAttacking(mergeGroup) then
		local mergeMembers = mergeGroup.members
		local mergeCount = #mergeMembers
		if ((mergeCount + memberCount) < AI_MAX_BITER_GROUP_SIZE) then
		    for memberIndex=1, mergeCount do
			group.add_member(mergeMembers[memberIndex])
		    end
		    if mergeSquad.kamikaze then
			squad.kamikaze = true
		    end
		    merge = true
		    mergeGroup.destroy()
		end
		memberCount = memberCount + mergeCount
		if (memberCount > AI_SQUAD_MERGE_THRESHOLD) then
		    maxed = true
		    break
		end
	    end
	end
    end
    return merge, memberCount, maxed
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
		local squadPosition = group.position
	        local mergedSquads
		local maxed
		local chunk = squad.chunk

		if chunk then
		    mergedSquads, memberCount, maxed = mergeGroups(getSquadsOnChunk(map, chunk),
								   squad,
								   group,
								   status,
								   squadPosition,
								   memberCount)
		    
		    if not maxed then
			local neighbors = getNeighborChunks(map, chunk.x, chunk.y)

			for x=1,#neighbors do
			    mergedSquads, memberCount, maxed = mergeGroups(getSquadsOnChunk(map, neighbors[x]),
									   squad,
									   group,
									   status,
									   squadPosition,
									   memberCount)
			    if maxed then
				break
			    end
			end
		    end
		end
		
	        if mergedSquads and not squad.kamikaze then
	    	    local kamikazeThreshold = unitGroupUtils.calculateKamikazeThreshold(#squad.group.members, natives)
	    	    if (mRandom() < kamikazeThreshold) then
	    		squad.kamikaze = true
	    	    end
	        end
	    end
	end
    end
    
    if (maxSquadIndex == squadCount) then
	natives.regroupIndex = 1
    else
	natives.regroupIndex = maxSquadIndex + 1
    end
end

return unitGroupUtils
