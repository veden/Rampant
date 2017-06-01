local unitGroupUtils = {}

-- imports

local mapUtils = require("MapUtils")
local constants = require("Constants")

-- constants

local MOVEMENT_PHEROMONE_GENERATOR_AMOUNT = constants.MOVEMENT_PHEROMONE_GENERATOR_AMOUNT

local SQUAD_QUEUE_SIZE = constants.SQUAD_QUEUE_SIZE

local DEFINES_GROUP_STATE_FINISHED = defines.group_state.finished
local DEFINES_GROUP_STATE_ATTACKING_TARGET = defines.group_state.attacking_target
local DEFINES_GROUP_STATE_ATTACKING_DISTRACTION = defines.group_state.attacking_distraction
local DEFINES_COMMAND_GROUP = defines.command.group
local DEFINES_DISTRACTION_NONE = defines.distraction.none

local SQUAD_RETREATING = constants.SQUAD_RETREATING
local SQUAD_GUARDING = constants.SQUAD_GUARDING
local GROUP_MERGE_DISTANCE = constants.GROUP_MERGE_DISTANCE

local NO_RETREAT_SQUAD_SIZE_BONUS_MAX = constants.NO_RETREAT_SQUAD_SIZE_BONUS_MAX

local AI_MAX_OVERFLOW_POINTS = constants.AI_MAX_OVERFLOW_POINTS

local AI_MAX_BITER_GROUP_SIZE = constants.AI_MAX_BITER_GROUP_SIZE
local AI_SQUAD_MERGE_THRESHOLD = constants.AI_SQUAD_MERGE_THRESHOLD

-- imported functions

local mLog = math.log10

local mMin = math.min

local tableRemove = table.remove
local tableInsert = table.insert
local euclideanDistanceNamed = mapUtils.euclideanDistanceNamed

-- module code

function unitGroupUtils.findNearBySquad(natives, position, distance, filter)
    local squads = natives.squads

    for i=1,#squads do
        local squad = squads[i]
        local unitGroup = squad.group
        if unitGroup.valid and (not filter or (filter and filter[squad.status])) then
            if (euclideanDistanceNamed(unitGroup.position, position) <= distance) then
                return squad
            end
        end
    end
end

function unitGroupUtils.createSquad(position, surface, natives)
    local unitGroup = surface.create_unit_group({position=position})
    
    local squad = { group = unitGroup, 
                    status = SQUAD_GUARDING,
                    penalties = {},
		    rabid = false,
		    frenzy = false,
		    kamikaze = false,
		    frenzyPosition = {x = 0,
				      y = 0},
                    cycles = 0 }
    natives.squads[#natives.squads+1] = squad
    return squad
end

function unitGroupUtils.membersToSquad(squad, members, overwriteGroup)
    if (members ~= nil) then
	local cmd = { type = DEFINES_COMMAND_GROUP,
		      group = squad.group,
		      distraction = DEFINES_DISTRACTION_NONE }
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
    local returnSquad = { group = unitGroup,
			  status = SQUAD_GUARDING,
			  penalties = {},
			  rabid = false,
			  frenzy = false,
			  kamikaze = false,
			  frenzyPosition = {x = 0,
					    y = 0},
			  cycles = 0 }
    squads[#squads+1] = returnSquad
    return returnSquad
end

function unitGroupUtils.addSquadMovementPenalty(squad, chunkX, chunkY)   
    local penalties = squad.penalties
    for i=1,#penalties do
        local penalty = penalties[i]
        if (penalty.x == chunkX) and (penalty.y == chunkY) then
            penalty.v = penalty.v + MOVEMENT_PHEROMONE_GENERATOR_AMOUNT
            return
        end
    end
    if (#penalties == 10) then
        tableRemove(penalties, 10)
    end
    tableInsert(penalties, 1, { v = MOVEMENT_PHEROMONE_GENERATOR_AMOUNT,
                                x = chunkX,
                                y = chunkY })
end

function unitGroupUtils.lookupSquadMovementPenalty(squad, chunkX, chunkY)
    local penalties = squad.penalties
    for i=1,#penalties do
        local penalty = penalties[i]
        if (penalty.x == chunkX) and (penalty.y == chunkY) then
            return penalty.v
        end
    end
    return 0
end

function unitGroupUtils.calculateKamikazeThreshold(squad, natives)
    local squadSizeBonus = mLog((#squad.group.members / natives.attackWaveMaxSize) + 0.1) + 1
    return natives.kamikazeThreshold + (NO_RETREAT_SQUAD_SIZE_BONUS_MAX * squadSizeBonus)
end

local function isAttacking(group)
    local state = group.state
    return (state == DEFINES_GROUP_STATE_ATTACKING_TARGET) or (state == DEFINES_GROUP_STATE_ATTACKING_DISTRACTION)
end

function unitGroupUtils.cleanSquads(natives)
    local squads = natives.squads
    local squadCount = #squads

    local weight = natives.unitRefundAmount

    local cleanSquads = {}

    for i=1, squadCount do
	local squad = squads[i]
	local group = squad.group
	if group.valid then
	    local memberCount = #group.members
	    if (memberCount == 0) then
		group.destroy()
	    elseif (memberCount > AI_MAX_BITER_GROUP_SIZE) then
		local members = group.members
		for x=1,memberCount do
		    members[x].destroy()
		end
		natives.points = natives.points + (memberCount * weight)

		if (natives.points > AI_MAX_OVERFLOW_POINTS) then
		    natives.points = AI_MAX_OVERFLOW_POINTS
		end
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
		elseif (group.state == DEFINES_GROUP_STATE_FINISHED) then
		    squad.status = SQUAD_GUARDING
		elseif (cycles > 0) then
		    squad.cycles = cycles - 1
		end

		cleanSquads[#cleanSquads+1] = squad
	    end
	end
    end
    
    natives.squads = cleanSquads
end


function unitGroupUtils.regroupSquads(natives)
    local groupThreshold = AI_SQUAD_MERGE_THRESHOLD

    local squads = natives.squads
    local squadCount = #squads

    local startIndex = natives.regroupIndex
    
    local maxSquadIndex = mMin(startIndex + SQUAD_QUEUE_SIZE, squadCount)
    for i=startIndex,maxSquadIndex do
	local squad = squads[i]
	local group = squad.group
	if group.valid and not isAttacking(group) then
	    local status = squad.status
	    local memberCount = #group.members
	    if (memberCount < groupThreshold) then
		local squadPosition = group.position
	        local mergedSquads = false
	        for x=i+1,squadCount do
	    	    local mergeSquad = squads[x]
	    	    local mergeGroup = mergeSquad.group
	    	    if mergeGroup.valid and (euclideanDistanceNamed(squadPosition, mergeGroup.position) < GROUP_MERGE_DISTANCE) and (mergeSquad.status == status) and not isAttacking(mergeGroup) then
	    		local mergeMembers = mergeGroup.members
	    		local mergeCount = #mergeMembers
	    		if ((mergeCount + memberCount) < AI_MAX_BITER_GROUP_SIZE) then
	    		    for memberIndex=1, mergeCount do
	    			group.add_member(mergeMembers[memberIndex])
	    		    end
	    		    if mergeSquad.kamikaze then
	    			squad.kamikaze = true
	    		    end
	    		    mergedSquads = true
	    		    mergeGroup.destroy()
	    		end
	    		memberCount = memberCount + mergeCount
	    		if (memberCount > groupThreshold) then
	    		    break
	    		end
	    	    end
	        end
	        if mergedSquads and not squad.kamikaze then
	    	    local kamikazeThreshold = unitGroupUtils.calculateKamikazeThreshold(squad, natives)
	    	    if (math.random() < kamikazeThreshold) then
	    		squad.kamikaze = true
	    	    end
	        end
	    end
	end
    end
    
    if (maxSquadIndex == squadCount) then
	natives.regroupIndex = 1
    else
	natives.regroupIndex = maxSquadIndex
    end
end

return unitGroupUtils
