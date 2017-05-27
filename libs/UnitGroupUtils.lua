local unitGroupUtils = {}

-- imports

local mapUtils = require("MapUtils")
local constants = require("Constants")

-- constants

local MOVEMENT_PHEROMONE_GENERATOR_AMOUNT = constants.MOVEMENT_PHEROMONE_GENERATOR_AMOUNT

local DEFINES_GROUP_STATE_FINISHED = defines.group_state.finished
local DEFINES_GROUP_STATE_ATTACKING_TARGET = defines.group_state.attacking_target
local DEFINES_GROUP_STATE_ATTACKING_DISTRACTION = defines.group_state.attacking_distraction

local SQUAD_RETREATING = constants.SQUAD_RETREATING
local SQUAD_GUARDING = constants.SQUAD_GUARDING
local GROUP_MERGE_DISTANCE = constants.GROUP_MERGE_DISTANCE

local NO_RETREAT_BASE_PERCENT = constants.NO_RETREAT_BASE_PERCENT
local NO_RETREAT_EVOLUTION_BONUS_MAX = constants.NO_RETREAT_EVOLUTION_BONUS_MAX
local NO_RETREAT_SQUAD_SIZE_BONUS_MAX = constants.NO_RETREAT_SQUAD_SIZE_BONUS_MAX

local AI_MAX_POINTS = constants.AI_MAX_POINTS
local AI_UNIT_REFUND = constants.AI_UNIT_REFUND

local AI_MAX_BITER_GROUP_SIZE = constants.AI_MAX_BITER_GROUP_SIZE

-- imported functions

local mLog = math.log10


local tableRemove = table.remove
local tableInsert = table.insert
local euclideanDistanceNamed = mapUtils.euclideanDistanceNamed

-- module code

function unitGroupUtils.findNearBySquad(natives, position, distance, filter)
    local squads = natives.squads
    for i=1,#squads do
        local squad = squads[i]
        local unitGroup = squad.group
        if unitGroup.valid and ((filter == nil) or (filter ~= nil and filter[squad.status])) then
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
	local cmd = { type = defines.command.group,
		      group = squad.group,
		      distraction = defines.distraction.none }
	for i=1,#members do
            local member = members[i]
            if member.valid and (overwriteGroup or (not overwriteGroup and (member.unit_group == nil))) then
		member.set_command(cmd)
            end
        end
    end
end

function unitGroupUtils.convertUnitGroupToSquad(natives, unitGroup)
    local returnSquad
    if (unitGroup ~= nil) then
        local squads = natives.squads
        for i=1,#squads do
            local squad = squads[i]
            if (squad.group == unitGroup) then  
                return squad
            end
        end
        returnSquad = { group = unitGroup,
                        status = SQUAD_GUARDING,
                        penalties = {},
			rabid = false,
			frenzy = false,
			kamikaze = false,
			frenzyPosition = {x = 0,
					  y = 0},
                        cycles = 0 }
        squads[#squads+1] = returnSquad
    end
    
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

function unitGroupUtils.calculateKamikazeThreshold(squad, natives, evolution_factor)
    local maxSize = natives.attackWaveMaxSize
    local kamikazeThreshold = NO_RETREAT_BASE_PERCENT + (evolution_factor * NO_RETREAT_EVOLUTION_BONUS_MAX)
    local squadSizeBonus = mLog((#squad.group.members / maxSize) + 0.1) + 1
    return kamikazeThreshold + (NO_RETREAT_SQUAD_SIZE_BONUS_MAX * squadSizeBonus)
end

local function isAttacking(group)
    local state = group.state
    return (state == DEFINES_GROUP_STATE_ATTACKING_TARGET) or (state == DEFINES_GROUP_STATE_ATTACKING_DISTRACTION)
end

function unitGroupUtils.cleanSquads(natives, evolution_factor)
    local squads = natives.squads
    local squadCount = #squads

    local weight = AI_UNIT_REFUND * evolution_factor

    local cleanSquads = {}
    
    for i=1,squadCount do
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
		    natives.points = natives.points + weight
		end

		if (natives.points > (AI_MAX_POINTS * 3)) then
		    natives.points = (AI_MAX_POINTS * 3)
		end
		group.destroy()
	    else
		local status = squad.status
		local squadPosition = group.position
		local cycles = squad.cycles
		if (status == SQUAD_RETREATING) and (cycles == 0) then
		    squad.status = SQUAD_GUARDING
		    squad.frenzy = true
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


function unitGroupUtils.regroupSquads(natives, evolution_factor)
    local groupThreshold = AI_MAX_BITER_GROUP_SIZE * 0.75

    local squads = natives.squads
    local squadCount = #squads
    
    for i=1,squadCount do
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
	    	    if mergeGroup.valid and (mergeSquad.status == status) and not isAttacking(mergeGroup) and (euclideanDistanceNamed(squadPosition, mergeGroup.position) < GROUP_MERGE_DISTANCE) then
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
	    	    local kamikazeThreshold = unitGroupUtils.calculateKamikazeThreshold(squad, natives, evolution_factor)
	    	    if (math.random() < kamikazeThreshold) then
	    		squad.kamikaze = true
	    	    end
	        end
	    end
	end
    end    
end

return unitGroupUtils
