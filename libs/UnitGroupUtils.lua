local unitGroupUtils = {}

-- imports

local mapUtils = require("MapUtils")
local constants = require("Constants")
package.path = "../?.lua;" .. package.path
local config = require("config")

-- constants

local MOVEMENT_PHEROMONE_GENERATOR_AMOUNT = constants.MOVEMENT_PHEROMONE_GENERATOR_AMOUNT

local GROUP_STATE_FINISHED = defines.group_state.finished
local GROUP_STATE_ATTACKING_TARGET = defines.group_state.attacking_target
local GROUP_STATE_ATTACKING_DISTRACTION = defines.group_state.attacking_distraction

local SQUAD_RETREATING = constants.SQUAD_RETREATING
local SQUAD_GUARDING = constants.SQUAD_GUARDING
local GROUP_MERGE_DISTANCE = constants.GROUP_MERGE_DISTANCE

local NO_RETREAT_BASE_PERCENT = constants.NO_RETREAT_BASE_PERCENT
local NO_RETREAT_EVOLUTION_BONUS_MAX = constants.NO_RETREAT_EVOLUTION_BONUS_MAX
local NO_RETREAT_SQUAD_SIZE_BONUS_MAX = constants.NO_RETREAT_SQUAD_SIZE_BONUS_MAX

local CONFIG_ATTACK_WAVE_MAX_SIZE = config.attackWaveMaxSize

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
        if (unitGroup ~= nil) and unitGroup.valid and ((filter == nil) or (filter ~= nil and filter[squad.status])) then
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

function unitGroupUtils.calculateKamikazeThreshold(squad, evolution_factor)
    local kamikazeThreshold = NO_RETREAT_BASE_PERCENT + (evolution_factor * NO_RETREAT_EVOLUTION_BONUS_MAX)
    local squadSizeBonus = mLog((#squad.group.members / CONFIG_ATTACK_WAVE_MAX_SIZE) + 0.1) + 1
    return kamikazeThreshold + (NO_RETREAT_SQUAD_SIZE_BONUS_MAX * squadSizeBonus)
end

local function isAttacking(squad)
    return (squad.state == GROUP_STATE_ATTACKING_TARGET) or (squad.state == GROUP_STATE_ATTACKING_DISTRACTION)
end

function unitGroupUtils.regroupSquads(natives, evolution_factor)
    local squads = natives.squads
    for i=1,#squads do
        local squad = squads[i]
        if squad.group.valid and not isAttacking(squad) then
            local squadPosition = squad.group.position
	    local mergedSquads = false
	    for x=i+1, #squads do
		local mergeSquad = squads[x]
		local mergeGroup = mergeSquad.group
		if mergeGroup.valid and (mergeSquad.status == squad.status) and not isAttacking(mergeSquad) and (euclideanDistanceNamed(squadPosition, mergeGroup.position) < GROUP_MERGE_DISTANCE) then
		    unitGroupUtils.membersToSquad(squad, mergeGroup.members, true)
		    if mergeSquad.kamikaze then
			squad.kamikaze = true
		    end
		    mergedSquads = true
		    mergeGroup.destroy()
		end
	    end
	    if mergedSquads and not squad.kamikaze then
		local kamikazeThreshold = unitGroupUtils.calculateKamikazeThreshold(squad, evolution_factor)
		if (math.random() < kamikazeThreshold) then
		    squad.kamikaze = true
		end
	    end
	end
    end

    for i=#squads,1,-1 do
	local squad = squads[i]
	if (squad.group == nil) then
	    tableRemove(squads, i)
	elseif not squad.group.valid then
	    tableRemove(squads, i)
	elseif (#squad.group.members == 0) then
	    squad.group.destroy()
	    tableRemove(squads, i)
	else         
	    if (squad.status == SQUAD_RETREATING) and (squad.cycles == 0) then
		squad.status = SQUAD_GUARDING
		squad.frenzy = true
		squad.frenzyPosition.x = squad.group.position.x
		squad.frenzyPosition.y = squad.group.position.y
	    elseif (squad.group.state == GROUP_STATE_FINISHED) then
		squad.status = SQUAD_GUARDING
	    elseif (squad.cycles > 0) then
		squad.cycles = squad.cycles - 1
	    end
	end
    end
end


return unitGroupUtils
