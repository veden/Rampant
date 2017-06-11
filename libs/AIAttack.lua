local aiAttack = {}

-- imports

local constants = require("Constants")
local mapUtils = require("MapUtils")
local unitGroupUtils = require("UnitGroupUtils")
local playerUtils = require("PlayerUtils")
local neighborUtils = require("NeighborUtils")

-- constants

local PLAYER_PHEROMONE = constants.PLAYER_PHEROMONE
local MOVEMENT_PHEROMONE = constants.MOVEMENT_PHEROMONE
local BASE_PHEROMONE = constants.BASE_PHEROMONE

local SQUAD_RAIDING = constants.SQUAD_RAIDING
local SQUAD_GUARDING = constants.SQUAD_GUARDING

local PLAYER_BASE_GENERATOR = constants.PLAYER_BASE_GENERATOR

local PLAYER_PHEROMONE_MULTIPLER = constants.PLAYER_PHEROMONE_MULTIPLER

local DEFINES_COMMAND_ATTACK_AREA = defines.command.attack_area
local DEFINES_GROUP_FINISHED = defines.group_state.finished
local DEFINES_GROUP_GATHERING = defines.group_state.gathering
local DEFINES_GROUP_MOVING = defines.group_state.moving
local DEFINES_GROUP_ATTACKING_DISTRACTION = defines.group_state.attacking_distraction
local DEFINES_GROUP_ATTACKING_TARGET = defines.group_state.attacking_target
local DEFINES_DISTRACTION_BY_ENEMY = defines.distraction.by_enemy
local DEFINES_DISTRACTION_BY_ANYTHING = defines.distraction.by_anything

-- imported functions

local getNeighborChunks = mapUtils.getNeighborChunks
local getChunkByPosition = mapUtils.getChunkByPosition
local addSquadMovementPenalty = unitGroupUtils.addSquadMovementPenalty
local lookupSquadMovementPenalty = unitGroupUtils.lookupSquadMovementPenalty
local calculateKamikazeThreshold = unitGroupUtils.calculateKamikazeThreshold
local positionFromDirectionAndChunk = mapUtils.positionFromDirectionAndChunk

local euclideanDistanceNamed = mapUtils.euclideanDistanceNamed

local playersWithinProximityToPosition = playerUtils.playersWithinProximityToPosition

local scoreNeighborsForAttack = neighborUtils.scoreNeighborsForAttack

-- module code

local function scoreAttackLocation(squad, neighborChunk)
    local damage = neighborChunk[MOVEMENT_PHEROMONE] + neighborChunk[BASE_PHEROMONE] + (neighborChunk[PLAYER_PHEROMONE] * PLAYER_PHEROMONE_MULTIPLER)
    return damage - lookupSquadMovementPenalty(squad, neighborChunk.cX, neighborChunk.cY)
end

function aiAttack.squadAttack(regionMap, surface, natives)
    local squads = natives.squads
    local attackPosition
    local attackCmd
    
    if (#squads > 0) then
	attackPosition = {x=0, y=0}
	attackCmd = { type = DEFINES_COMMAND_ATTACK_AREA,
		      destination = attackPosition,
		      radius = 32,
		      distraction = DEFINES_DISTRACTION_BY_ENEMY }
    end
    
    for i=1,#squads do
        local squad = squads[i]
        local group = squad.group
        if group.valid and (squad.status == SQUAD_RAIDING) then
	    local groupState = group.state
	    if (groupState == DEFINES_GROUP_FINISHED) or (groupState == DEFINES_GROUP_GATHERING) or ((groupState == DEFINES_GROUP_MOVING) and (squad.cycles == 0)) then
		local groupPosition = group.position
		local chunk = getChunkByPosition(regionMap, groupPosition.x, groupPosition.y)
		if chunk then
		    local attackChunk, attackDirection = scoreNeighborsForAttack(chunk,
										 getNeighborChunks(regionMap,
												   chunk.cX,
												   chunk.cY),
										 scoreAttackLocation,
										 squad)
		    addSquadMovementPenalty(natives, squad, chunk.cX, chunk.cY)
		    if group.valid and attackChunk then
			if (attackChunk[PLAYER_BASE_GENERATOR] == 0) or
			((groupState == DEFINES_GROUP_FINISHED) or (groupState == DEFINES_GROUP_GATHERING)) then
			    
			    positionFromDirectionAndChunk(attackDirection, groupPosition, attackPosition, 1.35)

			    if (#squad.group.members > 80) then
				squad.cycles = 6
			    else
				squad.cycles = 4
			    end

			    local moreFrenzy = not squad.rabid and squad.frenzy and (euclideanDistanceNamed(groupPosition, squad.frenzyPosition) < 100)
			    squad.frenzy = moreFrenzy
			    
			    if squad.rabid or squad.frenzy then
				attackCmd.distraction = DEFINES_DISTRACTION_BY_ANYTHING
			    else
				attackCmd.distraction = DEFINES_DISTRACTION_BY_ENEMY
			    end

			    if surface.can_place_entity({name="behemoth-biter", position=attackPosition}) then
				group.set_command(attackCmd)
				group.start_moving()
			    else
				local newAttackPosition = surface.find_non_colliding_position("behemoth-biter", attackPosition, 5, 2)
				if newAttackPosition then
				    attackPosition.x = newAttackPosition.x
				    attackPosition.y = newAttackPosition.y
				    group.set_command(attackCmd)
				    group.start_moving()
				else
				    addSquadMovementPenalty(natives, squad, attackChunk.cX, attackChunk.cY)
				end
			    end
			elseif not squad.frenzy and not squad.rabid and
			    ((groupState == DEFINES_GROUP_ATTACKING_DISTRACTION) or (groupState == DEFINES_GROUP_ATTACKING_TARGET) or
				(attackChunk[PLAYER_BASE_GENERATOR] ~= 0)) then
				squad.frenzy = true
				squad.frenzyPosition.x = groupPosition.x
				squad.frenzyPosition.y = groupPosition.y
			end
		    end
		end
	    end
        end
    end
end

function aiAttack.squadBeginAttack(natives, players)
    local squads = natives.squads
    for i=1,#squads do
        local squad = squads[i]
	local group = squad.group
        if (squad.status == SQUAD_GUARDING) and group.valid then
	    local groupPosition = group.position
	    local kamikazeThreshold = calculateKamikazeThreshold(squad, natives)
	    
	    local playerNearby = playersWithinProximityToPosition(players, groupPosition, 100)
	    if playerNearby then
		squad.frenzy = true
		squad.frenzyPosition.x = groupPosition.x
		squad.frenzyPosition.y = groupPosition.y
	    end
	    
	    if (math.random() < 0.70) then
		squad.kamikaze = math.random() < kamikazeThreshold
		squad.status = SQUAD_RAIDING
	    end
	end
    end
end

return aiAttack
