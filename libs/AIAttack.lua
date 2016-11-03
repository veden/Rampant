local aiAttack = {}

-- imports

local constants = require("Constants")
local mapUtils = require("MapUtils")
local unitGroupUtils = require("UnitGroupUtils")
local playerUtils = require("PlayerUtils")
local neighborUtils = require("NeighborUtils")
package.path = "../?.lua;" .. package.path
local config = require("config")

-- constants
          
local PLAYER_PHEROMONE = constants.PLAYER_PHEROMONE
local MOVEMENT_PHEROMONE = constants.MOVEMENT_PHEROMONE
local BASE_PHEROMONE = constants.BASE_PHEROMONE

local SQUAD_RAIDING = constants.SQUAD_RAIDING
local SQUAD_GUARDING = constants.SQUAD_GUARDING

local PLAYER_BASE_GENERATOR = constants.PLAYER_BASE_GENERATOR

local NO_RETREAT_BASE_PERCENT = constants.NO_RETREAT_BASE_PERCENT
local NO_RETREAT_EVOLUTION_BONUS_MAX = constants.NO_RETREAT_EVOLUTION_BONUS_MAX
local NO_RETREAT_SQUAD_SIZE_BONUS_MAX = constants.NO_RETREAT_SQUAD_SIZE_BONUS_MAX

local CONFIG_ATTACK_WAVE_MAX_SIZE = config.attackWaveMaxSize

-- imported functions

local getNeighborChunksWithDirection = mapUtils.getNeighborChunksWithDirection
local getChunkByPosition = mapUtils.getChunkByPosition
local canMoveChunkDirection = mapUtils.canMoveChunkDirection
local addSquadMovementPenalty = unitGroupUtils.addSquadMovementPenalty
local lookupSquadMovementPenalty = unitGroupUtils.lookupSquadMovementPenalty
local positionFromDirectionAndChunk = mapUtils.positionFromDirectionAndChunk

local euclideanDistanceNamed = mapUtils.euclideanDistanceNamed

local playersWithinProximityToPosition = playerUtils.playersWithinProximityToPosition

local scoreNeighborsWithDirection = neighborUtils.scoreNeighborsWithDirection

local mLog = math.log10

-- module code

local function validLocation(x, chunk, neighborChunk)
    return canMoveChunkDirection(x, chunk, neighborChunk)
end

local function scoreAttackLocation(position, squad, neighborChunk, surface)
    local squadMovementPenalty = lookupSquadMovementPenalty(squad, neighborChunk.cX, neighborChunk.cY)
    local r = surface.get_pollution(position) + neighborChunk[MOVEMENT_PHEROMONE] + neighborChunk[BASE_PHEROMONE] + (neighborChunk[PLAYER_PHEROMONE] * 25)
    return r - squadMovementPenalty
end

function aiAttack.squadAttack(regionMap, surface, natives)
    local squads = natives.squads
    local attackPosition
    local attackCmd
    if (#squads > 0) then
	attackPosition = {x=0, y=0}
	attackCmd = { type = defines.command.attack_area,
		      destination = attackPosition,
		      radius = 20,
		      distraction = defines.distraction.by_enemy }
    end
    for i=1,#squads do
        local squad = squads[i]
        local group = squad.group
        if group.valid and (squad.status == SQUAD_RAIDING) then 
	    if (group.state == defines.group_state.finished) or (group.state == defines.group_state.gathering) or ((group.state == defines.group_state.moving) and (squad.cycles == 0)) then
		local chunk = getChunkByPosition(regionMap, group.position.x, group.position.y)
		if (chunk ~= nil) then
		    local attackChunk, attackDirection = scoreNeighborsWithDirection(chunk,
										     getNeighborChunksWithDirection(regionMap, chunk.cX, chunk.cY),
										     validLocation,
										     scoreAttackLocation,
										     squad,
										     surface,
										     attackPosition,
										     true)
		    addSquadMovementPenalty(squad, chunk.cX, chunk.cY)
		    if (attackChunk ~= nil) then
			if (attackChunk[PLAYER_BASE_GENERATOR] == 0) or
			((group.state == defines.group_state.finished) or (group.state == defines.group_state.gathering)) then
                            
			    positionFromDirectionAndChunk(attackDirection, squad.group.position, attackPosition)

			    squad.cycles = 4

			    if not squad.rabid and squad.frenzy and (euclideanDistanceNamed(squad.group.position, squad.frenzyPosition) > 100) then
				squad.frenzy = false
			    end
			    
			    if squad.rabid or squad.frenzy then
				attackCmd.distraction = defines.distraction.by_anything
			    else
				attackCmd.distraction = defines.distraction.by_enemy
			    end
			    
			    group.set_command(attackCmd)
			    group.start_moving()
			elseif not squad.frenzy and not squad.rabid and
			    ((group.state == defines.group_state.attacking_distraction) or (group.state == defines.group_state.attacking_distraction) or
				(attackChunk[PLAYER_BASE_GENERATOR] ~= 0)) then
				squad.frenzy = true
				squad.frenzyPosition.x = squad.group.position.x
				squad.frenzyPosition.y = squad.group.position.y
			end
		    end
		end
	    end
        end
    end
end

function aiAttack.squadBeginAttack(natives, players, evolution_factor)
    local squads = natives.squads
    for i=1,#squads do
        local squad = squads[i]
        if (squad.status == SQUAD_GUARDING) and squad.group.valid then
            local kamikazeThreshold = NO_RETREAT_BASE_PERCENT + (evolution_factor * NO_RETREAT_EVOLUTION_BONUS_MAX)
	    local squadSizeBonus = mLog((#squad.group.members / CONFIG_ATTACK_WAVE_MAX_SIZE) + 0.1) + 1
	    kamikazeThreshold = kamikazeThreshold + (NO_RETREAT_SQUAD_SIZE_BONUS_MAX * squadSizeBonus)
	    
	    local playerNearby = playersWithinProximityToPosition(players, squad.group.position, 100)
	    if playerNearby then
		squad.frenzy = true
		squad.frenzyPosition.x = squad.group.position.x
		squad.frenzyPosition.y = squad.group.position.y
	    end
	               
	    if (math.random() < 0.70) then
		if (math.random() < kamikazeThreshold) then
		    squad.kamikaze = true
		end
		squad.status = SQUAD_RAIDING
	    end
	end
    end
end

return aiAttack
