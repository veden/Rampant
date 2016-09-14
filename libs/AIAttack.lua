local aiAttack = {}

-- imports

local constants = require("Constants")
local mapUtils = require("MapUtils")
local unitGroupUtils = require("UnitGroupUtils")
local playerUtils = require("PlayerUtils")
local neighborUtils = require("NeighborUtils")

-- constants
          
local PLAYER_PHEROMONE = constants.PLAYER_PHEROMONE
local DEATH_PHEROMONE = constants.DEATH_PHEROMONE
local ENEMY_BASE_PHEROMONE = constants.ENEMY_BASE_PHEROMONE
local PLAYER_BASE_PHEROMONE = constants.PLAYER_BASE_PHEROMONE
--local PLAYER_DEFENSE_PHEROMONE = constants.PLAYER_DEFENSE_PHEROMONE

local SQUAD_RAIDING = constants.SQUAD_RAIDING
local SQUAD_SUICIDE_RAID = constants.SQUAD_SUICIDE_RAID
local SQUAD_HUNTING = constants.SQUAD_HUNTING
local SQUAD_GUARDING = constants.SQUAD_GUARDING
local SQUAD_SUICIDE_HUNT = constants.SQUAD_SUICIDE_HUNT

--local ENEMY_BASE_GENERATOR = constants.ENEMY_BASE_GENERATOR
--local MAGIC_MAXIMUM_NUMBER = constants.MAGIC_MAXIMUM_NUMBER

--local HALF_CHUNK_SIZE = constants.HALF_CHUNK_SIZE
--local CHUNK_SIZE = constants.CHUNK_SIZE

local PLAYER_BASE_GENERATOR = constants.PLAYER_BASE_GENERATOR
local PLAYER_DEFENSE_GENERATOR = constants.PLAYER_DEFENSE_GENERATOR

-- imported functions

local getCardinalChunksWithDirection = mapUtils.getCardinalChunksWithDirection
local getChunkByPosition = mapUtils.getChunkByPosition
local canMoveChunkDirectionCardinal = mapUtils.canMoveChunkDirectionCardinal
local addSquadMovementPenalty = unitGroupUtils.addSquadMovementPenalty
local lookupSquadMovementPenalty = unitGroupUtils.lookupSquadMovementPenalty
local positionFromDirectionAndChunkCardinal = mapUtils.positionFromDirectionAndChunkCardinal

local euclideanDistanceNamed = mapUtils.euclideanDistanceNamed

local playersWithinProximityToPosition = playerUtils.playersWithinProximityToPosition

local scoreNeighborsWithDirection = neighborUtils.scoreNeighborsWithDirection



-- module code

local function validLocation(x, chunk, neighborChunk)
    return canMoveChunkDirectionCardinal(x, chunk, neighborChunk)
end

local function scoreAttackLocation(position, squad, neighborChunk, surface)
    local squadMovementPenalty = lookupSquadMovementPenalty(squad, neighborChunk.cX, neighborChunk.cY)
    local damageScore = surface.get_pollution(position) + neighborChunk[PLAYER_BASE_PHEROMONE] + neighborChunk[PLAYER_PHEROMONE] + neighborChunk[PLAYER_DEFENSE_GENERATOR]
    local avoidScore = neighborChunk[DEATH_PHEROMONE] + neighborChunk[ENEMY_BASE_PHEROMONE]
    return damageScore - avoidScore - squadMovementPenalty
end

local function scoreHuntPlayerLocation(position, squad, neighborChunk, surface)
    local squadMovementPenalty = lookupSquadMovementPenalty(squad, neighborChunk.cX, neighborChunk.cY)
    local damageScore = neighborChunk[PLAYER_PHEROMONE]
    local avoidScore = neighborChunk[DEATH_PHEROMONE] + neighborChunk[ENEMY_BASE_PHEROMONE] + neighborChunk[PLAYER_DEFENSE_GENERATOR]
    return damageScore - avoidScore - squadMovementPenalty
end

function aiAttack.squadAttack(regionMap, surface, natives)
    local squads = natives.squads
    local attackPosition
    local attackCmd
    if (#squads > 0) then
	attackPosition = {x=0, y=0}
	attackCmd = { type = defines.command.attack_area,
		      destination = attackPosition,
		      radius = 16,
		      distraction = defines.distraction.by_enemy }
    end
    for i=1,#squads do
        local squad = squads[i]
        local group = squad.group
        local raiding = false
        local hunting = false
        local scoreLocation
        if (squad.status == SQUAD_RAIDING) or (squad.status == SQUAD_SUICIDE_RAID) then
            raiding = true
            scoreLocation = scoreAttackLocation
        elseif (squad.status == SQUAD_HUNTING) or (squad.status == SQUAD_SUICIDE_HUNT) then
            hunting = true
            scoreLocation = scoreHuntPlayerLocation
        end
        if group.valid and (raiding or hunting) then 
            if (group.state == defines.group_state.finished) or (group.state == defines.group_state.gathering) or ((group.state == defines.group_state.moving) and (squad.cycles == 0)) then
                local chunk = getChunkByPosition(regionMap, group.position.x, group.position.y)
                if (chunk ~= nil) then
                    addSquadMovementPenalty(squad, chunk.cX, chunk.cY)
                    local attackChunk, attackDirection = scoreNeighborsWithDirection(chunk,
                                                                                     getCardinalChunksWithDirection(regionMap, chunk.cX, chunk.cY),
                                                                                     validLocation,
                                                                                     scoreLocation,
                                                                                     squad,
                                                                                     surface,
                                                                                     attackPosition)
                    if (attackChunk ~= nil) then
                        if ((attackChunk[PLAYER_BASE_GENERATOR] == 0) and (attackChunk[PLAYER_DEFENSE_GENERATOR] == 0)) or
                            ((group.state == defines.group_state.finished) or (group.state == defines.group_state.gathering)) then
                            
                            positionFromDirectionAndChunkCardinal(attackDirection, squad.group.position, attackPosition)

			    squad.cycles = 3

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
			    (((group.state == defines.group_state.attacking_distraction) or (group.state == defines.group_state.attacking_distraction)) or
				(attackChunk[PLAYER_BASE_GENERATOR] ~= 0) or (attackChunk[PLAYER_DEFENSE_GENERATOR] ~= 0)) then
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
            local threshold = 0.05 + (evolution_factor * 0.20) + (#squad.group.members * 0.0033)

	    local playerNearby = playersWithinProximityToPosition(players, squad.group.position, 100)
	    if playerNearby then
		squad.frenzy = true
		squad.frenzyPosition.x = squad.group.position.x
		squad.frenzyPosition.y = squad.group.position.y
	    end
	    
            -- check to hunt player
            if (math.random() < 0.30) and playerNearby then
                if (math.random() < threshold) then
                    squad.status = SQUAD_SUICIDE_HUNT
                else
                    squad.status = SQUAD_HUNTING
                end
            end
            
            -- check to raid base
            if (squad.status == SQUAD_GUARDING) and (math.random() < 0.70) then
		if (math.random() < threshold) then
                    squad.status = SQUAD_SUICIDE_RAID
                else
                    squad.status = SQUAD_RAIDING
                end
            end
        end
    end
end

return aiAttack
