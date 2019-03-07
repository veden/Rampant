if (squadAttackG) then
    return squadAttackG
end
local squadAttack = {}

-- imports

local constants = require("Constants")
local mapUtils = require("MapUtils")
local unitGroupUtils = require("UnitGroupUtils")
local playerUtils = require("PlayerUtils")
local movementUtils = require("MovementUtils")
local mathUtils = require("MathUtils")
local chunkPropertyUtils = require("ChunkPropertyUtils")

-- constants

local PLAYER_PHEROMONE = constants.PLAYER_PHEROMONE
local MOVEMENT_PHEROMONE = constants.MOVEMENT_PHEROMONE
local BASE_PHEROMONE = constants.BASE_PHEROMONE
local RESOURCE_PHEROMONE = constants.RESOURCE_PHEROMONE

-- local ATTACK_SCORE = constants.ATTACK_SCORE
local ATTACK_SCORE_KAMIKAZE = constants.ATTACK_SCORE_KAMIKAZE

local SQUAD_BUILDING = constants.SQUAD_BUILDING

local SQUAD_RAIDING = constants.SQUAD_RAIDING
local SQUAD_SETTLING = constants.SQUAD_SETTLING
local SQUAD_GUARDING = constants.SQUAD_GUARDING
local SQUAD_RETREATING = constants.SQUAD_RETREATING

local AI_MAX_BITER_GROUP_SIZE = constants.AI_MAX_BITER_GROUP_SIZE

local AI_STATE_SIEGE = constants.AI_STATE_SIEGE

local PLAYER_PHEROMONE_MULTIPLER = constants.PLAYER_PHEROMONE_MULTIPLER

local DEFINES_GROUP_FINISHED = defines.group_state.finished
local DEFINES_GROUP_GATHERING = defines.group_state.gathering
local DEFINES_GROUP_MOVING = defines.group_state.moving
local DEFINES_GROUP_ATTACKING_DISTRACTION = defines.group_state.attacking_distraction
local DEFINES_GROUP_ATTACKING_TARGET = defines.group_state.attacking_target
local DEFINES_DISTRACTION_NONE = defines.distraction.none
local DEFINES_DISTRACTION_BY_ENEMY = defines.distraction.by_enemy
local DEFINES_DISTRACTION_BY_ANYTHING = defines.distraction.by_anything

local SENTINEL_IMPASSABLE_CHUNK = constants.SENTINEL_IMPASSABLE_CHUNK

-- imported functions

local mRandom = math.random
local tRemove = table.remove

local euclideanDistancePoints = mathUtils.euclideanDistancePoints

local findMovementPosition = movementUtils.findMovementPosition

local removeSquadFromChunk = chunkPropertyUtils.removeSquadFromChunk

local getNestCount = chunkPropertyUtils.getNestCount

local getNeighborChunks = mapUtils.getNeighborChunks
local addSquadToChunk = chunkPropertyUtils.addSquadToChunk
local getChunkByXY = mapUtils.getChunkByXY
local positionToChunkXY = mapUtils.positionToChunkXY
local addMovementPenalty = movementUtils.addMovementPenalty
local lookupMovementPenalty = movementUtils.lookupMovementPenalty
local calculateKamikazeThreshold = unitGroupUtils.calculateKamikazeThreshold
local positionFromDirectionAndChunk = mapUtils.positionFromDirectionAndChunk

local euclideanDistanceNamed = mathUtils.euclideanDistanceNamed

local playersWithinProximityToPosition = playerUtils.playersWithinProximityToPosition
local getPlayerBaseGenerator = chunkPropertyUtils.getPlayerBaseGenerator
local getResourceGenerator = chunkPropertyUtils.getResourceGenerator

local scoreNeighborsForAttack = movementUtils.scoreNeighborsForAttack
local scoreNeighborsForSettling = movementUtils.scoreNeighborsForSettling

-- module code

local function scoreResourceLocation(squad, neighborChunk)
    local settle = neighborChunk[MOVEMENT_PHEROMONE] + neighborChunk[RESOURCE_PHEROMONE]
    return settle - lookupMovementPenalty(squad, neighborChunk) - (neighborChunk[PLAYER_PHEROMONE] * PLAYER_PHEROMONE_MULTIPLER)
end

local function scoreSiegeLocation(squad, neighborChunk)
    local settle = neighborChunk[MOVEMENT_PHEROMONE] + neighborChunk[BASE_PHEROMONE] + neighborChunk[RESOURCE_PHEROMONE] + (neighborChunk[PLAYER_PHEROMONE] * PLAYER_PHEROMONE_MULTIPLER)
    return settle - lookupMovementPenalty(squad, neighborChunk)
end

local function scoreAttackLocation(natives, squad, neighborChunk)
    local damage

    if (neighborChunk[MOVEMENT_PHEROMONE] >= 0) then
        damage = neighborChunk[MOVEMENT_PHEROMONE] + (neighborChunk[BASE_PHEROMONE] ) + (neighborChunk[PLAYER_PHEROMONE] * PLAYER_PHEROMONE_MULTIPLER)
    else
        damage = (neighborChunk[BASE_PHEROMONE] * (1 - (neighborChunk[MOVEMENT_PHEROMONE] / -natives.retreatThreshold))) + (neighborChunk[PLAYER_PHEROMONE] * PLAYER_PHEROMONE_MULTIPLER)
    end

    return damage - lookupMovementPenalty(squad, neighborChunk)
end

local function scoreAttackKamikazeLocation(natives, squad, neighborChunk)
    local damage = neighborChunk[BASE_PHEROMONE] + (neighborChunk[PLAYER_PHEROMONE] * PLAYER_PHEROMONE_MULTIPLER)
    return damage - lookupMovementPenalty(squad, neighborChunk)
end


local function settleMove(map, attackPosition, attackCmd, settleCmd, squad, group, natives, surface)
    local groupState = group.state

    if (groupState == DEFINES_GROUP_FINISHED) or (groupState == DEFINES_GROUP_GATHERING) or ((groupState == DEFINES_GROUP_MOVING) and (squad.cycles <= 0)) then
	local groupPosition = group.position
	local x, y = positionToChunkXY(groupPosition)
	local chunk = getChunkByXY(map, x, y)
        local scoreFunction = scoreResourceLocation
        if (natives.state == AI_STATE_SIEGE) then
            scoreFunction = scoreSiegeLocation
        end
	local attackChunk, attackDirection = scoreNeighborsForSettling(map,
								       chunk,
								       getNeighborChunks(map, x, y),
								       scoreFunction,
								       squad)
	if (chunk ~= SENTINEL_IMPASSABLE_CHUNK) then
	    addSquadToChunk(map, chunk, squad)
	    addMovementPenalty(natives, squad, chunk)
            -- elseif (attackChunk ~= SENTINEL_IMPASSABLE_CHUNK) then
            --     addSquadToChunk(map, attackChunk, squad)
            --     addMovementPenalty(natives, squad, attackChunk)
	end
	if group.valid and (attackChunk ~= SENTINEL_IMPASSABLE_CHUNK) then
	    local resourceGenerator = getResourceGenerator(map, chunk)
	    local distance = euclideanDistancePoints(groupPosition.x, groupPosition.y, squad.originPosition.x, squad.originPosition.y)

	    if (distance >= squad.maxDistance) or ((resourceGenerator ~= 0) and (getNestCount(map, chunk) == 0)) then
		local position = findMovementPosition(surface, groupPosition)
		if position then
		    attackPosition.x = position.x
		    attackPosition.y = position.y

                    if squad.kamikaze then
                        settleCmd.distraction = DEFINES_DISTRACTION_NONE
                    else
                        settleCmd.distraction = DEFINES_DISTRACTION_BY_ENEMY
                    end
                    
		    squad.status = SQUAD_BUILDING

		    group.set_command(settleCmd)
		    group.start_moving()
		else
		    addMovementPenalty(natives, squad, attackChunk)
		end
	    elseif (groupState == DEFINES_GROUP_FINISHED) or (groupState == DEFINES_GROUP_GATHERING) then
		squad.cycles = ((#squad.group.members > 80) and 6) or 4

                if squad.kamikaze then
                    attackCmd.distraction = DEFINES_DISTRACTION_NONE
                else
                    attackCmd.distraction = DEFINES_DISTRACTION_BY_ENEMY
                end

		local position = findMovementPosition(surface, positionFromDirectionAndChunk(attackDirection, groupPosition, attackPosition, 1.35))
		if position then
		    attackPosition.x = position.x
		    attackPosition.y = position.y

		    group.set_command(attackCmd)
		    group.start_moving()
		else
		    addMovementPenalty(natives, squad, attackChunk)
		end
	    end
	end
    end
end


local function attackMove(map, attackPosition, attackCmd, squad, group, natives, surface)
    local groupState = group.state

    if (groupState == DEFINES_GROUP_FINISHED) or (groupState == DEFINES_GROUP_GATHERING) or ((groupState == DEFINES_GROUP_MOVING) and (squad.cycles <= 0)) then
	local groupPosition = group.position
	local x, y = positionToChunkXY(groupPosition)
	local chunk = getChunkByXY(map, x, y)
        local attackScorer = scoreAttackLocation
        if (squad.attackScoreFunction == ATTACK_SCORE_KAMIKAZE) then
            attackScorer = scoreAttackKamikazeLocation
        end
	local attackChunk, attackDirection = scoreNeighborsForAttack(map,
                                                                     natives,
								     chunk,
								     getNeighborChunks(map, x, y),
								     attackScorer,
								     squad)
	if (chunk ~= SENTINEL_IMPASSABLE_CHUNK) then
	    addSquadToChunk(map, chunk, squad)
	    addMovementPenalty(natives, squad, chunk)
            -- elseif (attackChunk ~= SENTINEL_IMPASSABLE_CHUNK) then
            --     addSquadToChunk(map, attackChunk, squad)
            --     addMovementPenalty(natives, squad, attackChunk)
	end
	if group.valid and (attackChunk ~= SENTINEL_IMPASSABLE_CHUNK) then
	    local playerBaseGenerator = getPlayerBaseGenerator(map, attackChunk)
            -- local playerPheromone = attackChunk[PLAYER_PHEROMONE]
	    if (playerBaseGenerator == 0) or
                ((groupState == DEFINES_GROUP_FINISHED) or (groupState == DEFINES_GROUP_GATHERING))--  or
            -- (playerPheromone < natives.attackPlayerThreshold)
            then
		squad.cycles = ((#squad.group.members > 80) and 6) or 4

		local moreFrenzy = not squad.rabid and squad.frenzy and (euclideanDistanceNamed(groupPosition, squad.frenzyPosition) < 100)
		squad.frenzy = moreFrenzy

		if squad.rabid or squad.frenzy then
		    attackCmd.distraction = DEFINES_DISTRACTION_BY_ANYTHING
		else
		    attackCmd.distraction = DEFINES_DISTRACTION_BY_ENEMY
		end

		local position = findMovementPosition(surface, positionFromDirectionAndChunk(attackDirection, groupPosition, attackPosition, 1.35))
		if position then
		    attackPosition.x = position.x
		    attackPosition.y = position.y

		    group.set_command(attackCmd)
		    group.start_moving()
		else
		    addMovementPenalty(natives, squad, attackChunk)
		end
	    elseif not squad.frenzy and not squad.rabid and
		((groupState == DEFINES_GROUP_ATTACKING_DISTRACTION) or (groupState == DEFINES_GROUP_ATTACKING_TARGET) or
		    (playerBaseGenerator ~= 0)) then
		    squad.frenzy = true
		    squad.frenzyPosition.x = groupPosition.x
		    squad.frenzyPosition.y = groupPosition.y
	    end
	end
    end
end

function squadAttack.squadsDispatch(map, surface, natives)
    local squads = natives.squads
    local attackPosition = map.position
    local attackCmd = map.attackAreaCommand
    local settleCmd = map.settleCommand

    for i=#squads,1,-1 do
        local squad = squads[i]
        local group = squad.group
        if group and group.valid then
            
            local memberCount = #group.members
	    if (memberCount == 0) then
		tRemove(squads, i)
                removeSquadFromChunk(map, squad)
		group.destroy()                
	    elseif (memberCount > AI_MAX_BITER_GROUP_SIZE) then
		local members = group.members
		unitGroupUtils.recycleBiters(natives, members)
		tRemove(squads, i)
                removeSquadFromChunk(map, squad)
		group.destroy()
	    else
		local status = squad.status
		local cycles = squad.cycles

                if (status == SQUAD_RAIDING) then
                    attackMove(map, attackPosition, attackCmd, squad, group, natives, surface)
                elseif (status == SQUAD_SETTLING) then
                    settleMove(map, attackPosition, attackCmd, settleCmd, squad, group, natives, surface)
                elseif (status == SQUAD_RETREATING) and (cycles == 0) then
                    natives.pendingAttack[#natives.pendingAttack+1] = squad
                    squad.status = SQUAD_GUARDING                    
                elseif (status == SQUAD_BUILDING) then
                    tRemove(squads, i)
                    removeSquadFromChunk(map, squad)
                    natives.building[#natives.building+1] = squad
                end
                if (cycles > 0) then
		    squad.cycles = cycles - 1                   
                end                
	    end            
	    
        else
            tRemove(squads, i)
            removeSquadFromChunk(map, squad)
        end        
    end
end

function squadAttack.squadsBeginAttack(natives, players)
    local squads = natives.pendingAttack
    for i=1,#squads do
        local squad = squads[i]
	local group = squad.group
        if group and group.valid then
            local kamikazeThreshold = calculateKamikazeThreshold(#squad.group.members, natives)
            if not squad.kamikaze then
                squad.kamikaze = (mRandom() < kamikazeThreshold)
            end
            if squad.settlers then
                -- if (squad.status == SQUAD_GUARDING) then
                squad.status = SQUAD_SETTLING
                natives.squads[#natives.squads+1] = squad
            else
                -- if (squad.status == SQUAD_GUARDING) then
                local groupPosition = group.position
                if playersWithinProximityToPosition(players, groupPosition, 100, natives) then
                    squad.frenzy = true
                    squad.frenzyPosition.x = groupPosition.x
                    squad.frenzyPosition.y = groupPosition.y
                end

                if squad.kamikaze and (mRandom() < (kamikazeThreshold * 0.75)) then
                    squad.attackScoreFunction = ATTACK_SCORE_KAMIKAZE
                end
                squad.status = SQUAD_RAIDING
                natives.squads[#natives.squads+1] = squad                    
            end
            -- end
        end
    end
    natives.pendingAttack = {}
end

squadAttackG = squadAttack
return squadAttack
