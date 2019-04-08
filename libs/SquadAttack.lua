if (squadAttackG) then
    return squadAttackG
end
local squadAttack = {}

-- imports

local constants = require("Constants")
local mapUtils = require("MapUtils")
local unitGroupUtils = require("UnitGroupUtils")
local movementUtils = require("MovementUtils")
local mathUtils = require("MathUtils")
local chunkPropertyUtils = require("ChunkPropertyUtils")

-- constants

local PLAYER_PHEROMONE = constants.PLAYER_PHEROMONE
local MOVEMENT_PHEROMONE = constants.MOVEMENT_PHEROMONE
local BASE_PHEROMONE = constants.BASE_PHEROMONE
local RESOURCE_PHEROMONE = constants.RESOURCE_PHEROMONE

local ATTACK_SCORE_KAMIKAZE = constants.ATTACK_SCORE_KAMIKAZE

local BASE_CLEAN_DISTANCE = constants.BASE_CLEAN_DISTANCE

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


local function settleMove(map, squad, natives, surface)
    local attackPosition = map.position
    local group = squad.group
    
    local groupPosition = group.position
    local x, y = positionToChunkXY(groupPosition)
    local chunk = getChunkByXY(map, x, y)
    local scoreFunction = scoreResourceLocation
    if (natives.state == AI_STATE_SIEGE) then
        scoreFunction = scoreSiegeLocation
    end
    if squad.chunk and (squad.chunk ~= SENTINEL_IMPASSABLE_CHUNK) and (squad.chunk ~= chunk) then
        addMovementPenalty(squad, squad.chunk)
    end            
    local attackChunk, attackDirection = scoreNeighborsForSettling(map,
                                                                   chunk,
                                                                   getNeighborChunks(map, x, y),
                                                                   scoreFunction,
                                                                   squad)
    if (chunk ~= SENTINEL_IMPASSABLE_CHUNK) then
        addSquadToChunk(map, chunk, squad)
    end

    local resourceGenerator = getResourceGenerator(map, chunk)
    local distance = euclideanDistancePoints(groupPosition.x,
                                             groupPosition.y,
                                             squad.originPosition.x,
                                             squad.originPosition.y)

    local largeGroup = (#squad.group.members > 80)        
    local cmd
    local position
    
    if (distance >= squad.maxDistance) or
        ((resourceGenerator ~= 0) and (getNestCount(map, chunk) == 0))
    then
        if not ((group.state == DEFINES_GROUP_FINISHED) or ((group.state == DEFINES_GROUP_GATHERING) and (squad.cycles <= 0))) then
        -- if (group.state ~= DEFINES_GROUP_FINISHED) then
            return
        end
        
        position = findMovementPosition(surface, groupPosition)
        
        cmd = map.settleCommand
        if squad.kamikaze then
            cmd.distraction = DEFINES_DISTRACTION_NONE
        else
            cmd.distraction = DEFINES_DISTRACTION_BY_ENEMY
        end
        
        squad.status = SQUAD_BUILDING

        map.buildPositionTop.x = position.x - BASE_CLEAN_DISTANCE
        map.buildPositionTop.y = position.y - BASE_CLEAN_DISTANCE
        map.buildPositionBottom.x = position.x + BASE_CLEAN_DISTANCE
        map.buildPositionBottom.y = position.y + BASE_CLEAN_DISTANCE

        squad.cycles = 400
        
        local entities = surface.find_entities_filtered(map.filteredEntitiesClearBuildingQuery)
        for i=1,#entities do
            local entity = entities[i]
            if entity.valid and (entity.type ~= "cliff") then
                entity.die()
            end
        end                       
    else
        if (attackChunk == SENTINEL_IMPASSABLE_CHUNK) then
            return
        end
        
        if (getPlayerBaseGenerator(map, attackChunk) ~= 0) or
            (attackChunk[PLAYER_PHEROMONE] >= natives.attackPlayerThreshold)
        then    
            cmd = map.attackCommand            
        else
            cmd = map.moveCommand
            if squad.rabid or squad.kamikaze then
                cmd.distraction = DEFINES_DISTRACTION_NONE
            else
                cmd.distraction = DEFINES_DISTRACTION_BY_ENEMY
            end		
        end
    end

    if squad.status ~= SQUAD_BUILDING then
        position = findMovementPosition(surface,
                                        positionFromDirectionAndChunk(attackDirection,
                                                                      groupPosition,
                                                                      attackPosition,
                                                                      (largeGroup and 1.1) or 0.9))
    end
    
    if position then
        squad.cycles = (largeGroup and 6) or 4            
        attackPosition.x = position.x
        attackPosition.y = position.y

        group.set_command(cmd)
        group.start_moving()
    end
end


local function attackMove(map, squad, natives, surface)
    local attackPosition = map.position    
    local group = squad.group

    local groupPosition = group.position
    local x, y = positionToChunkXY(groupPosition)
    local chunk = getChunkByXY(map, x, y)
    local attackScorer = scoreAttackLocation
    if (squad.attackScoreFunction == ATTACK_SCORE_KAMIKAZE) then
        attackScorer = scoreAttackKamikazeLocation
    end
    if squad.chunk and (squad.chunk ~= SENTINEL_IMPASSABLE_CHUNK) and (squad.chunk ~= chunk) then
        addMovementPenalty(squad, squad.chunk)
    end    
    local attackChunk, attackDirection = scoreNeighborsForAttack(map,
                                                                 natives,
                                                                 chunk,
                                                                 getNeighborChunks(map, x, y),
                                                                 attackScorer,
                                                                 squad)
    if (chunk ~= SENTINEL_IMPASSABLE_CHUNK)then
        addSquadToChunk(map, chunk, squad)
    end    
    if (attackChunk ~= SENTINEL_IMPASSABLE_CHUNK) then
        local playerBaseGenerator = getPlayerBaseGenerator(map, attackChunk)
        local playerPheromone = attackChunk[PLAYER_PHEROMONE]
        local cmd
        local position
        local largeGroup = (#squad.group.members > 80)

        if (playerBaseGenerator == 0) and (playerPheromone < natives.attackPlayerThreshold) then
            squad.frenzy = (squad.frenzy and (euclideanDistanceNamed(groupPosition, squad.frenzyPosition) < 100))
            
            cmd = map.moveCommand
            if squad.rabid or squad.frenzy then
                cmd.distraction = DEFINES_DISTRACTION_BY_ANYTHING
            else
                cmd.distraction = DEFINES_DISTRACTION_BY_ENEMY
            end		
        else
            cmd = map.attackCommand
            
            if not squad.rabid then
                squad.frenzy = true
                squad.frenzyPosition.x = groupPosition.x
                squad.frenzyPosition.y = groupPosition.y
            end           
        end

        position = findMovementPosition(surface,
                                        positionFromDirectionAndChunk(attackDirection,
                                                                      groupPosition,
                                                                      attackPosition,
                                                                      (largeGroup and 1.1) or 0.9))
        
        if position then
            squad.cycles = (largeGroup and 6) or 4
            attackPosition.x = position.x
            attackPosition.y = position.y

            group.set_command(cmd)
            group.start_moving()
        end
    end
end

function squadAttack.squadsDispatch(map, surface, natives)
    local squads = natives.squads
    -- local attackPosition = map.position
    -- local attackCmd = map.attackAreaCommand
    -- local settleCmd = map.settleCommand

    -- print("start dispatch")
    -- local startIndex = natives.attackIndex

    -- local maxSquadIndex = mMin(startIndex + ATTACK_QUEUE_SIZE, #squads)
    -- for i=maxSquadIndex,startIndex,-1 do        
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
                local groupState = group.state

                if (status == SQUAD_RAIDING) then
                    if (groupState == DEFINES_GROUP_FINISHED) or
                        (groupState == DEFINES_GROUP_GATHERING) or
                        ((groupState == DEFINES_GROUP_MOVING) and (cycles <= 0))
                    then
                        attackMove(map, squad, natives, surface)
                    end
                elseif (status == SQUAD_SETTLING) then
                    if (groupState == DEFINES_GROUP_FINISHED) or
                        (groupState == DEFINES_GROUP_GATHERING) or
                        ((groupState == DEFINES_GROUP_MOVING) and (cycles <= 0))
                    then
                        settleMove(map, squad, natives, surface)
                    end
                elseif (status == SQUAD_RETREATING) and (cycles <= 0) then
                    natives.pendingAttack[#natives.pendingAttack+1] = squad
                    squad.status = SQUAD_GUARDING                    
                elseif (status == SQUAD_BUILDING) then
                    tRemove(squads, i)
                    removeSquadFromChunk(map, squad)
                    natives.building[#natives.building+1] = squad
                elseif (status == SQUAD_GUARDING) then
                    tRemove(squads, i)
                    natives.pendingAttack[#natives.pendingAttack+1] = squad
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


    -- print("end dispatch")
    -- if (maxSquadIndex >= #squads) then
    --     natives.attackIndex = 1
    -- else
    --     natives.attackIndex = maxSquadIndex + 1
    -- end
end

function squadAttack.squadsBeginAttack(natives)
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
                squad.status = SQUAD_SETTLING
                natives.squads[#natives.squads+1] = squad
            else              
                if squad.kamikaze and (mRandom() < (kamikazeThreshold * 0.75)) then
                    squad.attackScoreFunction = ATTACK_SCORE_KAMIKAZE
                end
                squad.status = SQUAD_RAIDING
                natives.squads[#natives.squads+1] = squad                    
            end
        end
    end
    natives.pendingAttack = {}
end

squadAttackG = squadAttack
return squadAttack
