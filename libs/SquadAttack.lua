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
local chunkUtils = require("ChunkUtils")
local aiPredicates = require("AIPredicates")

-- constants

local PLAYER_PHEROMONE = constants.PLAYER_PHEROMONE
local MOVEMENT_PHEROMONE = constants.MOVEMENT_PHEROMONE
local BASE_PHEROMONE = constants.BASE_PHEROMONE
local RESOURCE_PHEROMONE = constants.RESOURCE_PHEROMONE

local ATTACK_SCORE_KAMIKAZE = constants.ATTACK_SCORE_KAMIKAZE

local DIVISOR_DEATH_TRAIL_TABLE = constants.DIVISOR_DEATH_TRAIL_TABLE

local BASE_CLEAN_DISTANCE = constants.BASE_CLEAN_DISTANCE

local SQUAD_BUILDING = constants.SQUAD_BUILDING

local SQUAD_RAIDING = constants.SQUAD_RAIDING
local SQUAD_SETTLING = constants.SQUAD_SETTLING
local SQUAD_GUARDING = constants.SQUAD_GUARDING
local SQUAD_RETREATING = constants.SQUAD_RETREATING

local AI_MAX_BITER_GROUP_SIZE = constants.AI_MAX_BITER_GROUP_SIZE

local ATTACK_QUEUE_SIZE = constants.ATTACK_QUEUE_SIZE

local AI_STATE_SIEGE = constants.AI_STATE_SIEGE

local PLAYER_PHEROMONE_MULTIPLER = constants.PLAYER_PHEROMONE_MULTIPLER

local DEFINES_GROUP_FINISHED = defines.group_state.finished
local DEFINES_GROUP_GATHERING = defines.group_state.gathering
local DEFINES_GROUP_MOVING = defines.group_state.moving
local DEFINES_DISTRACTION_NONE = defines.distraction.none
local DEFINES_DISTRACTION_BY_ENEMY = defines.distraction.by_enemy
local DEFINES_DISTRACTION_BY_ANYTHING = defines.distraction.by_anything

-- imported functions

local mRandom = math.random
local mMin = math.min
local tRemove = table.remove

local canMigrate = aiPredicates.canMigrate

local euclideanDistancePoints = mathUtils.euclideanDistancePoints

local findMovementPosition = movementUtils.findMovementPosition

local removeSquadFromChunk = chunkPropertyUtils.removeSquadFromChunk
local addDeathGenerator = chunkPropertyUtils.addDeathGenerator
local getDeathGenerator = chunkPropertyUtils.getDeathGenerator

local getNestCount = chunkPropertyUtils.getNestCount

local getNeighborChunks = mapUtils.getNeighborChunks
local addSquadToChunk = chunkPropertyUtils.addSquadToChunk
local getChunkByXY = mapUtils.getChunkByXY
local positionToChunkXY = mapUtils.positionToChunkXY
local addMovementPenalty = movementUtils.addMovementPenalty
local lookupMovementPenalty = movementUtils.lookupMovementPenalty
local calculateKamikazeThreshold = unitGroupUtils.calculateKamikazeThreshold
local positionFromDirectionAndChunk = mapUtils.positionFromDirectionAndChunk
local positionFromDirectionAndFlat = mapUtils.positionFromDirectionAndFlat

local createSquad = unitGroupUtils.createSquad
local membersToSquad = unitGroupUtils.membersToSquad

local euclideanDistanceNamed = mathUtils.euclideanDistanceNamed

local getPlayerBaseGenerator = chunkPropertyUtils.getPlayerBaseGenerator
local getResourceGenerator = chunkPropertyUtils.getResourceGenerator

local getChunkByPosition = mapUtils.getChunkByPosition

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
    local movementPheromone = neighborChunk[MOVEMENT_PHEROMONE]

    if (movementPheromone >= 0) then
        damage = movementPheromone + (neighborChunk[BASE_PHEROMONE]) + (neighborChunk[PLAYER_PHEROMONE] * PLAYER_PHEROMONE_MULTIPLER)
    else
        damage = (neighborChunk[BASE_PHEROMONE] * (1 - (movementPheromone / -natives.retreatThreshold))) + (neighborChunk[PLAYER_PHEROMONE] * PLAYER_PHEROMONE_MULTIPLER)
    end

    return damage - lookupMovementPenalty(squad, neighborChunk)
end

local function scoreAttackKamikazeLocation(natives, squad, neighborChunk)
    local damage = neighborChunk[BASE_PHEROMONE] + (neighborChunk[PLAYER_PHEROMONE] * PLAYER_PHEROMONE_MULTIPLER)
    return damage - lookupMovementPenalty(squad, neighborChunk)
end


local function settleMove(map, squad, surface)
    local targetPosition = map.position
    local targetPosition2 = map.position2
    local group = squad.group

    local groupPosition = group.position
    local x, y = positionToChunkXY(groupPosition)
    local chunk = getChunkByXY(map, x, y)
    local scoreFunction = scoreResourceLocation
    local groupState = group.state
    local natives = map.natives
    if (natives.state == AI_STATE_SIEGE) then
        scoreFunction = scoreSiegeLocation
    end
    addMovementPenalty(squad, squad.chunk)
    addSquadToChunk(map, chunk, squad)
    local distance = euclideanDistancePoints(groupPosition.x,
                                             groupPosition.y,
                                             squad.originPosition.x,
                                             squad.originPosition.y)

    local cmd
    local position
    local position2
    
    if (distance >= squad.maxDistance) or ((getResourceGenerator(map, chunk) ~= 0) and (getNestCount(map, chunk) == 0))
    then
        if not ((groupState == DEFINES_GROUP_FINISHED) or ((groupState == DEFINES_GROUP_GATHERING) and (squad.cycles <= 0))) then
            return
        end

        position = findMovementPosition(surface, groupPosition)

        if not position then
            position = groupPosition
        end

        targetPosition.x = position.x
        targetPosition.y = position.y       
        
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

        local entities = surface.find_entities_filtered(map.filteredEntitiesClearBuildingQuery)
        for i=1,#entities do
            local entity = entities[i]
            if entity.valid and (entity.type ~= "cliff") then
                entity.die()
            end
        end

        squad.cycles = 40
        group.set_command(cmd)
    else
        local attackChunk, attackDirection, nextAttackChunk, nextAttackDirection = scoreNeighborsForSettling(map,
                                                                                                             chunk,
                                                                                                             getNeighborChunks(map, x, y),
                                                                                                             scoreFunction,
                                                                                                             squad)

        if (attackChunk == -1) then
            squad.cycles = 30
            cmd = map.wonderCommand
            group.set_command(cmd)
            return
        else
            positionFromDirectionAndFlat(attackDirection, groupPosition, targetPosition)
            position = findMovementPosition(surface, targetPosition)

            local attackPlayerThreshold = natives.attackPlayerThreshold
            
            if not position then
                squad.cycles = 30
                cmd = map.wonderCommand
                group.set_command(cmd)
                return
            else
                targetPosition.x = position.x
                targetPosition.y = position.y
                
                if (getPlayerBaseGenerator(map, attackChunk) ~= 0) or
                    (attackChunk[PLAYER_PHEROMONE] >= attackPlayerThreshold)
                then
                    cmd = map.attackCommand

                    if not squad.rabid then
                        squad.frenzy = true
                        squad.frenzyPosition.x = groupPosition.x
                        squad.frenzyPosition.y = groupPosition.y
                    end
                else
                    cmd = map.moveCommand
                    if squad.rabid or squad.kamikaze then
                        cmd.distraction = DEFINES_DISTRACTION_NONE
                    else
                        cmd.distraction = DEFINES_DISTRACTION_BY_ENEMY
                    end
                end
            end

            if (nextAttackChunk ~= -1) then                
                positionFromDirectionAndFlat(nextAttackDirection, targetPosition, targetPosition2)

                position2 = findMovementPosition(surface, targetPosition2)

                if position2 then
                    targetPosition.x = position2.x
                    targetPosition.y = position2.y

                    if ((cmd ~= map.attackCommand) and
                            ((getPlayerBaseGenerator(map, nextAttackChunk) ~= 0) or
                                    (nextAttackChunk[PLAYER_PHEROMONE] >= attackPlayerThreshold)))
                    then
                        cmd = map.attackCommand

                        if not squad.rabid then
                            squad.frenzy = true
                            squad.frenzyPosition.x = groupPosition.x
                            squad.frenzyPosition.y = groupPosition.y
                        end
                    end
                end
            end
        end

        if (attackDirection == 0) then
            cmd = map.settleCommand
            if squad.kamikaze then
                cmd.distraction = DEFINES_DISTRACTION_NONE
            else
                cmd.distraction = DEFINES_DISTRACTION_BY_ENEMY
            end

            squad.status = SQUAD_BUILDING

            map.buildPositionTop.x = targetPosition.x - BASE_CLEAN_DISTANCE
            map.buildPositionTop.y = targetPosition.y - BASE_CLEAN_DISTANCE
            map.buildPositionBottom.x = targetPosition.x + BASE_CLEAN_DISTANCE
            map.buildPositionBottom.y = targetPosition.y + BASE_CLEAN_DISTANCE

            local entities = surface.find_entities_filtered(map.filteredEntitiesClearBuildingQuery)
            for i=1,#entities do
                local entity = entities[i]
                if entity.valid and (entity.type ~= "cliff") then
                    entity.die()
                end
            end

            squad.cycles = 40
            group.set_command(cmd)
        else
            squad.cycles = 23
            group.set_command(cmd)
        end
    end
end

local function attackMove(map, squad, surface)
    local targetPosition = map.position
    local targetPosition2 = map.position2

    local group = squad.group

    local groupPosition = group.position
    local x, y = positionToChunkXY(groupPosition)
    local chunk = getChunkByXY(map, x, y)
    local attackScorer = ((squad.attackScoreFunction == ATTACK_SCORE_KAMIKAZE) and scoreAttackKamikazeLocation)
        or scoreAttackLocation

    local squadChunk = squad.chunk
    addMovementPenalty(squad, squadChunk)
    addSquadToChunk(map, chunk, squad)
    squad.frenzy = (squad.frenzy and (euclideanDistanceNamed(groupPosition, squad.frenzyPosition) < 100))
    local attackChunk, attackDirection, nextAttackChunk, nextAttackDirection = scoreNeighborsForAttack(map,
                                                                                                       chunk,
                                                                                                       getNeighborChunks(map, x, y),
                                                                                                       attackScorer,
                                                                                                       squad)
    if (attackChunk == -1) then
        squad.cycles = 30
        cmd = map.wonderCommand
        group.set_command(cmd)
        return
    else
        positionFromDirectionAndFlat(attackDirection, groupPosition, targetPosition)

        local attackPlayerThreshold = map.natives.attackPlayerThreshold
        local position = findMovementPosition(surface, targetPosition)

        if not position then
            squad.cycles = 30
            cmd = map.wonderCommand
            group.set_command(cmd)
            return
        else
            targetPosition.x = position.x
            targetPosition.y = position.y

            if (getPlayerBaseGenerator(map, attackChunk) ~= 0) and
                (attackChunk[PLAYER_PHEROMONE] >= attackPlayerThreshold)
            then
                cmd = map.attackCommand

                if not squad.rabid then
                    squad.frenzy = true
                    squad.frenzyPosition.x = groupPosition.x
                    squad.frenzyPosition.y = groupPosition.y
                end
            else
                cmd = map.moveCommand
                if squad.rabid or squad.frenzy then
                    cmd.distraction = DEFINES_DISTRACTION_BY_ANYTHING
                else
                    cmd.distraction = DEFINES_DISTRACTION_BY_ENEMY
                end
            end
        end

        local position2

        if (nextAttackChunk ~= -1) then
            positionFromDirectionAndFlat(nextAttackDirection, targetPosition, targetPosition2)

            position2 = findMovementPosition(surface, targetPosition2)

            if position2 then
                targetPosition.x = position2.x
                targetPosition.y = position2.y

                if (cmd ~= map.attackCommand) and
                    ((getPlayerBaseGenerator(map, nextAttackChunk) ~= 0) or
                            (nextAttackChunk[PLAYER_PHEROMONE] >= attackPlayerThreshold))
                then
                    cmd = map.attackCommand

                    if not squad.rabid then
                        squad.frenzy = true
                        squad.frenzyPosition.x = groupPosition.x
                        squad.frenzyPosition.y = groupPosition.y
                    end
                end
            end
        end

        squad.cycles = 23
        group.set_command(cmd)
    end
end

function squadAttack.squadsDispatch(map, surface)
    local natives = map.natives
    local squads = natives.squads

    -- print("start dispatch")
    -- local startIndex = natives.attackIndex

    -- local maxSquadIndex = mMin(startIndex + ATTACK_QUEUE_SIZE, #squads)
    -- for i=maxSquadIndex,startIndex,-1 do
    local pending = natives.pendingAttack
    local squadsLen = squads.len
    local x = 0

    for i=1,squadsLen do
        local squad = squads[i]
        local group = squad.group
        if group and group.valid then
            local memberCount = #group.members
            if (memberCount == 0) then
                removeSquadFromChunk(map, squad)
                local deathGen = getDeathGenerator(map, squad.chunk)
                local penalties = squad.penalties
                for xc=1,mMin(#squad.penalties,5) do
                    addDeathGenerator(map,
                                      penalties[xc].c,
                                      deathGen * DIVISOR_DEATH_TRAIL_TABLE[xc])
                end
                group.destroy()
            elseif (memberCount > AI_MAX_BITER_GROUP_SIZE) then
                local members = group.members
                unitGroupUtils.recycleBiters(natives, members)
                removeSquadFromChunk(map, squad)
                group.destroy()
            else
                local status = squad.status
                local cycles = squad.cycles
                local groupState = group.state

                if (status == SQUAD_RAIDING) then
                    x = x + 1
                    squads[x] = squad
                    if (groupState == DEFINES_GROUP_FINISHED) or
                        (groupState == DEFINES_GROUP_GATHERING) or
                        ((groupState == DEFINES_GROUP_MOVING) and (cycles <= 0))
                    then
                        attackMove(map, squad, surface)
                    else
                        local chunk = getChunkByPosition(map, group.position)
                        addSquadToChunk(map, chunk, squad)
                    end
                elseif (status == SQUAD_SETTLING) then
                    x = x + 1
                    squads[x] = squad
                    if (groupState == DEFINES_GROUP_FINISHED) or
                        (groupState == DEFINES_GROUP_GATHERING) or
                        ((groupState == DEFINES_GROUP_MOVING) and (cycles <= 0))
                    then
                        settleMove(map, squad, surface)
                    else
                        local chunk = getChunkByPosition(map, group.position)
                        addSquadToChunk(map, chunk, squad)
                    end
                elseif (status == SQUAD_RETREATING) then
                    if (groupState == DEFINES_GROUP_FINISHED) or
                        (groupState == DEFINES_GROUP_GATHERING) or
                        ((groupState == DEFINES_GROUP_MOVING) and (cycles <= 0))
                    then
                        pending.len = pending.len + 1
                        pending[pending.len] = squad
                        squad.status = SQUAD_GUARDING
                    else
                        x = x + 1
                        squads[x] = squad
                    end
                    local chunk = getChunkByPosition(map, group.position)
                    addSquadToChunk(map, chunk, squad)
                elseif (status == SQUAD_BUILDING) then
                    removeSquadFromChunk(map, squad)
                    natives.building[#natives.building+1] = squad
                elseif (status == SQUAD_GUARDING) then
                    pending.len = pending.len + 1
                    pending[pending.len] = squad
                else
                    x = x + 1
                    squads[x] = squad
                end
                if (cycles > 0) then
                    squad.cycles = cycles - 1
                end
            end

        else
            removeSquadFromChunk(map, squad)
        end
    end

    squads.len = x

    -- print("end dispatch")
    -- if (maxSquadIndex >= #squads) then
    --     natives.attackIndex = 1
    -- else
    --     natives.attackIndex = maxSquadIndex + 1
    -- end
end



function squadAttack.squadsBeginAttack(natives, surface)
    local squads = natives.squads
    local pendingAttack = natives.pendingAttack
    local pendingStealGroups = natives.pendingStealGroups    

    local cmd = natives.map.retreatCommand

    local x = 0
    
    for i=1, pendingStealGroups.len do
        local group = pendingStealGroups[i]
        if group and group.valid then
            if (group.state ~= DEFINES_GROUP_GATHERING) then
                local settlers = canMigrate(natives, surface) and (mRandom() > 0.25)                
                local squad = createSquad(group.position, surface, nil, settlers)
                pendingAttack.len = pendingAttack.len + 1
                pendingAttack[pendingAttack.len] = squad
                cmd.group = squad.group
                membersToSquad(cmd, #group.members, group.members, true)
                group.destroy()
            else
                x = x + 1
                pendingStealGroups[x] = group
            end
        end
    end

    pendingStealGroups.len = x

    x = 0
    
    for i=1,pendingAttack.len do
        local squad = pendingAttack[i]
        local group = squad.group
        if group and group.valid then
            local groupState = group.state            
            if (groupState ~= DEFINES_GROUP_FINISHED) and (squad.cycles ~= 0) then
                squad.cycles = squad.cycles - 1
                x = x + 1
                pendingAttack[x] = squad
            else
                local kamikazeThreshold = calculateKamikazeThreshold(#squad.group.members, natives)
                if not squad.kamikaze then
                    squad.kamikaze = (mRandom() < kamikazeThreshold)
                end
                if squad.settlers then
                    squad.status = SQUAD_SETTLING
                    squads.len = squads.len + 1
                    squads[squads.len] = squad
                else
                    if squad.kamikaze and (mRandom() < (kamikazeThreshold * 0.75)) then
                        squad.attackScoreFunction = ATTACK_SCORE_KAMIKAZE
                    end
                    squad.status = SQUAD_RAIDING
                    squads.len = squads.len + 1
                    squads[squads.len] = squad
                end
            end
        end
    end

    pendingAttack.len = x

end

squadAttackG = squadAttack
return squadAttack
