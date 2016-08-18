local aiAttack = {}

-- local attackLocationNeighbors = {1,2,3,4}

local factorio_defined = defines
local constants = require("Constants")
local mapUtils = require("MapUtils")
local utils = require("Utils")
local unitGroupUtils = require("UnitGroupUtils")

local attackPosition = {x=0, y=0} -- used to minimize garbage generation
local attackLocationNeighbors = {1,2,3,4,5,6,7,8}
local attackLocationCommand = {type=defines.command.attack_area,
                               destination=attackPosition,
                               radius=constants.HALF_CHUNK_SIZE,
                               distraction=defines.distraction.by_anything}

local attackObjectCommand = {type=defines.command.attack,
                             target=1,
                             distraction=defines.distraction.by_anything}
                               
local attackPlayerCommand = {type=defines.command.attack,
                             target=1}

local nearestAreaBox = {{1,2},
                        {3,4}}
local nearestTable = {area=nearestAreaBox,
                      force="player",
                      limit=1}
                             
local mRandom = math.random

function aiAttack.squadAttackLocation(regionMap, surface, natives)
    local PLAYER_PHEROMONE = constants.PLAYER_PHEROMONE
    local DEATH_PHEROMONE = constants.DEATH_PHEROMONE
    local SQUAD_RAIDING = constants.SQUAD_RAIDING
    local SQUAD_SUICIDE_RAID = constants.SQUAD_SUICIDE_RAID
    local ENEMY_BASE_PHEROMONE = constants.ENEMY_BASE_PHEROMONE
    local ENEMY_BASE_GENERATOR = constants.ENEMY_BASE_GENERATOR
    local PLAYER_BASE_PHEROMONE = constants.PLAYER_BASE_PHEROMONE
    local PLAYER_DEFENSE_PHEROMONE = constants.PLAYER_DEFENSE_PHEROMONE
    local MAGIC_MAXIMUM_NUMBER = constants.MAGIC_MAXIMUM_NUMBER
    local HALF_CHUNK_SIZE = constants.HALF_CHUNK_SIZE
    local CHUNK_SIZE = constants.CHUNK_SIZE
    local group_states = factorio_defined.group_state
    local mathRandom = mRandom
    
    -- local getNeighborChunks = mapUtils.getCardinalChunks
    local getNeighborChunks = mapUtils.getNeighborChunks
    local positionToChunk = mapUtils.positionToChunkOffset
    
    local squads = natives.squads
    for i=1, #squads do
        local squad = squads[i]
        local group = squad.group
        local squadStatus = squad.status
        if group.valid and ((squadStatus == SQUAD_RAIDING) or (squadStatus == SQUAD_SUICIDE_RAID)) then
            if ((group.state == group_states.finished) or (group.state == group_states.gathering) or (group.state == group_states.moving)) and (squad.cycles == 0) then
                local cX, cY
                -- if (squad.cX == nil) then
                    cX, cY = positionToChunk(group.position)
                -- else
                    -- cX = squad.cX
                    -- cY = squad.cY
                -- end
                getNeighborChunks(regionMap, cX, cY, attackLocationNeighbors)
                local attackChunk
                local attackScore = -MAGIC_MAXIMUM_NUMBER
                local attackDirection            
                -- print("------")
                for x=1, 8 do
                    local neighborChunk = attackLocationNeighbors[x]
                    if (neighborChunk ~= nil) then
                        attackPosition.x = neighborChunk.pX
                        attackPosition.y = neighborChunk.pY
                        local damageScore = surface.get_pollution(attackPosition) + neighborChunk[PLAYER_BASE_PHEROMONE] + neighborChunk[PLAYER_PHEROMONE] + neighborChunk[constants.PLAYER_DEFENSE_GENERATOR]
                        local avoidScore = neighborChunk[DEATH_PHEROMONE] + neighborChunk[ENEMY_BASE_GENERATOR] + neighborChunk[ENEMY_BASE_PHEROMONE] --
                        local score = damageScore - avoidScore
                        if (score > attackScore) then
                            attackScore = score
                            attackChunk = neighborChunk
                            attackDirection = x
                        end
                        -- print(x, score, damageScore, avoidScore, neighborChunk.cX, neighborChunk.cY)
                    end
                end
                if (attackChunk ~= nil) then
                    -- print("==")
                    -- print (attackDirection, cX, cY)
                    -- local target
                    if (attackChunk[constants.PLAYER_BASE_GENERATOR] == 0) or (attackChunk[constants.PLAYER_DEFENSE_GENERATOR] == 0) then
                        -- nearestAreaBox[1][1] = group.position.x - CHUNK_SIZE
                        -- nearestAreaBox[1][2] = group.position.y - CHUNK_SIZE
                        -- nearestAreaBox[2][1] = group.position.x + CHUNK_SIZE
                        -- nearestAreaBox[2][2] = group.position.y + CHUNK_SIZE
                        -- target = surface.find_entities_filtered(nearestTable)
                        -- if (#target == 1) and (target[1].prototype.max_health ~= 0) then
                            -- print(target[1].name)
                            -- attackObjectCommand.target = target[1]
                            -- group.set_command(attackObjectCommand)
                        -- end
                    
                        -- if (target == nil) then
                        -- utils.positionDirectionToChunkCorner(attackDirection, attackChunk, attackPosition)
                        attackPosition.x = attackChunk.pX + HALF_CHUNK_SIZE
                        attackPosition.y = attackChunk.pY + HALF_CHUNK_SIZE
                        squad.cycles = 4
                        squad.cX = attackChunk.cX
                        squad.cY = attackChunk.cY
                        
                        group.set_command(attackLocationCommand)
                        -- end
                        group.start_moving()
                    end
                end
            -- elseif (group.state == group_states.attacking_distraction) and (mathRandom() < 0.3) then
                -- local playerTarget = surface.find_nearest_enemy({position=group.position,
                                                                 -- max_distance=CHUNK_SIZE,
                                                                 -- force="enemy"})
                -- if (playerTarget ~= nil) then
                    -- group.set_command({type=defines.command.attack,
                                       -- target=playerTarget})
                -- end
            end
        end
    end
end


function aiAttack.squadAttackPlayer(regionMap, surface, natives, players)
    local SQUAD_HUNTING = constants.SQUAD_HUNTING
    local SQUAD_GUARDING = constants.SQUAD_GUARDING
    local SQUAD_SUICIDE_HUNT = constants.SQUAD_SUICIDE_HUNT
    local MAGIC_MAXIMUM_NUMBER = constants.MAGIC_MAXIMUM_NUMBER
    local findDistance = utils.euclideanDistanceNamed
    -- local commandAttack = 
    
    local mathRandom = mRandom
    
    local squads = natives.squads
    
    for si=1, #squads do
        local squad = squads[si]
        local group = squad.group
        if (group.valid) and (squad.status == SQUAD_GUARDING) then
            local closestPlayer
            local closestDistance = MAGIC_MAXIMUM_NUMBER
            for pi=1, #players do
                local playerCharacer = players[pi].character
                local distance = findDistance(playerCharacer.position, group.position)
                if (distance < closestDistance) then
                    closestPlayer = playerCharacer
                    closestDistance = distance
                end
            end
            if (closestDistance < 75) then
                local squadType = SQUAD_HUNTING
                if (mathRandom() < 0.10) then -- TODO add sliding scale based on number of members and evolution
                    squadType = SQUAD_SUICIDE_HUNT
                end
                squad.status = squadType
                attackPlayerCommand.target = closestPlayer
                group.set_command(attackPlayerCommand)
            end
        end
    end
end

function aiAttack.squadBeginAttack(natives)
    local SQUAD_GUARDING = constants.SQUAD_GUARDING
    local SQUAD_SUICIDE_RAID = constants.SQUAD_SUICIDE_RAID
    local SQUAD_RAIDING = constants.SQUAD_RAIDING
    local mathRandom = mRandom
    
    for i=1,#natives.squads do
        local squad = natives.squads[i]
        if (squad.status == SQUAD_GUARDING) and (mathRandom() < 0.7) then
            if (mathRandom() < 0.05) then
                squad.status = SQUAD_SUICIDE_RAID
            else
                squad.status = SQUAD_RAIDING
            end
        end
    end
end

return aiAttack