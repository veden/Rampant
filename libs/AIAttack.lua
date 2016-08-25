local aiAttack = {}

-- imports

local constants = require("Constants")
local mapUtils = require("MapUtils")
local unitGroupUtils = require("UnitGroupUtils")

-- constants
          
local PLAYER_PHEROMONE = constants.PLAYER_PHEROMONE
local DEATH_PHEROMONE = constants.DEATH_PHEROMONE
local ENEMY_BASE_PHEROMONE = constants.ENEMY_BASE_PHEROMONE
local PLAYER_BASE_PHEROMONE = constants.PLAYER_BASE_PHEROMONE
local PLAYER_DEFENSE_PHEROMONE = constants.PLAYER_DEFENSE_PHEROMONE

local SQUAD_RAIDING = constants.SQUAD_RAIDING
local SQUAD_SUICIDE_RAID = constants.SQUAD_SUICIDE_RAID
local SQUAD_HUNTING = constants.SQUAD_HUNTING
local SQUAD_GUARDING = constants.SQUAD_GUARDING
local SQUAD_SUICIDE_HUNT = constants.SQUAD_SUICIDE_HUNT

local AI_TUNNEL_COST = constants.AI_TUNNEL_COST

local ENEMY_BASE_GENERATOR = constants.ENEMY_BASE_GENERATOR
local MAGIC_MAXIMUM_NUMBER = constants.MAGIC_MAXIMUM_NUMBER

local HALF_CHUNK_SIZE = constants.HALF_CHUNK_SIZE
local CHUNK_SIZE = constants.CHUNK_SIZE

local PLAYER_BASE_GENERATOR = constants.PLAYER_BASE_GENERATOR
local PLAYER_DEFENSE_GENERATOR = constants.PLAYER_DEFENSE_GENERATOR

-- imported functions

local getCardinalChunksWithDirection = mapUtils.getCardinalChunksWithDirection
local positionToChunk = mapUtils.getChunkByPosition
local canMove = mapUtils.canMoveChunkDirectionCardinal
local addSquadMovementPenalty = unitGroupUtils.addSquadMovementPenalty
local lookupSquadMovementPenalty = unitGroupUtils.lookupSquadMovementPenalty
local positionDirectionToChunkCornerCardinal = mapUtils.positionDirectionToChunkCornerCardinal
local findDistance = mapUtils.euclideanDistanceNamed

-- module code

function aiAttack.squadAttackLocation(regionMap, surface, natives)
    local squads = natives.squads
    for i=1,#squads do
        local squad = squads[i]
        local group = squad.group
        if group.valid and ((squad.status == SQUAD_RAIDING) or (squad.status == SQUAD_SUICIDE_RAID)) then 
            if (group.state == defines.group_state.finished) or (group.state == defines.group_state.gathering) or ((group.state == defines.group_state.moving) and (squad.cycles == 0)) then
                local chunk = positionToChunk(regionMap, group.position.x, group.position.y)
                if (chunk ~= nil) then
                    addSquadMovementPenalty(squad, chunk.cX, chunk.cY)
                    local neighborDirectionChunks = getCardinalChunksWithDirection(regionMap, chunk.cX, chunk.cY)
                    local attackChunk
                    local attackScore = -MAGIC_MAXIMUM_NUMBER
                    local attackDirection    
                    local attackPosition = {x=0, y=0}
                    for x=1,#neighborDirectionChunks do
                        local neighborDirectionChunk = neighborDirectionChunks[x]
                        local neighborChunk = neighborDirectionChunk.c
                        if (neighborChunk ~= nil) and canMove(x, chunk, neighborChunk) then
                            attackPosition.x = neighborChunk.pX
                            attackPosition.y = neighborChunk.pY
                            local squadMovementPenalty = lookupSquadMovementPenalty(squad, neighborChunk.cX, neighborChunk.cY)
                            local damageScore = surface.get_pollution(attackPosition) + neighborChunk[PLAYER_BASE_PHEROMONE] + neighborChunk[PLAYER_PHEROMONE] + neighborChunk[PLAYER_DEFENSE_GENERATOR]
                            local avoidScore = neighborChunk[DEATH_PHEROMONE] + neighborChunk[ENEMY_BASE_PHEROMONE]
                            local score = damageScore - avoidScore - squadMovementPenalty
                            if (score > attackScore) then
                                attackScore = score
                                attackChunk = neighborChunk
                                attackDirection = neighborDirectionChunk.d
                            end
                        end
                    end
                    if (attackChunk ~= nil) then
                        if ((attackChunk[PLAYER_BASE_GENERATOR] == 0) or (attackChunk[PLAYER_DEFENSE_GENERATOR] == 0)) or 
                            ((group.state == defines.group_state.finished) or (group.state == defines.group_state.gathering)) then
                            
                            attackPosition = positionDirectionToChunkCornerCardinal(attackDirection, attackChunk)
                            squad.cycles = 2
                            
                            group.set_command({type=defines.command.attack_area,
                                               destination=attackPosition,
                                               radius=16,
                                               distraction=defines.distraction.by_anything})
                            group.start_moving()
                        end
                    end
                end
            end
        end
    end
end


function aiAttack.squadAttackPlayer(natives, players)
    local squads = natives.squads
    for i=1,#squads do
        local squad = squads[i]
        local group = squad.group
        if (group.valid) and (squad.status == SQUAD_GUARDING) then
            local closestPlayer
            local closestDistance = MAGIC_MAXIMUM_NUMBER
            for x=1,#players do
                local player = players[x]
                if (player ~= nil) and player.connected and (player.character ~= nil) and (player.character.surface.index == 1) then
                    local playerCharacter = player.character
                    local distance = findDistance(playerCharacter.position, group.position)
                    if (distance < closestDistance) then
                        closestPlayer = playerCharacter
                        closestDistance = distance
                    end
                end
            end
            if (closestDistance < 75) then
                local squadType = SQUAD_HUNTING
                if (math.random() < 0.10) then -- TODO add sliding scale based on number of members and evolution
                    squadType = SQUAD_SUICIDE_HUNT
                end
                squad.status = squadType
                group.set_command({type=defines.command.attack,
                                   target=closestPlayer})
            end
        end
    end
end

function aiAttack.squadBeginAttack(natives)
    local squads = natives.squads
    for i=1,#squads do
        local squad = squads[i]
        if (squad.status == SQUAD_GUARDING) and (math.random() < 0.70) then
            if (math.random() < 0.05) then
                squad.status = SQUAD_SUICIDE_RAID
            else
                squad.status = SQUAD_RAIDING
            end
        end
    end
end

return aiAttack