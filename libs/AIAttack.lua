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
local PLAYER_DEFENSE_PHEROMONE = constants.PLAYER_DEFENSE_PHEROMONE

local SQUAD_RAIDING = constants.SQUAD_RAIDING
local SQUAD_SUICIDE_RAID = constants.SQUAD_SUICIDE_RAID
local SQUAD_HUNTING = constants.SQUAD_HUNTING
local SQUAD_GUARDING = constants.SQUAD_GUARDING
local SQUAD_SUICIDE_HUNT = constants.SQUAD_SUICIDE_HUNT

local ENEMY_BASE_GENERATOR = constants.ENEMY_BASE_GENERATOR
local MAGIC_MAXIMUM_NUMBER = constants.MAGIC_MAXIMUM_NUMBER

local HALF_CHUNK_SIZE = constants.HALF_CHUNK_SIZE
local CHUNK_SIZE = constants.CHUNK_SIZE

local PLAYER_BASE_GENERATOR = constants.PLAYER_BASE_GENERATOR
local PLAYER_DEFENSE_GENERATOR = constants.PLAYER_DEFENSE_GENERATOR

-- imported functions

local getCardinalChunksWithDirection = mapUtils.getCardinalChunksWithDirection
local getChunkByPosition = mapUtils.getChunkByPosition
local canMoveChunkDirectionCardinal = mapUtils.canMoveChunkDirectionCardinal
local addSquadMovementPenalty = unitGroupUtils.addSquadMovementPenalty
local lookupSquadMovementPenalty = unitGroupUtils.lookupSquadMovementPenalty
local positionFromDirectionAndChunkCardinal = mapUtils.positionFromDirectionAndChunkCardinal

local playersWithinProximityToPosition = playerUtils.playersWithinProximityToPosition

local scoreNeighborsWithDirection = neighborUtils.scoreNeighborsWithDirection

local mMax = math.max

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

function aiAttack.squadAttackLocation(regionMap, surface, natives)
    local squads = natives.squads
    for i=1,#squads do
        local squad = squads[i]
        local group = squad.group
        if group.valid and ((squad.status == SQUAD_RAIDING) or (squad.status == SQUAD_SUICIDE_RAID)) then 
            if (group.state == defines.group_state.finished) or (group.state == defines.group_state.gathering) then
                local chunk = getChunkByPosition(regionMap, group.position.x, group.position.y)
                if (chunk ~= nil) then
                    addSquadMovementPenalty(squad, chunk.cX, chunk.cY)
                    local attackChunk, attackDirection = scoreNeighborsWithDirection(chunk,
                                                                                     getCardinalChunksWithDirection(regionMap, chunk.cX, chunk.cY),
                                                                                     validLocation,
                                                                                     scoreAttackLocation,
                                                                                     squad,
                                                                                     surface)
                    if (attackChunk ~= nil) then
                        if ((attackChunk[PLAYER_BASE_GENERATOR] == 0) and (attackChunk[PLAYER_DEFENSE_GENERATOR] == 0)) or
                            ((group.state == defines.group_state.finished) or (group.state == defines.group_state.gathering)) then
                            
                            attackPosition = positionFromDirectionAndChunkCardinal(attackDirection, attackChunk)
                            
                            group.set_command({type=defines.command.attack_area,
                                               destination=attackPosition,
                                               radius=32,
                                               distraction=defines.distraction.by_anything})
                            group.start_moving()
                        end
                    end
                end
            end
        end
    end
end

function aiAttack.squadAttackPlayer(regionMap, surface, natives)
    local squads = natives.squads
    for i=1,#squads do
        local squad = squads[i]
        local group = squad.group
        if (group.valid) and ((squad.status == SQUAD_HUNTING) or (squad.status == SQUAD_SUICIDE_HUNT)) then
            if (group.state == defines.group_state.finished) or (group.state == defines.group_state.gathering) then
                local chunk = getChunkByPosition(regionMap, group.position.x, group.position.y)
                if (chunk ~= nil) then
                    addSquadMovementPenalty(squad, chunk.cX, chunk.cY)
                    local attackChunk, attackDirection = scoreNeighborsWithDirection(chunk,
                                                                                     getCardinalChunksWithDirection(regionMap, chunk.cX, chunk.cY),
                                                                                     validLocation,
                                                                                     scoreHuntPlayerLocation,
                                                                                     squad,
                                                                                     surface)
                    if (attackChunk ~= nil) then
                        if ((attackChunk[PLAYER_BASE_GENERATOR] == 0) and (attackChunk[PLAYER_DEFENSE_GENERATOR] == 0)) or
                            ((group.state == defines.group_state.finished) or (group.state == defines.group_state.gathering)) then
                            
                            attackPosition = positionFromDirectionAndChunkCardinal(attackDirection, attackChunk)
                            
                            group.set_command({type=defines.command.attack_area,
                                               destination=attackPosition,
                                               radius=32,
                                               distraction=defines.distraction.by_anything})
                            group.start_moving()
                        end
                    end
                end
            end
        end
    end
end

function aiAttack.squadBeginAttack(natives, players, evolution_factor)
    local squads = natives.squads
    local threshold = 0.05 + (evolution_factor * 0.20)
    local groupSizeThreshold = mMax((evolution_factor * constants.AI_MAX_SQUAD_SIZE) + 1, 20)
    for i=1,#squads do
        local squad = squads[i]
        if (squad.status == SQUAD_GUARDING) and squad.group.valid then
            -- check to hunt player
            if (math.random() < 0.30) and playersWithinProximityToPosition(players, squad.group.position, 100) then
                print("player hunt")
                if (math.random() < threshold) or (groupSizeThreshold <= #squad.group.members) then
                    squad.status = SQUAD_SUICIDE_HUNT
                else
                    squad.status = SQUAD_HUNTING
                end
            end
            
            -- check to raid base
            if (squad.status == SQUAD_GUARDING) and (math.random() < 0.70) then
                print("raid")
                if (math.random() < threshold) or (groupSizeThreshold <= #squad.group.members) then
                    squad.status = SQUAD_SUICIDE_RAID
                else
                    squad.status = SQUAD_RAIDING
                end
            end
        end
    end
end

return aiAttack