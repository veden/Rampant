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

function aiAttack.squadAttack(regionMap, surface, natives, temps)
    local squads = natives.squads
    local attackPosition = temps[constants.ATTACK_POSITION]
    local cmd = temps[constants.ATTACK_COMMAND]
    local neighborsWithDirection = temps[constants.ATTACK_DIRECTION]
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
            if (group.state == defines.group_state.finished) or (group.state == defines.group_state.gathering) then
                local chunk = getChunkByPosition(regionMap, group.position.x, group.position.y)
                if (chunk ~= nil) then
                    addSquadMovementPenalty(squad, chunk.cX, chunk.cY)
                    getCardinalChunksWithDirection(regionMap, chunk.cX, chunk.cY, neighborsWithDirection)
                    local attackChunk, attackDirection = scoreNeighborsWithDirection(chunk,
                                                                                     neighborsWithDirection,
                                                                                     validLocation,
                                                                                     scoreLocation,
                                                                                     squad,
                                                                                     surface,
                                                                                     attackPosition)
                    if (attackChunk ~= nil) then
                        if ((attackChunk[PLAYER_BASE_GENERATOR] == 0) and (attackChunk[PLAYER_DEFENSE_GENERATOR] == 0)) or
                            ((group.state == defines.group_state.finished) or (group.state == defines.group_state.gathering)) then
                            
                            positionFromDirectionAndChunkCardinal(attackDirection, attackChunk, attackPosition)
                            
                            group.set_command(cmd)
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
    for i=1,#squads do
        local squad = squads[i]
        if (squad.status == SQUAD_GUARDING) and squad.group.valid then
            local threshold = 0.05 + (evolution_factor * 0.20) + (#squad.group.members * 0.0033)
            
            -- check to hunt player
            if (math.random() < 0.30) and playersWithinProximityToPosition(players, squad.group.position, 100) then
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
