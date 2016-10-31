local aiDefense = {}

-- imports

local constants = require("Constants")
local mapUtils = require("MapUtils")
local unitGroupUtils = require("UnitGroupUtils")
local neighborUtils = require("NeighborUtils")

-- constants

local MOVEMENT_PHEROMONE = constants.MOVEMENT_PHEROMONE
local PLAYER_PHEROMONE = constants.PLAYER_PHEROMONE
local BASE_PHEROMONE = constants.BASE_PHEROMONE

local RETREAT_MOVEMENT_PHEROMONE_LEVEL = constants.RETREAT_MOVEMENT_PHEROMONE_LEVEL

local ENEMY_BASE_GENERATOR = constants.ENEMY_BASE_GENERATOR

local HALF_CHUNK_SIZE = constants.HALF_CHUNK_SIZE

local SQUAD_RETREATING = constants.SQUAD_RETREATING
local SQUAD_SUICIDE_HUNT = constants.SQUAD_SUICIDE_HUNT
local SQUAD_SUICIDE_RAID = constants.SQUAD_SUICIDE_RAID

local RETREAT_FILTER = constants.RETREAT_FILTER

-- imported functions

local getChunkByPosition = mapUtils.getChunkByPosition
local getNeighborChunksWithDirection = mapUtils.getNeighborChunksWithDirection
local findNearBySquad = unitGroupUtils.findNearBySquad
local addSquadMovementPenalty = unitGroupUtils.addSquadMovementPenalty
local createSquad = unitGroupUtils.createSquad
local membersToSquad = unitGroupUtils.membersToSquad
local scoreNeighborsWithDirection = neighborUtils.scoreNeighborsWithDirection
local canMoveChunkDirection = mapUtils.canMoveChunkDirection

-- module code

local function validRetreatLocation(x, chunk, neighborChunk)
    return canMoveChunkDirection(x, chunk, neighborChunk)
end

local function scoreRetreatLocation(position, squad, neighborChunk, surface)
    local safeScore = -neighborChunk[BASE_PHEROMONE] + neighborChunk[MOVEMENT_PHEROMONE]
    local dangerScore = surface.get_pollution(position) + (neighborChunk[PLAYER_PHEROMONE] * 25) + (neighborChunk[ENEMY_BASE_GENERATOR] * 6)
    return safeScore - dangerScore
end

function aiDefense.retreatUnits(position, squad, regionMap, surface, natives)
    local chunk = getChunkByPosition(regionMap, position.x, position.y)
    if (chunk ~= nil) and (chunk[MOVEMENT_PHEROMONE] > -(game.evolution_factor * RETREAT_MOVEMENT_PHEROMONE_LEVEL)) then
        local performRetreat = false
        local enemiesToSquad
    
        if (squad == nil) then
            enemiesToSquad = surface.find_enemy_units(position, 15)
            if (#enemiesToSquad > 1) then
                performRetreat = true
            end
        elseif squad.group.valid and (squad.status ~= SQUAD_RETREATING) and (squad.status ~= SQUAD_SUICIDE_HUNT) and (squad.status ~= SQUAD_SUICIDE_RAID) then
            if (#squad.group.members > 1) then
                performRetreat = true
            end
        end
                
        if performRetreat then
	    local retreatPosition = {x=0, y=0}
            local exitPath,_  = scoreNeighborsWithDirection(chunk,
							    getNeighborChunksWithDirection(regionMap, chunk.cX, chunk.cY),
							    validRetreatLocation,
							    scoreRetreatLocation,
							    nil,
							    surface,
							    retreatPosition)
            if (exitPath ~= nil) then
                -- retreatPosition = positionDirectionToChunkCorner(exitDirection, exitPath)
                
                retreatPosition.x = exitPath.pX + HALF_CHUNK_SIZE
                retreatPosition.y = exitPath.pY + HALF_CHUNK_SIZE
                
                -- in order for units in a group attacking to retreat, we have to create a new group and give the command to join
                -- to each unit, this is the only way I have found to have snappy mid battle retreats even after 0.14.4
                
                local newSquad = findNearBySquad(natives, retreatPosition, HALF_CHUNK_SIZE, RETREAT_FILTER)
                
                if (newSquad == nil) then
                    newSquad = createSquad(retreatPosition, surface, natives)
                    newSquad.status = SQUAD_RETREATING
                    newSquad.cycles = 6
                end
		
                if (enemiesToSquad ~= nil) then
                    membersToSquad(newSquad, enemiesToSquad, false)
                else
                    membersToSquad(newSquad, squad.group.members, true)
		    newSquad.penalties = squad.penalties
		    if squad.rabid then
			newSquad.rabid = true
		    end
                end
		addSquadMovementPenalty(newSquad, chunk.cX, chunk.cY)
            end
        end
    end
end

return aiDefense
