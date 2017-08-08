local aiDefense = {}

-- imports

local constants = require("Constants")
local mapUtils = require("MapUtils")
local unitGroupUtils = require("UnitGroupUtils")
local neighborUtils = require("NeighborUtils")
local movementUtils = require("MovementUtils")

-- constants

local RETREAT_GRAB_RADIUS = constants.RETREAT_GRAB_RADIUS

local PLAYER_BASE_GENERATOR = constants.PLAYER_BASE_GENERATOR
local MOVEMENT_PHEROMONE = constants.MOVEMENT_PHEROMONE
local PLAYER_PHEROMONE = constants.PLAYER_PHEROMONE
local BASE_PHEROMONE = constants.BASE_PHEROMONE

local HALF_CHUNK_SIZE = constants.HALF_CHUNK_SIZE

local SQUAD_RETREATING = constants.SQUAD_RETREATING

local RETREAT_FILTER = constants.RETREAT_FILTER

local RETREAT_TRIGGERED = constants.RETREAT_TRIGGERED

local INTERVAL_LOGIC = constants.INTERVAL_LOGIC

local NEST_COUNT = constants.NEST_COUNT
local WORM_COUNT = constants.WORM_COUNT

-- imported functions

local positionFromDirectionAndChunk = mapUtils.positionFromDirectionAndChunk
local getNeighborChunks = mapUtils.getNeighborChunks
local findNearBySquad = unitGroupUtils.findNearBySquad
local addMovementPenalty = movementUtils.addMovementPenalty
local createSquad = unitGroupUtils.createSquad
local membersToSquad = unitGroupUtils.membersToSquad
local scoreNeighborsForRetreat = neighborUtils.scoreNeighborsForRetreat
local findMovementPosition = movementUtils.findMovementPosition

-- module code

local function scoreRetreatLocation(neighborChunk)
    return -(neighborChunk[BASE_PHEROMONE] + -neighborChunk[MOVEMENT_PHEROMONE] + (neighborChunk[PLAYER_PHEROMONE] * 100) + (neighborChunk[PLAYER_BASE_GENERATOR] * 20))
end

function aiDefense.retreatUnits(chunk, position, squad, regionMap, surface, natives, tick)
    if (tick - chunk[RETREAT_TRIGGERED] > INTERVAL_LOGIC) and (chunk[NEST_COUNT] == 0) and (chunk[WORM_COUNT] == 0) then
	local performRetreat = false
	local enemiesToSquad = nil
	
	if not squad then
	    enemiesToSquad = surface.find_enemy_units(chunk, RETREAT_GRAB_RADIUS)
	    performRetreat = #enemiesToSquad > 0
	elseif squad.group.valid and (squad.status ~= SQUAD_RETREATING) and not squad.kamikaze then
	    performRetreat = #squad.group.members > 1
	end
	
	if performRetreat then
	    chunk[RETREAT_TRIGGERED] = tick
	    local exitPath,exitDirection  = scoreNeighborsForRetreat(chunk,
								     getNeighborChunks(regionMap,
										       chunk.cX,
										       chunk.cY),
								     scoreRetreatLocation)
	    if exitPath then
		local retreatPosition = findMovementPosition(surface,
							     positionFromDirectionAndChunk(exitDirection,
											   position,
											   regionMap.position,
											   0.98),
							     false)

		
		if not retreatPosition then
		    return
		end
		
		-- in order for units in a group attacking to retreat, we have to create a new group and give the command to join
		-- to each unit, this is the only way I have found to have snappy mid battle retreats even after 0.14.4
                
		local newSquad = findNearBySquad(natives, retreatPosition, HALF_CHUNK_SIZE, RETREAT_FILTER)

		if not newSquad and (#natives.squads < natives.maxSquads) then
		    newSquad = createSquad(retreatPosition, surface, natives)
		    newSquad.status = SQUAD_RETREATING
		    newSquad.cycles = 4
		end

		if newSquad then
		    if enemiesToSquad then
			membersToSquad(newSquad, enemiesToSquad, false)
		    else
			membersToSquad(newSquad, squad.group.members, true)
			newSquad.penalties = squad.penalties
			if squad.rabid then
			    newSquad.rabid = true
			end
		    end
		    addMovementPenalty(natives, newSquad, chunk.cX, chunk.cY)
		end
	    end
	end
    end
end

return aiDefense
