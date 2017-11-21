local aiDefense = {}

-- imports

local constants = require("Constants")
local mapUtils = require("MapUtils")
local unitGroupUtils = require("UnitGroupUtils")
local movementUtils = require("MovementUtils")
local chunkUtils = require("ChunkUtils")

-- constants

local RETREAT_GRAB_RADIUS = constants.RETREAT_GRAB_RADIUS

local MOVEMENT_PHEROMONE = constants.MOVEMENT_PHEROMONE
local PLAYER_PHEROMONE = constants.PLAYER_PHEROMONE
local BASE_PHEROMONE = constants.BASE_PHEROMONE

local HALF_CHUNK_SIZE = constants.HALF_CHUNK_SIZE

local SQUAD_RETREATING = constants.SQUAD_RETREATING

local RETREAT_FILTER = constants.RETREAT_FILTER

local INTERVAL_LOGIC = constants.INTERVAL_LOGIC

local SENTINEL_IMPASSABLE_CHUNK = constants.SENTINEL_IMPASSABLE_CHUNK

-- imported functions

local positionFromDirectionAndChunk = mapUtils.positionFromDirectionAndChunk
local getNeighborChunks = mapUtils.getNeighborChunks
local findNearBySquad = unitGroupUtils.findNearBySquad
local addMovementPenalty = movementUtils.addMovementPenalty
local createSquad = unitGroupUtils.createSquad
local membersToSquad = unitGroupUtils.membersToSquad
local scoreNeighborsForRetreat = movementUtils.scoreNeighborsForRetreat
local findMovementPosition = movementUtils.findMovementPosition

local getRetreatTick = chunkUtils.getRetreatTick
local getPlayerBaseGenerator = chunkUtils.getPlayerBaseGenerator
local setRetreatTick = chunkUtils.setRetreatTick
local getNestCount = chunkUtils.getNestCount
local getWormCount = chunkUtils.getWormCount

-- module code

local function scoreRetreatLocation(regionMap, neighborChunk)
    return -(neighborChunk[BASE_PHEROMONE] + -neighborChunk[MOVEMENT_PHEROMONE] + (neighborChunk[PLAYER_PHEROMONE] * 100) + (getPlayerBaseGenerator(regionMap, neighborChunk) * 20))
end

function aiDefense.retreatUnits(chunk, position, squad, regionMap, surface, natives, tick)
    if (tick - getRetreatTick(regionMap, chunk) > INTERVAL_LOGIC) and (getNestCount(regionMap, chunk) == 0) and (getWormCount(regionMap, chunk) == 0) then
	local performRetreat = false
	local enemiesToSquad = nil
	
	if not squad then
	    enemiesToSquad = surface.find_enemy_units(position, RETREAT_GRAB_RADIUS)
	    performRetreat = #enemiesToSquad > 0
	elseif squad.group.valid and (squad.status ~= SQUAD_RETREATING) and not squad.kamikaze then
	    performRetreat = #squad.group.members > 1
	end
	
	if performRetreat then
	    setRetreatTick(regionMap, chunk, tick)
	    local exitPath,exitDirection  = scoreNeighborsForRetreat(chunk,
								     getNeighborChunks(regionMap, chunk.x, chunk.y),
								     scoreRetreatLocation,
								     regionMap)
	    if (exitPath ~= SENTINEL_IMPASSABLE_CHUNK) then
		local retreatPosition = findMovementPosition(surface,
							     positionFromDirectionAndChunk(exitDirection, position, regionMap.position, 0.98),
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
		    addMovementPenalty(natives, newSquad, chunk.x, chunk.y)
		end
	    end
	end
    end
end

return aiDefense
