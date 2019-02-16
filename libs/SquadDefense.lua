if aiDefenseG then
    return aiDefenseG
end
local aiDefense = {}

-- imports

local constants = require("Constants")
local mapUtils = require("MapUtils")
local unitGroupUtils = require("UnitGroupUtils")
local movementUtils = require("MovementUtils")
local chunkPropetyUtils = require("ChunkPropertyUtils")

-- constants

-- local RETREAT_SPAWNER_GRAB_RADIUS = constants.RETREAT_SPAWNER_GRAB_RADIUS
local MOVEMENT_PHEROMONE = constants.MOVEMENT_PHEROMONE
local PLAYER_PHEROMONE = constants.PLAYER_PHEROMONE
local BASE_PHEROMONE = constants.BASE_PHEROMONE

local PLAYER_PHEROMONE_MULTIPLER = constants.PLAYER_PHEROMONE_MULTIPLER

local SQUAD_RETREATING = constants.SQUAD_RETREATING

local INTERVAL_RETREAT = constants.INTERVAL_RETREAT

local SENTINEL_IMPASSABLE_CHUNK = constants.SENTINEL_IMPASSABLE_CHUNK

-- imported functions

local mRandom = math.random

local addSquadToChunk = chunkPropetyUtils.addSquadToChunk

local calculateKamikazeThreshold = unitGroupUtils.calculateKamikazeThreshold

local positionFromDirectionAndChunk = mapUtils.positionFromDirectionAndChunk
local getNeighborChunks = mapUtils.getNeighborChunks
local findNearbySquadFiltered = unitGroupUtils.findNearbySquadFiltered
local addMovementPenalty = movementUtils.addMovementPenalty
local createSquad = unitGroupUtils.createSquad
local membersToSquad = unitGroupUtils.membersToSquad
local scoreNeighborsForRetreat = movementUtils.scoreNeighborsForRetreat
local findMovementPosition = movementUtils.findMovementPosition

local getRetreatTick = chunkPropetyUtils.getRetreatTick
local getPlayerBaseGenerator = chunkPropetyUtils.getPlayerBaseGenerator
local setRetreatTick = chunkPropetyUtils.setRetreatTick
local getEnemyStructureCount = chunkPropetyUtils.getEnemyStructureCount

-- module code

local function scoreRetreatLocation(map, neighborChunk)
    return -(neighborChunk[BASE_PHEROMONE] + neighborChunk[MOVEMENT_PHEROMONE] + -(neighborChunk[PLAYER_PHEROMONE] * PLAYER_PHEROMONE_MULTIPLER) + -(getPlayerBaseGenerator(map, neighborChunk)))
end

function aiDefense.retreatUnits(chunk, position, squad, map, surface, natives, tick, radius, artilleryBlast, force)
    if (tick - getRetreatTick(map, chunk) > INTERVAL_RETREAT) and ((getEnemyStructureCount(map, chunk) == 0) or artilleryBlast or force) then
	local performRetreat = false
	local enemiesToSquad = nil

	if not squad then
	    enemiesToSquad = surface.find_enemy_units(position, radius)
	    performRetreat = #enemiesToSquad > 0
	    if (mRandom() < calculateKamikazeThreshold(#enemiesToSquad, natives)) then
		setRetreatTick(map, chunk, tick)
		performRetreat = false
	    end
	elseif squad.group and squad.group.valid and (squad.status ~= SQUAD_RETREATING) and not squad.kamikaze then
	    performRetreat = #squad.group.members > 1
	end

	if performRetreat then
	    setRetreatTick(map, chunk, tick)
	    local exitPath,exitDirection  = scoreNeighborsForRetreat(chunk,
								     getNeighborChunks(map, chunk.x, chunk.y),
								     scoreRetreatLocation,
								     map)
	    if (exitPath ~= SENTINEL_IMPASSABLE_CHUNK) then
		local retreatPosition = findMovementPosition(surface,
							     positionFromDirectionAndChunk(exitDirection, position, map.position, 0.98),
							     false)

		if not retreatPosition then
		    return
		end

		-- in order for units in a group attacking to retreat, we have to create a new group and give the command to join
		-- to each unit, this is the only way I have found to have snappy mid battle retreats even after 0.14.4

		local newSquad = findNearbySquadFiltered(map, exitPath, retreatPosition)

		if not newSquad then
		    newSquad = createSquad(retreatPosition, surface, natives)
		    newSquad.status = SQUAD_RETREATING
		    newSquad.cycles = 4
		end

		if newSquad then
		    local cmd = map.retreatCommand
		    cmd.group = newSquad.group
		    if enemiesToSquad then
			membersToSquad(cmd, enemiesToSquad, artilleryBlast)
		    else
			membersToSquad(cmd, squad.group.members, true)
			newSquad.penalties = squad.penalties
			if squad.rabid then
			    newSquad.rabid = true
			end
		    end
		    addSquadToChunk(map, chunk, newSquad)
		    addMovementPenalty(natives, newSquad, chunk)
		end
	    end
	end
    end
end

aiDefenseG = aiDefense
return aiDefense
