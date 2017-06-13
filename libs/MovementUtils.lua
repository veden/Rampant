local movementUtils = {}

-- imports

local constants = require("Constants")
local unitGroupUtils = require("UnitGroupUtils")

local mathUtils = require("MathUtils")

-- constants

local MOVEMENT_PHEROMONE_GENERATOR_AMOUNT = constants.MOVEMENT_PHEROMONE_GENERATOR_AMOUNT
local MAX_PENALTY_BEFORE_PURGE = constants.MAX_PENALTY_BEFORE_PURGE

-- imported functions

local recycleBiters = unitGroupUtils.recycleBiters

local tableRemove = table.remove
local tableInsert = table.insert

local distortPosition = mathUtils.distortPosition

-- module code

function movementUtils.findMovementPosition(surface, position, distort)
    local pos = position
    if not surface.can_place_entity({name="behemoth-biter", position=position}) then
	pos = surface.find_non_colliding_position("behemoth-biter", position, 5, 2) or position
    end
    return (distort and distortPosition(pos)) or pos
end

function movementUtils.addMovementPenalty(natives, units, chunkX, chunkY)
    local penalties = units.penalties
    for i=1,#penalties do
        local penalty = penalties[i]
        if (penalty.x == chunkX) and (penalty.y == chunkY) then
            penalty.v = penalty.v + MOVEMENT_PHEROMONE_GENERATOR_AMOUNT
	    if (penalty.v > MAX_PENALTY_BEFORE_PURGE) then
		local group = units.group
		if group then
		    recycleBiters(natives, group.members)
		    group.destroy()
		else
		    units.unit.destroy()
		end
	    end
            return
        end
    end
    if (#penalties == 7) then
        tableRemove(penalties, 7)
    end
    tableInsert(penalties, 1, { v = MOVEMENT_PHEROMONE_GENERATOR_AMOUNT,
                                x = chunkX,
                                y = chunkY })
end

function movementUtils.lookupMovementPenalty(squad, chunkX, chunkY)
    local penalties = squad.penalties
    for i=1,#penalties do
        local penalty = penalties[i]
        if (penalty.x == chunkX) and (penalty.y == chunkY) then
            return penalty.v
        end
    end
    return 0
end


return movementUtils
