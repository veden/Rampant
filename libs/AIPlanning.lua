if aiPlanningG then
    return aiPlanningG
end
local aiPlanning = {}

-- imports

local constants = require("Constants")
local mathUtils = require("MathUtils")
-- local aiPredicates = require("AIPredicates")

-- constants

local NO_RETREAT_BASE_PERCENT = constants.NO_RETREAT_BASE_PERCENT
local NO_RETREAT_EVOLUTION_BONUS_MAX = constants.NO_RETREAT_EVOLUTION_BONUS_MAX

local AI_STATE_PEACEFUL = constants.AI_STATE_PEACEFUL
local AI_STATE_AGGRESSIVE = constants.AI_STATE_AGGRESSIVE
local AI_STATE_RAIDING = constants.AI_STATE_RAIDING
local AI_STATE_MIGRATING = constants.AI_STATE_MIGRATING
local AI_STATE_ONSLAUGHT = constants.AI_STATE_ONSLAUGHT
local AI_STATE_SIEGE = constants.AI_STATE_SIEGE

local AGGRESSIVE_CAN_ATTACK_WAIT_MAX_DURATION = constants.AGGRESSIVE_CAN_ATTACK_WAIT_MAX_DURATION
local AGGRESSIVE_CAN_ATTACK_WAIT_MIN_DURATION = constants.AGGRESSIVE_CAN_ATTACK_WAIT_MIN_DURATION

local AI_UNIT_REFUND = constants.AI_UNIT_REFUND

local AI_MAX_OVERFLOW_POINTS = constants.AI_MAX_OVERFLOW_POINTS

local AI_MAX_POINTS = constants.AI_MAX_POINTS
local AI_POINT_GENERATOR_AMOUNT = constants.AI_POINT_GENERATOR_AMOUNT

local AI_MIN_STATE_DURATION = constants.AI_MIN_STATE_DURATION
local AI_MIN_TEMPERAMENT_DURATION = constants.AI_MIN_TEMPERAMENT_DURATION
local AI_MAX_STATE_DURATION = constants.AI_MAX_STATE_DURATION
local AI_MAX_TEMPERAMENT_DURATION = constants.AI_MAX_TEMPERAMENT_DURATION

local BASE_RALLY_CHANCE = constants.BASE_RALLY_CHANCE
local BONUS_RALLY_CHANCE = constants.BONUS_RALLY_CHANCE

local RETREAT_MOVEMENT_PHEROMONE_LEVEL_MIN = constants.RETREAT_MOVEMENT_PHEROMONE_LEVEL_MIN
local RETREAT_MOVEMENT_PHEROMONE_LEVEL_MAX = constants.RETREAT_MOVEMENT_PHEROMONE_LEVEL_MAX

-- imported functions

local randomTickEvent = mathUtils.randomTickEvent

local linearInterpolation = mathUtils.linearInterpolation

local mFloor = math.floor

local mRandom = math.random

local mMax = math.max

-- module code

function aiPlanning.planning(natives, evolution_factor, tick)
    local maxPoints = AI_MAX_POINTS * evolution_factor

    if natives.aiNocturnalMode then
	maxPoints = maxPoints * 0.85
    end

    local attackWaveMaxSize = natives.attackWaveMaxSize
    natives.retreatThreshold = linearInterpolation(evolution_factor, RETREAT_MOVEMENT_PHEROMONE_LEVEL_MIN, RETREAT_MOVEMENT_PHEROMONE_LEVEL_MAX)
    natives.rallyThreshold = BASE_RALLY_CHANCE + (evolution_factor * BONUS_RALLY_CHANCE)
    natives.formSquadThreshold = mMax((0.25 * evolution_factor), 0.10)

    natives.attackWaveSize = attackWaveMaxSize * (evolution_factor ^ 1.66667)
    natives.attackWaveDeviation = (attackWaveMaxSize * 0.5) * 0.333
    natives.attackWaveUpperBound = attackWaveMaxSize + (attackWaveMaxSize * 0.25)

    natives.settlerWaveSize = linearInterpolation(evolution_factor ^ 1.66667, natives.expansionMinSize, natives.expansionMaxSize)
    natives.settlerWaveDeviation = (natives.settlerWaveSize * 0.33)

    natives.settlerCooldown = mFloor(linearInterpolation(evolution_factor ^ 1.66667, natives.expansionMaxTime, natives.expansionMinTime))

    natives.unitRefundAmount = AI_UNIT_REFUND * evolution_factor
    natives.kamikazeThreshold = NO_RETREAT_BASE_PERCENT + (evolution_factor * NO_RETREAT_EVOLUTION_BONUS_MAX)

    local points = mFloor((AI_POINT_GENERATOR_AMOUNT * mRandom()) + ((AI_POINT_GENERATOR_AMOUNT * 0.7) * (evolution_factor ^ 2.5)) * natives.aiPointsScaler)

    if (natives.state == AI_STATE_ONSLAUGHT) then
        points = points * 2
    end

    natives.baseIncrement = points

    if (natives.points < maxPoints) then
	natives.points = natives.points + points
    end

    if (natives.temperamentTick <= tick) then
	natives.temperament = mRandom()
	natives.temperamentTick = randomTickEvent(tick, AI_MIN_TEMPERAMENT_DURATION, AI_MAX_TEMPERAMENT_DURATION)
    end

    if (natives.stateTick <= tick) then
	local roll = mRandom() * mMax(1 - evolution_factor, 0.15) * natives.aiAggressiveness
	if (roll > natives.temperament) then
	    natives.state = AI_STATE_PEACEFUL
	else
	    roll = mRandom()
	    if (roll < 0.65) then
	    	natives.state = AI_STATE_AGGRESSIVE
                natives.canAttackTick = randomTickEvent(tick,
                                                        AGGRESSIVE_CAN_ATTACK_WAIT_MIN_DURATION,
                                                        AGGRESSIVE_CAN_ATTACK_WAIT_MAX_DURATION)
	    elseif ((natives.enabledMigration) and (natives.expansion) and (roll < 0.75)) then
		natives.state = AI_STATE_MIGRATING
	    elseif ((natives.siegeAIToggle) and (natives.expansion) and (roll < 0.80)) then
		natives.state = AI_STATE_SIEGE
            elseif ((natives.onslaughtAIToggle) and (roll < 0.85)) then
		natives.state = AI_STATE_ONSLAUGHT
	    elseif ((natives.raidAIToggle) and (evolution_factor >= 0.04)) then
		natives.state = AI_STATE_RAIDING

		natives.points = natives.points + 1000
		if (natives.points > AI_MAX_OVERFLOW_POINTS) then
		    natives.points = AI_MAX_OVERFLOW_POINTS
		end
	    else
		natives.state = AI_STATE_AGGRESSIVE
	    end
	end
	natives.stateTick = randomTickEvent(tick, AI_MIN_STATE_DURATION, AI_MAX_STATE_DURATION)
    end

end

aiPlanningG = aiPlanning
return aiPlanning
