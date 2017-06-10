local aiPlanning = {}

-- imports

local constants = require("Constants")
local mathUtils = require("MathUtils")
local aiPredicates = require("AIPredicates")

-- constants

local NO_RETREAT_BASE_PERCENT = constants.NO_RETREAT_BASE_PERCENT
local NO_RETREAT_EVOLUTION_BONUS_MAX = constants.NO_RETREAT_EVOLUTION_BONUS_MAX

local AI_STATE_PEACEFUL = constants.AI_STATE_PEACEFUL
local AI_STATE_AGGRESSIVE = constants.AI_STATE_AGGRESSIVE
local AI_STATE_NOCTURNAL = constants.AI_STATE_NOCTURNAL

local AI_UNIT_REFUND = constants.AI_UNIT_REFUND

local AI_MAX_POINTS = constants.AI_MAX_POINTS
local AI_POINT_GENERATOR_AMOUNT = constants.AI_POINT_GENERATOR_AMOUNT

local AI_MIN_STATE_DURATION = constants.AI_MIN_STATE_DURATION
local AI_MIN_TEMPERAMENT_DURATION = constants.AI_MIN_TEMPERAMENT_DURATION
local AI_MAX_STATE_DURATION = constants.AI_MAX_STATE_DURATION
local AI_MAX_TEMPERAMENT_DURATION = constants.AI_MAX_TEMPERAMENT_DURATION

local AI_MAX_SQUAD_COUNT = constants.AI_MAX_SQUAD_COUNT

local BASE_RALLY_CHANCE = constants.BASE_RALLY_CHANCE
local BONUS_RALLY_CHANCE = constants.BONUS_RALLY_CHANCE

local RETREAT_MOVEMENT_PHEROMONE_LEVEL = constants.RETREAT_MOVEMENT_PHEROMONE_LEVEL

local TICKS_A_MINUTE = constants.TICKS_A_MINUTE

-- imported functions

local canAttack = aiPredicates.canAttack

local randomTickEvent = mathUtils.randomTickEvent

local mMax = math.max

-- module code

local function isShockwaveReady(evolution_factor, natives, surface, tick, maxPoints)
    return canAttack(natives, surface) and
	(tick - natives.lastShakeMessage > TICKS_A_MINUTE * 5) and
	((evolution_factor > 0.7) and
		(natives.points > maxPoints * 0.85) and
		(#natives.squads > AI_MAX_SQUAD_COUNT * 0.45))
end

function aiPlanning.planning(natives, evolution_factor, tick, surface)
    local maxPoints = AI_MAX_POINTS * evolution_factor

    if natives.aiNocturnalMode then
	maxPoints = maxPoints * 0.85
    end

    local attackWaveMaxSize = natives.attackWaveMaxSize
    natives.retreatThreshold = -(evolution_factor * RETREAT_MOVEMENT_PHEROMONE_LEVEL)
    natives.maxSquads = AI_MAX_SQUAD_COUNT * evolution_factor
    natives.rallyThreshold = BASE_RALLY_CHANCE + (evolution_factor * BONUS_RALLY_CHANCE)
    natives.formSquadThreshold = mMax((0.25 * evolution_factor), 0.10)
    natives.attackWaveSize = attackWaveMaxSize * (evolution_factor ^ 1.66667)
    natives.attackWaveDeviation = (attackWaveMaxSize * 0.5) * 0.333
    natives.attackWaveLowerBound = 1
    natives.attackWaveUpperBound = attackWaveMaxSize + (attackWaveMaxSize * 0.25)
    natives.unitRefundAmount = AI_UNIT_REFUND * evolution_factor
    natives.kamikazeThreshold = NO_RETREAT_BASE_PERCENT + (evolution_factor * NO_RETREAT_EVOLUTION_BONUS_MAX)
    local threshold = natives.attackThresholdRange
    natives.attackWaveThreshold = (threshold - (threshold * evolution_factor)) + natives.attackThresholdMin
    
    if (natives.points < maxPoints) then
	natives.points = natives.points + math.floor((AI_POINT_GENERATOR_AMOUNT * math.random()) +
		((AI_POINT_GENERATOR_AMOUNT * 0.7) * (evolution_factor ^ 2.5)) * natives.aiPointsScaler)
    end
    
    if (natives.temperamentTick == tick) then
	natives.temperament = math.random()
	natives.temperamentTick = randomTickEvent(tick, AI_MIN_TEMPERAMENT_DURATION, AI_MAX_TEMPERAMENT_DURATION)
    end

    if (natives.stateTick == tick) then
	local roll = math.random() * mMax(1 - evolution_factor, 0.15)
	if (roll > natives.temperament) then
	    natives.state = AI_STATE_PEACEFUL
	elseif natives.aiNocturnalMode then
	    natives.state = AI_STATE_NOCTURNAL
	else
	    natives.state = AI_STATE_AGGRESSIVE
	end
	natives.stateTick = randomTickEvent(tick, AI_MIN_STATE_DURATION, AI_MAX_STATE_DURATION)
    end

    if isShockwaveReady(evolution_factor, natives, surface, tick, maxPoints) then
	natives.lastShakeMessage = tick
	surface.print("Rampant: The ground begins to shake")
    end
    
end

return aiPlanning
