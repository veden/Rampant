local aiPlanning = {}

-- imports

local constants = require("Constants")
local mathUtils = require("MathUtils")
local nocturnalUtils = require("NocturnalUtils")

-- constants

local AI_STATE_PEACEFUL = constants.AI_STATE_PEACEFUL
local AI_STATE_AGGRESSIVE = constants.AI_STATE_AGGRESSIVE
local AI_STATE_NOCTURNAL = constants.AI_STATE_NOCTURNAL

local AI_MAX_POINTS = constants.AI_MAX_POINTS
local AI_POINT_GENERATOR_AMOUNT = constants.AI_POINT_GENERATOR_AMOUNT

local AI_MIN_STATE_DURATION = constants.AI_MIN_STATE_DURATION
local AI_MIN_TEMPERAMENT_DURATION = constants.AI_MIN_TEMPERAMENT_DURATION
local AI_MAX_STATE_DURATION = constants.AI_MAX_STATE_DURATION
local AI_MAX_TEMPERAMENT_DURATION = constants.AI_MAX_TEMPERAMENT_DURATION

local AI_MAX_SQUAD_COUNT = constants.AI_MAX_SQUAD_COUNT

local TICKS_A_MINUTE = constants.TICKS_A_MINUTE

-- imported functions

local canAttackNocturnal = nocturnalUtils.canAttack

local randomTickEvent = mathUtils.randomTickEvent

local mMax = math.max

-- module code

function aiPlanning.planning(natives, evolution_factor, tick, surface)
    local maxPoints = AI_MAX_POINTS * evolution_factor
    if natives.aiNocturnalMode then
	maxPoints = maxPoints * 0.85
    end
    if (natives.points < maxPoints) then
	natives.points = natives.points + math.floor((AI_POINT_GENERATOR_AMOUNT * math.random()) + ((AI_POINT_GENERATOR_AMOUNT * 0.7) * (evolution_factor ^ 2.5)) * natives.aiPointsScaler)
    end
    
    if (natives.temperamentTick == tick) then
	natives.temperament = math.random()
	natives.temperamentTick = randomTickEvent(tick, AI_MIN_TEMPERAMENT_DURATION, AI_MAX_TEMPERAMENT_DURATION)
    end

    if (natives.stateTick == tick) then
	local roll = math.random() * mMax(1 - evolution_factor, 0.15)
	if (roll > natives.temperament) then
	    natives.state = AI_STATE_PEACEFUL
	elseif (natives.aiNocturnalMode) then
	    natives.state = AI_STATE_NOCTURNAL
	else
	    natives.state = AI_STATE_AGGRESSIVE
	end
	natives.stateTick = randomTickEvent(tick, AI_MIN_STATE_DURATION, AI_MAX_STATE_DURATION)
    end

    if ((natives.state == AI_STATE_AGGRESSIVE) or canAttackNocturnal(natives, surface)) and (tick - natives.lastShakeMessage > TICKS_A_MINUTE * 5) and ((evolution_factor > 0.7) and (natives.points > maxPoints * 0.85) and (#natives.squads > AI_MAX_SQUAD_COUNT * 0.35)) then
	natives.lastShakeMessage = tick
	surface.print("Rampant: The ground begins to shake")
    end
end

return aiPlanning
