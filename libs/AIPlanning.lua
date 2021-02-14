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

local AI_MAX_POINTS = constants.AI_MAX_POINTS
local AI_POINT_GENERATOR_AMOUNT = constants.AI_POINT_GENERATOR_AMOUNT

local AI_MIN_STATE_DURATION = constants.AI_MIN_STATE_DURATION
local AI_MAX_STATE_DURATION = constants.AI_MAX_STATE_DURATION

local BASE_RALLY_CHANCE = constants.BASE_RALLY_CHANCE
local BONUS_RALLY_CHANCE = constants.BONUS_RALLY_CHANCE

local RETREAT_MOVEMENT_PHEROMONE_LEVEL_MIN = constants.RETREAT_MOVEMENT_PHEROMONE_LEVEL_MIN
local RETREAT_MOVEMENT_PHEROMONE_LEVEL_MAX = constants.RETREAT_MOVEMENT_PHEROMONE_LEVEL_MAX
local MINIMUM_AI_POINTS = constants.MINIMUM_AI_POINTS

-- imported functions

local randomTickEvent = mathUtils.randomTickEvent

local linearInterpolation = mathUtils.linearInterpolation

local mFloor = math.floor

local mRandom = math.random

local mMax = math.max
local mMin = math.min

-- module code

function aiPlanning.planning(natives, evolution_factor, tick)
    natives.evolutionLevel = evolution_factor

    local maxPoints = mMax(AI_MAX_POINTS * evolution_factor, MINIMUM_AI_POINTS)

    if not natives.ranIncompatibleMessage and natives.newEnemies and
        (game.active_mods["bobenemies"] or game.active_mods["Natural_Evolution_Enemies"]) then
        natives.ranIncompatibleMessage = true
        game.print({"description.rampant-bobs-nee-newEnemies"})
    end

    local maxOverflowPoints = maxPoints * 3

    local attackWaveMaxSize = natives.attackWaveMaxSize
    natives.retreatThreshold = linearInterpolation(evolution_factor,
                                                   RETREAT_MOVEMENT_PHEROMONE_LEVEL_MIN,
                                                   RETREAT_MOVEMENT_PHEROMONE_LEVEL_MAX)
    natives.rallyThreshold = BASE_RALLY_CHANCE + (evolution_factor * BONUS_RALLY_CHANCE)
    natives.formSquadThreshold = mMax((0.20 * evolution_factor), 0.05)

    natives.attackWaveSize = attackWaveMaxSize * (evolution_factor ^ 1.4)
    natives.attackWaveDeviation = (natives.attackWaveSize * 0.333)
    natives.attackWaveUpperBound = natives.attackWaveSize + (natives.attackWaveSize * 0.35)

    if (natives.attackWaveSize < 1) then
        natives.attackWaveSize = 2
        natives.attackWaveDeviation = 1
        natives.attackWaveUpperBound = 3
    end

    natives.settlerWaveSize = linearInterpolation(evolution_factor ^ 1.66667,
                                                  natives.expansionMinSize,
                                                  natives.expansionMaxSize)
    natives.settlerWaveDeviation = (natives.settlerWaveSize * 0.33)

    natives.settlerCooldown = mFloor(linearInterpolation(evolution_factor ^ 1.66667,
                                                         natives.expansionMaxTime,
                                                         natives.expansionMinTime))

    natives.unitRefundAmount = AI_UNIT_REFUND * evolution_factor
    natives.kamikazeThreshold = NO_RETREAT_BASE_PERCENT + (evolution_factor * NO_RETREAT_EVOLUTION_BONUS_MAX)

    local points = mFloor((AI_POINT_GENERATOR_AMOUNT * mRandom()) + (natives.activeNests * 0.25) +
        (((AI_POINT_GENERATOR_AMOUNT * 0.7) * (evolution_factor ^ 2.5)) * natives.aiPointsScaler))

    if (natives.state == AI_STATE_ONSLAUGHT) then
        points = points * 2
    end

    natives.baseIncrement = points

    local currentPoints = natives.points

    if (currentPoints < maxPoints) then
        natives.points = currentPoints + points
    end

    if (currentPoints > maxOverflowPoints) then
        natives.points = maxOverflowPoints
    end

    if (natives.stateTick <= tick) then
        -- local roll = mRandom() * mMax(1 - evolution_factor, 0.15) * natives.aiAggressiveness

        local roll = mRandom()
        if (natives.temperament < 0.05) then -- 0 - 0.05
            if natives.enabledMigration then
                natives.state = (natives.siegeAIToggle and AI_STATE_SIEGE) or AI_STATE_MIGRATING
            else
                if natives.raidAIToggle then
                    if (roll < 0.85) then
                        natives.state = AI_STATE_AGGRESSIVE
                        natives.canAttackTick = randomTickEvent(tick,
                                                                AGGRESSIVE_CAN_ATTACK_WAIT_MIN_DURATION,
                                                                AGGRESSIVE_CAN_ATTACK_WAIT_MAX_DURATION)
                    else
                        natives.state = AI_STATE_RAIDING
                    end
                else
                    natives.state = AI_STATE_AGGRESSIVE
                    natives.canAttackTick = randomTickEvent(tick,
                                                            AGGRESSIVE_CAN_ATTACK_WAIT_MIN_DURATION,
                                                            AGGRESSIVE_CAN_ATTACK_WAIT_MAX_DURATION)
                end
            end
        elseif (natives.temperament < 0.20) then -- 0.05 - 0.2
            if (natives.enabledMigration) then
                if (roll < 0.4) then
                    natives.state = (natives.siegeAIToggle and AI_STATE_SIEGE) or AI_STATE_MIGRATING
                else
                    natives.state = AI_STATE_MIGRATING
                end
            else
                if natives.raidAIToggle then
                    if (roll < 0.95) then
                        natives.state = AI_STATE_AGGRESSIVE
                        natives.canAttackTick = randomTickEvent(tick,
                                                                AGGRESSIVE_CAN_ATTACK_WAIT_MIN_DURATION,
                                                                AGGRESSIVE_CAN_ATTACK_WAIT_MAX_DURATION)
                    else
                        natives.state = AI_STATE_RAIDING
                    end
                else
                    natives.state = AI_STATE_AGGRESSIVE
                    natives.canAttackTick = randomTickEvent(tick,
                                                            AGGRESSIVE_CAN_ATTACK_WAIT_MIN_DURATION,
                                                            AGGRESSIVE_CAN_ATTACK_WAIT_MAX_DURATION)
                end
            end
        elseif (natives.temperament < 0.4) then -- 0.2 - 0.4
            if (natives.enabledMigration) then
                if (roll < 0.2) then
                    natives.state = AI_STATE_AGGRESSIVE
                    natives.canAttackTick = randomTickEvent(tick,
                                                            AGGRESSIVE_CAN_ATTACK_WAIT_MIN_DURATION,
                                                            AGGRESSIVE_CAN_ATTACK_WAIT_MAX_DURATION)
                elseif (roll < 0.8) then
                    natives.state = AI_STATE_MIGRATING
                else
                    natives.state = AI_STATE_PEACEFUL
                end
            else
                if (roll < 0.6) then
                    natives.state = AI_STATE_AGGRESSIVE
                    natives.canAttackTick = randomTickEvent(tick,
                                                            AGGRESSIVE_CAN_ATTACK_WAIT_MIN_DURATION,
                                                            AGGRESSIVE_CAN_ATTACK_WAIT_MAX_DURATION)
                else
                    natives.state = AI_STATE_PEACEFUL
                end
            end
        elseif (natives.temperament < 0.6) then -- 0.4 - 0.6
            if (roll < 0.5) then
                natives.state = AI_STATE_AGGRESSIVE
                natives.canAttackTick = randomTickEvent(tick,
                                                        AGGRESSIVE_CAN_ATTACK_WAIT_MIN_DURATION,
                                                        AGGRESSIVE_CAN_ATTACK_WAIT_MAX_DURATION)
            else
                natives.state = AI_STATE_PEACEFUL
            end
        elseif (natives.temperament < 0.8) then -- 0.6 - 0.8
            if (roll < 0.6) then
                natives.state = AI_STATE_AGGRESSIVE
                natives.canAttackTick = randomTickEvent(tick,
                                                        AGGRESSIVE_CAN_ATTACK_WAIT_MIN_DURATION,
                                                        AGGRESSIVE_CAN_ATTACK_WAIT_MAX_DURATION)
            elseif (roll < 0.8) then
                natives.state = AI_STATE_ONSLAUGHT
            else
                natives.state = AI_STATE_PEACEFUL
            end
        else -- 0.8 - 1
            if (natives.enabledMigration and natives.raidAIToggle) then
                if (roll < 0.15) then
                    natives.state = (natives.siegeAIToggle and AI_STATE_SIEGE) or AI_STATE_ONSLAUGHT
                elseif (roll < 0.6) then
                    natives.state = AI_STATE_ONSLAUGHT
                elseif (roll < 0.8) then
                    natives.state = AI_STATE_RAIDING
                else
                    natives.state = AI_STATE_AGGRESSIVE
                    natives.canAttackTick = randomTickEvent(tick,
                                                            AGGRESSIVE_CAN_ATTACK_WAIT_MIN_DURATION,
                                                            AGGRESSIVE_CAN_ATTACK_WAIT_MAX_DURATION)
                end
            elseif (natives.enabledMigration) then
                if (roll < 0.15) then
                    natives.state = (natives.siegeAIToggle and AI_STATE_SIEGE) or AI_STATE_ONSLAUGHT
                elseif (roll < 0.7) then
                    natives.state = AI_STATE_ONSLAUGHT
                else
                    natives.state = AI_STATE_AGGRESSIVE
                    natives.canAttackTick = randomTickEvent(tick,
                                                            AGGRESSIVE_CAN_ATTACK_WAIT_MIN_DURATION,
                                                            AGGRESSIVE_CAN_ATTACK_WAIT_MAX_DURATION)
                end
            elseif (natives.raidAIToggle) then
                if (roll < 0.4) then
                    natives.state = AI_STATE_ONSLAUGHT
                elseif (roll < 0.7) then
                    natives.state = AI_STATE_RAIDING
                else
                    natives.state = AI_STATE_AGGRESSIVE
                    natives.canAttackTick = randomTickEvent(tick,
                                                            AGGRESSIVE_CAN_ATTACK_WAIT_MIN_DURATION,
                                                            AGGRESSIVE_CAN_ATTACK_WAIT_MAX_DURATION)
                end
            else
                if (roll < 0.6) then
                    natives.state = AI_STATE_ONSLAUGHT
                else
                    natives.state = AI_STATE_AGGRESSIVE
                    natives.canAttackTick = randomTickEvent(tick,
                                                            AGGRESSIVE_CAN_ATTACK_WAIT_MIN_DURATION,
                                                            AGGRESSIVE_CAN_ATTACK_WAIT_MAX_DURATION)
                end
            end
        end

        -- print("changing state", natives.state)

        natives.destroyPlayerBuildings = 0
        natives.lostEnemyUnits = 0
        natives.lostEnemyBuilding = 0
        natives.rocketLaunched = 0
        natives.builtEnemyBuilding = 0
        natives.ionCannonBlasts = 0
        natives.artilleryBlasts = 0

        natives.stateTick = randomTickEvent(tick, AI_MIN_STATE_DURATION, AI_MAX_STATE_DURATION)
    end

end


function aiPlanning.temperamentPlanner(natives)
    local destroyPlayerBuildings = natives.destroyPlayerBuildings
    local lostEnemyUnits = natives.lostEnemyUnits
    local lostEnemyBuilding = natives.lostEnemyBuilding
    local rocketLaunched = natives.rocketLaunched
    local builtEnemyBuilding = natives.builtEnemyBuilding
    local ionCannonBlasts = natives.ionCannonBlasts
    local artilleryBlasts = natives.artilleryBlasts
    local activeNests = natives.activeNests
    local activeRaidNests = natives.activeRaidNests

    local currentTemperament = natives.temperamentScore
    local delta = 0

    if activeNests > 0 then
        local val = (0.375 * activeNests)
        delta = delta + val
    else
        delta = delta - 0.25
    end

    if destroyPlayerBuildings > 0 then
        if currentTemperament > 0 then
            delta = delta - (0.25 * destroyPlayerBuildings)
        else
            delta = delta + (0.25 * destroyPlayerBuildings)
        end
    end

    if activeRaidNests > 0 then
        local val = (0.003825 * activeRaidNests)
        delta = delta - val
    else
        delta = delta - 0.125
    end

    if lostEnemyUnits > 0 then
        local multipler
        if natives.evolutionLevel < 0.3 then
            multipler = 0.0005
        elseif natives.evolutionLevel < 0.5 then
            multipler = 0.000385
        elseif natives.evolutionLevel < 0.7 then
            multipler = 0.00025
        elseif natives.evolutionLevel < 0.9 then
            multipler = 0.00012
        elseif natives.evolutionLevel < 0.9 then
            multipler = 0.00006
        end
        local val = (multipler * lostEnemyUnits)
        if (currentTemperament > 0) then
            delta = delta - val
        else
            delta = delta + val
        end
    end

    if lostEnemyBuilding > 0 then
        local val = (1.25 * lostEnemyBuilding)
        if (currentTemperament > 0) then
            delta = delta - val
        else
            delta = delta + val
        end
    end

    if (builtEnemyBuilding > 0) then
        local val = (0.075 * builtEnemyBuilding)
        if (currentTemperament > 0) then
            delta = delta - val
        else
            delta = delta + val
        end
    else
        delta = delta - 0.125
    end

    if (rocketLaunched > 0) then
        local val = (5 * rocketLaunched)
        delta = delta + val
    end

    if (ionCannonBlasts > 0) then
        local val = (2.5 * ionCannonBlasts)
        delta = delta + val
    end

    if (artilleryBlasts > 0) then
        local val = (2.5 * artilleryBlasts)
        delta = delta + val
    end

    print("temperament", natives.activeNests, natives.activeRaidNests, natives.destroyPlayerBuildings,
          natives.lostEnemyUnits,
          natives.lostEnemyBuilding, natives.rocketLaunched, natives.builtEnemyBuilding, natives.ionCannonBlasts,
          natives.artilleryBlasts)

    -- natives.destroyPlayerBuildings = 0
    -- natives.lostEnemyUnits = 0
    -- natives.lostEnemyBuilding = 0
    -- natives.rocketLaunched = 0
    -- natives.builtEnemyBuilding = 0
    -- natives.ionCannonBlasts = 0
    -- natives.artilleryBlasts = 0

    natives.temperamentScore = mMin(10000, mMax(-10000, currentTemperament + delta))
    natives.temperament = ((natives.temperamentScore + 10000) * 0.00005)

    print("tempResult", natives.temperament, natives.temperamentScore)
    print("--")
end

aiPlanningG = aiPlanning
return aiPlanning
