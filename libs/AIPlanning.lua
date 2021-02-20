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

function aiPlanning.planning(map, evolution_factor, tick)
    local universe = map.universe
    map.evolutionLevel = evolution_factor
    universe.evolutionLevel = evolution_factor

    local maxPoints = mMax(AI_MAX_POINTS * evolution_factor, MINIMUM_AI_POINTS)

    if not universe.ranIncompatibleMessage and universe.newEnemies and
        (game.active_mods["bobenemies"] or game.active_mods["Natural_Evolution_Enemies"]) then
        universe.ranIncompatibleMessage = true
        game.print({"description.rampant-bobs-nee-newEnemies"})
    end

    local maxOverflowPoints = maxPoints * 3

    local attackWaveMaxSize = universe.attackWaveMaxSize
    universe.retreatThreshold = linearInterpolation(evolution_factor,
                                                   RETREAT_MOVEMENT_PHEROMONE_LEVEL_MIN,
                                                   RETREAT_MOVEMENT_PHEROMONE_LEVEL_MAX)
    universe.rallyThreshold = BASE_RALLY_CHANCE + (evolution_factor * BONUS_RALLY_CHANCE)
    universe.formSquadThreshold = mMax((0.20 * evolution_factor), 0.05)

    universe.attackWaveSize = attackWaveMaxSize * (evolution_factor ^ 1.4)
    map.attackWaveSize = universe.attackWaveSize
    universe.attackWaveDeviation = (universe.attackWaveSize * 0.333)
    universe.attackWaveUpperBound = universe.attackWaveSize + (universe.attackWaveSize * 0.35)

    if (universe.attackWaveSize < 1) then
        universe.attackWaveSize = 2
        universe.attackWaveDeviation = 1
        universe.attackWaveUpperBound = 3
    end

    universe.settlerWaveSize = linearInterpolation(evolution_factor ^ 1.66667,
                                                  universe.expansionMinSize,
                                                  universe.expansionMaxSize)
    universe.settlerWaveDeviation = (universe.settlerWaveSize * 0.33)

    universe.settlerCooldown = mFloor(linearInterpolation(evolution_factor ^ 1.66667,
                                                         universe.expansionMaxTime,
                                                         universe.expansionMinTime))

    universe.unitRefundAmount = AI_UNIT_REFUND * evolution_factor
    universe.kamikazeThreshold = NO_RETREAT_BASE_PERCENT + (evolution_factor * NO_RETREAT_EVOLUTION_BONUS_MAX)

    local points = mFloor((AI_POINT_GENERATOR_AMOUNT * mRandom()) + (map.activeNests * 0.25) +
        (((AI_POINT_GENERATOR_AMOUNT * 0.7) * (evolution_factor ^ 2.5)) * universe.aiPointsScaler))

    if (map.state == AI_STATE_ONSLAUGHT) then
        points = points * 2
    end

    map.baseIncrement = points

    local currentPoints = map.points

    if (currentPoints < maxPoints) then
        map.points = currentPoints + points
    end

    if (currentPoints > maxOverflowPoints) then
        map.points = maxOverflowPoints
    end

    if (map.stateTick <= tick) then
        -- local roll = mRandom() * mMax(1 - evolution_factor, 0.15) * map.aiAggressiveness

        local roll = mRandom()
        if (map.temperament < 0.05) then -- 0 - 0.05
            if universe.enabledMigration then
                map.state = (universe.siegeAIToggle and AI_STATE_SIEGE) or AI_STATE_MIGRATING
            else
                if universe.raidAIToggle then
                    if (roll < 0.85) then
                        map.state = AI_STATE_AGGRESSIVE
                        map.canAttackTick = randomTickEvent(tick,
                                                                AGGRESSIVE_CAN_ATTACK_WAIT_MIN_DURATION,
                                                                AGGRESSIVE_CAN_ATTACK_WAIT_MAX_DURATION)
                    else
                        map.state = AI_STATE_RAIDING
                    end
                else
                    map.state = AI_STATE_AGGRESSIVE
                    map.canAttackTick = randomTickEvent(tick,
                                                            AGGRESSIVE_CAN_ATTACK_WAIT_MIN_DURATION,
                                                            AGGRESSIVE_CAN_ATTACK_WAIT_MAX_DURATION)
                end
            end
        elseif (map.temperament < 0.20) then -- 0.05 - 0.2
            if (universe.enabledMigration) then
                if (roll < 0.4) then
                    map.state = (universe.siegeAIToggle and AI_STATE_SIEGE) or AI_STATE_MIGRATING
                else
                    map.state = AI_STATE_MIGRATING
                end
            else
                if universe.raidAIToggle then
                    if (roll < 0.95) then
                        map.state = AI_STATE_AGGRESSIVE
                        map.canAttackTick = randomTickEvent(tick,
                                                                AGGRESSIVE_CAN_ATTACK_WAIT_MIN_DURATION,
                                                                AGGRESSIVE_CAN_ATTACK_WAIT_MAX_DURATION)
                    else
                        map.state = AI_STATE_RAIDING
                    end
                else
                    map.state = AI_STATE_AGGRESSIVE
                    map.canAttackTick = randomTickEvent(tick,
                                                            AGGRESSIVE_CAN_ATTACK_WAIT_MIN_DURATION,
                                                            AGGRESSIVE_CAN_ATTACK_WAIT_MAX_DURATION)
                end
            end
        elseif (map.temperament < 0.4) then -- 0.2 - 0.4
            if (universe.enabledMigration) then
                if (roll < 0.2) then
                    map.state = AI_STATE_AGGRESSIVE
                    map.canAttackTick = randomTickEvent(tick,
                                                            AGGRESSIVE_CAN_ATTACK_WAIT_MIN_DURATION,
                                                            AGGRESSIVE_CAN_ATTACK_WAIT_MAX_DURATION)
                elseif (roll < 0.8) then
                    map.state = AI_STATE_MIGRATING
                else
                    map.state = AI_STATE_PEACEFUL
                end
            else
                if (roll < 0.6) then
                    map.state = AI_STATE_AGGRESSIVE
                    map.canAttackTick = randomTickEvent(tick,
                                                            AGGRESSIVE_CAN_ATTACK_WAIT_MIN_DURATION,
                                                            AGGRESSIVE_CAN_ATTACK_WAIT_MAX_DURATION)
                else
                    map.state = AI_STATE_PEACEFUL
                end
            end
        elseif (map.temperament < 0.6) then -- 0.4 - 0.6
            if (roll < 0.5) then
                map.state = AI_STATE_AGGRESSIVE
                map.canAttackTick = randomTickEvent(tick,
                                                        AGGRESSIVE_CAN_ATTACK_WAIT_MIN_DURATION,
                                                        AGGRESSIVE_CAN_ATTACK_WAIT_MAX_DURATION)
            else
                map.state = AI_STATE_PEACEFUL
            end
        elseif (map.temperament < 0.8) then -- 0.6 - 0.8
            if (roll < 0.6) then
                map.state = AI_STATE_AGGRESSIVE
                map.canAttackTick = randomTickEvent(tick,
                                                        AGGRESSIVE_CAN_ATTACK_WAIT_MIN_DURATION,
                                                        AGGRESSIVE_CAN_ATTACK_WAIT_MAX_DURATION)
            elseif (roll < 0.8) then
                map.state = AI_STATE_ONSLAUGHT
            else
                map.state = AI_STATE_PEACEFUL
            end
        else -- 0.8 - 1
            if (universe.enabledMigration and universe.raidAIToggle) then
                if (roll < 0.15) then
                    map.state = (universe.siegeAIToggle and AI_STATE_SIEGE) or AI_STATE_ONSLAUGHT
                elseif (roll < 0.6) then
                    map.state = AI_STATE_ONSLAUGHT
                elseif (roll < 0.8) then
                    map.state = AI_STATE_RAIDING
                else
                    map.state = AI_STATE_AGGRESSIVE
                    map.canAttackTick = randomTickEvent(tick,
                                                            AGGRESSIVE_CAN_ATTACK_WAIT_MIN_DURATION,
                                                            AGGRESSIVE_CAN_ATTACK_WAIT_MAX_DURATION)
                end
            elseif (universe.enabledMigration) then
                if (roll < 0.15) then
                    map.state = (universe.siegeAIToggle and AI_STATE_SIEGE) or AI_STATE_ONSLAUGHT
                elseif (roll < 0.7) then
                    map.state = AI_STATE_ONSLAUGHT
                else
                    map.state = AI_STATE_AGGRESSIVE
                    map.canAttackTick = randomTickEvent(tick,
                                                            AGGRESSIVE_CAN_ATTACK_WAIT_MIN_DURATION,
                                                            AGGRESSIVE_CAN_ATTACK_WAIT_MAX_DURATION)
                end
            elseif (universe.raidAIToggle) then
                if (roll < 0.4) then
                    map.state = AI_STATE_ONSLAUGHT
                elseif (roll < 0.7) then
                    map.state = AI_STATE_RAIDING
                else
                    map.state = AI_STATE_AGGRESSIVE
                    map.canAttackTick = randomTickEvent(tick,
                                                            AGGRESSIVE_CAN_ATTACK_WAIT_MIN_DURATION,
                                                            AGGRESSIVE_CAN_ATTACK_WAIT_MAX_DURATION)
                end
            else
                if (roll < 0.6) then
                    map.state = AI_STATE_ONSLAUGHT
                else
                    map.state = AI_STATE_AGGRESSIVE
                    map.canAttackTick = randomTickEvent(tick,
                                                            AGGRESSIVE_CAN_ATTACK_WAIT_MIN_DURATION,
                                                            AGGRESSIVE_CAN_ATTACK_WAIT_MAX_DURATION)
                end
            end
        end

        -- print("changing state", map.state)

        map.destroyPlayerBuildings = 0
        map.lostEnemyUnits = 0
        map.lostEnemyBuilding = 0
        map.rocketLaunched = 0
        map.builtEnemyBuilding = 0
        map.ionCannonBlasts = 0
        map.artilleryBlasts = 0

        map.stateTick = randomTickEvent(tick, AI_MIN_STATE_DURATION, AI_MAX_STATE_DURATION)
    end

end


function aiPlanning.temperamentPlanner(map)
    local destroyPlayerBuildings = map.destroyPlayerBuildings
    local lostEnemyUnits = map.lostEnemyUnits
    local lostEnemyBuilding = map.lostEnemyBuilding
    local rocketLaunched = map.rocketLaunched
    local builtEnemyBuilding = map.builtEnemyBuilding
    local ionCannonBlasts = map.ionCannonBlasts
    local artilleryBlasts = map.artilleryBlasts
    local activeNests = map.activeNests
    local activeRaidNests = map.activeRaidNests

    local currentTemperament = map.temperamentScore
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
        if map.evolutionLevel < 0.3 then
            multipler = 0.0005
        elseif map.evolutionLevel < 0.5 then
            multipler = 0.000385
        elseif map.evolutionLevel < 0.7 then
            multipler = 0.00025
        elseif map.evolutionLevel < 0.9 then
            multipler = 0.00012
        elseif map.evolutionLevel < 0.9 then
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

    print("temperament", map.activeNests, map.activeRaidNests, map.destroyPlayerBuildings,
          map.lostEnemyUnits,
          map.lostEnemyBuilding, map.rocketLaunched, map.builtEnemyBuilding, map.ionCannonBlasts,
          map.artilleryBlasts)

    -- map.destroyPlayerBuildings = 0
    -- map.lostEnemyUnits = 0
    -- map.lostEnemyBuilding = 0
    -- map.rocketLaunched = 0
    -- map.builtEnemyBuilding = 0
    -- map.ionCannonBlasts = 0
    -- map.artilleryBlasts = 0

    map.temperamentScore = mMin(10000, mMax(-10000, currentTemperament + delta))
    map.temperament = ((map.temperamentScore + 10000) * 0.00005)

    print("tempResult", map.temperament, map.temperamentScore)
    print("--")
end

aiPlanningG = aiPlanning
return aiPlanning
