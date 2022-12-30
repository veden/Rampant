-- Copyright (C) 2022  veden

-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <https://www.gnu.org/licenses/>.


if aiPlanningG then
    return aiPlanningG
end
local aiPlanning = {}

-- imports

local constants = require("Constants")
local mathUtils = require("MathUtils")
local baseUtils = require("BaseUtils")

-- constants

local BASE_PROCESS_INTERVAL = constants.BASE_PROCESS_INTERVAL

local BASE_GENERATION_STATE_ACTIVE = constants.BASE_GENERATION_STATE_ACTIVE
local BASE_GENERATION_STATE_DORMANT = constants.BASE_GENERATION_STATE_DORMANT

local BASE_GENERATION_MIN_STATE_DURATION = constants.BASE_GENERATION_MIN_STATE_DURATION
local BASE_GENERATION_MAX_STATE_DURATION = constants.BASE_GENERATION_MAX_STATE_DURATION

local TEMPERAMENT_RANGE_MAX = constants.TEMPERAMENT_RANGE_MAX
local TEMPERAMENT_RANGE_MIN = constants.TEMPERAMENT_RANGE_MIN
local TEMPERAMENT_DIVIDER = constants.TEMPERAMENT_DIVIDER
local NO_RETREAT_BASE_PERCENT = constants.NO_RETREAT_BASE_PERCENT
local NO_RETREAT_EVOLUTION_BONUS_MAX = constants.NO_RETREAT_EVOLUTION_BONUS_MAX

local AI_UNIT_REFUND = constants.AI_UNIT_REFUND

local AI_MAX_POINTS = constants.AI_MAX_POINTS

local BASE_RALLY_CHANCE = constants.BASE_RALLY_CHANCE
local BONUS_RALLY_CHANCE = constants.BONUS_RALLY_CHANCE

local RETREAT_MOVEMENT_PHEROMONE_LEVEL_MIN = constants.RETREAT_MOVEMENT_PHEROMONE_LEVEL_MIN
local RETREAT_MOVEMENT_PHEROMONE_LEVEL_MAX = constants.RETREAT_MOVEMENT_PHEROMONE_LEVEL_MAX
local MINIMUM_AI_POINTS = constants.MINIMUM_AI_POINTS

local ACTIVE_NESTS_PER_AGGRESSIVE_GROUPS = constants.ACTIVE_NESTS_PER_AGGRESSIVE_GROUPS
local ALL_NESTS_PER_EXPANSION_GROUPS = constants.ALL_NESTS_PER_EXPANSION_GROUPS

local BASE_AI_STATE_PEACEFUL = constants.BASE_AI_STATE_PEACEFUL
local BASE_AI_STATE_AGGRESSIVE = constants.BASE_AI_STATE_AGGRESSIVE
local BASE_AI_STATE_RAIDING = constants.BASE_AI_STATE_RAIDING
local BASE_AI_STATE_MIGRATING = constants.BASE_AI_STATE_MIGRATING
local BASE_AI_STATE_ONSLAUGHT = constants.BASE_AI_STATE_ONSLAUGHT
local BASE_AI_STATE_SIEGE = constants.BASE_AI_STATE_SIEGE
local AI_POINT_GENERATOR_AMOUNT = constants.AI_POINT_GENERATOR_AMOUNT

local BASE_AI_MIN_STATE_DURATION = constants.BASE_AI_MIN_STATE_DURATION
local BASE_AI_MAX_STATE_DURATION = constants.BASE_AI_MAX_STATE_DURATION


-- imported functions

local randomTickEvent = mathUtils.randomTickEvent
local randomTickDuration = mathUtils.randomTickDuration

local upgradeBaseBasedOnDamage = baseUtils.upgradeBaseBasedOnDamage

local linearInterpolation = mathUtils.linearInterpolation

local mFloor = math.floor

local mMax = math.max
local mMin = math.min

local mCeil = math.ceil

-- module code

local function getTimeStringFromTick(tick)

    local tickToSeconds = tick / 60

    local days = mFloor(tickToSeconds / 86400)
    local hours = mFloor((tickToSeconds % 86400) / 3600)
    local minutes = mFloor((tickToSeconds % 3600) / 60)
    local seconds = mFloor(tickToSeconds % 60)
    return days .. "d " .. hours .. "h " .. minutes .. "m " .. seconds .. "s"
end

function aiPlanning.planning(universe, evolutionLevel)
    universe.evolutionLevel = evolutionLevel
    local maxPoints = mMax(AI_MAX_POINTS * evolutionLevel, MINIMUM_AI_POINTS)
    universe.maxPoints = maxPoints

    local maxOverflowPoints = maxPoints * 3
    universe.maxOverflowPoints = maxOverflowPoints

    local attackWaveMaxSize = universe.attackWaveMaxSize
    universe.retreatThreshold = linearInterpolation(evolutionLevel,
                                                    RETREAT_MOVEMENT_PHEROMONE_LEVEL_MIN,
                                                    RETREAT_MOVEMENT_PHEROMONE_LEVEL_MAX)
    universe.rallyThreshold = BASE_RALLY_CHANCE + (evolutionLevel * BONUS_RALLY_CHANCE)
    universe.formSquadThreshold = mMax((0.35 * evolutionLevel), 0.1)

    universe.attackWaveSize = attackWaveMaxSize * (evolutionLevel ^ 1.4)
    universe.attackWaveDeviation = (universe.attackWaveSize * 0.333)
    universe.attackWaveUpperBound = universe.attackWaveSize + (universe.attackWaveSize * 0.35)

    if (universe.attackWaveSize < 1) then
        universe.attackWaveSize = 2
        universe.attackWaveDeviation = 1
        universe.attackWaveUpperBound = 3
    end

    universe.settlerWaveSize = linearInterpolation(evolutionLevel ^ 1.66667,
                                                   universe.expansionMinSize,
                                                   universe.expansionMaxSize)
    universe.settlerWaveDeviation = (universe.settlerWaveSize * 0.33)

    universe.settlerCooldown = randomTickDuration(universe.random,
                                                  universe.expansionMinTime,
                                                  mFloor(linearInterpolation(evolutionLevel ^ 1.66667,
                                                                             universe.expansionMaxTime,
                                                                             universe.expansionMinTime)))

    universe.unitRefundAmount = AI_UNIT_REFUND * evolutionLevel
    universe.kamikazeThreshold = NO_RETREAT_BASE_PERCENT + (evolutionLevel * NO_RETREAT_EVOLUTION_BONUS_MAX)
end

local function processBase(universe, base, tick)

    base.maxAggressiveGroups = mCeil(base.activeNests / ACTIVE_NESTS_PER_AGGRESSIVE_GROUPS)
    base.maxExpansionGroups = mCeil((base.activeNests + base.activeRaidNests) / ALL_NESTS_PER_EXPANSION_GROUPS)

    if (base.stateAI == BASE_AI_STATE_MIGRATING or base.stateAI == BASE_AI_STATE_SIEGE)
        and base.resetExpensionGroupsTick <= tick
    then
        base.resetExpensionGroupsTick = tick + universe.settlerCooldown
        base.sentExpansionGroups = 0
    end

    local points = (AI_POINT_GENERATOR_AMOUNT * universe.random()) +
        (base.activeNests * 0.144) +
        (AI_POINT_GENERATOR_AMOUNT * mMax(universe.evolutionLevel ^ 2.5, 0.1))

    if (base.temperament == 0) or (base.temperament == 1) then
        points = points + 24
    elseif (base.temperament < 0.20) or (base.temperament > 0.80) then
        points = points + 14.4
    elseif (base.temperament < 0.35) or (base.temperament > 0.65) then
        points = points + 9.6
    elseif (base.temperament < 0.45) or (base.temperament > 0.55) then
        points = points + 4.8
    end

    if (base.stateAI == BASE_AI_STATE_ONSLAUGHT) then
        points = points * 2
    end

    points = points * universe.aiPointsScaler

    local currentPoints = base.unitPoints

    if (currentPoints <= 0) then
        currentPoints = 0
    end

    if (currentPoints < universe.maxPoints) then
        base.unitPoints = currentPoints + points
    elseif currentPoints > universe.maxOverflowPoints then
        base.unitPoints = universe.maxOverflowPoints
    end

    if (base.points < universe.maxPoints) then
        base.points = base.points + (points * 0.75)
    else
        base.points = universe.maxPoints
    end

    if universe.NEW_ENEMIES then
        local deathThreshold = 0
        local evolutionLevel = universe.evolutionLevel
        if (evolutionLevel < universe.minimumAdaptationEvolution) then
            base.deathEvents = 0
        elseif (evolutionLevel < 0.5) then
            deathThreshold = 17000
        elseif (evolutionLevel < 0.7) then
            deathThreshold = 34000
        elseif (evolutionLevel < 0.9) then
            deathThreshold = 60000
        else
            deathThreshold = 100000
        end

        deathThreshold = universe.adaptationModifier * deathThreshold
        if ((base.deathEvents > deathThreshold) and (universe.random() > 0.95)) then
            if (base.mutations < universe.MAX_BASE_MUTATIONS) then
                base.mutations = base.mutations + 1
                upgradeBaseBasedOnDamage(universe, base)
            elseif (base.mutations == universe.MAX_BASE_MUTATIONS) then
                local roll = universe.random()
                if (roll < 0.001) then
                    base.mutations = 0
                    if (universe.printBaseAdaptation) then
                        game.print({"description.rampant--adaptationResetDebugMessage",
                                    base.x,
                                    base.y,
                                    base.mutations,
                                    universe.MAX_BASE_MUTATIONS})
                    end
                elseif (roll > 0.999) then
                    base.mutations = base.mutations + 1
                    if (universe.printBaseAdaptation) then
                        game.print({"description.rampant--adaptationFrozenDebugMessage",
                                    base.x,
                                    base.y})
                    end
                end
            end
            base.damagedBy = {}
            base.deathEvents = 0
        end
    else
        base.damagedBy = {}
        base.deathEvents = 0
    end

    if (base.stateGenerationTick <= tick) then
        local roll = universe.random()
        if (roll < 0.85) then
            base.stateGeneration = BASE_GENERATION_STATE_ACTIVE
        else
            base.stateGeneration = BASE_GENERATION_STATE_DORMANT
        end
        base.stateGenerationTick = randomTickEvent(universe.random,
                                                   tick,
                                                   BASE_GENERATION_MIN_STATE_DURATION,
                                                   BASE_GENERATION_MAX_STATE_DURATION)
    end

    base.tick = tick
end

local function temperamentPlanner(base, evolutionLevel)
    local destroyPlayerBuildings = base.destroyPlayerBuildings
    local lostEnemyUnits = base.lostEnemyUnits
    local lostEnemyBuilding = base.lostEnemyBuilding
    local rocketLaunched = base.rocketLaunched
    local builtEnemyBuilding = base.builtEnemyBuilding
    local ionCannonBlasts = base.ionCannonBlasts
    local artilleryBlasts = base.artilleryBlasts
    local activeNests = base.activeNests
    local activeRaidNests = base.activeRaidNests

    local currentTemperament = base.temperamentScore
    local delta = 0

    if activeNests > 0 then
        local val = (5.76 * activeNests)
        delta = delta + val
    else
        delta = delta - 5.553792
    end

    if destroyPlayerBuildings > 0 then
        if currentTemperament > 0 then
            delta = delta - (5.553792 * destroyPlayerBuildings)
        else
            delta = delta + (5.553792 * destroyPlayerBuildings)
        end
    end

    if activeRaidNests > 0 then
        local val = (0.2304 * activeRaidNests)
        delta = delta - val
    else
        delta = delta - 3.84
    end

    if lostEnemyUnits > 0 then
        local multipler
        if evolutionLevel < 0.3 then
            multipler = 0.083328
        elseif evolutionLevel < 0.5 then
            multipler = 0.041472
        elseif evolutionLevel < 0.7 then
            multipler = 0.020736
        elseif evolutionLevel < 0.9 then
            multipler = 0.010368
        elseif evolutionLevel < 0.9 then
            multipler = 0.005184
        else
            multipler = 0.002592
        end
        local val = (multipler * lostEnemyUnits)
        if (currentTemperament > 0) then
            delta = delta - val
        else
            delta = delta + val
        end
    end

    if lostEnemyBuilding > 0 then
        local val = (0.576 * lostEnemyBuilding)
        if (currentTemperament > 0) then
            delta = delta - val
        else
            delta = delta + val
        end
    end

    if builtEnemyBuilding > 0 then
        local val = (0.261952 * builtEnemyBuilding)
        if (currentTemperament > 0) then
            delta = delta - val
        else
            delta = delta + val
        end
    else
        delta = delta - 2.777088
    end

    if (rocketLaunched > 0) then
        local val = (27.76 * rocketLaunched)
        delta = delta + val
    end

    if (ionCannonBlasts > 0) then
        local val = (13.924864 * ionCannonBlasts)
        delta = delta + val
    end

    if (artilleryBlasts > 0) then
        local val = (13.924864 * artilleryBlasts)
        delta = delta + val
    end

    local universe = base.universe

    delta = delta * universe.temperamentRateModifier
    base.temperamentScore = mMin(TEMPERAMENT_RANGE_MAX, mMax(TEMPERAMENT_RANGE_MIN, currentTemperament + delta))
    base.temperament = ((base.temperamentScore + TEMPERAMENT_RANGE_MAX) * TEMPERAMENT_DIVIDER)

    if universe.debugTemperament then
        local strConsole = "Rampant Stats:\nbaseId:".. base.id .. ", aN:" .. base.activeNests ..
            ", aRN:" .. base.activeRaidNests .. ", dPB:" ..
            base.destroyPlayerBuildings .. ", lEU:" .. base.lostEnemyUnits .. ", lEB:" ..
            base.lostEnemyBuilding .. ", rL:" .. base.rocketLaunched .. ", bEB:" ..
            base.builtEnemyBuilding .. ", iCB:" .. base.ionCannonBlasts .. ", aB:" ..
            base.artilleryBlasts .. ", temp:" .. base.temperament .. ", tempScore:" .. base.temperamentScore ..
            ", points:" .. base.points .. ", unitPoints:" .. base.unitPoints .. ", state:" ..
            constants.stateEnglish[base.stateAI] .. ", surface:" .. base.map.surface.index .. " [" ..
            base.map.surface.name .. "]" .. ", aS:" .. universe.squadCount .. ", aB:" .. universe.builderCount ..
            ", atkSize:" .. universe.attackWaveSize .. ", stlSize:" .. universe.settlerWaveSize ..
            ", formGroup:" .. universe.formSquadThreshold .. ", sAgg:".. base.sentAggressiveGroups ..
            ", mAgg:" .. base.maxAggressiveGroups .. ", baseState:" .. base.stateGeneration .. ", sE:"
            .. base.sentExpansionGroups .. ", mE:" .. base.maxExpansionGroups
        game.print(strConsole)
        print(strConsole)
    end
end

local function processState(universe, base, tick)

    if (base.stateAITick > tick) or not universe.awake then
        if (not universe.awake) and (tick >= universe.initialPeaceTime) then
            universe.awake = true
            if universe.printAwakenMessage then
                game.print({"description.rampant--planetHasAwoken"})
            end
        else
            return
        end
    end
    local roll = universe.random()
    if (base.temperament < 0.05) then -- 0 - 0.05
        if universe.enabledMigration then
            if (roll < 0.30) then
                base.stateAI = BASE_AI_STATE_MIGRATING
            elseif (roll < 0.50) and universe.raidAIToggle then
                base.stateAI = BASE_AI_STATE_RAIDING
            elseif universe.siegeAIToggle then
                base.stateAI = BASE_AI_STATE_SIEGE
            else
                base.stateAI = BASE_AI_STATE_MIGRATING
            end
        else
            if universe.raidAIToggle then
                if (roll < 0.70) then
                    base.stateAI = BASE_AI_STATE_RAIDING
                else
                    base.stateAI = BASE_AI_STATE_AGGRESSIVE
                end
            else
                base.stateAI = BASE_AI_STATE_AGGRESSIVE
            end
        end
    elseif (base.temperament < 0.20) then -- 0.05 - 0.2
        if (universe.enabledMigration) then
            if (roll < 0.4) then
                base.stateAI = BASE_AI_STATE_MIGRATING
            elseif (roll < 0.55) and universe.raidAIToggle then
                base.stateAI = BASE_AI_STATE_RAIDING
            elseif universe.siegeAIToggle then
                base.stateAI = BASE_AI_STATE_SIEGE
            else
                base.stateAI = BASE_AI_STATE_MIGRATING
            end
        else
            if universe.raidAIToggle then
                if (roll < 0.40) then
                    base.stateAI = BASE_AI_STATE_AGGRESSIVE
                else
                    base.stateAI = BASE_AI_STATE_RAIDING
                end
            else
                base.stateAI = BASE_AI_STATE_AGGRESSIVE
            end
        end
    elseif (base.temperament < 0.4) then -- 0.2 - 0.4
        if (universe.enabledMigration) then
            if (roll < 0.2) and universe.raidAIToggle then
                base.stateAI = BASE_AI_STATE_RAIDING
            elseif (roll < 0.2) then
                base.stateAI = BASE_AI_STATE_AGGRESSIVE
            elseif (roll < 0.8) then
                base.stateAI = BASE_AI_STATE_MIGRATING
            elseif universe.peacefulAIToggle then
                base.stateAI = BASE_AI_STATE_PEACEFUL
            else
                base.stateAI = BASE_AI_STATE_MIGRATING
            end
        else
            if (roll < 0.3) then
                base.stateAI = BASE_AI_STATE_AGGRESSIVE
            elseif (roll < 0.6) and universe.raidAIToggle then
                base.stateAI = BASE_AI_STATE_RAIDING
            elseif (roll < 0.6) then
                base.stateAI = BASE_AI_STATE_AGGRESSIVE
            elseif universe.peacefulAIToggle then
                base.stateAI = BASE_AI_STATE_PEACEFUL
            else
                base.stateAI = BASE_AI_STATE_AGGRESSIVE
            end
        end
    elseif (base.temperament < 0.6) then -- 0.4 - 0.6
        if (roll < 0.4) then
            base.stateAI = BASE_AI_STATE_AGGRESSIVE
        elseif (roll < 0.5) and universe.raidAIToggle then
            base.stateAI = BASE_AI_STATE_RAIDING
        elseif (roll < 0.75) and universe.peacefulAIToggle then
            base.stateAI = BASE_AI_STATE_PEACEFUL
        else
            if universe.enabledMigration then
                base.stateAI = BASE_AI_STATE_MIGRATING
            else
                base.stateAI = BASE_AI_STATE_AGGRESSIVE
            end
        end
    elseif (base.temperament < 0.8) then -- 0.6 - 0.8
        if (roll < 0.4) then
            base.stateAI = BASE_AI_STATE_AGGRESSIVE
        elseif (roll < 0.6) then
            base.stateAI = BASE_AI_STATE_ONSLAUGHT
        elseif (roll < 0.8) then
            base.stateAI = BASE_AI_STATE_RAIDING
        elseif universe.peacefulAIToggle then
            base.stateAI = BASE_AI_STATE_PEACEFUL
        else
            base.stateAI = BASE_AI_STATE_AGGRESSIVE
        end
    elseif (base.temperament < 0.95) then -- 0.8 - 0.95
        if (universe.enabledMigration and universe.raidAIToggle) then
            if (roll < 0.20) and universe.siegeAIToggle then
                base.stateAI = BASE_AI_STATE_SIEGE
            elseif (roll < 0.45) then
                base.stateAI = BASE_AI_STATE_RAIDING
            elseif (roll < 0.85) then
                base.stateAI = BASE_AI_STATE_ONSLAUGHT
            else
                base.stateAI = BASE_AI_STATE_AGGRESSIVE
            end
        elseif (universe.enabledMigration) then
            if (roll < 0.20) and universe.siegeAIToggle then
                base.stateAI = BASE_AI_STATE_SIEGE
            elseif (roll < 0.75) then
                base.stateAI = BASE_AI_STATE_ONSLAUGHT
            else
                base.stateAI = BASE_AI_STATE_AGGRESSIVE
            end
        elseif (universe.raidAIToggle) then
            if (roll < 0.45) then
                base.stateAI = BASE_AI_STATE_ONSLAUGHT
            elseif (roll < 0.75) then
                base.stateAI = BASE_AI_STATE_RAIDING
            else
                base.stateAI = BASE_AI_STATE_AGGRESSIVE
            end
        else
            if (roll < 0.65) then
                base.stateAI = BASE_AI_STATE_ONSLAUGHT
            else
                base.stateAI = BASE_AI_STATE_AGGRESSIVE
            end
        end
    else
        if (universe.enabledMigration and universe.raidAIToggle) then
            if (roll < 0.30) and universe.siegeAIToggle then
                base.stateAI = BASE_AI_STATE_SIEGE
            elseif (roll < 0.65) then
                base.stateAI = BASE_AI_STATE_RAIDING
            else
                base.stateAI = BASE_AI_STATE_ONSLAUGHT
            end
        elseif (universe.enabledMigration) then
            if (roll < 0.30) and universe.siegeAIToggle then
                base.stateAI = BASE_AI_STATE_SIEGE
            else
                base.stateAI = BASE_AI_STATE_ONSLAUGHT
            end
        elseif (universe.raidAIToggle) then
            if (roll < 0.45) then
                base.stateAI = BASE_AI_STATE_ONSLAUGHT
            else
                base.stateAI = BASE_AI_STATE_RAIDING
            end
        else
            base.stateAI = BASE_AI_STATE_ONSLAUGHT
        end
    end

    base.destroyPlayerBuildings = 0
    base.lostEnemyUnits = 0
    base.lostEnemyBuilding = 0
    base.rocketLaunched = 0
    base.builtEnemyBuilding = 0
    base.ionCannonBlasts = 0
    base.artilleryBlasts = 0

    base.stateAITick = randomTickEvent(universe.random, tick, BASE_AI_MIN_STATE_DURATION, BASE_AI_MAX_STATE_DURATION)

    if universe.printAIStateChanges then
        game.print(base.id .. ": AI is now: " .. constants.stateEnglish[base.stateAI] .. ", Next state change is in "
                   .. string.format("%.2f", (base.stateAITick - tick) / (60*60)) .. " minutes @ " ..
                   getTimeStringFromTick(base.stateAITick) .. " playtime")
    end

end

function aiPlanning.processBaseAIs(universe, tick)
    local baseId = universe.processBaseAIIterator
    local base
    if not baseId then
        baseId, base = next(universe.bases, nil)
    else
        base = universe.bases[baseId]
    end
    if not baseId then
        universe.processBaseAIIterator = nil
        return
    else
        universe.processBaseAIIterator = next(universe.bases, baseId)
        if (tick - base.tick) <= BASE_PROCESS_INTERVAL then
            return
        end
        temperamentPlanner(base, universe.evolutionLevel)
        processState(universe, base, tick)
        processBase(universe, base, tick)
    end
end

aiPlanningG = aiPlanning
return aiPlanning
