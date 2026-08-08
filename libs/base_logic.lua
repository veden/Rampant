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
-- along with this program.  If not, see <https://www.gnu.org/licenses/>

--[[
    Base - Pure base decision logic.

    Zero Factorio dependencies. Takes data tables, returns values/commands.
]]

local Constants = require("Constants")

local BASE_AI_STATE_RAIDING    = Constants.BASE_AI_STATE_RAIDING
local BASE_AI_STATE_AGGRESSIVE = Constants.BASE_AI_STATE_AGGRESSIVE
local BASE_AI_STATE_MIGRATING  = Constants.BASE_AI_STATE_MIGRATING
local BASE_AI_STATE_SIEGE      = Constants.BASE_AI_STATE_SIEGE
local BASE_AI_STATE_ONSLAUGHT  = Constants.BASE_AI_STATE_ONSLAUGHT
local BASE_AI_STATE_PEACEFUL   = Constants.BASE_AI_STATE_PEACEFUL

local Base = {}

--- Check if a base can attack. Pure evaluation.
function Base.canAttack(params)
    local goodAI = false

    if params.stateAI == BASE_AI_STATE_AGGRESSIVE and params.sentAggressiveGroups < params.maxAggressiveGroups then
        goodAI = true
    end
    if params.stateAI == BASE_AI_STATE_RAIDING then
        goodAI = true
    end
    if params.stateAI == BASE_AI_STATE_ONSLAUGHT then
        goodAI = true
    end
    if params.raidAIToggle and params.stateAI == BASE_AI_STATE_SIEGE and params.sentExpansionGroups >= params.maxExpansionGroups then
        goodAI = true
    end

    if not goodAI then return false end
    if params.peacefulMode then return false end

    local nocturnal = (not params.aiNocturnalMode) or (params.aiNocturnalMode and params.darkness > 0.65)
    if not nocturnal then return false end

    return true
end

--- Check if a base can migrate. Pure evaluation.
function Base.canMigrate(params)
    local badAIState = (params.stateAI ~= BASE_AI_STATE_MIGRATING) and (params.stateAI ~= BASE_AI_STATE_SIEGE)
    if badAIState then return false end
    if not params.expansion then return false end
    if params.peacefulMode then return false end

    local nocturnal = (not params.aiNocturnalMode) or (params.aiNocturnalMode and params.darkness > 0.65)
    if not nocturnal then return false end

    return true
end

--- Calculate temperament score delta from base activity.
-- Returns delta (to be added to current temperament score).
function Base.calculateTemperamentDelta(base, evolutionLevel, rateModifier)
    local currentTemperament = base.temperamentScore or 0
    local delta = 0

    -- Active nests
    if (base.activeNests or 0) > 0 then
        delta = delta + (5.76 * base.activeNests)
    else
        delta = delta - 5.553792
    end

    -- Destroyed player buildings
    if (base.destroyPlayerBuildings or 0) > 0 then
        if currentTemperament > 0 then
            delta = delta - (5.553792 * base.destroyPlayerBuildings)
        else
            delta = delta + (5.553792 * base.destroyPlayerBuildings)
        end
    end

    -- Active raid nests
    if (base.activeRaidNests or 0) > 0 then
        delta = delta - (0.2304 * base.activeRaidNests)
    else
        delta = delta - 3.84
    end

    -- Lost enemy units
    if (base.lostEnemyUnits or 0) > 0 then
        local multiplier
        if evolutionLevel < 0.3 then multiplier = 0.083328
        elseif evolutionLevel < 0.5 then multiplier = 0.041472
        elseif evolutionLevel < 0.7 then multiplier = 0.020736
        elseif evolutionLevel < 0.9 then multiplier = 0.010368
        else multiplier = 0.002592
        end
        if currentTemperament > 0 then
            delta = delta - (multiplier * base.lostEnemyUnits)
        else
            delta = delta + (multiplier * base.lostEnemyUnits)
        end
    end

    -- Lost enemy buildings
    if (base.lostEnemyBuilding or 0) > 0 then
        if currentTemperament > 0 then
            delta = delta - (0.576 * base.lostEnemyBuilding)
        else
            delta = delta + (0.576 * base.lostEnemyBuilding)
        end
    end

    -- Built enemy buildings
    if (base.builtEnemyBuilding or 0) > 0 then
        if currentTemperament > 0 then
            delta = delta - (0.261952 * base.builtEnemyBuilding)
        else
            delta = delta + (0.261952 * base.builtEnemyBuilding)
        end
    else
        delta = delta - 2.777088
    end

    -- Rocket launched
    if (base.rocketLaunched or 0) > 0 then
        delta = delta + (27.76 * base.rocketLaunched)
    end

    -- Ion cannon blasts
    if (base.ionCannonBlasts or 0) > 0 then
        delta = delta + (13.924864 * base.ionCannonBlasts)
    end

    -- Artillery blasts
    if (base.artilleryBlasts or 0) > 0 then
        delta = delta + (13.924864 * base.artilleryBlasts)
    end

    return delta * (rateModifier or 1)
end

--- Select next AI state based on temperament and settings.
-- Pure state machine: temperament bands × random roll → state.
-- @param params table with: temperament (0-1), roll (0-1), enabledMigration,
--   raidAIToggle, siegeAIToggle, peacefulAIToggle
-- @return string: new AI state constant
function Base.selectState(params)
    local temp = params.temperament or 0
    local roll = params.roll or 0
    local migration = params.enabledMigration
    local raid = params.raidAIToggle
    local siege = params.siegeAIToggle
    local peaceful = params.peacefulAIToggle

    if temp < 0.05 then
        if migration then
            if roll < 0.30 then return BASE_AI_STATE_MIGRATING
            elseif roll < 0.50 and raid then return BASE_AI_STATE_RAIDING
            elseif siege then return BASE_AI_STATE_SIEGE
            else return BASE_AI_STATE_MIGRATING
            end
        else
            if raid then
                if roll < 0.70 then return BASE_AI_STATE_RAIDING
                else return BASE_AI_STATE_AGGRESSIVE
                end
            else
                return BASE_AI_STATE_AGGRESSIVE
            end
        end

    elseif temp < 0.20 then
        if migration then
            if roll < 0.4 then return BASE_AI_STATE_MIGRATING
            elseif roll < 0.55 and raid then return BASE_AI_STATE_RAIDING
            elseif siege then return BASE_AI_STATE_SIEGE
            else return BASE_AI_STATE_MIGRATING
            end
        else
            if raid then
                if roll < 0.40 then return BASE_AI_STATE_AGGRESSIVE
                else return BASE_AI_STATE_RAIDING
                end
            else
                return BASE_AI_STATE_AGGRESSIVE
            end
        end

    elseif temp < 0.40 then
        if migration then
            if roll < 0.2 and raid then return BASE_AI_STATE_RAIDING
            elseif roll < 0.2 then return BASE_AI_STATE_AGGRESSIVE
            elseif roll < 0.8 then return BASE_AI_STATE_MIGRATING
            elseif peaceful then return BASE_AI_STATE_PEACEFUL
            else return BASE_AI_STATE_MIGRATING
            end
        else
            if roll < 0.3 then return BASE_AI_STATE_AGGRESSIVE
            elseif roll < 0.6 and raid then return BASE_AI_STATE_RAIDING
            elseif roll < 0.6 then return BASE_AI_STATE_AGGRESSIVE
            elseif peaceful then return BASE_AI_STATE_PEACEFUL
            else return BASE_AI_STATE_AGGRESSIVE
            end
        end

    elseif temp < 0.60 then
        if roll < 0.4 then return BASE_AI_STATE_AGGRESSIVE
        elseif roll < 0.5 and raid then return BASE_AI_STATE_RAIDING
        elseif roll < 0.75 and peaceful then return BASE_AI_STATE_PEACEFUL
        else
            if migration then return BASE_AI_STATE_MIGRATING
            else return BASE_AI_STATE_AGGRESSIVE
            end
        end

    elseif temp < 0.80 then
        if roll < 0.4 then return BASE_AI_STATE_AGGRESSIVE
        elseif roll < 0.6 then return BASE_AI_STATE_ONSLAUGHT
        elseif roll < 0.8 then return BASE_AI_STATE_RAIDING
        elseif peaceful then return BASE_AI_STATE_PEACEFUL
        else return BASE_AI_STATE_AGGRESSIVE
        end

    elseif temp < 0.95 then
        if migration and raid then
            if roll < 0.20 and siege then return BASE_AI_STATE_SIEGE
            elseif roll < 0.45 then return BASE_AI_STATE_RAIDING
            elseif roll < 0.85 then return BASE_AI_STATE_ONSLAUGHT
            else return BASE_AI_STATE_AGGRESSIVE
            end
        elseif migration then
            if roll < 0.20 and siege then return BASE_AI_STATE_SIEGE
            elseif roll < 0.75 then return BASE_AI_STATE_ONSLAUGHT
            else return BASE_AI_STATE_AGGRESSIVE
            end
        elseif raid then
            if roll < 0.45 then return BASE_AI_STATE_ONSLAUGHT
            elseif roll < 0.75 then return BASE_AI_STATE_RAIDING
            else return BASE_AI_STATE_AGGRESSIVE
            end
        else
            if roll < 0.65 then return BASE_AI_STATE_ONSLAUGHT
            else return BASE_AI_STATE_AGGRESSIVE
            end
        end

    else -- temp >= 0.95
        if migration and raid then
            if roll < 0.30 and siege then return BASE_AI_STATE_SIEGE
            elseif roll < 0.65 then return BASE_AI_STATE_RAIDING
            else return BASE_AI_STATE_ONSLAUGHT
            end
        elseif migration then
            if roll < 0.30 and siege then return BASE_AI_STATE_SIEGE
            else return BASE_AI_STATE_ONSLAUGHT
            end
        elseif raid then
            if roll < 0.45 then return BASE_AI_STATE_ONSLAUGHT
            else return BASE_AI_STATE_RAIDING
            end
        else
            return BASE_AI_STATE_ONSLAUGHT
        end
    end
end

return Base
