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


if (aiPredicatesG) then
    return aiPredicatesG
end
local aiPredicates = {}

--

local universe

-- imports

local constants = require("Constants")

-- constants

local BASE_AI_STATE_RAIDING = constants.BASE_AI_STATE_RAIDING
local BASE_AI_STATE_AGGRESSIVE = constants.BASE_AI_STATE_AGGRESSIVE
local BASE_AI_STATE_MIGRATING = constants.BASE_AI_STATE_MIGRATING
local BASE_AI_STATE_SIEGE = constants.BASE_AI_STATE_SIEGE
local BASE_AI_STATE_ONSLAUGHT = constants.BASE_AI_STATE_ONSLAUGHT

-- imported functions

-- module code

function aiPredicates.canAttack(map, base)
    local isAggressive = ((base.stateAI == BASE_AI_STATE_AGGRESSIVE)
        and (base.sentAggressiveGroups < base.maxAggressiveGroups))
    local isRaiding = (base.stateAI == BASE_AI_STATE_RAIDING)
    local isOnslaught = (base.stateAI == BASE_AI_STATE_ONSLAUGHT)
    local isRaidSieging = universe.raidAIToggle
        and (base.stateAI == BASE_AI_STATE_SIEGE)
        and (base.sentExpansionGroups >= base.maxExpansionGroups)
    local goodAI = isAggressive or isRaiding or isOnslaught or isRaidSieging
    if not goodAI then
        return false
    end
    local surface = map.surface
    if surface.peaceful_mode then
        return false
    end
    local nocturalMode = universe.aiNocturnalMode
    local noctural = (not nocturalMode) or (nocturalMode and surface.darkness > 0.65)
    if not noctural then
        return false
    end
    return true
end

function aiPredicates.canMigrate(map, base)
    local badAIState = (base.stateAI ~= BASE_AI_STATE_MIGRATING) and (base.stateAI ~= BASE_AI_STATE_SIEGE)
    if badAIState then
        return false
    end
    if not universe.expansion then
        return false
    end
    local surface = map.surface
    if surface.peaceful_mode then
        return false
    end
    local nocturalMode = universe.aiNocturnalMode
    local noctural = (not nocturalMode) or (nocturalMode and surface.darkness > 0.65)
    if not noctural then
        return false
    end
    return true
end

function aiPredicates.init(universeGlobal)
    universe = universeGlobal
end

aiPredicatesG = aiPredicates
return aiPredicates
