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
    local surface = map.surface
    local goodAI = (((base.stateAI == BASE_AI_STATE_AGGRESSIVE) and (base.sentAggressiveGroups < base.maxAggressiveGroups)) or
        (base.stateAI == BASE_AI_STATE_RAIDING) or
        (base.stateAI == BASE_AI_STATE_ONSLAUGHT) or
        (map.universe.raidAIToggle and (base.stateAI == BASE_AI_STATE_SIEGE) and (base.sentSiegeGroups >= base.maxSiegeGroups)))
    local notPeaceful = not surface.peaceful_mode
    local nocturalMode = map.universe.aiNocturnalMode
    local noctural = (not nocturalMode) or (nocturalMode and surface.darkness > 0.65)
    return goodAI and notPeaceful and noctural
end

function aiPredicates.canMigrate(map, base)
    local surface = map.surface
    local universe = map.universe
    local nocturalMode = universe.aiNocturnalMode
    local goodAI = (base.stateAI == BASE_AI_STATE_MIGRATING) or (base.stateAI == BASE_AI_STATE_SIEGE)
    local noctural = (not nocturalMode) or (nocturalMode and surface.darkness > 0.65)
    return goodAI and universe.expansion and not surface.peaceful_mode and noctural
end

aiPredicatesG = aiPredicates
return aiPredicates
