if (aiPredicatesG) then
    return aiPredicatesG
end
local aiPredicates = {}

-- imports

local constants = require("Constants")

-- constants

local AI_STATE_RAIDING = constants.AI_STATE_RAIDING
local AI_STATE_AGGRESSIVE = constants.AI_STATE_AGGRESSIVE
local AI_STATE_MIGRATING = constants.AI_STATE_MIGRATING
local AI_STATE_SIEGE = constants.AI_STATE_SIEGE
local AI_STATE_ONSLAUGHT = constants.AI_STATE_ONSLAUGHT

-- imported functions

-- module code

function aiPredicates.canAttack(map, tick)
    local surface = map.surface
    local goodAI = (((map.state == AI_STATE_AGGRESSIVE) and (map.canAttackTick < tick)) or
            (map.state == AI_STATE_RAIDING) or
            (map.state == AI_STATE_ONSLAUGHT))
    local notPeaceful = not surface.peaceful_mode
    local nocturalMode = map.universe.aiNocturnalMode
    local noctural = (not nocturalMode) or (nocturalMode and surface.darkness > 0.65)
    return goodAI and notPeaceful and noctural
end

function aiPredicates.canMigrate(map)
    local surface = map.surface
    local universe = map.universe
    local nocturalMode = universe.aiNocturnalMode
    local goodAI = (map.state == AI_STATE_MIGRATING) or (map.state == AI_STATE_SIEGE)
    local noctural = (not nocturalMode) or (nocturalMode and surface.darkness > 0.65)
    return goodAI and universe.expansion and not surface.peaceful_mode and noctural
end

aiPredicatesG = aiPredicates
return aiPredicates
