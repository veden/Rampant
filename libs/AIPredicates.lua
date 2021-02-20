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

function aiPredicates.canAttack(native, tick)
    local surface = native.surface
    local goodAI = (((native.state == AI_STATE_AGGRESSIVE) and (native.canAttackTick > tick)) or
            (native.state == AI_STATE_RAIDING) or
            (native.state == AI_STATE_ONSLAUGHT))
    local notPeaceful = not surface.peaceful_mode
    local noctural = (not native.aiNocturnalMode) or (native.aiNocturnalMode and surface.darkness > 0.65)
    return goodAI and notPeaceful and noctural
end

function aiPredicates.canMigrate(native)
    local surface = native.surface
    return ((native.state == AI_STATE_MIGRATING) or
            (native.state == AI_STATE_SIEGE))
        and native.expansion
        and not surface.peaceful_mode
        and ((not native.aiNocturnalMode) or
                (native.aiNocturnalMode and surface.darkness > 0.65))
end

aiPredicatesG = aiPredicates
return aiPredicates
