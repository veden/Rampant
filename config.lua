local config = {}

local mathUtils = require("libs/MathUtils")
local gaussianRandomRange = mathUtils.gaussianRandomRange

--[[
    Causes buildings to regenerate and become untargetable after they are destroyed by biters
--]]
config.safeBuildings = settings.startup["rampant-safeBuildings"].value
config.safeEntities = {}
config.safeEntityName = {}
config.safeEntities["curved-rail"] = settings.startup["rampant-safeBuildings-curvedRail"].value
config.safeEntities["straight-rail"] = settings.startup["rampant-safeBuildings-straightRail"].value
config.safeEntityName["big-electric-pole"] = settings.startup["rampant-safeBuildings-bigElectricPole"].value

--[[
    attackWaveScaling is used to calculate the attack wave size from the evolutionFactor
    default is config.attackWaveMaxSize * (evolutionFactor ^ 1.666667)
    DOES NOT affect vanilla biters waves
--]]
config.attackWaveScaling = function (evolutionFactor)
    local attackWaveMaxSize = settings.startup["rampant-attackWaveMaxSize"].value
    return math.ceil(gaussianRandomRange(attackWaveMaxSize * (evolutionFactor ^ 1.66667),
					 (attackWaveMaxSize * 0.5) * 0.333,
					 1,
					 attackWaveMaxSize + (attackWaveMaxSize * 0.25)))
end

return config


