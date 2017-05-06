local config = {}

local mathUtils = require("libs/MathUtils")
local gaussianRandomRange = mathUtils.gaussianRandomRange

--[[
    attackWaveScaling is used to calculate the attack wave size from the evolutionFactor
    default is natives.attackWaveMaxSize * (evolutionFactor ^ 1.666667)
    DOES NOT affect vanilla biters waves
--]]
config.attackWaveScaling = function (evolutionFactor, natives)
    local attackWaveMaxSize = natives.attackWaveMaxSize
    return math.ceil(gaussianRandomRange(attackWaveMaxSize * (evolutionFactor ^ 1.66667),
					 (attackWaveMaxSize * 0.5) * 0.333,
					 1,
					 attackWaveMaxSize + (attackWaveMaxSize * 0.25)))
end

return config


