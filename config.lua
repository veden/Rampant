local config = {}

-- imported

local mathUtils = require("libs/MathUtils")

-- imported functions

local gaussianRandomRange = mathUtils.gaussianRandomRange
local mCeil = math.ceil

-- configurations

--[[
    attackWaveScaling is used to calculate the attack wave size from the evolutionFactor
    default is natives.attackWaveMaxSize * (evolutionFactor ^ 1.666667)
    DOES NOT affect vanilla biters waves
--]]
config.attackWaveScaling = function (natives)
    return mCeil(gaussianRandomRange(natives.attackWaveSize,
				     natives.attackWaveDeviation,
				     natives.attackWaveLowerBound,
				     natives.attackWaveUpperBound))
end

return config


