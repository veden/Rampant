local config = {}

-- imported

local mathUtils = require("libs/MathUtils")

-- imported functions

local gaussianRandomRange = mathUtils.gaussianRandomRange
local mCeil = math.ceil


-- automatic mod detection

config.ionCannonPresent = settings.startup["ion-cannon-radius"] ~= nil

-- configurations

--[[
    attackWaveScaling is used to calculate the attack wave size from the evolutionFactor
    default is natives.attackWaveMaxSize * (evolutionFactor ^ 1.666667)
    DOES NOT affect vanilla biters waves
--]]
config.attackWaveScaling = function (natives)
    return mCeil(gaussianRandomRange(natives.attackWaveSize,
				     natives.attackWaveDeviation,
				     1,
				     natives.attackWaveUpperBound))
end

config.settlerWaveScaling = function (natives)
    return mCeil(gaussianRandomRange(natives.settlerWaveSize,
				     natives.settlerWaveDeviation,
				     natives.expansionMinSize,
				     natives.expansionMaxSize))
end

return config


