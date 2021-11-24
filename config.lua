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
    default is universe.attackWaveMaxSize * (evolutionFactor ^ 1.666667)
    DOES NOT affect vanilla biters waves
--]]
config.attackWaveScaling = function (universe)
    return mCeil(gaussianRandomRange(universe.attackWaveSize,
                                     universe.attackWaveDeviation,
                                     1,
                                     universe.attackWaveUpperBound))
end

config.settlerWaveScaling = function (universe)
    return mCeil(gaussianRandomRange(universe.settlerWaveSize,
                                     universe.settlerWaveDeviation,
                                     universe.expansionMinSize,
                                     universe.expansionMaxSize))
end

return config


