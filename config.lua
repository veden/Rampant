local config = {}

local mathUtils = require("libs/MathUtils")
local gaussianRandomRange = mathUtils.gaussianRandomRange

--[[
    turns off homing projectiles for worms and spitters
]]
config.useDumbProjectiles = true

--[[
    the attackWaveGenerationUse* options are used to score chunks with biter nests that will generate a Rampant attack wave.
    Pollution, the vanilla pollution mechanic (shown on the minimap).
    Player Proximity, if a player moves near a biter nest there is a chance for the nest to spawn attack waves (not shown on the minimap).
    switching all to false will turn off Rampant biter waves
    DOES NOT affect vanilla biters waves
--]]
config.attackWaveGenerationUsePollution = true
config.attackWaveGenerationUsePlayerProximity = true

--[[
    attackWaveGenerationThreshold is the score that the attackWaveGenerationUse* has to reach in order for an attack wave to spawn.
    increasing this will reduce the radius of attack wave generation.
    DOES NOT affect vanilla biters waves
    scaling linearly with evolution factor
    starts 20 @ 0.0 evolution
    ends 0 @ 100.0 evolution
    default max is 20
    default min is 0
--]]
config.attackWaveGenerationThresholdMax = 20
config.attackWaveGenerationThresholdMin = 0

--[[
    attackWaveMaxSize is the largest size that can be initially spawned by Rampant
--]]
config.attackWaveMaxSize = 150

--[[
    attackWaveScaling is used to calculate the attack wave size from the evolutionFactor
    default is config.attackWaveMaxSize * (evolutionFactor ^ 1.666667)
    DOES NOT affect vanilla biters waves
--]]
config.attackWaveScaling = function (evolutionFactor)
    return math.ceil(gaussianRandomRange(config.attackWaveMaxSize * (evolutionFactor ^ 1.66667),
					 (config.attackWaveMaxSize * 0.5) * 0.333,
					 1,
					 config.attackWaveMaxSize + (config.attackWaveMaxSize * 0.25)))
end

return config


