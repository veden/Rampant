local config = {}

--[[
    the attackWaveGenerationUse* options are used to score chunks with biter nests that will generate a Rampant attack wave.
    Pollution, the vanilla pollution mechanic (shown on the minimap).
    Player Proximity, if a player moves near a biter nest there is a chance for the nest to spawn attack waves (not shown on the minimap).
    Player Base Proximity, if the player builds near biters nest there is a chance for the nest to spawn attack waves (not shown on the minimap).
    Player Defense Proximity, if the player builds defense near biters nest there is a chance for the nest to spawn attack waves (not shown on the minimap).
    switching all to false will turn off Rampant biter waves, does not turn off vanilla biter waves.
--]]
config.attackWaveGenerationUsePollution = true
config.attackWaveGenerationUsePlayerProximity = true
config.attackWaveGenerationUsePlayerBaseProximity = true
config.attackWaveGenerationUsePlayerDefenseProximity = true

--[[
    attackWaveGenerationThreshold is the score that the attackWaveGenerationUse* has to reach in order for an attack wave to spawn.
    increasing this will reduce the radius of attack wave generation.
    default number is 70
--]]
config.attackWaveGenerationThreshold = 70

--[[
    attackWaveScaling is used to calculate the attack wave size from the evolutionFactor
    default is 150 * (evolutionFactor ^ 1.666667)
    150 is the max group size
--]]
config.attackWaveScaling = function (evolutionFactor)
    return math.ceil(150 * (evolutionFactor ^ 1.666667))
end

return config


