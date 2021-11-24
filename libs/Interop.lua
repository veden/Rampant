if interopG then
    return interopG
end
local interop = {}

-- function interop.addAIPoints(value)
--     global.natives.points = global.natives.points + value
-- end

-- function interop.getAIPoints()
--     return global.natives.points
-- end

-- function interop.setNocturnalMode(flag)
--     global.natives.aiNocturnalMode = flag
-- end

-- function interop.getNocturnalMode()
--     return global.natives.aiNocturnalMode
-- end

-- function interop.setPointsPerCycleScaling(scale)
--     global.natives.aiPointsScaler = scale
-- end

-- function interop.getPointsPerCycleScaling()
--     return global.natives.aiPointsScaler
-- end

-- function interop.changeState(aiState)
--     global.natives.state = aiState
-- end

-- function interop.getState()
--     return global.natives.state
-- end

-- function interop.getNextStateTick()
--     return global.natives.stateTick
-- end

-- function interop.getMaxWaveSize()
--     return global.natives.attackWaveMaxSize
-- end

-- function interop.getThresholds()
--     return global.natives.attackThresholdMin, global.natives.attackThresholdMax
-- end

-- function interop.changeMaxWaveSize(waveSize)
--     global.natives.attackWaveMaxSize = waveSize
-- end

-- function interop.getSettlerCooldown()
--     return global.natives.settlerCooldown
-- end

-- function interop.getSettlerWaveSize()
--     return global.natives.settlerWaveSize
-- end

-- function interop.changeThreshold(min, max)
--     global.natives.attackThresholdMin = min
--     global.natives.attackThresholdMax = max
--     global.natives.attackThresholdRange = max - min
-- end

-- function interop.changePlayerThreshold(value)
--     global.natives.attackPlayerThreshold = value
-- end

-- function interop.getPlayerThreshold()
--     return global.natives.attackPlayerThreshold
-- end

-- function interop.changeAttackUsePollution(bool)
--     global.natives.attackUsePollution = bool
-- end

-- function interop.changeAttackUsePlayer(bool)
--     global.natives.attackUsePlayer = bool
-- end

-- function interop.getAttackUsePollution()
--     return global.natives.attackUsePollution
-- end

-- function interop.getAttackUsePlayer()
--     return global.natives.attackUsePlayer
-- end

-- function interop.registerUnitGroup(unitGroup, isSettler)
--     local squad = unitGroupUtils.createSquad(unitGroup.position, unitGroup.surface, unitGroup, isSettler)
--     global.natives.pendingAttack.len = global.natives.pendingAttack.len + 1
--     global.natives.pendingAttack[global.natives.pendingAttack.len] = squad
-- end

interopG = interop
return interop
