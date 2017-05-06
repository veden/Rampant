local interop = {}

function interop.addAIPoints(value)
    global.natives.points = global.natives.points + value
end

function interop.getAIPoints()
    return global.natives.points
end

function interop.changeState(aiState)
    global.natives.state = aiState
end

function interop.getState()
    return global.natives.state
end

function interop.getNextStateTick()
    return global.natives.stateTick
end

function interop.getMaxWaveSize()
    return global.natives.attackMaxWaveSize
end

function interop.getThresholds()
    return global.natives.attackThresholdMin, global.natives.attackThresholdMax
end

function interop.changeMaxWaveSize(waveSize)
    global.natives.attackMaxWaveSize = waveSize
end

function interop.changeThreshold(min, max)
    global.natives.attackThresholdMin = min
    global.natives.attackThresholdMax = max
    global.natives.attackThresholdRange = max - min
end

function interop.changePlayerThreshold(value)
    global.natives.attackPlayerThreshold = value
end

function interop.getPlayerThreshold()
    return global.natives.attackPlayerThreshold
end

function interop.changeAttackUsePollution(bool)
    global.natives.attackUsePollution = bool
end

function interop.changeAttackUsePlayer(bool)
    global.natives.attackUsePlayer = bool
end

function interop.getAttackUsePollution()
    return global.natives.attackUsePollution
end

function interop.getAttackUsePlayer()
    return global.natives.attackUsePlayer
end

return interop
