local stringUtils = {}

local sFind = string.find

function stringUtils.isRampant(str)
    return sFind(str, "rampant", -#"rampant")
end

function stringUtils.isSpawnerEgg(str)
    return sFind(str, "spawner", -#"spawner")
end

return stringUtils
