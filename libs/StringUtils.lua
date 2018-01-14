local stringUtils = {}

function stringUtils.isRampant(str)
    return (string.sub(str,1,7) == "rampant")
end

function stringUtils.starts(str, start)
    return (string.sub(str,1,string.len(start)) == start)
end

function stringUtils.ends(str, tail)
    return (tail == '') or (string.sub(str, -string.len(tail)) == tail)
end

return stringUtils
