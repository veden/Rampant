if playerUtilsG then
    return playerUtilsG
end
local playerUtils = {}

-- imports

-- imported functions

-- module code

function playerUtils.validPlayer(player)
    if player and player.valid then
        local char = player.character
        return char and char.valid
    end
    return false
end

playerUtilsG = playerUtils
return playerUtils
