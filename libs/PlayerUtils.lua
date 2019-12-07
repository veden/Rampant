if playerUtilsG then
    return playerUtilsG
end
local playerUtils = {}

-- imports

-- imported functions

-- module code

function playerUtils.validPlayer(player, natives)
    if player and player.valid then
        local char = player.character
        return char and char.valid and (char.surface.index == natives.activeSurface)
    end
    return false
end

playerUtilsG = playerUtils
return playerUtils
