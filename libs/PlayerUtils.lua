if playerUtilsG then
    return playerUtilsG
end
local playerUtils = {}

-- imports

local mathUtils = require("MathUtils")

-- imported functions

local euclideanDistanceNamed = mathUtils.euclideanDistanceNamed

-- module code

function playerUtils.validPlayer(player, natives)
    if player and player.valid and player.connected then
        local char = player.character
        return char and char.valid and (char.surface.index == natives.activeSurface)            
    end
    return false
end

function playerUtils.playersWithinProximityToPosition(players, position, distance, natives)
    for _,player in pairs(players) do
        if player and player.valid and player.connected then
            local char = player.character
            if ((char and char.valid and (char.surface.index == natives.activeSurface)) and
                (euclideanDistanceNamed(char.position, position) < distance)) then                
                return true
            end
        end
    end
    return false
end

playerUtilsG = playerUtils
return playerUtils
