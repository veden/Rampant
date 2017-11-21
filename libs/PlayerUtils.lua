local playerUtils = {}

-- imports

local mathUtils = require("MathUtils")

-- imported functions

local euclideanDistanceNamed = mathUtils.euclideanDistanceNamed

-- module code

function playerUtils.validPlayer(player)
    return player and player.valid and player.connected and player.character and player.character.valid and (player.character.surface.index == 1)
end

function playerUtils.playersWithinProximityToPosition(players, position, distance)
    for _,player in pairs(players) do
	if (player ~= nil) and player.connected and (player.character ~= nil) and player.character.valid and (player.character.surface.index == 1) then
	    if (euclideanDistanceNamed(player.character.position, position) < distance) then
		return true
	    end
	end
    end
    return false
end

return playerUtils
