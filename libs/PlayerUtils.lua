local playerUtils = {}

-- imports

local mapUtils = require("MapUtils")

-- imported functions

local euclideanDistanceNamed = mapUtils.euclideanDistanceNamed

-- module code

function playerUtils.validPlayer(player)
    return (player ~= nil) and player.connected and (player.character ~= nil) and player.character.valid and (player.character.surface.index == 1)
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
