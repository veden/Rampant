local vanillaUpdates = {}

function vanillaUpdates.addWallAcidResistance()
    local walls = data.raw["wall"]

    for _,wall in pairs(walls) do
	local foundAcid = false
	for _,resistance in pairs(wall.resistances) do
	    if resistance.type == "acid" then
		resistance.percent = 60
		foundAcid = true
		break
	    end
	end
	if not foundAcid then
	    wall.resistances[#wall.resistances+1] = {type="acid",percent=60}
	end
    end
end

return vanillaUpdates
