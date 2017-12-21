local vanillaBuildings = require("prototypes/buildings/UpdatesVanilla")

if settings.startup["rampant-enableSwarm"] then    
    for k, unit in pairs(data.raw["unit"]) do
	if (string.find(k, "biter") or string.find(k, "spitter")) and unit.collision_box then
	    unit.collision_box = {
		{unit.collision_box[1][1] * 0.40, unit.collision_box[1][2] * 0.40},
		{unit.collision_box[2][1] * 0.40, unit.collision_box[2][2] * 0.40}
	    }
	end
    end
end

for k, cliff in pairs(data.raw["cliff"]) do
    if not cliff.collision_mask then
	cliff.collision_mask = {}
    end
    cliff.collision_mask[#cliff.collision_mask+1] = "water-tile" 

end

if settings.startup["rampant-addWallResistanceAcid"].value then
    vanillaBuildings.addWallAcidResistance()
end
