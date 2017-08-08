local vanillaBuildings = require("prototypes/buildings/UpdatesVanilla")

if settings.startup["rampant-addWallResistanceAcid"].value then
    vanillaBuildings.addWallAcidResistance()
end
