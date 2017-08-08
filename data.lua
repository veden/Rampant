require("prototypes/enemies/AttackAcidBall")
require("prototypes/enemies/AttackAcidFlame")

require("prototypes/buildings/tunnel")
require("prototypes/buildings/UnitSpawners")

require("prototypes/tile/fillableDirt")

require("prototypes/enemies/UnitSuicideBiters")
require("prototypes/enemies/UnitFireSpitters")
require("prototypes/enemies/UnitTendril")


if settings.startup["rampant-enableBuildings"].value then
    require("prototypes/buildings/ItemCollector")
end


