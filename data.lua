require("prototypes/enemies/AttackAcidBall")
require("prototypes/enemies/AttackAcidFlame")

require("prototypes/buildings/tunnel")
require("prototypes/buildings/UnitSpawners")

require("prototypes/tile/fillableDirt")



if settings.startup["rampant-newEnemies"].value then
    require("BuildSwarm")
end

-- require("prototypes/enemies/UnitSuicideBiters")
-- require("prototypes/enemies/UnitFireSpitters")
-- require("prototypes/enemies/UnitTendril")
