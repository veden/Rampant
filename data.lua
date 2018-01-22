local acidBall = require("prototypes/utils/AttackAcidBall")

if settings.startup["rampant-useDumbProjectiles"].value then
    acidBall.generateLegacy()
end

require("prototypes/buildings/ChunkScanner")

if settings.startup["rampant-newEnemies"].value then
    require("prototypes/Neutral")
    require("prototypes/Acid")
    require("prototypes/Physical")
    require("prototypes/Suicide")
    require("prototypes/Fire")
    require("prototypes/Inferno")
    require("prototypes/Nuclear")
    -- require("prototypes/Fast")
    -- require("prototypes/Troll")
    -- require("prototypes/Decaying")
    -- require("prototypes/Undying")
    -- require("prototypes/Wasp")
    -- require("prototypes/Laser")
    require("prototypes/Electric")
end
