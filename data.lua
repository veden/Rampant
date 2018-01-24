local acidBall = require("prototypes/utils/AttackBall")

if not settings.startup["rampant-newEnemies"].value and settings.startup["rampant-useDumbProjectiles"].value then
    acidBall.generateLegacy()
end

require("prototypes/buildings/ChunkScanner")

