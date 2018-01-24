local acidBall = require("prototypes/utils/AttackBall")

if settings.startup["rampant-useDumbProjectiles"].value then
    acidBall.generateLegacy()
end

require("prototypes/buildings/ChunkScanner")

