-- import

local acidBall = require("prototypes/utils/AttackBall")
local colorUtils = require("prototypes/utils/ColorUtils")
local smokeUtils = require("SmokeUtils")

-- imported functions

local makeSmokeSoft = smokeUtils.makeSmokeSoft
local makeSmokeWithGlow = smokeUtils.makeSmokeWithGlow
local makeSmokeWithoutGlow = smokeUtils.makeSmokeWithoutGlow
local makeSmokeAddingFuel = smokeUtils.makeSmokeAddingFuel

local makeColor = colorUtils.makeColor

-- module code

makeSmokeSoft({name="the", softSmokeTint=makeColor(0.3, 0.75, 0.3, 0.1)})
makeSmokeWithGlow({name="the", smokeWithGlowTint=makeColor(0.3, 0.75, 0.3, 0.1)})
makeSmokeWithoutGlow({name="the", smokeWithoutGlowTint=makeColor(0.3, 0.75, 0.3, 0.1)})
makeSmokeAddingFuel({name="the"})

if settings.startup["rampant-useDumbProjectiles"].value then
    acidBall.generateLegacy()
end

require("prototypes/buildings/ChunkScanner")

if not data.raw["corpse"]["acid-splash-purple"] then
    data:extend({
            {
                type = "corpse",
                name = "acid-splash-purple",
                flags = {"not-on-map"},
                time_before_removed = 60 * 30,
                final_render_layer = "corpse",
                splash =
                    {
                        {
                            filename = "__Rampant__/graphics/acid-splash-purple/splash-1.png",
                            line_length = 5,
                            width = 199,
                            height = 159,
                            frame_count = 20,
                            shift = {0.484375, -0.171875}
                        },
                        {
                            filename = "__Rampant__/graphics/acid-splash-purple/splash-2.png",
                            line_length = 5,
                            width = 238,
                            height = 157,
                            frame_count = 20,
                            shift = {0.8125, -0.15625}
                        },
                        {
                            filename = "__Rampant__/graphics/acid-splash-purple/splash-3.png",
                            line_length = 5,
                            width = 240,
                            height = 162,
                            frame_count = 20,
                            shift = {0.71875, -0.09375}
                        },
                        {
                            filename = "__Rampant__/graphics/acid-splash-purple/splash-4.png",
                            line_length = 5,
                            width = 241,
                            height = 146,
                            frame_count = 20,
                            shift = {0.703125, -0.375}
                        }
                    },
                splash_speed = 0.03
            }
    })
end
