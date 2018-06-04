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


-- data:extend({
-- 	{
-- 	    type = "item",
-- 	    name = "accumulator2",
-- 	    icon = "__base__/graphics/icons/accumulator.png",
-- 	    icon_size = 32,
-- 	    flags = {"goes-to-quickbar"},
-- 	    subgroup = "energy",
-- 	    order = "e[accumulator]-a[accumulator]",
-- 	    place_result = "electric-energy-interface2",
-- 	    stack_size = 50
-- 	},
	
-- 	{
-- 	    type = "radar",
-- 	    name = "electric-energy-interface2",
-- 	    icon = "__base__/graphics/icons/radar.png",
-- 	    icon_size = 32,
-- 	    flags = {"placeable-enemy"},
-- 	    minable = {hardness = 0.2, mining_time = 0.5, result = "radar"},
-- 	    max_health = 500,
-- 	    corpse = "small-remnants",
-- 	    collision_box = {{-1.2, -1.2}, {1.2, 1.2}},
-- 	    selection_box = {{-1.5, -1.5}, {1.5, 1.5}},
-- 	    energy_per_sector = "10MJ",
-- 	    has_backer_name = false,
-- 	    max_distance_of_sector_revealed = 0,
-- 	    max_distance_of_nearby_sector_revealed = 0,
-- 	    energy_per_nearby_scan = "250kJ",
-- 	    energy_source =
-- 		{
-- 		    type = "electric",
-- 		    usage_priority = "primary-input"
-- 		},
-- 	    energy_usage = "500kW",
-- 	    pictures =
-- 		{
-- 		    filename = "__Rampant__/graphics/entities/thief/crystal.png",
-- 		    priority = "low",
-- 		    width = 128,
-- 		    height = 128,
-- 		    apply_projection = false,
-- 		    direction_count = 32,
-- 		    animation_speed = 0.5,
-- 		    line_length = 8
-- 		},
-- 	    integration_patch =
-- 		{
-- 		    filename = "__base__/graphics/entity/radar/radar-integration.png",
-- 		    priority = "low",
-- 		    width = 119,
-- 		    height = 108,
-- 		    apply_projection = false,
-- 		    direction_count = 1,
-- 		    repeat_count = 64,
-- 		    line_length = 1,
-- 		    shift = util.by_pixel(1.5, 4),
-- 		    hr_version =
-- 			{
-- 			    filename = "__base__/graphics/entity/radar/hr-radar-integration.png",
-- 			    priority = "low",
-- 			    width = 238,
-- 			    height = 216,
-- 			    apply_projection = false,
-- 			    direction_count = 1,
-- 			    repeat_count = 64,
-- 			    line_length = 1,
-- 			    shift = util.by_pixel(1.5, 4),
-- 			    scale = 0.5
-- 			}
-- 		},

-- 	    vehicle_impact_sound =  { filename = "__base__/sound/car-metal-impact.ogg", volume = 0.65 },
-- 	    working_sound =
-- 		{
-- 		    sound = {
-- 			{
-- 			    filename = "__base__/sound/radar.ogg"
-- 			}
-- 		    },
-- 		    apparent_volume = 2,
-- 		},
-- 	    radius_minimap_visualisation_color = { r = 0.059, g = 0.092, b = 0.8, a = 0.275 },
-- 	}

-- })
