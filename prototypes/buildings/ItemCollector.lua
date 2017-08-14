-- overlays

local radar = util.table.deepcopy(data.raw["radar"]["radar"])
radar.name = "item-collector-base-rampant"
radar.icon = "__Rampant__/graphics/icon/itemCollectorIcon.png"
radar.collision_box = {{-0.35, -0.35}, {0.35, 0.35}}
radar.selection_box = {{-0.485, -0.7}, {0.465, -0.1}}
radar.energy_per_sector = "27MJ"
radar.max_distance_of_nearby_sector_revealed = 1
radar.max_distance_of_sector_revealed = 0
radar.energy_per_nearby_scan = "27MJ"
radar.energy_usage = "450KW"
radar.flags[#radar.flags+1] = "not-deconstructable"
radar.pictures = {
    filename = "__Rampant__/graphics/entities/chest/itemCollector.png",
    priority = "low",
    width = 46,
    height = 49,
    apply_projection = false,
    direction_count = 64,
    line_length = 8,
    shift = {0.1875, -0.24}
}
radar.minable = nil

-- local particle = {
--     type = "explosion",
--     name = "item-collector-base-particle-rampant",
--     flags = {"not-on-map"},
--     animations =
-- 	{
-- 	    {
-- 		filename = "__Rampant__/graphics/entities/chest/itemCollectorParticle.png",
-- 		priority = "extra-high",
-- 		width = 46,
-- 		height = 49,
-- 		frame_count = 16,
-- 		line_length = 8,
-- 		animation_speed = 0.2
-- 	    }
-- 	},
--     light = {intensity = 1, size = 20, color = {r=1.0, g=1.0, b=1.0}},
--     smoke = "smoke-fast",
--     smoke_count = 0,
--     smoke_slow_down_factor = 1,
--     sound =
-- 	{
-- 	    aggregation =
-- 		{
-- 		    max_count = 1,
-- 		    remove = true
-- 		},
-- 	    variations =
-- 		{
-- 		    {
-- 			filename = "__base__/sound/fight/small-explosion-1.ogg",
-- 			volume = 0.75
-- 		    }
-- 		}
-- 	}
-- }

local radarOverlay = util.table.deepcopy(radar)
radarOverlay.name = "item-collector-base-overlay-rampant"
radarOverlay.pictures.filename = "__Rampant__/graphics/entities/chest/itemCollectorOverlay2.png"
radarOverlay.pictures.width = 2048
radarOverlay.pictures.height = 2048
radarOverlay.pictures.direction_count = 1
radarOverlay.pictures.line_length = 1
radarOverlay.pictures.shift[2] = 0.07
radarOverlay.pictures.hr_version = {
    filename = "__Rampant__/graphics/entities/chest/itemCollectorOverlay2.5.png",
    priority = "low",
    width = 3200,
    height = 3200,
    apply_projection = false,
    direction_count = 1,
    line_length = 1,
    shift = {0.1875, -0.24}
}

local chest = util.table.deepcopy(data.raw["container"]["steel-chest"])
chest.name = "item-collector-chest-rampant"
chest.picture = {
    filename = "__core__/graphics/empty.png",
    priority = "low",
    width = 46,
    height = 49,
    line_length = 1,
    shift = {0.1875, -0.2}
}
chest.selection_box = {{-0.485, -0.1}, {0.465, 0.6}}
chest.collision_mask = {}
chest.minable.result = "item-collector-base-rampant"


data:extend({
	radar,
	radarOverlay,
	chest-- ,
	-- particle
})

data:extend({

	{
	    type = "recipe",
	    name = "item-collector-base-rampant",
	    normal = {
		enabled = false,
		energy_required = 10,
		ingredients = {
		    {"steel-chest", 1},
		    {"accumulator", 1},
		    {"radar", 1}
		},
		result = "item-collector-base-rampant",
		requester_paste_multiplier = 4
	    },
	    expensive = {
		enabled = false,
		energy_required = 10,
		ingredients = {
		    {"steel-chest", 2},
		    {"accumulator", 2},
		    {"radar", 2}
		},
		result = "item-collector-base-rampant",
		requester_paste_multiplier = 4
	    }
	},

	{
	    type = "item",
	    name = "item-collector-base-rampant",
	    icon = "__Rampant__/graphics/icon/itemCollectorIcon.png",
	    flags = {"goes-to-quickbar"},
	    subgroup = "storage",
	    order = "a[items]-c[steel-collector]",
	    place_result = "item-collector-base-rampant",
	    stack_size = 50
	},

	{
	    type = "item",
	    name = "item-collector-base-overlay-rampant",
	    icon = "__Rampant__/graphics/icon/itemCollectorIcon.png",
	    flags = {"goes-to-quickbar"},
	    subgroup = "storage",
	    order = "a[items]-c[steel-collector]",
	    place_result = "item-collector-base-overlay-rampant",
	    stack_size = 50
	},
	
	{
	    type = "item",
	    name = "item-collector-chest-rampant",
	    icon = "__Rampant__/graphics/icon/itemCollectorIcon.png",
	    flags = {"goes-to-quickbar"},
	    subgroup = "storage",
	    order = "a[items]-c[steel-collector]",
	    place_result = "item-collector-chest-rampant",
	    stack_size = 50
	}
})

-- technology insertions

data:extend({
	{
	    type = "technology",
	    name = "short-range-electrodynamics-1-rampant",
	    icon = "__Rampant__/graphics/technology/itemCollectorTech.png",
	    icon_size = 128,
	    localised_name = {"technology-name.short-range-electrodynamics-1-rampant"},
	    effects =
		{
		    {
			type = "unlock-recipe",
			recipe = "item-collector-base-rampant"
		    }
		},
	    prerequisites = {"electric-energy-accumulators-1"},
	    unit =
		{
		    count = 200,
		    ingredients =
			{
			    {"science-pack-1", 1},
			    {"science-pack-2", 1}
			},
		    time = 22
		},
	    order = "c-e-a",
	},
})

