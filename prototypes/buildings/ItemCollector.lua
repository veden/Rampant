-- overlays

-- data:extend({

-- 	-- {
-- 	--     type = "logistic-container",
-- 	--     name = "logistic-collector-active-provider-overlay-rampant",
-- 	--     icon = "__base__/graphics/icons/logistic-chest-active-provider.png",
-- 	--     flags = {"placeable-player", "player-creation"},
-- 	--     minable = {hardness = 0.2, mining_time = 0.5, result = "logistic-collector-active-provider-overlay-rampant"},
-- 	--     max_health = 350,
-- 	--     corpse = "small-remnants",
-- 	--     collision_box = {{-0.35, -0.35}, {0.35, 0.35}},
-- 	--     selection_box = {{-0.5, -0.5}, {0.5, 0.5}},
-- 	--     resistances =
-- 	-- 	{
-- 	-- 	    {
-- 	-- 		type = "fire",
-- 	-- 		percent = 90
-- 	-- 	    },
-- 	-- 	    {
-- 	-- 		type = "impact",
-- 	-- 		percent = 60
-- 	-- 	    }
-- 	-- 	},
-- 	--     fast_replaceable_group = "container",
-- 	--     inventory_size = 48,
-- 	--     logistic_mode = "active-provider",
-- 	--     open_sound = { filename = "__base__/sound/metallic-chest-open.ogg", volume=0.65 },
-- 	--     close_sound = { filename = "__base__/sound/metallic-chest-close.ogg", volume = 0.7 },
-- 	--     vehicle_impact_sound =  { filename = "__base__/sound/car-metal-impact.ogg", volume = 0.65 },
-- 	--     picture =
-- 	-- 	{
-- 	-- 	    filename = "__Rampant__/graphics/entities/chest/logisticActiveCollectorOverlay.png",
-- 	-- 	    priority = "extra-high",
-- 	-- 	    width = 1600,
-- 	-- 	    height = 1600,
-- 	-- 	    shift = {0.09375, 0}
-- 	-- 	},
-- 	--     circuit_wire_connection_point =
-- 	-- 	{
-- 	-- 	    shadow =
-- 	-- 		{
-- 	-- 		    red = {0.734375, 0.453125},
-- 	-- 		    green = {0.609375, 0.515625},
-- 	-- 		},
-- 	-- 	    wire =
-- 	-- 		{
-- 	-- 		    red = {0.40625, 0.21875},
-- 	-- 		    green = {0.40625, 0.375},
-- 	-- 		}
-- 	-- 	},
-- 	--     circuit_wire_max_distance = 9,
-- 	--     circuit_connector_sprites = get_circuit_connector_sprites({0.1875, 0.15625}, nil, 18),
-- 	-- },

-- 	-- {
-- 	--     type = "logistic-container",
-- 	--     name = "logistic-collector-passive-provider-overlay-rampant",
-- 	--     icon = "__base__/graphics/icons/logistic-chest-passive-provider.png",
-- 	--     flags = {"placeable-player", "player-creation"},
-- 	--     minable = {hardness = 0.2, mining_time = 0.5, result = "logistic-collector-passive-provider-overlay-rampant"},
-- 	--     max_health = 350,
-- 	--     corpse = "small-remnants",
-- 	--     collision_box = {{-0.35, -0.35}, {0.35, 0.35}},
-- 	--     selection_box = {{-0.5, -0.5}, {0.5, 0.5}},
-- 	--     resistances =
-- 	-- 	{
-- 	-- 	    {
-- 	-- 		type = "fire",
-- 	-- 		percent = 90
-- 	-- 	    },
-- 	-- 	    {
-- 	-- 		type = "impact",
-- 	-- 		percent = 60
-- 	-- 	    }
-- 	-- 	},
-- 	--     fast_replaceable_group = "container",
-- 	--     inventory_size = 48,
-- 	--     logistic_mode = "active-provider",
-- 	--     open_sound = { filename = "__base__/sound/metallic-chest-open.ogg", volume=0.65 },
-- 	--     close_sound = { filename = "__base__/sound/metallic-chest-close.ogg", volume = 0.7 },
-- 	--     vehicle_impact_sound =  { filename = "__base__/sound/car-metal-impact.ogg", volume = 0.65 },
-- 	--     picture =
-- 	-- 	{
-- 	-- 	    filename = "__Rampant__/graphics/entities/chest/logisticPassiveCollectorOverlay.png",
-- 	-- 	    priority = "extra-high",
-- 	-- 	    width = 1600,
-- 	-- 	    height = 1600,
-- 	-- 	    shift = {0.09375, 0}
-- 	-- 	},
-- 	--     circuit_wire_connection_point =
-- 	-- 	{
-- 	-- 	    shadow =
-- 	-- 		{
-- 	-- 		    red = {0.734375, 0.453125},
-- 	-- 		    green = {0.609375, 0.515625},
-- 	-- 		},
-- 	-- 	    wire =
-- 	-- 		{
-- 	-- 		    red = {0.40625, 0.21875},
-- 	-- 		    green = {0.40625, 0.375},
-- 	-- 		}
-- 	-- 	},
-- 	--     circuit_wire_max_distance = 9,
-- 	--     circuit_connector_sprites = get_circuit_connector_sprites({0.1875, 0.15625}, nil, 18),
-- 	-- },
	
-- 	-- {
-- 	--     type = "container",
-- 	--     name = "steel-collector-overlay-rampant",
-- 	--     icon = "__base__/graphics/icons/steel-chest.png",
-- 	--     flags = {"placeable-neutral", "player-creation"},
-- 	--     minable = {mining_time = 1, result = "steel-collector-overlay-rampant"},
-- 	--     max_health = 350,
-- 	--     corpse = "small-remnants",
-- 	--     open_sound = { filename = "__base__/sound/metallic-chest-open.ogg", volume=0.65 },
-- 	--     close_sound = { filename = "__base__/sound/metallic-chest-close.ogg", volume = 0.7 },
-- 	--     resistances =
-- 	-- 	{
-- 	-- 	    {
-- 	-- 		type = "fire",
-- 	-- 		percent = 90
-- 	-- 	    },
-- 	-- 	    {
-- 	-- 		type = "impact",
-- 	-- 		percent = 60
-- 	-- 	    }
-- 	-- 	},
-- 	--     collision_box = {{-0.35, -0.35}, {0.35, 0.35}},
-- 	--     selection_box = {{-0.5, -0.5}, {0.5, 0.5}},
-- 	--     fast_replaceable_group = "container",
-- 	--     inventory_size = 48,
-- 	--     vehicle_impact_sound =  { filename = "__base__/sound/car-metal-impact.ogg", volume = 0.65 },
-- 	--     picture =
-- 	-- 	{
-- 	-- 	    filename = "__Rampant__/graphics/entities/chest/steelCollectorOverlay.png",
-- 	-- 	    priority = "extra-high",
-- 	-- 	    width = 1000,
-- 	-- 	    height = 1000,
-- 	-- 	    shift = {0.1875, 0}
-- 	-- 	},
-- 	--     circuit_wire_connection_point =
-- 	-- 	{
-- 	-- 	    shadow =
-- 	-- 		{
-- 	-- 		    red = {0.734375, 0.453125},
-- 	-- 		    green = {0.609375, 0.515625},
-- 	-- 		},
-- 	-- 	    wire =
-- 	-- 		{
-- 	-- 		    red = {0.40625, 0.21875},
-- 	-- 		    green = {0.40625, 0.375},
-- 	-- 		}
-- 	-- 	},
-- 	--     circuit_connector_sprites = get_circuit_connector_sprites({0.1875, 0.15625}, nil, 18),
-- 	--     circuit_wire_max_distance = 9
-- 	-- }
-- })

-- entities

data:extend({
	{
	    type = "container",
	    name = "steel-collector-rampant",
	    icon = "__base__/graphics/icons/steel-chest.png",
	    flags = {"placeable-neutral", "player-creation"},
	    minable = {mining_time = 1, result = "steel-collector-overlay-rampant"},
	    max_health = 350,
	    corpse = "small-remnants",
	    open_sound = { filename = "__base__/sound/metallic-chest-open.ogg", volume=0.65 },
	    close_sound = { filename = "__base__/sound/metallic-chest-close.ogg", volume = 0.7 },
	    resistances =
		{
		    {
			type = "fire",
			percent = 90
		    },
		    {
			type = "impact",
			percent = 60
		    }
		},
	    collision_box = {{-0.35, -0.35}, {0.35, 0.35}},
	    selection_box = {{-0.5, -0.5}, {0.5, 0.5}},
	    fast_replaceable_group = "container",
	    inventory_size = 48,
	    vehicle_impact_sound =  { filename = "__base__/sound/car-metal-impact.ogg", volume = 0.65 },
	    picture =
		{
		    filename = "__Rampant__/graphics/entities/chest/steelCollector.png",
		    priority = "extra-high",
		    width = 48,
		    height = 34,
		    shift = {0.1875, 0}
		},
	    circuit_wire_connection_point =
		{
		    shadow =
			{
			    red = {0.734375, 0.453125},
			    green = {0.609375, 0.515625},
			},
		    wire =
			{
			    red = {0.40625, 0.21875},
			    green = {0.40625, 0.375},
			}
		},
	    circuit_connector_sprites = get_circuit_connector_sprites({0.1875, 0.15625}, nil, 18),
	    circuit_wire_max_distance = 9
	},

	-- {
	--     type = "logistic-container",
	--     name = "logistic-collector-passive-provider-rampant",
	--     icon = "__base__/graphics/icons/logistic-chest-passive-provider.png",
	--     flags = {"placeable-player", "player-creation"},
	--     minable = {hardness = 0.2, mining_time = 0.5, result = "logistic-collector-passive-provider-rampant"},
	--     max_health = 350,
	--     corpse = "small-remnants",
	--     collision_box = {{-0.35, -0.35}, {0.35, 0.35}},
	--     selection_box = {{-0.5, -0.5}, {0.5, 0.5}},
	--     resistances =
	-- 	{
	-- 	    {
	-- 		type = "fire",
	-- 		percent = 90
	-- 	    },
	-- 	    {
	-- 		type = "impact",
	-- 		percent = 60
	-- 	    }
	-- 	},
	--     fast_replaceable_group = "container",
	--     inventory_size = 48,
	--     logistic_mode = "passive-provider",
	--     open_sound = { filename = "__base__/sound/metallic-chest-open.ogg", volume=0.65 },
	--     close_sound = { filename = "__base__/sound/metallic-chest-close.ogg", volume = 0.7 },
	--     vehicle_impact_sound =  { filename = "__base__/sound/car-metal-impact.ogg", volume = 0.65 },
	--     picture =
	-- 	{
	-- 	    filename = "__Rampant__/graphics/entities/chest/logisticPassiveCollector.png",
	-- 	    priority = "extra-high",
	-- 	    width = 38,
	-- 	    height = 32,
	-- 	    shift = {0.09375, 0}
	-- 	},
	--     circuit_wire_connection_point =
	-- 	{
	-- 	    shadow =
	-- 		{
	-- 		    red = {0.734375, 0.453125},
	-- 		    green = {0.609375, 0.515625},
	-- 		},
	-- 	    wire =
	-- 		{
	-- 		    red = {0.40625, 0.21875},
	-- 		    green = {0.40625, 0.375},
	-- 		}
	-- 	},
	--     circuit_wire_max_distance = 9,
	--     circuit_connector_sprites = get_circuit_connector_sprites({0.1875, 0.15625}, nil, 18),
	-- },

	-- {
	--     type = "logistic-container",
	--     name = "logistic-collector-active-provider-rampant",
	--     icon = "__base__/graphics/icons/logistic-chest-active-provider.png",
	--     flags = {"placeable-player", "player-creation"},
	--     minable = {hardness = 0.2, mining_time = 0.5, result = "logistic-collector-active-provider-overlay-rampant"},
	--     max_health = 350,
	--     corpse = "small-remnants",
	--     collision_box = {{-0.35, -0.35}, {0.35, 0.35}},
	--     selection_box = {{-0.5, -0.5}, {0.5, 0.5}},
	--     resistances =
	-- 	{
	-- 	    {
	-- 		type = "fire",
	-- 		percent = 90
	-- 	    },
	-- 	    {
	-- 		type = "impact",
	-- 		percent = 60
	-- 	    }
	-- 	},
	--     fast_replaceable_group = "container",
	--     inventory_size = 48,
	--     logistic_mode = "active-provider",
	--     open_sound = { filename = "__base__/sound/metallic-chest-open.ogg", volume=0.65 },
	--     close_sound = { filename = "__base__/sound/metallic-chest-close.ogg", volume = 0.7 },
	--     vehicle_impact_sound =  { filename = "__base__/sound/car-metal-impact.ogg", volume = 0.65 },
	--     picture =
	-- 	{
	-- 	    filename = "__Rampant__/graphics/entities/chest/logisticActiveCollector.png",
	-- 	    priority = "extra-high",
	-- 	    width = 38,
	-- 	    height = 32,
	-- 	    shift = {0.09375, 0}
	-- 	},
	--     circuit_wire_connection_point =
	-- 	{
	-- 	    shadow =
	-- 		{
	-- 		    red = {0.734375, 0.453125},
	-- 		    green = {0.609375, 0.515625},
	-- 		},
	-- 	    wire =
	-- 		{
	-- 		    red = {0.40625, 0.21875},
	-- 		    green = {0.40625, 0.375},
	-- 		}
	-- 	},
	--     circuit_wire_max_distance = 9,
	--     circuit_connector_sprites = get_circuit_connector_sprites({0.1875, 0.15625}, nil, 18),
	-- }
	
})

-- recipes

data:extend({
	
	-- {
	--     type = "recipe",
	--     name = "logistic-collector-active-provider-rampant",
	--     enabled = false,
	--     ingredients =     {
	-- 	{"steel-chest", 1},
	-- 	{"electronic-circuit", 3},
	-- 	{"advanced-circuit", 1}
	--     },
	--     result = "logistic-collector-active-provider-overlay-rampant",
	--     requester_paste_multiplier = 4
	-- },
	
	{
	    type = "recipe",
	    name = "steel-collector-rampant",
	    enabled = false,
	    ingredients = {{"steel-plate", 8}},
	    result = "steel-collector-rampant",
	    requester_paste_multiplier = 4
	},

	-- {
	--     type = "recipe",
	--     name = "logistic-collector-passive-provider-rampant",
	--     enabled = false,
	--     ingredients =     {
	-- 	{"steel-chest", 1},
	-- 	{"electronic-circuit", 3},
	-- 	{"advanced-circuit", 1}
	--     },
	--     result = "logistic-collector-passive-provider-overlay-rampant",
	--     requester_paste_multiplier = 4
	-- }	
})

-- items

data:extend({
	
	-- {
	--     type = "item",
	--     name = "steel-collector-overlay-rampant",
	--     icon = "__base__/graphics/icons/steel-chest.png",
	--     flags = {"goes-to-quickbar"},
	--     subgroup = "storage",
	--     order = "a[items]-c[steel-collector]",
	--     place_result = "steel-collector-overlay-rampant",
	--     stack_size = 50
	-- },

	-- {
	--     type = "item",
	--     name = "logistic-collector-passive-provider-overlay-rampant",
	--     icon = "__base__/graphics/icons/steel-chest.png",
	--     flags = {"goes-to-quickbar"},
	--     subgroup = "storage",
	--     order = "a[items]-c[logistic-collector-passive-provider]",
	--     place_result = "logistic-collector-passive-provider-overlay-rampant",
	--     stack_size = 50
	-- },
		
	-- {
	--     type = "item",
	--     name = "logistic-collector-active-provider-overlay-rampant",
	--     icon = "__base__/graphics/icons/steel-chest.png",
	--     flags = {"goes-to-quickbar"},
	--     subgroup = "storage",
	--     order = "a[items]-c[logistic-collector-active-provider]",
	--     place_result = "logistic-collector-active-provider-overlay-rampant",
	--     stack_size = 50
	-- },
	
	{
	    type = "item",
	    name = "steel-collector-rampant",
	    icon = "__base__/graphics/icons/steel-chest.png",
	    flags = {"goes-to-quickbar"},
	    subgroup = "storage",
	    order = "a[items]-c[steel-collector]",
	    place_result = "steel-collector-rampant",
	    stack_size = 50
	}-- ,

	-- {
	--     type = "item",
	--     name = "logistic-collector-passive-provider-rampant",
	--     icon = "__base__/graphics/icons/steel-chest.png",
	--     flags = {"goes-to-quickbar"},
	--     subgroup = "storage",
	--     order = "a[items]-c[logistic-collector-passive-provider]",
	--     place_result = "logistic-collector-passive-provider-rampant",
	--     stack_size = 50
	-- },

	-- {
	--     type = "item",
	--     name = "logistic-collector-active-provider-rampant",
	--     icon = "__base__/graphics/icons/steel-chest.png",
	--     flags = {"goes-to-quickbar"},
	--     subgroup = "storage",
	--     order = "a[items]-c[logistic-collector-active-provider]",
	--     place_result = "logistic-collector-active-provider-rampant",
	--     stack_size = 50
	-- }	
})

-- technology insertions

table.insert(data.raw.technology["steel-processing"].effects,
	     {
		 type = "unlock-recipe",
		 recipe = "steel-collector-rampant"

	     })

-- table.insert(data.raw.technology["steel-processing"].effects,
-- 	     {
-- 		 type = "unlock-recipe",
-- 		 recipe = "logistic-collector-passive-provider-rampant"

-- 	     })

-- table.insert(data.raw.technology["steel-processing"].effects,
-- 	     {
-- 		 type = "unlock-recipe",
-- 		 recipe = "logistic-collector-active-provider-rampant"

-- })
