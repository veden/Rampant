
data:extend({
	{
	    type = "container",
	    name = "chunk-scanner-squad-rampant",
	    icon = "__base__/graphics/icons/wooden-chest.png",
	    icon_size = 32,
	    flags = {},
	    collision_mask = {"player-layer", "object-layer", "water-tile"},
	    collision_box = {{-6, -6}, {6, 6}},
	    selection_box = {{-6, -6}, {6, 6}},
	    minable = {mining_time = 1, result = "wooden-chest"},
	    max_health = 100,
	    corpse = "small-remnants",
	    fast_replaceable_group = "container",
	    inventory_size = 16,
	    open_sound = { filename = "__base__/sound/wooden-chest-open.ogg" },
	    close_sound = { filename = "__base__/sound/wooden-chest-close.ogg" },
	    vehicle_impact_sound =  { filename = "__base__/sound/car-wood-impact.ogg", volume = 1.0 },
	    picture =
		{
		    filename = "__base__/graphics/entity/wooden-chest/wooden-chest.png",
		    priority = "extra-high",
		    width = 46,
		    height = 33,
		    shift = {0.25, 0.015625}
		},
	    circuit_wire_connection_point = circuit_connector_definitions["chest"].points,
	    circuit_connector_sprites = circuit_connector_definitions["chest"].sprites,
	    circuit_wire_max_distance = default_circuit_wire_max_distance
        },

	{
	    type = "container",
	    name = "chunk-scanner-ns-rampant",
	    icon = "__base__/graphics/icons/wooden-chest.png",
	    icon_size = 32,
	    flags = {},
	    collision_mask = {"player-layer", "object-layer", "water-tile"},
	    collision_box = {{-0.5, 0}, {0, 32}},
	    -- minable = {mining_time = 1, result = "chunk-scanner-ns-rampant"},
            minable = {mining_time = 1, result = "wooden-chest"},
	    max_health = 100,
	    corpse = "small-remnants",
	    fast_replaceable_group = "container",
	    selection_box = {{-0.5, 0}, {0, 32}},
	    inventory_size = 16,
	    open_sound = { filename = "__base__/sound/wooden-chest-open.ogg" },
	    close_sound = { filename = "__base__/sound/wooden-chest-close.ogg" },
	    vehicle_impact_sound =  { filename = "__base__/sound/car-wood-impact.ogg", volume = 1.0 },
	    picture =
		{
		    filename = "__base__/graphics/entity/wooden-chest/wooden-chest.png",
		    priority = "extra-high",
		    width = 46,
		    height = 33,
		    shift = {0.25, 0.015625}
		},
	    circuit_wire_connection_point = circuit_connector_definitions["chest"].points,
	    circuit_connector_sprites = circuit_connector_definitions["chest"].sprites,
	    circuit_wire_max_distance = default_circuit_wire_max_distance
	},

	{
	    type = "container",
	    name = "chunk-scanner-ew-rampant",
	    icon = "__base__/graphics/icons/wooden-chest.png",
	    icon_size = 32,
	    flags = {},
	    collision_box = {{0, -0.5}, {32, 0}},
	    collision_mask = {"player-layer", "object-layer", "water-tile"},
	    -- minable = {mining_time = 1, result = "chunk-scanner-ew-rampant"},
            minable = {mining_time = 1, result = "wooden-chest"},
	    max_health = 100,
	    corpse = "small-remnants",
	    fast_replaceable_group = "container",
	    selection_box = {{0, -0.5}, {32, 0}},
	    inventory_size = 16,
	    open_sound = { filename = "__base__/sound/wooden-chest-open.ogg" },
	    close_sound = { filename = "__base__/sound/wooden-chest-close.ogg" },
	    vehicle_impact_sound =  { filename = "__base__/sound/car-wood-impact.ogg", volume = 1.0 },
	    picture =
		{
		    filename = "__base__/graphics/entity/wooden-chest/wooden-chest.png",
		    priority = "extra-high",
		    width = 46,
		    height = 33,
		    shift = {0.25, 0.015625}
		},
	    circuit_wire_connection_point = circuit_connector_definitions["chest"].points,
	    circuit_connector_sprites = circuit_connector_definitions["chest"].sprites,
	    circuit_wire_max_distance = default_circuit_wire_max_distance
	}

})

-- local d = table.deepcopy(data.raw["item"]["wooden-chest"])
-- d.name = "chunk-scanner-ew-rampant"
-- d.hidden = true
-- d.place_result = "chunk-scanner-ew-rampant"

-- data:extend({
--         d
-- })

-- d = table.deepcopy(data.raw["item"]["wooden-chest"])
-- d.name = "chunk-scanner-ns-rampant"
-- d.hidden = true
-- d.place_result = "chunk-scanner-ns-rampant"

-- data:extend({
--         d
-- })
