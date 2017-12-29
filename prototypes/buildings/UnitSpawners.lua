
function spawner_idle_animation(variation, tint)
    return
	{
	    layers =
		{
		    {
			filename = "__base__/graphics/entity/spawner/spawner-idle.png",
			line_length = 8,
			width = 243,
			height = 181,
			frame_count = 8,
			animation_speed = 0.18,
			direction_count = 1,
			run_mode = "forward-then-backward",
			shift = {0.140625 - 0.65, -0.234375},
			y = variation * 181
		    },
		    {
			filename = "__base__/graphics/entity/spawner/spawner-idle-mask.png",
			flags = { "mask" },
			width = 166,
			height = 148,
			frame_count = 8,
			animation_speed = 0.18,
			run_mode = "forward-then-backward",
			shift = {-0.34375 - 0.65, -0.375},
			line_length = 8,
			tint = tint,
			y = variation * 148
		    }
		}
	}
end

function spawner_die_animation(variation, tint)
    return
	{
	    layers =
		{
		    {
			width = 255,
			height = 184,
			frame_count = 20,
			direction_count = 1,
			shift = {-0.015625 - 0.65, -0.28125},
			stripes =
			    {
				{
				    filename = "__base__/graphics/entity/spawner/spawner-die-01.png",
				    width_in_frames = 7,
				    height_in_frames = 4,
				    y = variation * 184
				},
				{
				    filename = "__base__/graphics/entity/spawner/spawner-die-02.png",
				    width_in_frames = 7,
				    height_in_frames = 4,
				    y = variation * 184
				},
				{
				    filename = "__base__/graphics/entity/spawner/spawner-die-03.png",
				    width_in_frames = 6,
				    height_in_frames = 4,
				    y = variation * 184
				}
			    }
		    },
		    {
			flags = { "mask" },
			width = 166,
			height = 148,
			frame_count = 20,
			direction_count = 1,
			shift = {-0.34375 - 0.65, -0.375},
			tint = tint,
			stripes =
			    {
				{
				    filename = "__base__/graphics/entity/spawner/spawner-die-mask-01.png",
				    width_in_frames = 10,
				    height_in_frames = 4,
				    y = variation * 148
				},
				{
				    filename = "__base__/graphics/entity/spawner/spawner-die-mask-02.png",
				    width_in_frames = 10,
				    height_in_frames = 4,
				    y = variation * 148
				}
			    }
		    }
		}
	}
end


local biter_spawner_powered_tint = {r=1.0, g=0, b=0, a=1.0}

data:extend({


	{
	    type = "unit-spawner",
	    name = "biter-spawner-hive-rampant",
	    icon = "__base__/graphics/icons/biter-spawner.png",
	    icon_size = 32,
	    flags = {"placeable-player", "placeable-enemy", "not-repairable"},
	    max_health = 350,
	    order="b-b-g",
	    subgroup="enemies",
	    resistances =
		{
		    {
			type = "physical",
			decrease = 2,
			percent = 15
		    },
		    {
			type = "explosion",
			decrease = 5,
			percent = 15,
		    },
		    {
			type = "fire",
			decrease = 3,
			percent = 60,
		    }
		},
	    working_sound = {
		sound =
		    {
			{
			    filename = "__base__/sound/creatures/spawner.ogg",
			    volume = 1.0
			}
		    },
		apparent_volume = 2
	    },
	    dying_sound =
		{
		    {
			filename = "__base__/sound/creatures/spawner-death-1.ogg",
			volume = 1.0
		    },
		    {
			filename = "__base__/sound/creatures/spawner-death-2.ogg",
			volume = 1.0
		    }
		},
	    healing_per_tick = 0.02,
	    collision_box = {{-6, -6}, {6, 6}},
	    selection_box = {{-6, -6}, {6, 6}},
	    -- in ticks per 1 pu
	    pollution_absorbtion_absolute = 20,
	    pollution_absorbtion_proportional = 0.01,
	    corpse = "biter-spawner-corpse",
	    dying_explosion = "blood-explosion-huge",
	    max_count_of_owned_units = 7,
	    max_friends_around_to_spawn = 5,
	    animations =
		{
		    spawner_idle_animation(0, biter_spawner_powered_tint),
		    spawner_idle_animation(1, biter_spawner_powered_tint),
		    spawner_idle_animation(2, biter_spawner_powered_tint),
		    spawner_idle_animation(3, biter_spawner_powered_tint)
		},
	    result_units = (function()
		    local res = {}
		    res[1] = {"small-biter", {{0.0, 0.3}, {0.6, 0.0}}}
		    if not data.is_demo then
			-- from evolution_factor 0.3 the weight for medium-biter is linearly rising from 0 to 0.3
			-- this means for example that when the evolution_factor is 0.45 the probability of spawning
			-- a small biter is 66% while probability for medium biter is 33%.
			res[2] = {"medium-biter", {{0.2, 0.0}, {0.6, 0.3}, {0.7, 0.1}}}
			-- for evolution factor of 1 the spawning probabilities are: small-biter 0%, medium-biter 1/8, big-biter 4/8, behemoth biter 3/8
			res[3] = {"big-biter", {{0.5, 0.0}, {1.0, 0.4}}}
			res[4] = {"behemoth-biter", {{0.9, 0.0}, {1.0, 0.3}}}
		    end
		    return res
			   end)(),
	    -- With zero evolution the spawn rate is 6 seconds, with max evolution it is 2.5 seconds
	    spawning_cooldown = {360, 150},
	    spawning_radius = 10,
	    spawning_spacing = 3,
	    max_spawn_shift = 0,
	    max_richness_for_spawn_shift = 100,
	    call_for_help_radius = 50
	},

	{
	    type = "container",
	    name = "chunk-scanner-ns-rampant",
	    icon = "__base__/graphics/icons/wooden-chest.png",
	    icon_size = 32,
	    flags = {},
	    collision_mask = {"player-layer", "object-layer", "water-tile"},
	    collision_box = {{-0.5, 0}, {0, 32}},
	    minable = {mining_time = 1, result = "wooden-chest"},
	    max_health = 100,
	    corpse = "small-remnants",
	    fast_replaceable_group = "container",
	    selection_box = {{-0.5, -0.5}, {0.5, 0.5}},
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
	    minable = {mining_time = 1, result = "wooden-chest"},
	    max_health = 100,
	    corpse = "small-remnants",
	    fast_replaceable_group = "container",
	    selection_box = {{-0.5, -0.5}, {0.5, 0.5}},
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
