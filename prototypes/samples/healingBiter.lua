local smallbiterscale = 0.5
local small_biter_tint1 = {r=0.56, g=0.46, b=0.42, a=0.65}
local small_biter_tint2 = {r=1, g=0.63, b=0, a=0.4}

data:extend({
	{ -- this defines a damage type so resistances don't effect this damage
	    type = "damage-type", 
	    name = "healing"
	},
	{ -- this defines your healing cloud
	    type = "smoke-with-trigger",
	    name = "healing-cloud-overlord",
	    flags = {"not-on-map"},
	    show_when_smoke_off = true,
	    animation =
		{
		    filename = "__base__/graphics/entity/cloud/cloud-45-frames.png",
		    flags = { "compressed" },
		    priority = "low",
		    width = 256,
		    height = 256,
		    frame_count = 45,
		    animation_speed = 0.5,
		    line_length = 7,
		    scale = 3,
		},
	    slow_down_factor = 0,
	    affected_by_wind = false,
	    cyclic = true,
	    duration = 60 * 5,
	    fade_away_duration = 5 * 60,
	    spread_duration = 10,
	    color = { r = 0.0, g = 0.0, b = 0.9, a = 0.2 },
	    action =
		{
		    type = "direct",
		    action_delivery =
			{
			    type = "instant",
			    target_effects =
				{
				    type = "nested-result",
				    action =
					{
					    type = "area",
					    perimeter = 11,
					    entity_flags = {"breaths-air"},
					    action_delivery =
						{
						    type = "instant",
						    target_effects =
							{
							    type = "damage",
							    damage = { amount = -2, type = "healing"}
							}
						}
					}
				}
			}
		},
	    action_cooldown = 30
	},
	{ -- this is your custom projectile that will create the healing cloud
	    type = "projectile",
	    name = "healing-orb-overlord",
	    flags = {"not-on-map"},
	    acceleration = 0.005,
	    action =
		{
		    type = "direct",
		    action_delivery =
			{
			    type = "instant",
			    target_effects =
				{
				    {
					type = "play-sound",
					sound =
					    {
						{
						    filename = "__base__/sound/creatures/projectile-acid-burn-1.ogg",
						    volume = 0.8
						},
						{
						    filename = "__base__/sound/creatures/projectile-acid-burn-2.ogg",
						    volume = 0.8
						},
						{
						    filename = "__base__/sound/creatures/projectile-acid-burn-long-1.ogg",
						    volume = 0.8
						},
						{
						    filename = "__base__/sound/creatures/projectile-acid-burn-long-2.ogg",
						    volume = 0.8
						}
					    }
				    },
				    {
					type = "create-entity",
					entity_name = "healing-cloud-overlord"
				    }
				}
			}
		},
	    animation =
		{
		    filename = "__base__/graphics/entity/acid-projectile-purple/acid-projectile-purple.png",
		    line_length = 5,
		    width = 16,
		    height = 18,
		    frame_count = 33,
		    priority = "high"
		},
	    shadow =
		{
		    filename = "__base__/graphics/entity/acid-projectile-purple/acid-projectile-purple-shadow.png",
		    line_length = 5,
		    width = 28,
		    height = 16,
		    frame_count = 33,
		    priority = "high",
		    shift = {-0.09, 0.395}
		},
	    rotatable = false
	},
	{
	    type = "unit",
	    name = "small-healing-biter-overlord",
	    icon = "__base__/graphics/icons/small-biter.png",
	    flags = {"placeable-player", "placeable-enemy", "placeable-off-grid", "breaths-air"},
	    max_health = 15,
	    order = "b-b-a",
	    subgroup="enemies",
	    healing_per_tick = 0.01,
	    collision_box = {{-0.2, -0.2}, {0.2, 0.2}},
	    selection_box = {{-0.4, -0.7}, {0.7, 0.4}},
	    attack_parameters = {
		type = "projectile",
		ammo_category = "rocket",
		damage_modifier = 10,
		cooldown = 150,
		projectile_center = {0, 0},
		projectile_creation_distance = 0.6,
		range = 15, -- make this whatever distance you want the unit to stop at
		animation = biterattackanimation(smallbiterscale, small_biter_tint1, small_biter_tint2),
		sound = {
		    filename = "__base__/sound/fight/pulse.ogg",
		    volume = 0.7
		},
		ammo_type = {
		    type = "projectile",
		    category = "biological",
		    speed = 1,
		    action =
			{
			    {
				type = "direct",
				action_delivery =
				    {
					type = "projectile",
					projectile = "healing-orb-overlord",
					starting_speed = 0.5,
					max_range = 1 -- the distance you want the cloud to appear from the unit
				    }
			    },
			    {
				type = "direct",
				action_delivery =
				    {
					type = "projectile",
					projectile = "acid-projectile-purple",
					starting_speed = 0.5,
					max_range = 15 -- should be whatever range you want the unit to shoot for
				    }
			    }
			}
		}
	    },
	    vision_distance = 30,
	    movement_speed = 0.2,
	    distance_per_frame = 0.1,
	    pollution_to_join_attack = 200,
	    distraction_cooldown = 300,
	    min_pursue_time = 10 * 60,
	    max_pursue_distance = 50,
	    corpse = "small-biter-corpse",
	    dying_explosion = "blood-explosion-small",
	    dying_sound =  make_biter_dying_sounds(0.4),
	    working_sound =  make_biter_calls(0.3),
	    run_animation = biterrunanimation(smallbiterscale, small_biter_tint1, small_biter_tint2)
	}
})
