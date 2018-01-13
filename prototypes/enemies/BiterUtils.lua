local biterFunctions = {}

function biterFunctions.makeBiter(name, biterAttributes, biterAttack, biterResistances)
    return {
	type = "unit",
	name = name,
	icon = "__base__/graphics/icons/small-biter.png",
	icon_size = 32,
	flags = biterAttributes.flags or {"placeable-player", "placeable-enemy", "placeable-off-grid", "breaths-air"},
	max_health = biterAttributes.health,
	order = "b-b-a",
	subgroup="enemies",
	healing_per_tick = biterAttributes.healing,
	resistances = biterResistances,
	collision_box = {{-0.4 * biterAttributes.scale, -0.4 * biterAttributes.scale}, 
	    {0.4 * biterAttributes.scale, 0.4 * biterAttributes.scale}},
	selection_box = {{-0.7 * biterAttributes.scale, -1.5 * biterAttributes.scale}, 
	    {0.7 * biterAttributes.scale, 0.3 * biterAttributes.scale}},
	sticker_box = {{-0.6 * biterAttributes.scale, -0.8 * biterAttributes.scale}, 
	    {0.6 * biterAttributes.scale, 0}},
	attack_parameters = biterAttack,
	vision_distance = biterAttributes.vision or 30,
	movement_speed = biterAttributes.movement,
	distance_per_frame = biterAttributes.distancePerFrame or 0.1,
	pollution_to_join_attack = biterAttributes.pollutionToAttack or 200,
	distraction_cooldown = biterAttributes.distractionCooldown or 300,
	corpse = biterAttributes.corpse,
	dying_explosion = biterAttributes.explosion,
	dying_sound =  make_biter_dying_sounds(1.0),
	working_sound =  make_biter_calls(0.7),
	run_animation = biterrunanimation(biterAttributes.scale, biterAttributes.tint1, biterAttributes.tint2)
    }
end

function biterFunctions.makeSpitter(name, biterAttributes, biterAttack, biterResistances)
    return {
	type = "unit",
	name = name,
	icon = "__base__/graphics/icons/small-spitter.png",
	icon_size = 32,
	flags = biterAttributes.flags or {"placeable-player", "placeable-enemy", "placeable-off-grid", "breaths-air"},
	max_health = biterAttributes.health,
	order = "b-b-a",
	subgroup="enemies",
	healing_per_tick = biterAttributes.healing,
	resistances = biterResistances,
	collision_box = {{-0.4 * biterAttributes.scale, -0.4 * biterAttributes.scale}, 
	    {0.4 * biterAttributes.scale, 0.4 * biterAttributes.scale}},
	selection_box = {{-0.7 * biterAttributes.scale, -1.5 * biterAttributes.scale}, 
	    {0.7 * biterAttributes.scale, 0.3 * biterAttributes.scale}},
	sticker_box = {{-0.6 * biterAttributes.scale, -0.8 * biterAttributes.scale}, 
	    {0.6 * biterAttributes.scale, 0}},
	attack_parameters = biterAttack,
	vision_distance = biterAttributes.vision or 30,
	movement_speed = biterAttributes.movement,
	distance_per_frame = biterAttributes.distancePerFrame or 0.1,
	pollution_to_join_attack = biterAttributes.pollutionToAttack or 200,
	distraction_cooldown = biterAttributes.distractionCooldown or 300,
	corpse = biterAttributes.corpse,
	dying_explosion = biterAttributes.explosion,
	dying_sound =  make_biter_dying_sounds(1.0),
	working_sound =  make_biter_calls(0.7),
	run_animation = spitterrunanimation(biterAttributes.scale, biterAttributes.tint1, biterAttributes.tint2)
    }
end

function biterFunctions.makeUnitSpawner(name, biterAttributes, biterResistances, unitSet)
    local o = {
    type = "unit-spawner",
    name = name,
    icon = "__base__/graphics/icons/biter-spawner.png",
    icon_size = 32,
    flags = {"placeable-player", "placeable-enemy", "not-repairable"},
    max_health = biterAttributes.health,
    order="b-b-g",
    subgroup="enemies",
    resistances = biterResistances,
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
    healing_per_tick = biterAttributes.healing or 0.02,
    collision_box = {{-3.2 * biterAttributes.scale, -2.2 * biterAttributes.scale}, {2.2 * biterAttributes.scale, 2.2 * biterAttributes.scale}},
    selection_box = {{-3.5 * biterAttributes.scale, -2.5 * biterAttributes.scale}, {2.5 * biterAttributes.scale, 2.5 * biterAttributes.scale}},
    -- in ticks per 1 pu
    pollution_absorbtion_absolute = biterAttributes.pollutionAbsorbtionAbs or 20,
    pollution_absorbtion_proportional = biterAttributes.pollutionAbsorbtionPro or 0.01,
    corpse = "biter-spawner-corpse",
    dying_explosion = "blood-explosion-huge",
    max_count_of_owned_units = biterAttributes.unitsOwned or 7,
    max_friends_around_to_spawn = biterAttributes.unitsToSpawn or 5,
    animations =
    {
      spawner_idle_animation(0, biterAttributes.tint),
      spawner_idle_animation(1, biterAttributes.tint),
      spawner_idle_animation(2, biterAttributes.tint),
      spawner_idle_animation(3, biterAttributes.tint)
    },
    result_units = unitSet,
    -- With zero evolution the spawn rate is 6 seconds, with max evolution it is 2.5 seconds
    spawning_cooldown = biterAttributes.spawningCooldown or {360, 150},
    spawning_radius = biterAttributes.spawningRadius or 10,
    spawning_spacing = biterAttributes.spawningSpacing or 3,
    max_spawn_shift = 0,
    max_richness_for_spawn_shift = 100,    
    call_for_help_radius = 50
    }
    if biterAttributes.autoplace then
	o["autoplace"] = enemy_spawner_autoplace(biterAttributes.autoplace)
    end
    return o
end

function biterFunctions.createSuicideAttack(attributes)
    return { type = "projectile",
             range = 0.5,
             cooldown = 35,
             ammo_category = "melee",
             ammo_type = { 
		 category = "biological",
		 action = {
		     {
			 type = "direct",
			 action_delivery = {
			     type = "instant",
			     target_effects =
				 {
				     {
					 type = "create-entity",
					 entity_name = attributes.explosion,
					 check_buildability = true
				     },
				     {
					 type = "nested-result",
					 action = {
					     type = "area",
					     radius = attributes.area,
					     action_delivery = {
						 type = "instant",
						 target_effects = {
						     {
							 type = "damage",
							 damage = {
							     amount = attributes.damage, 
							     type = "explosion"
							 }
						     },
						 }
					     }
					 }
				     },
				     {
					 type = "create-entity",
					 entity_name = attributes.scorchmark,
					 check_buildability = true
				     }
				 }
			     
			 },
		     },
		     {
			 type = "cluster",
			 cluster_count = attributes.explosionCount,
			 distance = attributes.explosionDistance,
			 distance_deviation = 3,
			 action_delivery =
			     {
				 type = "instant",
				 target_effects= {
				     {
					 type="create-entity",
					 entity_name = attributes.explosion,
					 check_buildability = true
				     },
				     {
					 type = "create-entity",
					 entity_name = attributes.scorchmark,
					 check_buildability = true
				     },
				     {
					 type = "nested-result",
					 action = {
					     type = "area",
					     radius = attributes.area,
					     action_delivery = {
						 type = "instant",
						 target_effects = {
						     {
							 type = "damage",
							 damage = {
							     amount = attributes.damage, 
							     type = "explosion"
							 }
						     },
						 }
					     }
					 }
				     }
				 }
			     }
		     }
		     
		 }
	     },
	     sound = make_biter_roars(0.5),
	     animation = biterattackanimation(attributes.scale, attributes.tint1, attributes.tint2)
    }
end

function biterFunctions.findRunScale(entity)
    return entity.run_animation.layers[1].scale
end

function biterFunctions.findRange(entity)
    return entity.attack_parameters.range
end

function biterFunctions.findTint(entity)
    return entity.run_animation.layers[2].tint
end

function biterFunctions.createFireAttack(attributes, fireAttack)
    local attack = {
	type = "stream",
	ammo_category = "flamethrower",
	cooldown = attributes.cooldown,
	range = attributes.range,
	min_range = attributes.minRange,
	
	turn_range = attributes.turnRange,
	fire_penalty = attributes.firePenalty,

	warmup = attributes.warmup or 0,
	
	damage_modifier = attributes.damageModifier or 1.0,
	
	gun_barrel_length = 2 * attributes.scale,
	gun_center_shift = {
	    north = {0, -0.65 * attributes.scale},
	    east = {0, 0},
	    south = {0, 0},
	    west = {0, 0}
	},
	ammo_type =
	    {
                category = "flamethrower",
                action =
		    {
			type = "direct",
			action_delivery =
			    {
				type = "stream",
				stream = fireAttack,
				duration = 160,
			    }
		    }
	    },
	
	cyclic_sound =
	    {
                begin_sound =
		    {
			{
			    filename = "__base__/sound/creatures/spitter-1.ogg",
			    volume = 0.7
			},
			{
			    filename = "__base__/sound/creatures/spitter-2.ogg",
			    volume = 0.7
			},
			{
			    filename = "__base__/sound/creatures/spitter-3.ogg",
			    volume = 0.7
			},
			{
			    filename = "__base__/sound/creatures/spitter-4.ogg",
			    volume = 0.7
			},
			{
			    filename = "__base__/sound/creatures/spitter-5.ogg",
			    volume = 0.7
			},
			{
			    filename = "__base__/sound/creatures/spitter-6.ogg",
			    volume = 0.7
			},
			{
			    filename = "__base__/sound/creatures/spitter-7.ogg",
			    volume = 0.7
			},
			{
			    filename = "__base__/sound/creatures/spitter-8.ogg",
			    volume = 0.7
			}
		    },
                middle_sound =
		    {
			{
			    filename = attributes.midSound or "__Rampant__/sounds/attacks/acid-mid.ogg",
			    volume = 0.5
			}
		    },
                end_sound =
		    {
			{
			    filename = attributes.endSound or "__Rampant__/sounds/attacks/acid-end.ogg",
			    volume = 0.5
			}
		    }
	    }
    } 

    if (attributes.tint1 ~= nil) then
	attack.animation = spitterattackanimation(attributes.scale, 
						  attributes.tint1, 
						  attributes.tint2)
    end
    
    return attack
end

function biterFunctions.makeResistance(name, decrease, percentage)
    local obj = {
	type = name,
    }
    if (decrease ~= 0) then
        obj.decrease = decrease
    end
    if (percentage ~= 0) then
        obj.percentage = percentage
    end
    return obj
end

return biterFunctions
