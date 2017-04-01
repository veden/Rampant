local biterFunctions = {}

function biterFunctions.makeBiter(biterAttributes, biterAttack, biterResistances)
    biterAttack.scale = biterAttributes.scale;
    biterAttack.tint1 = biterAttributes.tint1;
    biterAttack.tint2 = biterAttributes.tint2;
    return {
	type = "unit",
	name = biterAttributes.name,
	icon = "__base__/graphics/icons/small-biter.png",
	flags = {"placeable-player", "placeable-enemy", "placeable-off-grid", "breaths-air"},
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
	vision_distance = 30,
	movement_speed = biterAttributes.movement,
	distance_per_frame = 0.1,
	pollution_to_join_attack = 200,
	distraction_cooldown = 300,
	corpse = biterAttributes.corpse,
	dying_explosion = biterAttributes.explosion,
	dying_sound =  make_biter_dying_sounds(1.0),
	working_sound =  make_biter_calls(0.7),
	run_animation = biterrunanimation(biterAttributes.scale, biterAttributes.tint1, biterAttributes.tint2)
    }
end

function biterFunctions.makeSpitter(biterAttributes, biterAttack, biterResistances)
    -- biterAttack.scale = biterAttributes.scale;
    -- biterAttack.tint1 = biterAttributes.tint1;
    -- biterAttack.tint2 = biterAttributes.tint2;
    return {
	type = "unit",
	name = biterAttributes.name,
	icon = "__base__/graphics/icons/small-spitter.png",
	flags = {"placeable-player", "placeable-enemy", "placeable-off-grid", "breaths-air"},
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
	vision_distance = 30,
	movement_speed = biterAttributes.movement,
	distance_per_frame = biterAttributes.distancePerFrame,
	pollution_to_join_attack = 200,
	distraction_cooldown = 300,
	corpse = biterAttributes.corpse,
	dying_explosion = biterAttributes.explosion,
	dying_sound =  make_biter_dying_sounds(1.0),
	working_sound =  make_biter_calls(0.7),
	run_animation = spitterrunanimation(biterAttributes.scale, biterAttributes.tint1, biterAttributes.tint2)
    }
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
					     perimeter = attributes.area,
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
					     perimeter = attributes.area,
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

function biterFunctions.findTint(entity)
    return entity.run_animation.layers[2].tint
end

function biterFunctions.createFireAttack(attributes, fireAttack)
    local attack = {
	type = "stream",
	ammo_category = "flame-thrower",
	cooldown = attributes.cooldown,
	range = attributes.range,
	min_range = attributes.minRange,
	
	turn_range = attributes.turnRange,
	fire_penalty = attributes.firePenalty,

	gun_barrel_length = 2 * attributes.scale,
	gun_center_shift = {
	    north = {0, -0.65 * attributes.scale},
	    east = {0, 0},
	    south = {0, 0},
	    west = {0, 0}
	},
	ammo_type =
	    {
                category = "flame-thrower",
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
			    filename = "__base__/sound/fight/flamethrower-start.ogg",
			    volume = 0.7
			}
		    },
                middle_sound =
		    {
			{
			    filename = "__base__/sound/fight/flamethrower-mid.ogg",
			    volume = 0.7
			}
		    },
                end_sound =
		    {
			{
			    filename = "__base__/sound/fight/flamethrower-end.ogg",
			    volume = 0.7
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
