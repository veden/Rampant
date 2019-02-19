local biterFunctions = {}

local unitSpawnerUtils = require("UnitSpawnerUtils")


local FORCE_OLD_PROJECTILES = settings.startup["rampant-forceOldProjectiles"].value


local spawner_idle_animation = unitSpawnerUtils.spawner_idle_animation
local spawner_die_animation = unitSpawnerUtils.spawner_die_animation



function biterFunctions.makeSpitterCorpse(attributes)
    local name = attributes.name .. "-corpse-rampant"
    data:extend(
	{
	    {
		type = "corpse",
		name = name,
		icon = "__base__/graphics/icons/big-biter-corpse.png",
		icon_size = 32,
		selectable_in_game = false,
		selection_box = {{-1, -1}, {1, 1}},
		subgroup="corpses",
		order = "c[corpse]-b[spitter]-a[small]",
		flags = {"placeable-neutral", "placeable-off-grid", "building-direction-8-way", "not-on-map"},
		dying_speed = 0.04,
		time_before_removed = 15 * 60 * 60,
		final_render_layer = "corpse",
		animation = spitterdyinganimation(attributes.scale, attributes.tint)
	    }
    })
    return name
end

function biterFunctions.makeBiterCorpse(attributes)
    local name = attributes.name .. "-corpse-rampant"
    data:extend({
	    {
		type = "corpse",
		name = name,
		icon = "__base__/graphics/icons/big-biter-corpse.png",
		icon_size = 32,
		selectable_in_game = false,
		selection_box = {{-1, -1}, {1, 1}},
		subgroup="corpses",
		order = "c[corpse]-b[spitter]-a[small]",
		flags = {"placeable-neutral", "placeable-off-grid", "building-direction-8-way", "not-on-map"},
		dying_speed = 0.04,
		time_before_removed = 15 * 60 * 60,
		final_render_layer = "corpse",
		animation = biterdieanimation(attributes.scale, attributes.tint1, attributes.tint2)
	    }
    })
    return name
end

function biterFunctions.makeUnitSpawnerCorpse(attributes)
    local name = attributes.name .. "-corpse-rampant"
    data:extend({
	    {
		type = "corpse",
		name = name,
		flags = {"placeable-neutral", "placeable-off-grid", "not-on-map"},
		icon = "__base__/graphics/icons/biter-spawner-corpse.png",
		icon_size = 32,
		collision_box = {{-2, -2}, {2, 2}},
		selection_box = {{-2, -2}, {2, 2}},
		selectable_in_game = false,
		dying_speed = 0.04,
		time_before_removed = 15 * 60 * 60,
		subgroup="corpses",
		order = "c[corpse]-c[spitter-spawner]",
		final_render_layer = "remnants",
		animation =
		    {
			spawner_die_animation(0, attributes.tint, attributes.scale),
			spawner_die_animation(1, attributes.tint, attributes.scale),
			spawner_die_animation(2, attributes.tint, attributes.scale),
			spawner_die_animation(3, attributes.tint, attributes.scale)
		    }
	    }
    })
    return name
end

function biterFunctions.makeWormCorpse(attributes)
    local name = attributes.name .. "-corpse-rampant"
    data:extend({
	    {
		type = "corpse",
		name = name,
		icon = "__base__/graphics/icons/medium-worm-corpse.png",
		icon_size = 32,
		selection_box = {{-0.8, -0.8}, {0.8, 0.8}},
		selectable_in_game = false,
		subgroup="corpses",
		order = "c[corpse]-c[worm]-b[medium]",
		flags = {"placeable-neutral", "placeable-off-grid", "building-direction-8-way", "not-repairable", "not-on-map"},
		dying_speed = 0.01,
		time_before_removed = 15 * 60 * 60,
		final_render_layer = "corpse",
		animation = worm_die_animation(attributes.scale, attributes.tint)
	    }
    })
    return name
end

function biterFunctions.makeBiter(name, biterAttributes, biterAttack, biterResistances)
    local resistances = {}
    for k,v in pairs(biterResistances) do
	v.type = k
	resistances[#resistances+1] = v
    end
--    print(name .. " " .. biterAttributes.health)
    return {
	type = "unit",
	name = name .. "-rampant",
	icon = "__base__/graphics/icons/small-biter.png",
	icon_size = 32,
	flags = biterAttributes.flags or {"placeable-player", "placeable-enemy", "placeable-off-grid", "breaths-air"},
	max_health = biterAttributes.health,
	order = "b-b-a",
	subgroup="enemies",
	healing_per_tick = biterAttributes.healing,
	resistances = resistances,
	collision_box = {{-0.4 * biterAttributes.scale, -0.4 * biterAttributes.scale},
	    {0.4 * biterAttributes.scale, 0.4 * biterAttributes.scale}},
	selection_box = {{-0.7 * biterAttributes.scale, -1.5 * biterAttributes.scale},
	    {0.7 * biterAttributes.scale, 0.3 * biterAttributes.scale}},
	sticker_box = {{-0.6 * biterAttributes.scale, -0.8 * biterAttributes.scale},
	    {0.6 * biterAttributes.scale, 0}},
	attack_parameters = biterAttack,
	vision_distance = biterAttributes.vision or 30,
	movement_speed = biterAttributes.movement,
	loot = biterAttributes.loot,
	spawning_time_modifier = biterAttributes.spawningTimeModifer or 0,
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
    local resistances = {}
    for k,v in pairs(biterResistances) do
	v.type = k
	resistances[#resistances+1] = v
    end
--    print(name .. " " .. biterAttributes.health)
    return {
	type = "unit",
	name = name .. "-rampant",
	icon = "__base__/graphics/icons/small-spitter.png",
	icon_size = 32,
	flags = biterAttributes.flags or {"placeable-player", "placeable-enemy", "placeable-off-grid", "breaths-air"},
	max_health = biterAttributes.health,
	order = "b-b-a",
	subgroup="enemies",
	healing_per_tick = biterAttributes.healing,
	resistances = resistances,
	collision_box = {{-0.4 * biterAttributes.scale, -0.4 * biterAttributes.scale},
	    {0.4 * biterAttributes.scale, 0.4 * biterAttributes.scale}},
	selection_box = {{-0.7 * biterAttributes.scale, -1.5 * biterAttributes.scale},
	    {0.7 * biterAttributes.scale, 0.3 * biterAttributes.scale}},
	sticker_box = {{-0.6 * biterAttributes.scale, -0.8 * biterAttributes.scale},
	    {0.6 * biterAttributes.scale, 0}},
	attack_parameters = biterAttack,
	loot = biterAttributes.loot,
	vision_distance = biterAttributes.vision or 30,
	movement_speed = biterAttributes.movement,
	spawning_time_modifier = biterAttributes.spawningTimeModifer or 0,
	distance_per_frame = biterAttributes.distancePerFrame or 0.1,
	pollution_to_join_attack = biterAttributes.pollutionToAttack or 200,
	distraction_cooldown = biterAttributes.distractionCooldown or 300,
	corpse = biterAttributes.corpse,
	dying_explosion = biterAttributes.explosion,
	dying_sound =  make_spitter_dying_sounds(0.8),
	working_sound =  make_biter_calls(0.7),
	run_animation = spitterrunanimation(biterAttributes.scale, biterAttributes.tint)
    }
end

function biterFunctions.makeUnitSpawner(name, biterAttributes, biterResistances, unitSet)
    local resistances = {}
    for k,v in pairs(biterResistances) do
	v.type = k
	resistances[#resistances+1] = v
    end
--    print(name .. " " .. biterAttributes.health)
    local o = {
	type = "unit-spawner",
	name = name .. "-rampant",
	icon = "__base__/graphics/icons/biter-spawner.png",
	icon_size = 32,
	flags = {"placeable-player", "placeable-enemy", "not-repairable"},
	max_health = biterAttributes.health,
	order="b-b-g",
	subgroup="enemies",
	loot = biterAttributes.loot,
	resistances = resistances,
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
	collision_box = {{-3.0 * biterAttributes.scale, -2.0 * biterAttributes.scale}, {2.0 * biterAttributes.scale, 2.0 * biterAttributes.scale}},
	selection_box = {{-3.5 * biterAttributes.scale, -2.5 * biterAttributes.scale}, {2.5 * biterAttributes.scale, 2.5 * biterAttributes.scale}},
	-- in ticks per 1 pu
	pollution_absorbtion_absolute = biterAttributes.pollutionAbsorbtionAbs or 20,
	pollution_absorbtion_proportional = biterAttributes.pollutionAbsorbtionPro or 0.01,
	corpse = biterAttributes.corpse,
	dying_explosion = "blood-explosion-huge",
	max_count_of_owned_units = biterAttributes.unitsOwned or 7,
	max_friends_around_to_spawn = biterAttributes.unitsToSpawn or 5,
	animations =
	    {
		spawner_idle_animation(0, biterAttributes.tint, biterAttributes.scale),
		spawner_idle_animation(1, biterAttributes.tint, biterAttributes.scale),
		spawner_idle_animation(2, biterAttributes.tint, biterAttributes.scale),
		spawner_idle_animation(3, biterAttributes.tint, biterAttributes.scale)
	    },
	result_units = unitSet,
	-- With zero evolution the spawn rate is 6 seconds, with max evolution it is 2.5 seconds
	spawning_cooldown = biterAttributes.spawningCooldown or {360, 150},
	spawning_radius = biterAttributes.spawningRadius or 10,
	spawning_spacing = biterAttributes.spawningSpacing or 3,
	max_spawn_shift = 0,
	max_richness_for_spawn_shift = 100,
	build_base_evolution_requirement = biterAttributes.evolutionRequirement or 0.0,
	call_for_help_radius = 50
    }
    if biterAttributes.autoplace then
	o["autoplace"] = enemy_spawner_autoplace(biterAttributes.autoplace)
    end
    return o
end

function biterFunctions.makeWorm(name, attributes, attack, wormResistances)
    local resistances = {}
    for k,v in pairs(wormResistances) do
	v.type = k
	resistances[#resistances+1] = v
    end
--    print(name .. " " .. attributes.health)
    local o = {
	type = "turret",
	name = name .. "-rampant",
	icon = "__base__/graphics/icons/medium-worm.png",
	icon_size = 32,
	flags = attributes.flags or {"placeable-player", "placeable-enemy", "not-repairable", "breaths-air"},
	order="b-b-e",
	subgroup="enemies",
	max_health = attributes.health,
	loot = attributes.loot,
	resistances = resistances,
	healing_per_tick = attributes.healing or 0.01,
	collision_box = {{-1.1 * attributes.scale, -1.0 * attributes.scale}, {1.1 * attributes.scale, 1.0 * attributes.scale}},
	selection_box = {{-1.1 * attributes.scale, -1.0 * attributes.scale}, {1.1 * attributes.scale, 1.0 * attributes.scale}},
	shooting_cursor_size = attributes.cursorSize or 3,
	rotation_speed = attributes.rotationSpeed or 1,
	corpse = attributes.corpse,
	dying_explosion = "blood-explosion-big",
	inventory_size = attributes.inventorySize,
	dying_sound = make_worm_dying_sounds(0.9),
	folded_speed = 0.01,
	folded_animation = worm_folded_animation(attributes.scale, attributes.tint),
	preparing_speed = attributes.preparingSpeed or 0.025,
	preparing_animation = worm_preparing_animation(attributes.scale, attributes.tint, "forward"),
	prepared_speed = 0.015,
	prepared_animation = worm_prepared_animation(attributes.scale, attributes.tint),
	starting_attack_speed = 0.03,
	starting_attack_animation = worm_attack_animation(attributes.scale, attributes.tint, "forward"),
	starting_attack_sound = make_worm_roars(0.8),
	ending_attack_speed = 0.03,
	ending_attack_animation = worm_attack_animation(attributes.scale, attributes.tint, "backward"),
	folding_speed = attributes.foldingSpeed or 0.015,
	folding_animation =  worm_preparing_animation(attributes.scale, attributes.tint, "backward"),
	prepare_range = attributes.prepareRange or 30,
	attack_parameters = attack,
	build_base_evolution_requirement = attributes.evolutionRequirement or 0.0,
	call_for_help_radius = 40
    }

    if attributes.autoplace then
	o["autoplace"] = enemy_worm_autoplace(attributes.autoplace)
    end
    return o

end

function biterFunctions.createSuicideAttack(attributes, blastWave)
    local o = {
	type = "projectile",
	range = attributes.range or 0.5,
	cooldown = attributes.cooldown or 35,
	ammo_category = "melee",
	ammo_type = {
	    category = "biological"
	},
	sound = make_biter_roars(0.5),
	animation = biterattackanimation(attributes.scale, attributes.tint1, attributes.tint2)
    }

    if attributes.nuclear then
	o.ammo_type.action = {
	    type = "direct",
	    action_delivery =
		{
		    type = "instant",
		    target_effects =
			{
			    {
				repeat_count = 100,
				type = "create-trivial-smoke",
				smoke_name = "nuclear-smoke",
				offset_deviation = {{-1, -1}, {1, 1}},
				slow_down_factor = 1,
				starting_frame = 3,
				starting_frame_deviation = 5,
				starting_frame_speed = 0,
				starting_frame_speed_deviation = 5,
				speed_from_center = 0.5,
				speed_deviation = 0.2
			    },
			    {
				type = "create-entity",
				entity_name = "explosion"
			    },
			    {
				type = "damage",
				damage = {amount = attributes.damage or 400, type = attributes.damageType or "explosion"}
			    },
			    {
				type = "create-entity",
				entity_name = "small-scorchmark",
				check_buildability = true
			    },
			    {
				type = "nested-result",
				action =
				    {
					type = "area",
					target_entities = false,
					repeat_count = attributes.repeatCount or 2000,
					radius = attributes.radius or 35,
					action_delivery =
					    {
						type = "projectile",
						projectile = blastWave or "atomic-bomb-wave",
						starting_speed = 0.5
					    }
				    }
			    }
			}
		}
	}
    else
	o.ammo_type.action = {
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
				    radius = attributes.radius,
				    action_delivery = {
					type = "instant",
					target_effects = {
					    {
						type = "damage",
						damage = {
						    amount = attributes.damage,
						    type = attributes.damageType or "explosion"
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
				    radius = attributes.radius,
				    action_delivery = {
					type = "instant",
					target_effects = {
					    {
						type = "damage",
						damage = {
						    amount = attributes.damage,
						    type = attributes.damageType or "explosion"
						}
					    }
					}
				    }
				}
			    }
			}
		    }
	    }

	}

    end
    return o
end

function biterFunctions.makeWormAlienLootTable(name)
    local biterLoot

    local a = settings.startup["bobmods-enemies-enableartifacts"]
    local b = settings.startup["NE_Alien_Artifacts"]
    local d = settings.startup["NE_Bio_Ammo_Damage"]
    local artifacts = (a and a.value) or (b and b.value) or d
    local c = settings.startup["bobmods-enemies-enablenewartifacts"]
    local newArtifacts = c and c.value

    if newArtifacts and name then
	biterLoot = {
	    [1] = {  item = "alien-artifact-" .. name,  count_min = 1,  count_max = 1,  probability = 0.5 },
	    [2] = {  item = "alien-artifact-" .. name,  count_min = 1,  count_max = 2,  probability = 0.5 },
	    [3] = {  item = "alien-artifact-" .. name,  count_min = 1,  count_max = 3,  probability = 0.5 },
	    [4] = {  item = "alien-artifact-" .. name,  count_min = 2,  count_max = 3,  probability = 0.5 },
	    [5] = {  item = "alien-artifact-" .. name,  count_min = 2,  count_max = 3,  probability = 0.5 },
	    [6] = {  item = "alien-artifact-" .. name,  count_min = 2,  count_max = 3,  probability = 0.5 },
	    [7] = {  item = "alien-artifact-" .. name,  count_min = 2,  count_max = 4,  probability = 0.75 },
	    [8] = {  item = "alien-artifact-" .. name,  count_min = 2,  count_max = 4,  probability = 0.75 },
	    [9] = {  item = "alien-artifact-" .. name,  count_min = 2,  count_max = 4,  probability = 0.75 },
	    [10] = {  item = "alien-artifact-" .. name,  count_min = 2,  count_max = 4,  probability = 0.75 }
	}
    elseif artifacts then
	biterLoot = {
	    [1] = {  item = "alien-artifact",  count_min = 1,  count_max = 1,  probability = 0.5 },
	    [2] = {  item = "alien-artifact",  count_min = 1,  count_max = 2,  probability = 0.5 },
	    [3] = {  item = "alien-artifact",  count_min = 1,  count_max = 3,  probability = 0.5 },
	    [4] = {  item = "alien-artifact",  count_min = 2,  count_max = 3,  probability = 0.5 },
	    [5] = {  item = "alien-artifact",  count_min = 2,  count_max = 3,  probability = 0.5 },
	    [6] = {  item = "alien-artifact",  count_min = 2,  count_max = 3,  probability = 0.5 },
	    [7] = {  item = "alien-artifact",  count_min = 2,  count_max = 4,  probability = 0.75 },
	    [8] = {  item = "alien-artifact",  count_min = 2,  count_max = 4,  probability = 0.75 },
	    [9] = {  item = "alien-artifact",  count_min = 2,  count_max = 4,  probability = 0.75 },
	    [10] = {  item = "alien-artifact",  count_min = 3,  count_max = 4,  probability = 0.75 }
	}
    end

    return biterLoot
end

function biterFunctions.makeSpawnerAlienLootTable(name)
    local biterLoot
    local a = settings.startup["bobmods-enemies-enableartifacts"]
    local b = settings.startup["NE_Alien_Artifacts"]
    local d = settings.startup["NE_Bio_Ammo_Damage"]
    local artifacts = (a and a.value) or (b and b.value) or d
    local c = settings.startup["bobmods-enemies-enablenewartifacts"]
    local newArtifacts = c and c.value

    if newArtifacts and name then
	biterLoot = {
	    [1] = {  item = "alien-artifact-" .. name,  count_min = 1,  count_max = 1,  probability = 0.5 },
	    [2] = {  item = "alien-artifact-" .. name,  count_min = 1,  count_max = 2,  probability = 0.5 },
	    [3] = {  item = "alien-artifact-" .. name,  count_min = 1,  count_max = 3,  probability = 0.5 },
	    [4] = {  item = "alien-artifact-" .. name,  count_min = 2,  count_max = 4,  probability = 0.5 },
	    [5] = {  item = "alien-artifact-" .. name,  count_min = 2,  count_max = 5,  probability = 0.5 },
	    [6] = {  item = "alien-artifact-" .. name,  count_min = 2,  count_max = 5,  probability = 0.5 },
	    [7] = {  item = "alien-artifact-" .. name,  count_min = 2,  count_max = 6,  probability = 0.75 },
	    [8] = {  item = "alien-artifact-" .. name,  count_min = 2,  count_max = 6,  probability = 0.75 },
	    [9] = {  item = "alien-artifact-" .. name,  count_min = 3,  count_max = 7,  probability = 0.75 },
	    [10] = {  item = "alien-artifact-" .. name,  count_min = 3,  count_max = 7,  probability = 0.75 }
	}
    elseif artifacts then
	biterLoot = {
	    [1] = {  item = "alien-artifact",  count_min = 1,  count_max = 1,  probability = 0.5 },
	    [2] = {  item = "alien-artifact",  count_min = 1,  count_max = 2,  probability = 0.5 },
	    [3] = {  item = "alien-artifact",  count_min = 1,  count_max = 3,  probability = 0.5 },
	    [4] = {  item = "alien-artifact",  count_min = 2,  count_max = 4,  probability = 0.5 },
	    [5] = {  item = "alien-artifact",  count_min = 2,  count_max = 5,  probability = 0.5 },
	    [6] = {  item = "alien-artifact",  count_min = 1,  count_max = 3,  probability = 0.5 },
	    [7] = {  item = "alien-artifact",  count_min = 2,  count_max = 5,  probability = 0.75 },
	    [8] = {  item = "alien-artifact",  count_min = 3,  count_max = 6,  probability = 0.75 },
	    [9] = {  item = "alien-artifact",  count_min = 3,  count_max = 6,  probability = 0.75 },
	    [10] = {  item = "alien-artifact",  count_min = 3,  count_max = 7,  probability = 0.75 }
	}
    end

    return biterLoot
end

function biterFunctions.makeUnitAlienLootTable(name)
    local biterLoot
    local a = settings.startup["bobmods-enemies-enableartifacts"]
    local b = settings.startup["NE_Alien_Artifacts"]
    local artifacts = (a and a.value) or (b and b.value)
    local c = settings.startup["bobmods-enemies-enablesmallartifacts"]
    local smallArtifacts = (c and c.value) or (b and b.value)
    local d = settings.startup["bobmods-enemies-enablenewartifacts"]
    local newArtifacts = d and d.value

    if (c and c.value) and newArtifacts and name then
	biterLoot = {
	    [1] = {  item = "small-alien-artifact-" .. name,  count_min = 1,  count_max = 1,  probability = 0.25 },
	    [2] = {  item = "small-alien-artifact-" .. name,  count_min = 1,  count_max = 2,  probability = 0.25 },
	    [3] = {  item = "small-alien-artifact-" .. name,  count_min = 1,  count_max = 3,  probability = 0.25 },
	    [4] = {  item = "small-alien-artifact-" .. name,  count_min = 2,  count_max = 4,  probability = 0.5 },
	    [5] = {  item = "small-alien-artifact-" .. name,  count_min = 2,  count_max = 5,  probability = 0.5 },
	    [6] = {  item = "small-alien-artifact-" .. name,  count_min = 2,  count_max = 5,  probability = 0.5 },
	    [7] = {  item = "small-alien-artifact-" .. name,  count_min = 2,  count_max = 6,  probability = 0.75 },
	    [8] = {  item = "small-alien-artifact-" .. name,  count_min = 2,  count_max = 6,  probability = 0.75 },
	    [9] = {  item = "small-alien-artifact-" .. name,  count_min = 3,  count_max = 7,  probability = 0.75 },
	    [10] = {  item = "small-alien-artifact-" .. name,  count_min = 3,  count_max = 7,  probability = 0.75 }
	}
    elseif smallArtifacts then
	biterLoot = {
	    [1] = {  item = "small-alien-artifact",  count_min = 1,  count_max = 1,  probability = 0.25 },
	    [2] = {  item = "small-alien-artifact",  count_min = 1,  count_max = 2,  probability = 0.25 },
	    [3] = {  item = "small-alien-artifact",  count_min = 1,  count_max = 3,  probability = 0.25 },
	    [4] = {  item = "small-alien-artifact",  count_min = 2,  count_max = 4,  probability = 0.5 },
	    [5] = {  item = "small-alien-artifact",  count_min = 2,  count_max = 5,  probability = 0.5 },
	    [6] = {  item = "small-alien-artifact",  count_min = 2,  count_max = 5,  probability = 0.5 },
	    [7] = {  item = "small-alien-artifact",  count_min = 2,  count_max = 6,  probability = 0.75 },
	    [8] = {  item = "small-alien-artifact",  count_min = 2,  count_max = 6,  probability = 0.75 },
	    [9] = {  item = "small-alien-artifact",  count_min = 3,  count_max = 7,  probability = 0.75 },
	    [10] = {  item = "small-alien-artifact",  count_min = 3,  count_max = 7,  probability = 0.75 }
	}
    elseif artifacts then
	biterLoot = {
	    [1] = nil,
	    [2] = nil,
	    [3] = nil,
	    [4] = nil,
	    [5] = nil,
	    [6] = {  item = "alien-artifact",  count_min = 1,  count_max = 3,  probability = 0.5 },
	    [7] = {  item = "alien-artifact",  count_min = 2,  count_max = 5,  probability = 0.75 },
	    [8] = {  item = "alien-artifact",  count_min = 3,  count_max = 6,  probability = 0.75 },
	    [9] = {  item = "alien-artifact",  count_min = 3,  count_max = 6,  probability = 0.75 },
	    [10] = {  item = "alien-artifact",  count_min = 3,  count_max = 7,  probability = 0.75 }
	}
    end

    return biterLoot
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

function biterFunctions.acidSplashSounds()
    return {
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
    }
end

function biterFunctions.createElectricAttack(attributes, electricBeam, animation)
    return
	{
	    type = "beam",
	    ammo_category = "combat-robot-beam",
	    cooldown = attributes.cooldown or 20,
	    min_attack_distance = (attributes.range and (attributes.range - 2)) or 15,
	    range = (attributes.range and (attributes.range + 2)) or 15,
	    ammo_type =
		{
		    category = "combat-robot-beam",
		    action =
			{
			    type = "line",
			    range = (attributes.range and (attributes.range + 2)) or 15,
			    width = attributes.width or 0.5,
			    action_delivery = (attributes.actions and attributes.actions(attributes, electricBeam)) or
				{
				    type = "beam",
				    beam = electricBeam or "electric-beam",
				    duration = attributes.duration or 20
				}
			}
		},
	    animation = animation
	}
end

function biterFunctions.createProjectileAttack(attributes, projectile, animation)
    return {
        type = "projectile",
        ammo_category = "rocket",
        cooldown = attributes.cooldown or 15,
        projectile_creation_distance = 0.6,
        range = attributes.range or 20,
        ammo_type =
	    {
		category = "rocket",
		clamp_position = true,
		target_type = "position",
		action =
		    {
			type = "direct",
			action_delivery =
			    {
				type = "projectile",
				projectile = projectile or "defender-bullet",
				starting_speed = attributes.startingSpeed or 0.3,
				max_range = attributes.maxRange or attributes.range or 20
			    }
		    }
	    },
	animation = animation
    }
end

function biterFunctions.createMeleeAttack(attributes)
    return {
	type = "projectile",
	range = attributes.range or 0.5,
	cooldown = attributes.cooldown or 35,
	ammo_category = "melee",
	ammo_type = {
	    category = "melee",
	    target_type = "entity",
	    action =
		{
		    type = "direct",
		    action_delivery =
			{
			    type = "instant",
			    target_effects =
				{
				    type = "damage",
				    damage = { amount = attributes.damage, type = attributes.damageType or "physical" }
				}
			}
		}
	},
	sound = make_biter_roars(0.4),
	animation = biterattackanimation(attributes.scale, attributes.tint1, attributes.tint2)
    }
end

function biterFunctions.biterAttackSounds()
    return {
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
    }
end

function biterFunctions.createRangedAttack(attributes, attack, animation)
    if (attributes.type == "stream") or FORCE_OLD_PROJECTILES then
	return biterFunctions.createStreamAttack(attributes, attack, animation)
    elseif (attributes.type == "projectile") then
	return biterFunctions.createProjectileAttack(attributes, attack, animation)
    end
end

function biterFunctions.createStreamAttack(attributes, fireAttack, animation)
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
                begin_sound = biterFunctions.biterAttackSounds(),
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
	    },
	animation = animation
    }

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
