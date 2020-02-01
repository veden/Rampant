local sounds = require("__base__.prototypes.entity.demo-sounds")
local biterFunctions = {}

local unitSpawnerUtils = require("UnitSpawnerUtils")
local unitUtils = require("UnitUtils")
local wormUtils = require("WormUtils")

-- local FORCE_OLD_PROJECTILES = settings.startup["rampant-forceOldProjectiles"].value

local biterdieanimation = unitUtils.biterdieanimation
local biterattackanimation = unitUtils.biterattackanimation
local biterrunanimation = unitUtils.biterrunanimation
local spitter_alternative_attacking_animation_sequence = unitUtils.spitter_alternative_attacking_animation_sequence
local spawner_integration = unitSpawnerUtils.spawner_integration
local spawner_idle_animation = unitSpawnerUtils.spawner_idle_animation
local spawner_die_animation = unitSpawnerUtils.spawner_die_animation
local wormFoldedAnimation = wormUtils.wormFoldedAnimation
local wormPreparingAnimation = wormUtils.wormPreparingAnimation
local wormPreparedAnimation = wormUtils.wormPreparedAnimation
local wormPreparedAlternativeAnimation = wormUtils.wormPreparedAlternativeAnimation
local wormStartAttackAnimation = wormUtils.wormStartAttackAnimation
local wormEndAttackAnimation = wormUtils.wormEndAttackAnimation
local wormDieAnimation = wormUtils.wormDieAnimation


local function makeSpitterCorpse(attributes)
    local name = attributes.name .. "-corpse-rampant"

    local corpse = {
        type = "corpse",
        name = name,
        icon = "__base__/graphics/icons/big-biter-corpse.png",
        icon_size = 32,
        selectable_in_game = false,
        selection_box = {{-1, -1}, {1, 1}},
        subgroup="corpses",
        order = "c[corpse]-b[spitter]-a[small]",
        flags = {"placeable-neutral", "placeable-off-grid", "building-direction-8-way", "not-on-map"},
    }

    corpse.animation = spitterdyinganimation(attributes.scale, attributes.tint, attributes.tint2 or attributes.tint)
    corpse.dying_speed = 0.04
    corpse.time_before_removed = 15 * 60 * 60
    corpse.direction_shuffle = { { 1, 2, 3, 16 }, { 4, 5, 6, 7 }, { 8, 9, 10, 11 }, { 12, 13, 14, 15 } }
    corpse.shuffle_directions_at_frame = 4
    corpse.final_render_layer = "lower-object-above-shadow"

    corpse.ground_patch_render_layer = "decals" -- "transport-belt-integration"
    corpse.ground_patch_fade_in_delay = 1 / 0.02 --  in ticks; 1/dying_speed to delay the animation until dying animation finishes
    corpse.ground_patch_fade_in_speed = 0.002
    corpse.ground_patch_fade_out_start = 50 * 60
    corpse.ground_patch_fade_out_duration = 20 * 60

    local a = 1
    local d = 0.9
    corpse.ground_patch =
        {
            sheet =
                {
                    filename = "__base__/graphics/entity/biter/blood-puddle-var-main.png",
                    flags = { "low-object" },
                    line_length = 4,
                    variation_count = 4,
                    frame_count = 1,
                    width = 84,
                    height = 68,
                    shift = util.by_pixel(1, 0),
                    tint = attributes.tint2 or attributes.tint or {r = 0.6 * d * a, g = 0.1 * d * a, b = 0.6 * d * a, a = a},
                    scale = attributes.scale * 2,
                    hr_version =
                        {
                            filename = "__base__/graphics/entity/biter/hr-blood-puddle-var-main.png",
                            flags = { "low-object" },
                            line_length = 4,
                            variation_count = 4,
                            frame_count = 1,
                            width = 164,
                            height = 134,
                            shift = util.by_pixel(-0.5,-0.5),
                            tint = attributes.tint2 or attributes.tint or {r = 0.6 * d * a, g = 0.1 * d * a, b = 0.6 * d * a, a = a},
                            scale = attributes.scale
                        }
                }
        }

    data:extend(
        {
            corpse
    })
    return name
end

local function makeBiterCorpse(attributes)
    local name = attributes.name .. "-corpse-rampant"

    local corpse = {
        type = "corpse",
        name = name,
        icon = "__base__/graphics/icons/small-biter-corpse.png",
        icon_size = 32,
        selection_box = {{-0.8, -0.8}, {0.8, 0.8}},
        selectable_in_game = false,
        subgroup="corpses",
        order = "c[corpse]-a[biter]-a[small]",
        flags = {"placeable-neutral", "placeable-off-grid", "building-direction-8-way", "not-repairable", "not-on-map"}
    }

    corpse.animation = biterdieanimation(attributes.scale, attributes.tint, attributes.tint2 or attributes.tint, attributes.altBiter)
    corpse.dying_speed = 0.04
    corpse.time_before_removed = 15 * 60 * 60
    corpse.direction_shuffle = { { 1, 2, 3, 16 }, { 4, 5, 6, 7 }, { 8, 9, 10, 11 }, { 12, 13, 14, 15 } }
    corpse.shuffle_directions_at_frame = 7
    corpse.final_render_layer = "lower-object-above-shadow"

    corpse.ground_patch_render_layer = "decals" -- "transport-belt-integration"
    corpse.ground_patch_fade_in_delay = 1 / 0.02 --  in ticks; 1/dying_speed to delay the animation until dying animation finishes
    corpse.ground_patch_fade_in_speed = 0.002
    corpse.ground_patch_fade_out_start = 50 * 60
    corpse.ground_patch_fade_out_duration = 20 * 60
    
    local a = 1
    local d = 0.9
    corpse.ground_patch =
        {
            sheet =
                {
                    filename = "__base__/graphics/entity/biter/blood-puddle-var-main.png",
                    flags = { "low-object" },
                    line_length = 4,
                    variation_count = 4,
                    frame_count = 1,
                    width = 84,
                    height = 68,
                    shift = util.by_pixel(1, 0),
                    tint = attributes.tint2 or attributes.tint or {r = 0.6 * d * a, g = 0.1 * d * a, b = 0.6 * d * a, a = a},
                    scale = attributes.scale * 2,
                    hr_version =
                        {
                            filename = "__base__/graphics/entity/biter/hr-blood-puddle-var-main.png",
                            flags = { "low-object" },
                            line_length = 4,
                            variation_count = 4,
                            frame_count = 1,
                            width = 164,
                            height = 134,
                            shift = util.by_pixel(-0.5,-0.5),
                            tint = attributes.tint2 or attributes.tint or {r = 0.6 * d * a, g = 0.1 * d * a, b = 0.6 * d * a, a = a},
                            scale = attributes.scale
                        }
                }
        }

    data:extend({
            corpse
    })
    return name
end

local function makeUnitSpawnerCorpse(attributes)
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
                        spawner_die_animation(0, attributes.tint, attributes.scale, attributes.tint2),
                        spawner_die_animation(1, attributes.tint, attributes.scale, attributes.tint2),
                        spawner_die_animation(2, attributes.tint, attributes.scale, attributes.tint2),
                        spawner_die_animation(3, attributes.tint, attributes.scale, attributes.tint2)
                    }
            }
    })
    return name
end

local function makeWormCorpse(attributes)
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
                final_render_layer = "lower-object-above-shadow",
                animation = wormDieAnimation(attributes.scale, attributes.tint, attributes.tint2 or attributes.tint),
                ground_patch =
                    {
                        sheet = worm_integration(attributes.scale)
                    }
            }
    })
    return name
end

function biterFunctions.makeBiter(attributes)
    local resistances = {}
    for k,v in pairs(attributes.resistances) do
        v.type = k
        resistances[#resistances+1] = v
    end
    --    print(name .. " " .. biterAttributes.health)
    local entity = {
        type = "unit",
        name = attributes.name .. "-rampant",
        icon = "__base__/graphics/icons/small-biter.png",
        icon_size = 32,
        flags = attributes.flags or {"placeable-player", "placeable-enemy", "placeable-off-grid", "breaths-air"},
        max_health = attributes.health,
        order = "b-b-a",
        subgroup="enemies",
        healing_per_tick = attributes.healing,
        resistances = resistances,
        collision_box = {
            {-0.4 * attributes.scale, -0.4 * attributes.scale},
            {0.4 * attributes.scale, 0.4 * attributes.scale}
        },
        selection_box = {
            {-0.7 * attributes.scale, -1.5 * attributes.scale},
            {0.7 * attributes.scale, 0.3 * attributes.scale}
        },
        sticker_box = {
            {-0.6 * attributes.scale, -0.8 * attributes.scale},
            {0.6 * attributes.scale, 0}
        },
        attack_parameters = attributes.attack,
        vision_distance = attributes.vision or 30,
        movement_speed = attributes.movement,
        loot = attributes.loot,
        spawning_time_modifier = attributes.spawningTimeModifer or nil,
        distance_per_frame = attributes.distancePerFrame or 0.1,
        pollution_to_join_attack = attributes.pollutionToAttack or 200,
        distraction_cooldown = attributes.distractionCooldown or 300,
        corpse = makeBiterCorpse(attributes),
        dying_explosion = attributes.explosion,
        dying_trigger_effect = attributes.dyingEffect, 
        enemy_map_color = ((not settings.startup["rampant-oldRedEnemyMapColor"].value) and attributes.tint2) or nil,
        affected_by_tiles = true,
        dying_sound = sounds.biter_dying(0.3 + (0.05 * attributes.effectiveLevel)),
        working_sound =  sounds.biter_calls(0.2 + (0.05 * attributes.effectiveLevel)),
        run_animation = biterrunanimation(attributes.scale, attributes.tint, attributes.tint2 or attributes.tint, attributes.altBiter),
        ai_settings = { destroy_when_commands_fail = false, allow_try_return_to_spawner = true, path_resolution_modifier = -5, do_seperation = true }
    }
    if attributes.collisionMask then
        entity.collision_mask = attributes.collisionMask
    end
    return entity
end

function biterFunctions.makeSpitter(attributes)
    local resistances = {}
    for k,v in pairs(attributes.resistances) do
        v.type = k
        resistances[#resistances+1] = v
    end
    --    print(name .. " " .. biterAttributes.health)
    local entity = {
        type = "unit",
        name = attributes.name .. "-rampant",
        icon = "__base__/graphics/icons/small-spitter.png",
        icon_size = 32,
        flags = attributes.flags or {"placeable-player", "placeable-enemy", "placeable-off-grid", "breaths-air"},
        max_health = attributes.health,
        order = "b-b-a",
        subgroup="enemies",
        healing_per_tick = attributes.healing,
        resistances = resistances,
        collision_box = {{-0.4 * attributes.scale, -0.4 * attributes.scale},
            {0.4 * attributes.scale, 0.4 * attributes.scale}},
        selection_box = {{-0.7 * attributes.scale, -1.5 * attributes.scale},
            {0.7 * attributes.scale, 0.3 * attributes.scale}},
        sticker_box = {{-0.6 * attributes.scale, -0.8 * attributes.scale},
            {0.6 * attributes.scale, 0}},
        attack_parameters = attributes.attack,
        loot = attributes.loot,
        vision_distance = attributes.vision or 30,
        movement_speed = attributes.movement,
        spawning_time_modifier = attributes.spawningTimeModifer or nil,
        distance_per_frame = attributes.distancePerFrame or 0.1,
        pollution_to_join_attack = attributes.pollutionToAttack or 200,
        distraction_cooldown = attributes.distractionCooldown or 300,
        alternative_attacking_frame_sequence = spitter_alternative_attacking_animation_sequence(),
        corpse = makeSpitterCorpse(attributes),
        dying_explosion = attributes.explosion,
        enemy_map_color = ((not settings.startup["rampant-oldRedEnemyMapColor"].value) and attributes.tint2) or nil,
        dying_trigger_effect = attributes.dyingEffect,        
        dying_sound =  sounds.spitter_dying(0.3 + (0.05 * attributes.effectiveLevel)),
        working_sound =  sounds.biter_calls(0.2 + (0.05 * attributes.effectiveLevel)),
        affected_by_tiles = true,
        run_animation = spitterrunanimation(attributes.scale, attributes.tint, attributes.tint2 or attributes.tint),
        ai_settings = { destroy_when_commands_fail = false, allow_try_return_to_spawner = true, path_resolution_modifier = -5, do_seperation = true }
    }
    if attributes.collisionMask then
        entity.collision_mask = attributes.collisionMask
    end
    return entity
end

function biterFunctions.makeUnitSpawner(attributes)
    local resistances = {}
    for k,v in pairs(attributes.resistances) do
        v.type = k
        resistances[#resistances+1] = v
    end
    -- print(attributes.name)
    local o = {
        type = "unit-spawner",
        name = attributes.name .. "-rampant",
        icon = "__base__/graphics/icons/biter-spawner.png",
        icon_size = 32,
        flags = {"placeable-player", "placeable-enemy", "not-repairable"},
        max_health = attributes.health,
        order="b-b-g",
        subgroup="enemies",
        loot = attributes.loot,
        resistances = resistances,
        working_sound =
		{
            sound =
                {
                    {
                        filename = "__base__/sound/creatures/spawner.ogg",
                        volume = 0.2 + (0.05 * attributes.effectiveLevel)
                    }
                },
            apparent_volume = 0.4 + (0.1 * attributes.effectiveLevel)
        },
        dying_sound =
            {
                {
                    filename = "__base__/sound/creatures/spawner-death-1.ogg",
                    volume = 0.4 + (0.05 * attributes.effectiveLevel)
                },
                {
                    filename = "__base__/sound/creatures/spawner-death-2.ogg",
                    volume = 0.4 + (0.05 * attributes.effectiveLevel)
                }
            },
        healing_per_tick = attributes.healing or 0.02,
        collision_box = {{-3.0 * attributes.scale, -2.0 * attributes.scale}, {2.0 * attributes.scale, 2.0 * attributes.scale}},
        selection_box = {{-3.5 * attributes.scale, -2.5 * attributes.scale}, {2.5 * attributes.scale, 2.5 * attributes.scale}},
        -- in ticks per 1 pu
        pollution_absorption_absolute = attributes.pollutionAbsorptionAbs or 20,
        pollution_absorption_proportional = attributes.pollutionAbsorptionPro or 0.01,
        map_generator_bounding_box = {{-4.2 * attributes.scale, -3.2 * attributes.scale}, {3.2 * attributes.scale, 3.2 * attributes.scale}},
        corpse = makeUnitSpawnerCorpse(attributes),
        dying_explosion = attributes.explosion or "blood-explosion-huge",
        dying_trigger_effect = attributes.dyingEffect,
        max_count_of_owned_units = attributes.unitsOwned or 7,
        max_friends_around_to_spawn = attributes.unitsToSpawn or 5,
        enemy_map_color = ((not settings.startup["rampant-oldRedEnemyMapColor"].value) and attributes.tint2) or nil,
        animations =
            {
                spawner_idle_animation(0, attributes.tint, attributes.scale, attributes.tint2 or attributes.tint),
                spawner_idle_animation(1, attributes.tint, attributes.scale, attributes.tint2 or attributes.tint),
                spawner_idle_animation(2, attributes.tint, attributes.scale, attributes.tint2 or attributes.tint),
                spawner_idle_animation(3, attributes.tint, attributes.scale, attributes.tint2 or attributes.tint)
            },
        integration =
            {
                sheet = spawner_integration(attributes.scale)
            },
        result_units = attributes.unitSet,
        -- With zero evolution the spawn rate is 6 seconds, with max evolution it is 2.5 seconds
        spawning_cooldown = attributes.spawningCooldown or ((attributes.spawningCooldownStart and attributes.spawningCooldownEnd) and {attributes.spawningCooldownStart, attributes.spawningCooldownEnd}) or {360, 150},
        spawning_radius = attributes.spawningRadius or 10,
        spawning_spacing = attributes.spawningSpacing or 3,
        max_spawn_shift = 0,
        max_richness_for_spawn_shift = 100,
        build_base_evolution_requirement = attributes.evolutionRequirement or 0.0,
        call_for_help_radius = attributes.helpRadius or 50
    }
    if attributes.autoplace then
        o["autoplace"] = enemy_spawner_autoplace(attributes.autoplace)
    end
    return o
end

function biterFunctions.makeWorm(attributes)
    local resistances = {}
    for k,v in pairs(attributes.resistances) do
        v.type = k
        resistances[#resistances+1] = v
    end
    --    print(name .. " " .. attributes.health)
    local o = {
        type = "turret",
        name = attributes.name .. "-rampant",
        icon = "__base__/graphics/icons/medium-worm.png",
        icon_size = 32,
        flags = attributes.flags or {"placeable-player", "placeable-enemy", "not-repairable", "breaths-air"},
        order="b-b-e",
        subgroup="enemies",
        max_health = attributes.health,
        loot = attributes.loot,
        shooting_cursor_size = 3.5 * attributes.scale,
        resistances = resistances,
        healing_per_tick = attributes.healing or 0.01,
        collision_box = {{-1.1 * attributes.scale, -1.0 * attributes.scale}, {1.1 * attributes.scale, 1.0 * attributes.scale}},
        selection_box = {{-1.1 * attributes.scale, -1.0 * attributes.scale}, {1.1 * attributes.scale, 1.0 * attributes.scale}},
        shooting_cursor_size = attributes.cursorSize or 3,
        rotation_speed = attributes.rotationSpeed or 1,
        corpse = makeWormCorpse(attributes),
        dying_explosion = attributes.explosion or "blood-explosion-big",
        dying_trigger_effect = attributes.dyingEffect,
        inventory_size = attributes.inventorySize,
        dying_sound = sounds.worm_dying(0.3 + (0.05 * attributes.effectiveLevel)),
        folded_speed = 0.01,
        folded_speed_secondary = 0.024,
        folded_animation = wormFoldedAnimation(attributes.scale, attributes.tint, attributes.tint2 or attributes.tint),
        preparing_speed = attributes.preparingSpeed or 0.024,
        preparing_animation = wormPreparingAnimation(attributes.scale, attributes.tint, "forward", attributes.tint2 or attributes.tint),
        preparing_sound = sounds.worm_standup(0.4 + (0.05 * attributes.effectiveLevel)),
        prepared_speed = 0.024,
        prepared_speed_secondary = 0.012,
        prepared_animation = wormPreparedAnimation(attributes.scale, attributes.tint, attributes.tint2 or attributes.tint),
        prepared_sound = sounds.worm_breath(0.2 + (0.05 * attributes.effectiveLevel)),
        prepared_alternative_speed = 0.014,
        prepared_alternative_speed_secondary = 0.010,
        prepared_alternative_chance = 0.2,
        prepared_alternative_animation = wormPreparedAlternativeAnimation(attributes.scale, attributes.tint, attributes.tint2 or attributes.tint),
        prepared_alternative_sound = sounds.worm_roar_alternative(0.2 + (0.05 * attributes.effectiveLevel)),

        starting_attack_speed = 0.034,
        starting_attack_animation = wormStartAttackAnimation(attributes.scale, attributes.tint, attributes.tint2 or attributes.tint),
        starting_attack_sound = sounds.worm_roars(0.2 + (0.05 * attributes.effectiveLevel)),
        ending_attack_speed = 0.016,
        ending_attack_animation = wormEndAttackAnimation(attributes.scale, attributes.tint, attributes.tint2 or attributes.tint),
        folding_speed = attributes.foldingSpeed or 0.015,
        folding_animation =  wormPreparingAnimation(attributes.scale, attributes.tint, "backward", attributes.tint2 or attributes.tint),
        folding_sound = sounds.worm_fold (0.4 + (0.05 * attributes.effectiveLevel)),
        prepare_range = attributes.prepareRange or 30,
        integration = worm_integration(attributes.scale),
        attack_parameters = attributes.attack,
        secondary_animation = true,
        enemy_map_color = ((not settings.startup["rampant-oldRedEnemyMapColor"].value) and attributes.tint2) or nil,
        random_animation_offset = true,
        attack_from_start_frame = true,

        allow_turning_when_starting_attack = true,
        build_base_evolution_requirement = attributes.evolutionRequirement or 0.0,
        call_for_help_radius = attributes.helpRadius or 60
    }

    if attributes.autoplace then
        o["autoplace"] = enemy_worm_autoplace(attributes.autoplace)
    end
    return o

end

function biterFunctions.createSuicideAttack(attributes, blastWave, animation)
    local o = {
        type = "projectile",
        range = attributes.range or 0.5,
        cooldown = attributes.cooldown or 35,
        ammo_category = "melee",
        ammo_type = {
            category = "biological"
        },
        sound = sounds.biter_roars(0.3 + (attributes.effectiveLevel * 0.05)),
        animation = animation
    }

    if attributes.nuclear then
        o.ammo_type.action = {
            {
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
                                    entity_name = attributes.attackExplosion
                                },
                                {
                                    type = "damage",
                                    damage = {amount = attributes.damage or 400, type = attributes.damageType or "explosion"}
                                },
                                {
                                    type = "create-entity",
                                    entity_name = attributes.attackScorchmark or "small-scorchmark",
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
            },
            {
                type = "cluster",
                cluster_count = attributes.explosionCount,
                distance = attributes.explosionDistance,
                distance_deviation = attributes.explosionDistance,
                action_delivery =
                    {
                        type = "instant",
                        target_effects= {
                            {
                                type="create-entity",
                                entity_name = attributes.attackExplosion,
                            },
                            {
                                type = "create-entity",
                                entity_name = attributes.attackScorchmark or "small-scorchmark",
                                check_buildability = true
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
                                entity_name = attributes.attackExplosion
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
                                entity_name = attributes.attackScorchmark or "small-scorchmark",
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
                                entity_name = attributes.attackExplosion,
                            },
                            {
                                type = "create-entity",
                                entity_name = attributes.attackScorchmark or "small-scorchmark",
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

function biterFunctions.findRange(entity)
    return entity.attack_parameters.range
end

local function findKey(key, obj)
    for k,v in pairs(obj) do
        if (k == key) and v then
            return v
        elseif (type(v) == "table") then
            local val = findKey(key, v)
            if val then
                return val
            end
        end
    end
end

function biterFunctions.findRunScale(entity)
    return findKey("scale", entity.run_animation.layers)
end

function biterFunctions.findTint(entity)
    return findKey("tint", entity.run_animation.layers)
end

function biterFunctions.createElectricAttack(attributes, electricBeam, animation)
    return
        {
            type = "beam",
            ammo_category = "biological",
            cooldown = attributes.cooldown or 20,
            min_attack_distance = (attributes.range and (attributes.range - 2)) or 15,
            range = (attributes.range and (attributes.range + 2)) or 15,
            ammo_type =
                {
                    category = "biological",
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
        ammo_category = "biological",
        cooldown = attributes.cooldown or 15,
        projectile_creation_distance = 0.6,
        range = attributes.range or 20,
        lead_target_for_projectile_speed = 0.6,
        use_shooter_direction = true,
        ammo_type =
            {
                category = "biological",
                clamp_position = true,
                target_type = "position",
                action =
                    {
                        type = "direct",
                        action_delivery =
                            {
                                type = "projectile",
                                projectile = projectile or "defender-bullet",
                                starting_speed = attributes.startingSpeed or 0.6,
                                max_range = attributes.maxRange or (attributes.range + 1) or 20
                            }
                    }
            },
        animation = animation
    }
end

function biterFunctions.createMeleeAttack(attackAttributes)
    local meleeAttackEffects = {
        type = "damage",
        damage = { amount = attackAttributes.damage * 0.25, type = attackAttributes.damageType or "physical" }
    }

    if attackAttributes.meleePuddleGenerator then
        local o = {}
        o[1] = meleeAttackEffects
        o[2] = attackAttributes.meleePuddleGenerator(attackAttributes)
        meleeAttackEffects = o
    end
    
    return {
        type = "projectile",
        range = attackAttributes.range or 0.5,
        cooldown = attackAttributes.cooldown or 35,
        ammo_category = "melee",
        ammo_type = {
            category = "melee",
            target_type = "entity",
            action =
                {
                    {
                        type = "area",
                        radius = attackAttributes.radius,
                        force = "enemy",
                        ignore_collision_condition = true,
                        action_delivery =
                            {
                                type = "instant",
                                target_effects =
                                    {
                                        type = "damage",
                                        damage = { amount = attackAttributes.damage * 0.75, type = attackAttributes.damageType or "physical" }
                                    }
                            }
                    },
                    {
                        type = "direct",
                        action_delivery =
                            {
                                type = "instant",
                                target_effects = meleeAttackEffects
                            }
                    }
                }
        },
        sound = sounds.biter_roars(0.2 + (attackAttributes.effectiveLevel * 0.05)),
        animation = biterattackanimation(attackAttributes.scale, attackAttributes.tint, attackAttributes.tint2 or attackAttributes.tint, attackAttributes.altBiter)
    }
end

function biterFunctions.biterAttackSounds(effectiveLevel)
    return {
        {
            filename = "__base__/sound/creatures/Spiters_1_1.ogg",
            volume = 0.2 + (effectiveLevel * 0.05)
        },
        {
            filename = "__base__/sound/creatures/Spiters_1_2.ogg",
            volume = 0.2 + (effectiveLevel * 0.05)
        },
        {
            filename = "__base__/sound/creatures/Spiters_2_1.ogg",
            volume = 0.2 + (effectiveLevel * 0.05)
        },
        {
            filename = "__base__/sound/creatures/Spiters_2_2.ogg",
            volume = 0.2 + (effectiveLevel * 0.05)
        },
        {
            filename = "__base__/sound/creatures/Spiters_3_1.ogg",
            volume = 0.2 + (effectiveLevel * 0.05)
        },
        {
            filename = "__base__/sound/creatures/Spiters_3_2.ogg",
            volume = 0.2 + (effectiveLevel * 0.05)
        },
        {
            filename = "__base__/sound/creatures/Spiters_4_1.ogg",
            volume = 0.2 + (effectiveLevel * 0.05)
        },
        {
            filename = "__base__/sound/creatures/Spiters_4_2.ogg",
            volume = 0.2 + (effectiveLevel * 0.05)
        },
        {
            filename = "__base__/sound/creatures/Spiters_5_1.ogg",
            volume = 0.2 + (effectiveLevel * 0.05)
        },
        {
            filename = "__base__/sound/creatures/Spiters_5_2.ogg",
            volume = 0.2 + (effectiveLevel * 0.05)
        }
    }
end

function biterFunctions.createRangedAttack(attributes, attack, animation)
    if (attributes.attackType == "stream") then
        return biterFunctions.createStreamAttack(attributes, attack, animation)
    elseif (attributes.attackType == "projectile") then
        return biterFunctions.createProjectileAttack(attributes, attack, animation)
    else
        error("Unknown range attack")
    end
end

function biterFunctions.createStreamAttack(attributes, fireAttack, animation)
    local attack = {
        type = "stream",
        ammo_category = "biological",
        cooldown = attributes.cooldown,
        range = attributes.range,
        min_range = attributes.minRange,

        turn_range = attributes.turnRange,
        fire_penalty = attributes.firePenalty,

        warmup = attributes.warmup or 0,

        damage_modifier = attributes.damageModifier or 1.0,

        lead_target_for_projectile_speed = attributes.particleHoizontalSpeed or 0.6,

        projectile_creation_parameters = spitter_shoot_shiftings(attributes.scale, attributes.scale * 20),

        use_shooter_direction = true,

        gun_barrel_length = 2 * attributes.scale,
        gun_center_shift = {
            north = {0, -0.65 * attributes.scale},
            east = {0, 0},
            south = {0, 0},
            west = {0, 0}
        },
        ammo_type =
            {
                category = "biological",
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
                begin_sound = biterFunctions.biterAttackSounds(attributes.effectiveLevel),
                middle_sound =
                    {
                        {
                            filename = attributes.midSound or "__Rampant__/sounds/attacks/acid-mid.ogg",
                            volume = 0.2 + (attributes.effectiveLevel * 0.05)
                        }
                    },
                end_sound =
                    {
                        {
                            filename = attributes.endSound or "__Rampant__/sounds/attacks/acid-end.ogg",
                            volume = 0.2 + (attributes.effectiveLevel * 0.05)
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
