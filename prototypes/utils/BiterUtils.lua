-- Copyright (C) 2022  veden

-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <https://www.gnu.org/licenses/>.


local biterUtils = {}

local sounds = require("__base__.prototypes.entity.sounds")
local particleUtils = require("ParticleUtils")
local unitUtils = require("UnitUtils")
local wormUtils = require("WormUtils")

-- Animation functions still needed for worms
local biterdieanimation = unitUtils.biterdieanimation
local biterattackanimation = unitUtils.biterattackanimation
local biterrunanimation = unitUtils.biterrunanimation
local spitter_alternative_attacking_animation_sequence = unitUtils.spitter_alternative_attacking_animation_sequence
local wormFoldedAnimation = wormUtils.wormFoldedAnimation
local wormPreparingAnimation = wormUtils.wormPreparingAnimation
local wormPreparedAnimation = wormUtils.wormPreparedAnimation
local wormPreparedAlternativeAnimation = wormUtils.wormPreparedAlternativeAnimation
local wormStartAttackAnimation = wormUtils.wormStartAttackAnimation
local wormEndAttackAnimation = wormUtils.wormEndAttackAnimation
local wormDieAnimation = wormUtils.wormDieAnimation
local makeDamagedParticle = particleUtils.makeDamagedParticle
local biter_water_reflection = unitUtils.biter_water_reflection
local spitter_water_reflection = unitUtils.spitter_water_reflection

local DISALLOW_FRIENDLY_FIRE = settings.startup["rampant--disallowFriendlyFire"].value

local corpseSet = {}
-- local corpseCount = 0

local function makeSpitterCorpse(attributes)
    local name = "spitter-".. attributes.faction .. "-" .. attributes.effectiveLevel .. "-corpse-rampant"

    if not corpseSet[name] then
        corpseSet[name] = true
    else
        return name
    end

    -- Deep copy from vanilla spitter corpse
    local vanillaCorpseName = "small-spitter-corpse"
    if attributes.scale and attributes.scale > 1.0 then
        vanillaCorpseName = "medium-spitter-corpse"
    end
    local corpse = util.table.deepcopy(data.raw.corpse[vanillaCorpseName])

    corpse.name = name
    if corpse.ground_patch and corpse.ground_patch.sheet then
        corpse.ground_patch.sheet.tint = attributes.tint2 or attributes.tint or {r=0.54, g=0.09, b=0.54, a=1}
        corpse.ground_patch.sheet.scale = (corpse.ground_patch.sheet.scale or 0.5) * (attributes.scale / 0.5)
    end

    data:extend({corpse})
    return name
end

local function makeBiterCorpse(attributes)
    local name = "biter-" .. attributes.faction .. "-" .. attributes.effectiveLevel .. "-corpse-rampant"

    if not corpseSet[name] then
        corpseSet[name] = true
    else
        return name
    end

    -- Deep copy from vanilla biter corpse
    local vanillaCorpseName = "small-biter-corpse"
    if attributes.scale and attributes.scale > 1.0 then
        vanillaCorpseName = "medium-biter-corpse"
    end
    local corpse = util.table.deepcopy(data.raw.corpse[vanillaCorpseName])

    corpse.name = name
    if corpse.ground_patch and corpse.ground_patch.sheet then
        corpse.ground_patch.sheet.tint = attributes.tint2 or attributes.tint or {r=0.54, g=0.09, b=0.54, a=1}
        corpse.ground_patch.sheet.scale = (corpse.ground_patch.sheet.scale or 0.5) * (attributes.scale / 0.5)
    end

    data:extend({corpse})
    return name
end

-- Apply custom tints and scale to spawner animation layers (inherited from vanilla)
local function applySpawnerTints(animations, tint, scale, tint2)
    if not animations then return end
    for _, anim in pairs(animations) do
        if anim.layers then
            for _, layer in pairs(anim.layers) do
                layer.scale = scale
                -- Skip shadow layers
                if layer.draw_as_shadow then
                    goto continue_layer
                end
                -- Check for mask flag
                local isMask = false
                if layer.flags then
                    for _, f in pairs(layer.flags) do
                        if f == "mask" then isMask = true break end
                    end
                end
                -- Apply tint2 to mask layers, primary tint to main layers
                layer.tint = isMask and tint2 or tint
                ::continue_layer::
            end
        end
    end
end

local function makeUnitSpawnerCorpse(attributes)
    local name = "spawner-" .. attributes.faction .. "-" .. attributes.type .. "-" .. attributes.effectiveLevel .. "-corpse-rampant"
    if not corpseSet[name] then
        corpseSet[name] = true
    else
        return name
    end

    -- Deep copy from vanilla spawner corpse
    local vanillaName = (attributes.type == "spitter-spawner") and "spitter-spawner-corpse" or "biter-spawner-corpse"
    local vanillaPrototype = data.raw.corpse[vanillaName]
    local corpse = util.table.deepcopy(vanillaPrototype)

    corpse.name = name
    corpse.icon = "__base__/graphics/icons/biter-spawner-corpse.png"
    corpse.icon_size = 64
    corpse.icon_mipmaps = 4
    corpse.dying_speed = 0.04
    corpse.time_before_removed = 15 * 60 * 60
    corpse.order = "c[corpse]-c[spitter-spawner]"
    corpse.final_render_layer = "remnants"

    -- Apply custom tints to death and decay animations
    applySpawnerTints(corpse.animation, attributes.tint, attributes.scale, attributes.tint2)
    applySpawnerTints(corpse.decay_animation, attributes.tint, attributes.scale, attributes.tint2)

    data:extend({corpse})
    return name
end

local function makeWormCorpse(attributes)
    local name = "worm-" .. attributes.faction .. "-" .. attributes.effectiveLevel .. "-corpse-rampant"
    if not corpseSet[name] then
        corpseSet[name] = true
        -- corpseCount = corpseCount + 1
        -- print(corpseCount)
    else
        return name
    end

    data:extend({
            {
                type = "corpse",
                name = name,
                icon = "__base__/graphics/icons/medium-worm-corpse.png",
                icon_size = 64,
                icon_mipmaps = 4,
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

function biterUtils.getLocalisedName(attributes)
    if attributes.isRampant then
        return {
            "",
            {"rampant."..attributes.faction},
            {"rampant."..attributes.unit_name},
            {"rampant.t"..attributes.tier}
        }
    end

    return {attributes.name .. "-rampant"}
end

function biterUtils.getLocalisedDescription(attributes)
    if attributes.isRampant then
        return {
            "",
            {"entity-description."..attributes.unit_name}
        }
    end

    return {attributes.name .. "-rampant"}
end

function biterUtils.makeBiter(attributes)
    local resistances = {}
    for k,v in pairs(attributes.resistances) do
        v.type = k
        resistances[#resistances+1] = v
    end

    -- Deep copy from vanilla biter based on scale tier
    local vanillaName = "small-biter"
    if attributes.scale and attributes.scale > 1.0 then
        vanillaName = "medium-biter"
    end
    local vanillaPrototype = data.raw.unit[vanillaName]
    local entity = util.table.deepcopy(vanillaPrototype)

    -- Save vanilla attack animation (works) before we override attack_parameters
    local vanillaAttackAnimation = nil
    if vanillaPrototype.attack_parameters and vanillaPrototype.attack_parameters.animation then
        vanillaAttackAnimation = util.table.deepcopy(vanillaPrototype.attack_parameters.animation)
    end

    -- Override fields that differ from vanilla
    entity.name = attributes.name .. "-rampant"
    entity.localised_name = biterUtils.getLocalisedName(attributes)
    entity.max_health = attributes.health
    entity.healing_per_tick = attributes.healing
    entity.damaged_trigger_effect = ((not settings.startup["rampant--removeBloodParticles"].value) and makeDamagedParticle(attributes)) or nil
    entity.water_reflection = biter_water_reflection(attributes.scale)
    entity.resistances = resistances
    entity.collision_box = {
        {-0.4 * attributes.scale, -0.4 * attributes.scale},
        {0.4 * attributes.scale, 0.4 * attributes.scale}
    }
    entity.selection_box = {
        {-0.7 * attributes.scale, -1.5 * attributes.scale},
        {0.7 * attributes.scale, 0.3 * attributes.scale}
    }
    entity.sticker_box = {
        {-0.6 * attributes.scale, -0.8 * attributes.scale},
        {0.6 * attributes.scale, 0}
    }
    entity.attack_parameters = attributes.attack
    -- Restore working vanilla attack animation (custom one uses broken sprite defs)
    if vanillaAttackAnimation then
        entity.attack_parameters.animation = vanillaAttackAnimation
    end
    entity.vision_distance = attributes.vision or 30
    entity.movement_speed = attributes.movement
    entity.loot = attributes.loot
    entity.spawning_time_modifier = attributes.spawningTimeModifer or nil
    entity.distance_per_frame = attributes.distancePerFrame or 0.1
    entity.absorptions_to_join_attack = { pollution = attributes.pollutionToAttack or 200 }
    entity.distraction_cooldown = attributes.distractionCooldown or 300
    entity.corpse = makeBiterCorpse(attributes)
    entity.dying_explosion = attributes.explosion
    entity.dying_trigger_effect = attributes.dyingEffect
    entity.enemy_map_color = ((not settings.startup["rampant--oldRedEnemyMapColor"].value) and attributes.tint2) or nil
    entity.affected_by_tiles = settings.startup["rampant--unitsAffectedByTiles"].value
    entity.dying_sound = attributes.dyingSounds
    entity.walking_sound = attributes.walkingSounds

    if attributes.flags then
        entity.flags = attributes.flags
    end
    if attributes.collisionMask then
        entity.collision_mask = attributes.collisionMask
    end
    if attributes.appendFlags then
        for flag in pairs(attributes.appendFlags) do
            entity.flags[#entity.flags+1] = flag
        end
    end
    return entity
end

function biterUtils.makeSpitter(attributes)
    local resistances = {}
    for k,v in pairs(attributes.resistances) do
        v.type = k
        resistances[#resistances+1] = v
    end

    -- Deep copy from vanilla spitter based on scale tier
    local vanillaName = "small-spitter"
    if attributes.scale and attributes.scale > 1.0 then
        vanillaName = "medium-spitter"
    end
    local vanillaPrototype = data.raw.unit[vanillaName]
    local entity = util.table.deepcopy(vanillaPrototype)

    -- Save vanilla attack animation (works) before we override attack_parameters
    local vanillaAttackAnimation = nil
    if vanillaPrototype.attack_parameters and vanillaPrototype.attack_parameters.animation then
        vanillaAttackAnimation = util.table.deepcopy(vanillaPrototype.attack_parameters.animation)
    end

    -- Override fields that differ from vanilla
    entity.name = attributes.name .. "-rampant"
    entity.localised_name = biterUtils.getLocalisedName(attributes)
    entity.max_health = attributes.health
    entity.healing_per_tick = attributes.healing
    entity.resistances = resistances
    entity.collision_box = {{-0.4 * attributes.scale, -0.4 * attributes.scale},
        {0.4 * attributes.scale, 0.4 * attributes.scale}}
    entity.selection_box = {{-0.7 * attributes.scale, -1.5 * attributes.scale},
        {0.7 * attributes.scale, 0.3 * attributes.scale}}
    entity.sticker_box = {{-0.6 * attributes.scale, -0.8 * attributes.scale},
        {0.6 * attributes.scale, 0}}
    entity.attack_parameters = attributes.attack
    -- Restore working vanilla attack animation (custom one uses broken sprite defs)
    if vanillaAttackAnimation then
        entity.attack_parameters.animation = vanillaAttackAnimation
    end
    entity.loot = attributes.loot
    entity.vision_distance = attributes.vision or 30
    entity.movement_speed = attributes.movement
    entity.spawning_time_modifier = attributes.spawningTimeModifer or nil
    entity.distance_per_frame = attributes.distancePerFrame or 0.1
    entity.absorptions_to_join_attack = { pollution = attributes.pollutionToAttack or 200 }
    entity.distraction_cooldown = attributes.distractionCooldown or 300
    entity.alternative_attacking_frame_sequence = spitter_alternative_attacking_animation_sequence()
    entity.corpse = makeSpitterCorpse(attributes)
    entity.dying_explosion = attributes.explosion
    entity.enemy_map_color = ((not settings.startup["rampant--oldRedEnemyMapColor"].value) and attributes.tint2) or nil
    entity.dying_trigger_effect = attributes.dyingEffect
    entity.dying_sound = attributes.dyingSounds
    entity.walking_sound = attributes.walkingSounds
    entity.water_reflection = spitter_water_reflection(attributes.scale)
    entity.damaged_trigger_effect = ((not settings.startup["rampant--removeBloodParticles"].value) and makeDamagedParticle(attributes)) or nil
    entity.affected_by_tiles = settings.startup["rampant--unitsAffectedByTiles"].value

    if attributes.flags then
        entity.flags = attributes.flags
    end
    if attributes.collisionMask then
        entity.collision_mask = attributes.collisionMask
    end
    if attributes.appendFlags then
        for flag in pairs(attributes.appendFlags) do
            entity.flags[#entity.flags+1] = flag
        end
    end
    return entity
end

function biterUtils.makeUnitSpawner(attributes)
    local resistances = {}
    for k,v in pairs(attributes.resistances) do
        v.type = k
        resistances[#resistances+1] = v
    end

    -- Deep copy from vanilla spawner prototype
    local vanillaName = (attributes.type == "spitter-spawner") and "spitter-spawner" or "biter-spawner"
    local vanillaPrototype = data.raw["unit-spawner"][vanillaName]
    local entity = util.table.deepcopy(vanillaPrototype)

    -- Never let map generation place this spawner. The prototype is a deep
    -- copy of the vanilla one, so it inherits vanilla's autoplace; with every
    -- faction, tier and variant generated the mod would ship 354 autoplaced
    -- spawner types instead of vanilla's 2, and map generation piles them up
    -- near the starting area. The design is the other way round: vanilla
    -- spawners generate, and the mod upgrades them in place.
    entity.autoplace = nil

    -- Override fields that differ from vanilla
    entity.name = attributes.name .. "-rampant"
    entity.localised_name = biterUtils.getLocalisedName(attributes)
    entity.localised_description = biterUtils.getLocalisedDescription(attributes)
    entity.icon = "__base__/graphics/icons/biter-spawner.png"
    entity.icon_size = 64
    entity.icon_mipmaps = 4
    entity.flags = {"placeable-player", "placeable-enemy", "not-repairable"}
    entity.max_health = attributes.health
    entity.order = "b-b-g"
    entity.loot = attributes.loot
    entity.resistances = resistances
    entity.working_sound =
        {
            sound =
                {
                    {
                        filename = "__base__/sound/creatures/spawner.ogg",
                        volume = 0.2 + (0.05 * attributes.effectiveLevel)
                    }
                },
            apparent_volume = 0.4 + (0.1 * attributes.effectiveLevel)
        }
    entity.dying_sound =
        {
            {
                filename = "__base__/sound/creatures/spawner-death-1.ogg",
                volume = 0.4 + (0.05 * attributes.effectiveLevel)
            },
            {
                filename = "__base__/sound/creatures/spawner-death-2.ogg",
                volume = 0.4 + (0.05 * attributes.effectiveLevel)
            }
        }
    entity.damaged_trigger_effect = ((not settings.startup["rampant--removeBloodParticles"].value) and makeDamagedParticle(attributes)) or nil
    entity.healing_per_tick = attributes.healing or 0.02
    entity.collision_box = {{-3.0 * attributes.scale, -2.0 * attributes.scale}, {2.0 * attributes.scale, 2.0 * attributes.scale}}
    entity.selection_box = {{-3.5 * attributes.scale, -2.5 * attributes.scale}, {2.5 * attributes.scale, 2.5 * attributes.scale}}
    entity.absorptions_per_second = { pollution = { absolute = attributes.pollutionAbsorptionAbs or 20, proportional = attributes.pollutionAbsorptionPro or 0.01 } }
    entity.map_generator_bounding_box = {{-4.2 * attributes.scale, -3.2 * attributes.scale}, {3.2 * attributes.scale, 3.2 * attributes.scale}}
    entity.corpse = makeUnitSpawnerCorpse(attributes)
    entity.dying_explosion = attributes.explosion or "blood-explosion-huge"
    entity.min_darkness_to_spawn = attributes.minSpawnDarkness or 0
    entity.max_darkness_to_spawn = attributes.maxSpawnDarkness or 1
    entity.dying_trigger_effect = attributes.dyingEffect
    entity.max_count_of_owned_units = attributes.unitsOwned or 7
    entity.max_friends_around_to_spawn = attributes.unitsToSpawn or 5
    entity.enemy_map_color = ((not settings.startup["rampant--oldRedEnemyMapColor"].value) and attributes.tint2) or nil
    entity.result_units = attributes.unitSet
    entity.spawning_cooldown = attributes.spawningCooldown or ((attributes.spawningCooldownStart and attributes.spawningCooldownEnd) and {attributes.spawningCooldownStart, attributes.spawningCooldownEnd}) or {360, 150}
    entity.spawning_radius = attributes.spawningRadius or 10
    entity.spawning_spacing = attributes.spawningSpacing or 3
    -- build_base_evolution_requirement: removed in 2.1 (was 1.1 spawner base-building gate).
    entity.call_for_help_radius = attributes.helpRadius or 50

    -- Apply custom tints to inherited animations
    applySpawnerTints(entity.graphics_set.animations, attributes.tint, attributes.scale, attributes.tint2 or attributes.tint)

    -- Remove integration (uses broken sprite refs)
    entity.integration = nil

    if attributes.appendFlags then
        for flag in pairs(attributes.appendFlags) do
            entity.flags[#entity.flags+1] = flag
        end
    end
    return entity
end

function biterUtils.makeWorm(attributes)
    local resistances = {}
    for k,v in pairs(attributes.resistances) do
        v.type = k
        resistances[#resistances+1] = v
    end
    local o = {
        type = "turret",
        name = attributes.name .. "-rampant",
        localised_name = biterUtils.getLocalisedName(attributes),
        icon = "__base__/graphics/icons/medium-worm.png",
        icon_size = 64, icon_mipmaps = 4,
        flags = attributes.flags or {"placeable-player", "placeable-enemy", "not-repairable", "not-repairable", "breaths-air"},
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
        corpse = makeWormCorpse(attributes),
        graphics_set = {},
        dying_explosion = attributes.explosion or "blood-explosion-big",
        dying_trigger_effect = attributes.dyingEffect,
        inventory_size = attributes.inventorySize,
        dying_sound = attributes.dyingSounds,
        folded_speed = 0.01,
        folded_speed_secondary = 0.024,
        folded_animation = wormFoldedAnimation(attributes.scale, attributes.tint, attributes.tint2 or attributes.tint),
        preparing_speed = attributes.preparingSpeed or 0.024,
        preparing_animation = wormPreparingAnimation(attributes.scale, attributes.tint, "forward", attributes.tint2 or attributes.tint),
        preparing_sound = attributes.standupSounds,
        prepared_speed = 0.024,
        prepared_speed_secondary = 0.012,
        prepared_animation = wormPreparedAnimation(attributes.scale, attributes.tint, attributes.tint2 or attributes.tint),
        prepared_sound = attributes.breathSounds,
        prepared_alternative_speed = 0.014,
        prepared_alternative_speed_secondary = 0.010,
        prepared_alternative_chance = 0.2,
        prepared_alternative_animation = wormPreparedAlternativeAnimation(attributes.scale, attributes.tint, attributes.tint2 or attributes.tint),
        prepared_alternative_sound = attributes.altRoarSounds,

        damaged_trigger_effect = ((not settings.startup["rampant--removeBloodParticles"].value) and makeDamagedParticle(attributes)) or nil,

        starting_attack_speed = 0.034,
        starting_attack_animation = wormStartAttackAnimation(attributes.scale, attributes.tint, attributes.tint2 or attributes.tint),
        starting_attack_sound = attributes.roarSounds,
        ending_attack_speed = 0.016,
        ending_attack_animation = wormEndAttackAnimation(attributes.scale, attributes.tint, attributes.tint2 or attributes.tint),
        folding_speed = attributes.foldingSpeed or 0.015,
        folding_animation =  wormPreparingAnimation(attributes.scale, attributes.tint, "backward", attributes.tint2 or attributes.tint),
        folding_sound = attributes.foldSounds,
        prepare_range = attributes.prepareRange or 30,
        integration = worm_integration(attributes.scale),
        attack_parameters = attributes.attack,
        secondary_animation = true,
        enemy_map_color = ((not settings.startup["rampant--oldRedEnemyMapColor"].value) and attributes.tint2) or nil,
        random_animation_offset = true,
        attack_from_start_frame = true,

        allow_turning_when_starting_attack = true,
        -- build_base_evolution_requirement: removed in 2.1.
        call_for_help_radius = attributes.helpRadius or 60,
        spawn_decorations_on_expansion = true,
        spawn_decoration =
            {
                {
                    decorative = "worms-decal",
                    spawn_min = 1,
                    spawn_max = 3,
                    spawn_min_radius = 1,
                    spawn_max_radius = 5
                },
                {
                    decorative = "shroom-decal",
                    spawn_min = 1,
                    spawn_max = 2,
                    spawn_min_radius = 1,
                    spawn_max_radius = 2
                },
                {
                    decorative = "enemy-decal",
                    spawn_min = 1,
                    spawn_max = 4,
                    spawn_min_radius = 1,
                    spawn_max_radius = 4,
                },
                {
                    decorative = "enemy-decal-transparent",
                    spawn_min = 3,
                    spawn_max = 5,
                    spawn_min_radius = 1,
                    spawn_max_radius = 4,
                },
            }
    }

    if attributes.autoplace then
        o["autoplace"] = enemy_worm_autoplace(attributes.autoplace)
    end
    if attributes.appendFlags then
        for flag in pairs(attributes.appendFlags) do
            o.flags[#o.flags+1] = flag
        end
    end
    return o

end

function biterUtils.createSuicideAttack(attributes, blastWave, animation)
    local o = {
        type = "projectile",
        range = -- attributes.range or
            1,
        cooldown = attributes.cooldown or 35,
        range_mode = "bounding-box-to-bounding-box",
        ammo_category = "melee",
        ammo_type = {
            category = "biological"
        },
        sound = attributes.roarSounds,
        animation = animation
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
                                type = "set-tile",
                                tile_name = "nuclear-ground",
                                radius = attributes.radius * 0.25,
                                apply_projection = true,
                                tile_collision_mask = {layers={water_tile=true}},
                            },
                            {
                                type = "destroy-cliffs",
                                radius = attributes.radius * 0.18,
                                explosion = "explosion"
                            },
                            {
                                type = "create-entity",
                                entity_name = "nuke-explosion"
                            },
                            {
                                type = "camera-effect",
                                effect = "screen-burn",
                                duration = 60,
                                ease_in_duration = 5,
                                ease_out_duration = 60,
                                delay = 0,
                                strength = 6,
                                full_strength_max_distance = 200,
                                max_distance = 800
                            },
                            {
                                type = "play-sound",
                                sound = sounds.nuclear_explosion(0.9),
                                play_on_target_position = false,
                                -- min_distance = 200,
                                max_distance = 1000,
                                -- volume_modifier = 1,
                                audible_distance_modifier = 3
                            },
                            {
                                type = "play-sound",
                                sound = sounds.nuclear_explosion_aftershock(0.4),
                                play_on_target_position = false,
                                -- min_distance = 200,
                                max_distance = 1000,
                                -- volume_modifier = 1,
                                audible_distance_modifier = 3
                            },
                            {
                                type = "damage",
                                damage = {amount = attributes.damage, type = "explosion"}
                            },
                            {
                                type = "create-entity",
                                entity_name = "huge-scorchmark",
                                check_buildability = true,
                            },
                            {
                                type = "invoke-tile-trigger",
                                repeat_count = 1,
                            },
                            {
                                type = "destroy-decoratives",
                                include_soft_decoratives = true, -- soft decoratives are decoratives with grows_through_rail_path = true
                                include_decals = true,
                                invoke_decorative_trigger = true,
                                decoratives_with_trigger_only = false, -- if true, destroys only decoratives that have trigger_effect set
                                radius = attributes.radius * 0.35 -- large radius for demostrative purposes
                            },
                            {
                                type = "create-decorative",
                                decorative = "nuclear-ground-patch",
                                spawn_min_radius = attributes.radius * 0.1,
                                spawn_max_radius = attributes.radius * 0.12,
                                spawn_min = 30,
                                spawn_max = 40,
                                apply_projection = true,
                                spread_evenly = true
                            },
                            {
                                type = "nested-result",
                                action =
                                    {
                                        type = "area",
                                        target_entities = false,
                                        trigger_from_target = true,
                                        repeat_count = 100,
                                        radius = attributes.radius * 0.10,
                                        action_delivery =
                                            {
                                                type = "projectile",
                                                projectile = "atomic-bomb-ground-zero-projectile",
                                                starting_speed = 0.6 * 0.8,
                                                starting_speed_deviation = nuke_shockwave_starting_speed_deviation,
                                            }
                                    }
                            },
                            {
                                type = "nested-result",
                                action =
                                    {
                                        type = "area",
                                        target_entities = false,
                                        trigger_from_target = true,
                                        repeat_count = 100,
                                        radius = attributes.radius,
                                        action_delivery =
                                            {
                                                type = "projectile",
                                                projectile = "atomic-bomb-wave",
                                                starting_speed = 0.5 * 0.7,
                                                starting_speed_deviation = nuke_shockwave_starting_speed_deviation,
                                            }
                                    }
                            },
                            {
                                type = "nested-result",
                                action =
                                    {
                                        type = "area",
                                        show_in_tooltip = false,
                                        target_entities = false,
                                        trigger_from_target = true,
                                        repeat_count = 100,
                                        radius = attributes.radius * 0.75,
                                        action_delivery =
                                            {
                                                type = "projectile",
                                                projectile = "atomic-bomb-wave-spawns-cluster-nuke-explosion",
                                                starting_speed = 0.5 * 0.7,
                                                starting_speed_deviation = nuke_shockwave_starting_speed_deviation,
                                            }
                                    }
                            },
                            {
                                type = "nested-result",
                                action =
                                    {
                                        type = "area",
                                        show_in_tooltip = false,
                                        target_entities = false,
                                        trigger_from_target = true,
                                        repeat_count = 50,
                                        radius = attributes.radius * 0.07,
                                        action_delivery =
                                            {
                                                type = "projectile",
                                                projectile = "atomic-bomb-wave-spawns-fire-smoke-explosion",
                                                starting_speed = 0.5 * 0.65,
                                                starting_speed_deviation = nuke_shockwave_starting_speed_deviation,
                                            }
                                    }
                            },
                            {
                                type = "nested-result",
                                action =
                                    {
                                        type = "area",
                                        show_in_tooltip = false,
                                        target_entities = false,
                                        trigger_from_target = true,
                                        repeat_count = 100,
                                        radius = attributes.radius * 0.07,
                                        action_delivery =
                                            {
                                                type = "projectile",
                                                projectile = "atomic-bomb-wave-spawns-nuke-shockwave-explosion",
                                                starting_speed = 0.5 * 0.65,
                                                starting_speed_deviation = nuke_shockwave_starting_speed_deviation,
                                            }
                                    }
                            },
                            {
                                type = "nested-result",
                                action =
                                    {
                                        type = "area",
                                        show_in_tooltip = false,
                                        target_entities = false,
                                        trigger_from_target = true,
                                        repeat_count = 100,
                                        radius = attributes.radius * 0.85,
                                        action_delivery =
                                            {
                                                type = "projectile",
                                                projectile = "atomic-bomb-wave-spawns-nuclear-smoke",
                                                starting_speed = 0.5 * 0.65,
                                                starting_speed_deviation = nuke_shockwave_starting_speed_deviation,
                                            }
                                    }
                            },
                            {
                                type = "nested-result",
                                action =
                                    {
                                        type = "area",
                                        show_in_tooltip = false,
                                        target_entities = false,
                                        trigger_from_target = true,
                                        repeat_count = 10,
                                        radius = attributes.radius * 0.18,
                                        action_delivery =
                                            {
                                                type = "instant",
                                                target_effects =
                                                    {
                                                        {
                                                            type = "create-entity",
                                                            entity_name = "nuclear-smouldering-smoke-source",
                                                            tile_collision_mask = {layers={water_tile=true}}
                                                        }
                                                    }
                                            }
                                    }
                            }
                        }
                }
        }
    else
        o.ammo_type.action = {
            type = "direct",
            action_delivery = {
                type = "instant",
                source_effects = {
                    {
                        type = "damage",
                        affects_target = true,
                        show_in_tooltip = false,
                        damage = {amount = attributes.healthDamage or 5, type = "explosion"}
                    }
                },
                target_effects = {
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
        }

    end
    return o
end

function biterUtils.makeWormAlienLootTable(name)
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

function biterUtils.makeSpawnerAlienLootTable(name)
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

function biterUtils.makeUnitAlienLootTable(name)
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

function biterUtils.findRange(entity)
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

function biterUtils.findRunScale(entity)
    return findKey("scale", entity.run_animation.layers)
end

function biterUtils.findTint(entity)
    return findKey("tint", entity.run_animation.layers)
end

function biterUtils.createElectricAttack(attributes, electricBeam, animation)
    return
        {
            type = "beam",
            ammo_category = "biological",
            range_mode = "bounding-box-to-bounding-box",
            cooldown = attributes.cooldown or 20,
            warmup = attributes.warmup,
            min_attack_distance = (attributes.range and (attributes.range - 2)) or 15,
            range = (attributes.range and (attributes.range + 2)) or 15,
            sound = make_laser_sounds(),
            ammo_type =
                {
                    category = "biological",
                    action =
                        {
                            type = "line",
                            range = (attributes.range and (attributes.range + 2)) or 15,
                            width = attributes.width or 0.5,
                            force = (DISALLOW_FRIENDLY_FIRE and "not-same") or nil,
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

function biterUtils.createBeamAttack(attributes, electricBeam, animation)
    return
        {
            type = "beam",
            ammo_category = "biological",
            range_mode = "bounding-box-to-bounding-box",
            cooldown = attributes.cooldown or 20,
            warmup = attributes.warmup,
            damage_modifier = 2.5,
            min_attack_distance = (attributes.range and (attributes.range - 2)) or 15,
            range = (attributes.range and (attributes.range + 2)) or 15,
            sound = make_laser_sounds(),
            ammo_type =
                {
                    category = "biological",
                    action =
                        {
                            type = "direct",
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

function biterUtils.createProjectileAttack(attributes, projectile, animation)
    return {
        type = "projectile",
        ammo_category = "biological",
        range_mode = "bounding-box-to-bounding-box",
        cooldown = attributes.cooldown or 15,
        warmup = attributes.warmup,
        cooldown_deviation = 0.15,
        projectile_creation_distance = 0.6,
        force_condition = (settings.startup["rampant--disableCollidingProjectiles"].value and "not-same") or nil,
        range = attributes.range or 20,
        min_attack_distance = (attributes.range and (attributes.range - 2)) or 20,
        lead_target_for_projectile_speed = 0.95,
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
        sound = biterUtils.biterAttackSounds(attributes.effectiveLevel),
        animation = animation
    }
end

function biterUtils.createMeleeAttack(attackAttributes)
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
        cooldown_deviation = 0.15,
        range_mode = "bounding-box-to-bounding-box",
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
        sound = attackAttributes.roarSounds,
        animation = biterattackanimation(attackAttributes.scale, attackAttributes.tint, attackAttributes.tint2 or attackAttributes.tint, attackAttributes.altBiter)
    }
end

function biterUtils.biterAttackSounds(effectiveLevel)
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

function biterUtils.createRangedAttack(attributes, attack, animation)
    if (attributes.attackType == "stream") then
        return biterUtils.createStreamAttack(attributes, attack, animation)
    elseif (attributes.attackType == "projectile") then
        return biterUtils.createProjectileAttack(attributes, attack, animation)
    else
        error("Unknown range attack")
    end
end

function biterUtils.createStreamAttack(attributes, fireAttack, animation)
    local attack = {
        type = "stream",
        ammo_category = "biological",
        cooldown = attributes.cooldown,
        range = attributes.range,
        min_range = attributes.minRange,
        cooldown_deviation = 0.15,

        turn_range = attributes.turnRange,
        fire_penalty = attributes.firePenalty,

        warmup = attributes.warmup or 0,

        damage_modifier = attributes.damageModifier or 1.0,
        range_mode = "bounding-box-to-bounding-box",
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
                begin_sound = biterUtils.biterAttackSounds(attributes.effectiveLevel),
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

function biterUtils.makeResistance(name, decrease, percentage)
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

return biterUtils
