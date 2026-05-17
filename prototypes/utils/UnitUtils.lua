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


local unitUtils = {}

function unitUtils.spitter_water_reflection(scale)
  return
  {
    pictures =
    {
      filename = "__base__/graphics/entity/spitter/spitter-reflection.png",
      priority = "extra-high",
      width = 20,
      height = 32,
      shift = util.by_pixel(5, 15),
      scale = 5 * scale,
      variation_count = 1,
    },
    rotate = true,
    orientation_to_variation = false
  }
end

function unitUtils.biter_water_reflection(scale)
  return
  {
    pictures =
    {
      filename = "__base__/graphics/entity/biter/biter-reflection.png",
      priority = "extra-high",
      width = 20,
      height = 28,
      shift = util.by_pixel(5, 15),
      scale = 5 * scale,
      variation_count = 1,
    },
    rotate = true,
    orientation_to_variation = false
  }
end


function unitUtils.spitter_alternative_attacking_animation_sequence()
    return {
        warmup_frame_sequence = { 1, 2, 3, 4, 5, 6 },
        warmup2_frame_sequence = { 7, 7, 7, 7, 7, 7 },
        attacking_frame_sequence = { 7, 8, 9, 10, 11,  12, 13, 14, 13, 14,  13, 12, 11, 10, 9,  8 },
        cooldown_frame_sequence = { 7 },
        prepared_frame_sequence = { 7 },
        back_to_walk_frame_sequence = { 6, 5, 4, 3, 2, 1 },

        warmup_animation_speed = 1 / 6 * 0.4,
        attacking_animation_speed = 1 / 16 * 0.4,
        cooldown_animation_speed = 1 / 1 * 0.4 * 0.125;
        prepared_animation_speed = 1 / 1 * 0.5 * 0.4,
        back_to_walk_animation_speed = 1 / 6 * 0.4,
    }
end

local function vanillaDieBiter(scale, tint1, tint2)
    -- Factorio 2.0: use util.sprite_load() instead of manual filename arrays
    return {
        layers = {
            util.sprite_load("__base__/graphics/entity/biter/biter-die", {
                slice = 8,
                frame_count = 17,
                direction_count = 16,
                scale = scale * 0.5,
                multiply_shift = scale,
                flags = { "corpse-decay" },
                allow_forced_downscale = true,
                surface = "nauvis",
                usage = "enemy",
            }),
            util.sprite_load("__base__/graphics/entity/biter/biter-die-mask1", {
                slice = 8,
                frame_count = 17,
                direction_count = 16,
                scale = scale * 0.5,
                tint = tint1,
                multiply_shift = scale,
                flags = { "corpse-decay" },
                allow_forced_downscale = true,
                surface = "nauvis",
                usage = "enemy",
            }),
            util.sprite_load("__base__/graphics/entity/biter/biter-die-mask2", {
                slice = 8,
                frame_count = 17,
                direction_count = 16,
                scale = scale * 0.5,
                tint = tint2,
                tint_as_overlay = true,
                multiply_shift = scale,
                flags = { "corpse-decay" },
                allow_forced_downscale = true,
                surface = "nauvis",
                usage = "enemy",
            }),
            util.sprite_load("__base__/graphics/entity/biter/biter-die-shadow", {
                slice = 6,
                frame_count = 17,
                direction_count = 16,
                scale = scale * 0.5,
                draw_as_shadow = true,
                multiply_shift = scale,
                allow_forced_downscale = true,
                surface = "nauvis",
                usage = "enemy",
            }),
        },
    }
end

local function armoredDieBiter(scale, tint1, tint2)
    -- Factorio 2.0: use util.sprite_load() for ArmouredBiters compatibility
    -- Note: ArmouredBiters mod must provide sprite sheets compatible with Factorio 2.0
    return {
        layers = {
            util.sprite_load("__ArmouredBiters__/graphics/armoured-biter/LowRes/armoured-biter-die", {
                slice = 4,
                frame_count = 16,
                direction_count = 16,
                scale = scale * 0.5,
                tint = tint1,
                multiply_shift = scale,
                flags = { "corpse-decay" },
                allow_forced_downscale = true,
            }),
            util.sprite_load("__ArmouredBiters__/graphics/armoured-biter/LowRes/armoured-biter-die-mask1", {
                slice = 4,
                frame_count = 16,
                direction_count = 16,
                scale = scale * 0.5,
                tint = tint2,
                multiply_shift = scale,
                flags = { "corpse-decay" },
                allow_forced_downscale = true,
            }),
            util.sprite_load("__ArmouredBiters__/graphics/armoured-biter/LowRes/armoured-biter-die-mask2", {
                slice = 4,
                frame_count = 16,
                direction_count = 16,
                scale = scale * 0.5,
                tint = tint2,
                tint_as_overlay = true,
                multiply_shift = scale,
                flags = { "corpse-decay" },
                allow_forced_downscale = true,
            }),
            util.sprite_load("__ArmouredBiters__/graphics/armoured-biter/LowRes/armoured-biter-die-shadow", {
                slice = 4,
                frame_count = 16,
                direction_count = 16,
                scale = scale * 0.5,
                draw_as_shadow = true,
                multiply_shift = scale,
                allow_forced_downscale = true,
            }),
        },
    }
end

local function vanillaAttackBiter(scale, tint1, tint2)
    return {
        layers = {
            util.sprite_load("__base__/graphics/entity/biter/biter-attack", {
                slice = 11,
                frame_count = 11,
                direction_count = 16,
                scale = scale * 0.5,
                animation_speed = 0.4,
                multiply_shift = scale,
                allow_forced_downscale = true,
                surface = "nauvis",
                usage = "enemy",
            }),
            util.sprite_load("__base__/graphics/entity/biter/biter-attack-mask1", {
                slice = 11,
                frame_count = 11,
                direction_count = 16,
                scale = scale * 0.5,
                tint = tint1,
                animation_speed = 0.4,
                multiply_shift = scale,
                allow_forced_downscale = true,
                surface = "nauvis",
                usage = "enemy",
            }),
            util.sprite_load("__base__/graphics/entity/biter/biter-attack-mask2", {
                slice = 11,
                frame_count = 11,
                direction_count = 16,
                scale = scale * 0.5,
                tint = tint2,
                tint_as_overlay = true,
                animation_speed = 0.4,
                multiply_shift = scale,
                allow_forced_downscale = true,
                surface = "nauvis",
                usage = "enemy",
            }),
            util.sprite_load("__base__/graphics/entity/biter/biter-attack-shadow", {
                slice = 8,
                frame_count = 11,
                direction_count = 16,
                scale = scale * 0.5,
                animation_speed = 0.4,
                draw_as_shadow = true,
                multiply_shift = scale,
                allow_forced_downscale = true,
                surface = "nauvis",
                usage = "enemy",
            }),
        },
    }
end

local function armoredAttackBiter(scale, tint1, tint2)
    return {
        layers = {
            util.sprite_load("__ArmouredBiters__/graphics/armoured-biter/LowRes/armoured-biter-attack", {
                slice = 11,
                frame_count = 11,
                direction_count = 16,
                scale = scale * 0.5,
                tint = tint1,
                animation_speed = 0.4,
                multiply_shift = scale,
                allow_forced_downscale = true,
            }),
            util.sprite_load("__ArmouredBiters__/graphics/armoured-biter/LowRes/armoured-biter-attack-mask1", {
                slice = 11,
                frame_count = 11,
                direction_count = 16,
                scale = scale * 0.5,
                tint = tint2,
                animation_speed = 0.4,
                multiply_shift = scale,
                allow_forced_downscale = true,
            }),
            util.sprite_load("__ArmouredBiters__/graphics/armoured-biter/LowRes/armoured-biter-attack-mask2", {
                slice = 11,
                frame_count = 11,
                direction_count = 16,
                scale = scale * 0.5,
                tint = tint2,
                tint_as_overlay = true,
                animation_speed = 0.4,
                multiply_shift = scale,
                allow_forced_downscale = true,
            }),
            util.sprite_load("__ArmouredBiters__/graphics/armoured-biter/LowRes/armoured-biter-attack-shadow", {
                slice = 8,
                frame_count = 11,
                direction_count = 16,
                scale = scale * 0.5,
                animation_speed = 0.4,
                draw_as_shadow = true,
                multiply_shift = scale,
                allow_forced_downscale = true,
            }),
        },
    }
end

local function armoredRunBiter(scale, tint1, tint2)
    return {
        layers = {
            util.sprite_load("__ArmouredBiters__/graphics/armoured-biter/LowRes/armoured-biter-run", {
                slice = 8,
                frame_count = 16,
                direction_count = 16,
                scale = scale * 0.5,
                tint = tint1,
                multiply_shift = scale,
                allow_forced_downscale = true,
            }),
            util.sprite_load("__ArmouredBiters__/graphics/armoured-biter/LowRes/armoured-biter-run-mask1", {
                slice = 8,
                frame_count = 16,
                direction_count = 16,
                scale = scale * 0.5,
                tint = tint2,
                multiply_shift = scale,
                allow_forced_downscale = true,
            }),
            util.sprite_load("__ArmouredBiters__/graphics/armoured-biter/LowRes/armoured-biter-run-mask2", {
                slice = 8,
                frame_count = 16,
                direction_count = 16,
                scale = scale * 0.5,
                tint = tint2,
                tint_as_overlay = true,
                multiply_shift = scale,
                allow_forced_downscale = true,
            }),
            util.sprite_load("__ArmouredBiters__/graphics/armoured-biter/LowRes/armoured-biter-run-shadow", {
                slice = 8,
                frame_count = 16,
                direction_count = 16,
                scale = scale * 0.5,
                draw_as_shadow = true,
                multiply_shift = scale,
                allow_forced_downscale = true,
            }),
        },
    }
end

local function vanillaRunBiter(scale, tint1, tint2)
    return {
        layers = {
            util.sprite_load("__base__/graphics/entity/biter/biter-run", {
                slice = 8,
                frame_count = 16,
                direction_count = 16,
                scale = scale * 0.5,
                multiply_shift = scale,
                allow_forced_downscale = true,
                surface = "nauvis",
                usage = "enemy",
            }),
            util.sprite_load("__base__/graphics/entity/biter/biter-run-mask1", {
                slice = 8,
                frame_count = 16,
                direction_count = 16,
                scale = scale * 0.5,
                tint = tint1,
                multiply_shift = scale,
                allow_forced_downscale = true,
                surface = "nauvis",
                usage = "enemy",
            }),
            util.sprite_load("__base__/graphics/entity/biter/biter-run-mask2", {
                slice = 8,
                frame_count = 16,
                direction_count = 16,
                scale = scale * 0.5,
                tint = tint2,
                tint_as_overlay = true,
                multiply_shift = scale,
                allow_forced_downscale = true,
                surface = "nauvis",
                usage = "enemy",
            }),
            util.sprite_load("__base__/graphics/entity/biter/biter-run-shadow", {
                slice = 8,
                frame_count = 16,
                direction_count = 16,
                scale = scale * 0.5,
                draw_as_shadow = true,
                multiply_shift = scale,
                allow_forced_downscale = true,
                surface = "nauvis",
                usage = "enemy",
            }),
        },
    }
end

function unitUtils.biterdieanimation(scale, tint1, tint2, altBiter)
    if (altBiter == "armored") and mods["ArmouredBiters"] then
        return armoredDieBiter(scale, tint1, tint2)
    else
        return vanillaDieBiter(scale, tint1, tint2)
    end
end

function unitUtils.biterattackanimation(scale, tint1, tint2, altBiter)
    if (altBiter == "armored") and mods["ArmouredBiters"] then
        return armoredAttackBiter(scale, tint1, tint2)
    else
        return vanillaAttackBiter(scale, tint1, tint2)
    end
end


function unitUtils.biterrunanimation(scale, tint1, tint2, altBiter)
    if (altBiter == "armored") and mods["ArmouredBiters"] then
        return armoredRunBiter(scale, tint1, tint2)
    else
        return vanillaRunBiter(scale, tint1, tint2)
    end
end


return unitUtils
