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


local particleUtils = {}

-- module code

local particleSet = {}
-- local particleCount = 0

function particleUtils.makeDamagedParticle(attributes)
    local effectiveLevel = attributes.effectiveLevel
    local name = "particle-"
    if effectiveLevel and effectiveLevel ~= 0 then
        name = name .. effectiveLevel .. "-"
    end
    name = name .. (attributes.faction or attributes.name) .. "-damaged-particle-rampant"

    local o = {
        type = "create-entity",
        entity_name = name,
        offset_deviation = {{-0.5, -0.5}, {0.5, 0.5}},
        offsets = {{0,0}},
        damage_type_filters = {"fire", "acid"}
    }

    if not particleSet[name] then
        particleSet[name] = true
        -- particleCount = particleCount + 1
        -- print(particleCount)
    else
        return o
    end

    data:extend({
            {
                type = "explosion",
                name = name,
                flags = {"not-on-map"},
                subgroup = "hit-effects",
                icon = "__core__/graphics/icons/mip/trash.png",
                height = 0.3,
                icon_size = 32,
                animations =
                    {
                        util.empty_sprite()
                    },
                created_effect =
                    {
                        type = "direct",
                        action_delivery =
                            {
                                type = "instant",
                                target_effects =
                                    {
                                        {
                                            type = "create-entity",
                                            entity_name = attributes.hitSprayName,
                                            repeat_count = 1,
                                        }
                                    }
                            }
                    }
            }
    })

    return o
end


local function makeBloodParticle(attributes)
    local name = "particle-" .. ((attributes.effectiveLevel and (attributes.effectiveLevel .. "-")) or "")
        .. (attributes.faction or attributes.name) .. "-blood-particle-rampant"
    local tint = attributes.tint2
    local scale = attributes.effectiveLevel and 0.5 or 0.75

    if not particleSet[name] then
        particleSet[name] = true
        -- particleCount = particleCount + 1
        -- print(particleCount)
    else
        return name
    end

    -- Factorio 2.0: use sprite sheet format
    local pictures = {
        sheet = {
            filename = "__base__/graphics/particle/blood-particle/blood-particle.png",
            line_length = 12,
            width = 32,
            height = 24,
            frame_count = 12,
            variation_count = 7,
            tint = tint,
            scale = 0.5 * scale,
            shift = util.by_pixel(0, 0.5)
        }
    }

    local shadows = {
        sheet = {
            filename = "__base__/graphics/particle/blood-particle/blood-particle.png",
            line_length = 12,
            width = 32,
            height = 24,
            frame_count = 12,
            variation_count = 7,
            tint = {r=0, g=0, b=0, a=0.3},
            scale = 0.5 * scale,
            shift = util.by_pixel(1, 0)
        }
    }

    data:extend({
            {
                type = "optimized-particle",
                name = name,
                flags = {"not-on-map"},
                render_layer = "higher-object-under",
                render_layer_when_on_ground = "corpse",
                movement_modifier_when_on_ground = 0.2,
                life_time = 240,
                pictures = pictures,
                shadows = shadows,
                draw_shadow_when_on_ground = true,
                ended_in_water_trigger_effect = nil
            }
    })

    return name
end


function particleUtils.makeBloodParticleDamageEffect(attributes)
    local name = makeBloodParticle(attributes)

    return {
        type = "direct",
        action_delivery =
            {
                type = "instant",
                target_effects =
                    {
                        {
                            type = "create-particle",
                            repeat_count = 6,
                            particle_name = name,
                            offsets = {{0,0}},
                            offset_deviation = {{-0.5, -0.5}, {0.5, 0.5}},
                            initial_height = 0.2,
                            initial_vertical_speed = 0.05,
                            initial_vertical_speed_deviation = 0.05,
                            speed_from_center = 0.05
                        }
                    }
            }
    }
end

function particleUtils.makeBloodParticleOnDeathEffect(attributes)
    local name = makeBloodParticle(attributes)

    return {
        type = "create-particle",
        repeat_count = 13,
        repeat_count_deviation = 1,
        particle_name = name,
        offsets = {{0, 0}},
        offset_deviation = {{-1, -1}, {1, 1}},
        initial_height = -0,
        initial_height_deviation = 0,
        initial_vertical_speed = 0.009,
        initial_vertical_speed_deviation = 0.005,
        speed_from_center = 0.05,
        speed_from_center_deviation = 0.041,
        frame_speed = 1,
        frame_speed_deviation = 0,
        tail_length = 11,
        tail_length_deviation = 0,
        tail_width = 1,
        rotate_offsets = false
    }
end

function particleUtils.makeBloodFountains(attributes)

    local particleScaler = settings.startup["rampant--scaleParticleCount"].value

    local bloodParticle = makeBloodParticle(attributes)

    data:extend({
            {
                type = "optimized-particle",
                name = attributes.name .. "-spitter-spitting-cascade-rampant",
                flags = {"not-on-map"},
                render_layer = "higher-object-under",
                render_layer_when_on_ground = "corpse",
                movement_modifier_when_on_ground = 0.2,
                life_time = 180,
                pictures = {
                    sheet = {
                        filename = "__base__/graphics/particle/blood-particle/blood-particle.png",
                        line_length = 12,
                        width = 32,
                        height = 24,
                        frame_count = 12,
                        variation_count = 7,
                        tint = attributes.tint2,
                        scale = 0.5 * 1,
                        shift = util.by_pixel(0, 0.5)
                    }
                },
                shadows = {
                    sheet = {
                        filename = "__base__/graphics/particle/blood-particle/blood-particle.png",
                        line_length = 12,
                        width = 32,
                        height = 24,
                        frame_count = 12,
                        variation_count = 7,
                        tint = {r=0, g=0, b=0, a=0.3},
                        scale = 0.5 * 1,
                        shift = util.by_pixel(1, 0)
                    }
                },
                draw_shadow_when_on_ground = true,
                ended_in_water_trigger_effect = nil
            },
            {
                type = "optimized-particle",
                name = attributes.name .. "-spitter-spitting-fountain-rampant",
                flags = {"not-on-map"},
                render_layer = "higher-object-under",
                render_layer_when_on_ground = "corpse",
                movement_modifier_when_on_ground = 0.2,
                life_time = 120,
                pictures = {
                    sheet = {
                        filename = "__base__/graphics/particle/blood-particle/blood-particle.png",
                        line_length = 12,
                        width = 32,
                        height = 24,
                        frame_count = 12,
                        variation_count = 7,
                        tint = attributes.tint2,
                        scale = 0.5 * 1,
                        shift = util.by_pixel(0, 0.5)
                    }
                },
                shadows = {
                    sheet = {
                        filename = "__base__/graphics/particle/blood-particle/blood-particle.png",
                        line_length = 12,
                        width = 32,
                        height = 24,
                        frame_count = 12,
                        variation_count = 7,
                        tint = {r=0, g=0, b=0, a=0.3},
                        scale = 0.5 * 1,
                        shift = util.by_pixel(1, 0)
                    }
                },
                draw_shadow_when_on_ground = true,
                ended_in_water_trigger_effect = nil
            },
            {
                type = "optimized-particle",
                name = attributes.name .. "-biter-dying-cascade-rampant",
                flags = {"not-on-map"},
                render_layer = "higher-object-under",
                render_layer_when_on_ground = "corpse",
                movement_modifier_when_on_ground = 0.2,
                life_time = 180,
                pictures = {
                    sheet = {
                        filename = "__base__/graphics/particle/blood-particle/blood-particle.png",
                        line_length = 12,
                        width = 32,
                        height = 24,
                        frame_count = 12,
                        variation_count = 7,
                        tint = attributes.tint2,
                        scale = 0.5 * 1,
                        shift = util.by_pixel(0, 0.5)
                    }
                },
                shadows = {
                    sheet = {
                        filename = "__base__/graphics/particle/blood-particle/blood-particle.png",
                        line_length = 12,
                        width = 32,
                        height = 24,
                        frame_count = 12,
                        variation_count = 7,
                        tint = {r=0, g=0, b=0, a=0.3},
                        scale = 0.5 * 1,
                        shift = util.by_pixel(1, 0)
                    }
                },
                draw_shadow_when_on_ground = true,
                ended_in_water_trigger_effect = nil
            },
            {
                type = "optimized-particle",
                name = attributes.name .. "-biter-dying-fountain-rampant",
                flags = {"not-on-map"},
                render_layer = "higher-object-under",
                render_layer_when_on_ground = "corpse",
                movement_modifier_when_on_ground = 0.2,
                life_time = 120,
                pictures = {
                    sheet = {
                        filename = "__base__/graphics/particle/blood-particle/blood-particle.png",
                        line_length = 12,
                        width = 32,
                        height = 24,
                        frame_count = 12,
                        variation_count = 7,
                        tint = attributes.tint2,
                        scale = 0.5 * 1,
                        shift = util.by_pixel(0, 0.5)
                    }
                },
                shadows = {
                    sheet = {
                        filename = "__base__/graphics/particle/blood-particle/blood-particle.png",
                        line_length = 12,
                        width = 32,
                        height = 24,
                        frame_count = 12,
                        variation_count = 7,
                        tint = {r=0, g=0, b=0, a=0.3},
                        scale = 0.5 * 1,
                        shift = util.by_pixel(1, 0)
                    }
                },
                draw_shadow_when_on_ground = true,
                ended_in_water_trigger_effect = nil
            },
            {
                type = "optimized-particle",
                name = attributes.name .. "-biter-dying-spawner-cascade-rampant",
                flags = {"not-on-map"},
                render_layer = "higher-object-under",
                render_layer_when_on_ground = "corpse",
                movement_modifier_when_on_ground = 0.2,
                life_time = 180,
                pictures = {
                    sheet = {
                        filename = "__base__/graphics/particle/blood-particle/blood-particle.png",
                        line_length = 12,
                        width = 32,
                        height = 24,
                        frame_count = 12,
                        variation_count = 7,
                        tint = attributes.tint2,
                        scale = 0.5 * 1,
                        shift = util.by_pixel(0, 0.5)
                    }
                },
                shadows = {
                    sheet = {
                        filename = "__base__/graphics/particle/blood-particle/blood-particle.png",
                        line_length = 12,
                        width = 32,
                        height = 24,
                        frame_count = 12,
                        variation_count = 7,
                        tint = {r=0, g=0, b=0, a=0.3},
                        scale = 0.5 * 1,
                        shift = util.by_pixel(1, 0)
                    }
                },
                draw_shadow_when_on_ground = true,
                ended_in_water_trigger_effect = nil
            },
            {
                type = "optimized-particle",
                name = attributes.name .. "-biter-dying-spawner-fountain-rampant",
                flags = {"not-on-map"},
                render_layer = "higher-object-under",
                render_layer_when_on_ground = "corpse",
                movement_modifier_when_on_ground = 0.2,
                life_time = 120,
                pictures = {
                    sheet = {
                        filename = "__base__/graphics/particle/blood-particle/blood-particle.png",
                        line_length = 12,
                        width = 32,
                        height = 24,
                        frame_count = 12,
                        variation_count = 7,
                        tint = attributes.tint2,
                        scale = 0.5 * 1,
                        shift = util.by_pixel(0, 0.5)
                    }
                },
                shadows = {
                    sheet = {
                        filename = "__base__/graphics/particle/blood-particle/blood-particle.png",
                        line_length = 12,
                        width = 32,
                        height = 24,
                        frame_count = 12,
                        variation_count = 7,
                        tint = {r=0, g=0, b=0, a=0.3},
                        scale = 0.5 * 1,
                        shift = util.by_pixel(1, 0)
                    }
                },
                draw_shadow_when_on_ground = true,
                ended_in_water_trigger_effect = nil
            }
        })
end

return particleUtils