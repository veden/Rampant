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


local attackFlame = {}

-- imported

local streamUtils = require("StreamUtils")
local fireUtils = require("FireUtils")
local stickerUtils = require("StickerUtils")

-- constants

local DISALLOW_FRIENDLY_FIRE = settings.startup["rampant--disallowFriendlyFire"].value

-- imported functions

local makeStream = streamUtils.makeStream
local makeFire = fireUtils.makeFire
local makeSticker = stickerUtils.makeSticker
local makeSpreadEffect = fireUtils.makeSpreadEffect

-- module code

function attackFlame.createAttackFlame(attributes)

    local spawnEntityName = makeSpreadEffect({
            name = attributes.name,
            tint2 = attributes.tint2,
            fireDamagePerTick = attributes.fireDamagePerTick,
            fireDamagePerTickType = attributes.fireDamagePerTickType,
    })
    local stickerName = makeSticker({
            name = attributes.name,
            spawnEntityName = spawnEntityName,
            stickerDuration = attributes.stickerDuration,
            stickerDamagePerTick = attributes.stickerDamagePerTick,
            stickerDamagePerTickType = attributes.stickerDamagePerTickType,
            stickerMovementModifier = attributes.stickerMovementModifier,
            tint2 = attributes.tint2,
            fireSpreadRadius = attributes.fireSpreadRadius
    })
    local fireName = makeFire({
            name = attributes.name,
            tint2 = attributes.tint2 or {r=0, g=0.9, b=0, a=0.5},
            spawnEntityName = spawnEntityName,
            fireDamagePerTick = attributes.fireDamagePerTick,
            fireDamagePerTickType = attributes.fireDamagePerTickType,
            damageMaxMultipler = attributes.damageMaxMultipler,
            multiplerIncrease = attributes.multiplerIncrease,
            multiplerDecrease = attributes.multiplerDecrease,
            stickerName = stickerName
    })

    return makeStream({
            name = attributes.name,
            tint2 = attributes.tint2 or {r=0, g=1, b=1, a=0.5},
            particleTimeout = attributes.particleTimeout,
            scale = attributes.scale,
            actions = {
                {
                    type = "area",
                    radius = attributes.radius or 2.5,
                    force = (DISALLOW_FRIENDLY_FIRE and "not-same") or nil,
                    action_delivery =
                        {
                            type = "instant",
                            target_effects =
                                {
                                    {
                                        type = "create-sticker",
                                        sticker = stickerName,
                                        check_buildability = true
                                    },
                                    {
                                        type = "create-entity",
                                        entity_name = "water-splash",
                                        tile_collision_mask = { "ground-tile" }
                                    },
                                    {
                                        type = "damage",
                                        damage = { amount = attributes.damage, type = attributes.damageType or "fire" }
                                    }
                                }
                        }
                },
                {
                    type = "cluster",
                    cluster_count = 2,
                    distance = 2 + (0.1 * attributes.effectiveLevel),
                    distance_deviation = 1.5,
                    action_delivery = {
                        type = "instant",
                        target_effects = {
                            {
                                type="create-fire",
                                entity_name = fireName,
                                check_buildability = true,
                                initial_ground_flame_count = 2,
                                show_in_tooltip = true
                            }
                        }
                    }
                },
                {
                    type = "direct",
                    action_delivery = {
                        type = "instant",
                        target_effects = {
                            type= "create-fire",
                            entity_name = fireName,
                            check_buildability = true,
                            show_in_tooltip = true
                        }
                    }
                }
            }
    })
end

return attackFlame
