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


-- import

local fireUtils = require("FireUtils")
local stickerUtils = require("StickerUtils")
local streamUtils = require("StreamUtils")
local projectileUtils = require("ProjectileUtils")

-- constants

local DISALLOW_FRIENDLY_FIRE = settings.startup["rampant--disallowFriendlyFire"].value

-- imported functions

local makeStream = streamUtils.makeStream
local makeSticker = stickerUtils.makeSticker
local makeProjectile = projectileUtils.makeProjectile
local makeAcidSplashFire = fireUtils.makeAcidSplashFire

-- dumb acid projectiles
local AttackBall = {}

function AttackBall.createAttackBall(attributes)

    local templateAOEDamage = { amount = attributes.damage * 0.75, type = attributes.damageType or "acid" }
    local templateDirectDamage = { amount = attributes.damage * 0.25, type = attributes.damageType or "acid" }
    local templateArea = {
        type = "area",
        radius = attributes.radius,
        force = (DISALLOW_FRIENDLY_FIRE and "not-same") or attributes.force or nil,
        ignore_collision_condition = true,
        action_delivery = (attributes.areaActionDelivery and attributes.areaActionDelivery(attributes)) or
            {
                {
                    type = "instant",
                    target_effects = (attributes.areaEffects and attributes.areaEffects(attributes)) or
                        {
                            {
                                type = "damage",
                                damage = templateAOEDamage
                            }
                        }
                }
            }
    }

    local targetEffects
    if attributes.attackPointEffects then
        targetEffects = (attributes.attackPointEffects and attributes.attackPointEffects(attributes))
    else
        local rec = {
            {
                type = "damage",
                damage = templateDirectDamage
            },
            {
                type = "create-entity",
                entity_name = "water-splash",
                tile_collision_mask = { "ground-tile" }
            },
            {
                type = "play-sound",
                sound =
                    {
                        {
                            filename = "__base__/sound/creatures/projectile-acid-burn-1.ogg",
                            volume = 0.25 + (attributes.effectiveLevel * 0.05)
                        },
                        {
                            filename = "__base__/sound/creatures/projectile-acid-burn-2.ogg",
                            volume = 0.25 + (attributes.effectiveLevel * 0.05)
                        },
                        {
                            filename = "__base__/sound/creatures/projectile-acid-burn-long-1.ogg",
                            volume = 0.25 + (attributes.effectiveLevel * 0.05)
                        },
                        {
                            filename = "__base__/sound/creatures/projectile-acid-burn-long-2.ogg",
                            volume = 0.25 + (attributes.effectiveLevel * 0.05)
                        }
                    }
            }
        }
        if not attributes.noAcidPuddle then
            rec[#rec+1] = {
                type="create-fire",
                entity_name = makeAcidSplashFire(attributes, attributes.stickerName or makeSticker(attributes)),
                check_buildability = true,
                initial_ground_flame_count = 1,
                show_in_tooltip = true
            }
        end
        targetEffects = rec
    end

    local templateActions = {
        templateArea,
        {
            type = "direct",
            action_delivery = {
                type = "instant",
                target_effects = targetEffects
            }
        }
    }

    local name
    -- local template
    if (attributes.attackType == "stream") then
        -- template = {
        --     name = attributes.name,
        --     tint = attributes.tint,
        --     particleVertialAcceleration = attributes.particleVertialAcceleration,
        --     particleHoizontalSpeed = attributes.particleHoizontalSpeed,
        --     particleHoizontalSpeedDeviation = attributes.particleHoizontalSpeedDeviation,
        --     actions = templateActions,
        --     scale = attributes.scale
        -- }
        attributes.actions = templateActions
        name = makeStream(attributes)
    else
        name = makeProjectile(attributes, templateActions)
    end

    return name
end

function AttackBall.generateVanilla()
    AttackBall.createAttackBall({name="acid-ball", scale=0.5, directionOnly=true, attackType="projectile", tint2={r=0, g=1, b=0.3, a=0.5}, damage=4, damagePerTick=0.1, stickerName="acid-sticker-small", radius=1.2, effectiveLevel=1})
    AttackBall.createAttackBall({name="acid-ball-1", scale=0.65, directionOnly=true, attackType="projectile", tint2={r=0, g=1, b=0.3, a=0.5}, damage=7.5, damagePerTick=0.2, stickerName="acid-sticker-medium", radius=1.3, effectiveLevel=3})

    AttackBall.createAttackBall({name="acid-ball-2-direction", scale=0.85, directionOnly=true, attackType="projectile", tint2={r=0, g=1, b=0.3, a=0.5}, damage=11.25, damagePerTick=0.03, stickerName="acid-sticker-big", radius=1.4, effectiveLevel=5})
    AttackBall.createAttackBall({name="acid-ball-3-direction", scale=1.0, directionOnly=true, attackType="projectile", tint2={r=0, g=1, b=0.3, a=0.5}, damage=15, damagePerTick=0.55, stickerName="acid-sticker-behemoth", radius=1.5, effectiveLevel=7})

    AttackBall.createAttackBall({name="acid-ball-2", scale=0.85,  attackType="projectile", tint2={r=0, g=1, b=0.3, a=0.5}, damage=11.25, damagePerTick=0.2, stickerName="acid-sticker-small", radius=1.4, effectiveLevel=3})
    AttackBall.createAttackBall({name="acid-ball-3", scale=1.0,  attackType="projectile", tint2={r=0, g=1, b=0.3, a=0.5}, damage=15, damagePerTick=0.5, stickerName="acid-sticker-medium", radius=1.5, effectiveLevel=5})
    AttackBall.createAttackBall({name="acid-ball-4", scale=1.2,  attackType="projectile", tint2={r=0, g=1, b=0.3, a=0.5}, damage=22.5, damagePerTick=0.75, stickerName="acid-sticker-big", radius=1.75, effectiveLevel=7})
    AttackBall.createAttackBall({name="acid-ball-5", scale=1.3,  attackType="projectile", tint2={r=0, g=1, b=0.3, a=0.5}, damage=32.5, damagePerTick=1, stickerName="acid-sticker-behemoth", radius=2, effectiveLevel=8})
end

return AttackBall
