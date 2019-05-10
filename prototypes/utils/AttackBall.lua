-- import

local fireUtils = require("FireUtils")
local stickerUtils = require("StickerUtils")
local streamUtils = require("StreamUtils")
local projectileUtils = require("ProjectileUtils")

-- constants

local DISALLOW_FRIENDLY_FIRE = settings.startup["rampant-disallowFriendlyFire"].value

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
	force = (DISALLOW_FRIENDLY_FIRE and "enemy") or attributes.force or nil,
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

    local templateActions = {
	templateArea,
	{
	    type = "direct",
	    action_delivery = {
		type = "instant",
		target_effects = (attributes.pointEffects and attributes.pointEffects(attributes)) or
		    {			
                        {
                            type="create-fire",
                            entity_name = makeAcidSplashFire(attributes, attributes.stickerName or makeSticker(attributes)),
                            check_buildability = true,
                            initial_ground_flame_count = 1
                        },
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
		    }
	    }
	}
    }

    local name
    local template
    if (attributes.type == "stream") then
	template = {
	    name = attributes.name,
	    tint = attributes.tint,
	    particleVertialAcceleration = attributes.particleVertialAcceleration,
	    particleHoizontalSpeed = attributes.particleHoizontalSpeed,
	    particleHoizontalSpeedDeviation = attributes.particleHoizontalSpeedDeviation,
	    actions = templateActions,
            scale = attributes.scale
	}
	name = makeStream(template)
    else
	name = makeProjectile(attributes.name,
			      attributes,
			      templateActions)
    end

    return name
end

function AttackBall.generateVanilla()
    AttackBall.createAttackBall({name="acid-ball", scale=0.5, directionOnly=true, type="projectile", tint={r=0, g=1, b=0.3, a=0.5}, damage=16, damagePerTick=0.1, stickerName="acid-sticker-small", radius=1.2})
    AttackBall.createAttackBall({name="acid-ball-1", scale=0.65, directionOnly=true, type="projectile", tint={r=0, g=1, b=0.3, a=0.5}, damage=32, damagePerTick=0.2, stickerName="acid-sticker-medium", radius=1.3})

    AttackBall.createAttackBall({name="acid-ball-2-direction", scale=0.85, directionOnly=true, type="projectile", tint={r=0, g=1, b=0.3, a=0.5}, damage=60, damagePerTick=0.6, stickerName="acid-sticker-big", radius=1.4})
    AttackBall.createAttackBall({name="acid-ball-3-direction", scale=1.0, directionOnly=true, type="projectile", tint={r=0, g=1, b=0.3, a=0.5}, damage=90, damagePerTick=1.2, stickerName="acid-sticker-behemoth", radius=1.5})

    AttackBall.createAttackBall({name="acid-ball-2", scale=0.85,  type="projectile", tint={r=0, g=1, b=0.3, a=0.5}, damage=36, damagePerTick=0.2, stickerName="acid-sticker-small", radius=1.4})
    AttackBall.createAttackBall({name="acid-ball-3", scale=1.0,  type="projectile", tint={r=0, g=1, b=0.3, a=0.5}, damage=45, damagePerTick=0.6, stickerName="acid-sticker-medium", radius=1.5})
    AttackBall.createAttackBall({name="acid-ball-4", scale=1.2,  type="projectile", tint={r=0, g=1, b=0.3, a=0.5}, damage=85, damagePerTick=1.2, stickerName="acid-sticker-big", radius=1.75})
    AttackBall.createAttackBall({name="acid-ball-5", scale=1.3,  type="projectile", tint={r=0, g=1, b=0.3, a=0.5}, damage=135, damagePerTick=1.5, stickerName="acid-sticker-behemoth", radius=2})
end

return AttackBall
