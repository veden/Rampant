-- import

local stickerUtils = require("StickerUtils")
local streamUtils = require("StreamUtils")
local projectileUtils = require("ProjectileUtils")

-- constants

local FORCE_OLD_PROJECTILES = settings.startup["rampant-forceOldProjectiles"].value
local DISALLOW_FRIENDLY_FIRE = settings.startup["rampant-disallowFriendlyFire"].value

-- imported functions

local makeStream = streamUtils.makeStream
local makeProjectile = projectileUtils.makeProjectile

-- dumb acid projectiles
local AttackBall = {}

stickerUtils.makeSticker({
	name = "the-sticker"

})

function AttackBall.createAttackBall(attributes)

    if (attributes.type == "stream") or FORCE_OLD_PROJECTILES then
        
    elseif attributes.damage and (attributes.type == "projectile") then
	attributes.damage = attributes.damage * 2.7
    end


    local templateDamage = { amount = attributes.damage, type = attributes.damageType or "acid" }
    local templateArea = {
	type = "area",
	radius = attributes.radius,
	force = (DISALLOW_FRIENDLY_FIRE and "enemy") or attributes.force or nil,
	action_delivery = (attributes.areaActionDelivery and attributes.areaActionDelivery(attributes)) or
	    {
		{
		    type = "instant",
		    target_effects = (attributes.areaEffects and attributes.areaEffects(attributes)) or
			{
			    {
				type = "damage",
				damage = templateDamage
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
                            type= "create-entity",
                            entity_name = attributes.crater or "acid-splash-purple"
			},
			{
                            type = "damage",
                            damage = templateDamage
			}
		    }
	    }
	}
    }

    local name
    local template
    if (attributes.type == "stream") or FORCE_OLD_PROJECTILES then
	template = {
	    name = attributes.name,
	    particleTint = attributes.pTint,
	    spineAnimationTint = attributes.sTint,
	    softSmokeName = attributes.softSmokeName,
	    particleVertialAcceleration = attributes.particleVertialAcceleration,
	    particleHoizontalSpeed = attributes.particleHoizontalSpeed,
	    particleHoizontalSpeedDeviation = attributes.particleHoizontalSpeedDeviation,
	    actions = templateActions
	}
	name = makeStream(template)
    elseif (attributes.type == "projectile") then
	name = makeProjectile(attributes.name,
			      attributes,
			      templateActions)
    end

    return name
end

function AttackBall.generateLegacy()
    AttackBall.createAttackBall({name="acid-ball", type="projectile", pTint={r=0, g=1, b=1, a=0.5}, sTint={r=0, g=1, b=1, a=0.5}, softSmokeName="the-soft-smoke-rampant", damage=4, radius=1.2})
    AttackBall.createAttackBall({name="acid-ball-1", type="projectile", pTint={r=0, g=1, b=1, a=0.5}, sTint={r=0, g=1, b=1, a=0.5}, softSmokeName="the-soft-smoke-rampant", damage=9, radius=1.3})
    AttackBall.createAttackBall({name="acid-ball-2", type="projectile", pTint={r=0, g=1, b=1, a=0.5}, sTint={r=0, g=1, b=1, a=0.5}, softSmokeName="the-soft-smoke-rampant", damage=14, radius=1.4})
    AttackBall.createAttackBall({name="acid-ball-3", type="projectile", pTint={r=0, g=1, b=1, a=0.5}, sTint={r=0, g=1, b=1, a=0.5}, softSmokeName="the-soft-smoke-rampant", damage=23, radius=1.5})
    AttackBall.createAttackBall({name="wide-acid-ball", type="projectile", pTint={r=0, g=1, b=1, a=0.5}, sTint={r=0, g=1, b=1, a=0.5}, softSmokeName="the-soft-smoke-rampant", damage=18, radius=3})
    AttackBall.createAttackBall({name="acid-ball-4", type="projectile", pTint={r=0, g=1, b=1, a=0.5}, sTint={r=0, g=1, b=1, a=0.5}, softSmokeName="the-soft-smoke-rampant", damage=25, radius=1.75})
    AttackBall.createAttackBall({name="acid-ball-5", type="projectile", pTint={r=0, g=1, b=1, a=0.5}, sTint={r=0, g=1, b=1, a=0.5}, softSmokeName="the-soft-smoke-rampant", damage=50, radius=2})
    AttackBall.createAttackBall({name="acid-ball-6", type="projectile", pTint={r=0, g=1, b=1, a=0.5}, sTint={r=0, g=1, b=1, a=0.5}, softSmokeName="the-soft-smoke-rampant", damage=70, radius=2.5})
    if not FORCE_OLD_PROJECTILES then
	AttackBall.createAttackBall({name="acid-ball-direction", directionOnly=true, type="projectile", pTint={r=0, g=1, b=1, a=0.5}, sTint={r=0, g=1, b=1, a=0.5}, softSmokeName="the-soft-smoke-rampant", damage=4, radius=1.2})
	AttackBall.createAttackBall({name="acid-ball-1-direction", directionOnly=true, type="projectile", pTint={r=0, g=1, b=1, a=0.5}, sTint={r=0, g=1, b=1, a=0.5}, softSmokeName="the-soft-smoke-rampant", damage=9, radius=1.3})
	AttackBall.createAttackBall({name="acid-ball-2-direction", directionOnly=true, type="projectile", pTint={r=0, g=1, b=1, a=0.5}, sTint={r=0, g=1, b=1, a=0.5}, softSmokeName="the-soft-smoke-rampant", damage=14, radius=1.4})
	AttackBall.createAttackBall({name="acid-ball-3-direction", directionOnly=true, type="projectile", pTint={r=0, g=1, b=1, a=0.5}, sTint={r=0, g=1, b=1, a=0.5}, softSmokeName="the-soft-smoke-rampant", damage=23, radius=1.5})
	AttackBall.createAttackBall({name="wide-acid-ball-direction", directionOnly=true, type="projectile", pTint={r=0, g=1, b=1, a=0.5}, sTint={r=0, g=1, b=1, a=0.5}, softSmokeName="the-soft-smoke-rampant", damage=18, radius=3})
	AttackBall.createAttackBall({name="acid-ball-4-direction", directionOnly=true, type="projectile", pTint={r=0, g=1, b=1, a=0.5}, sTint={r=0, g=1, b=1, a=0.5}, softSmokeName="the-soft-smoke-rampant", damage=25, radius=1.75})
	AttackBall.createAttackBall({name="acid-ball-5-direction", directionOnly=true, type="projectile", pTint={r=0, g=1, b=1, a=0.5}, sTint={r=0, g=1, b=1, a=0.5}, softSmokeName="the-soft-smoke-rampant", damage=50, radius=2})
	AttackBall.createAttackBall({name="acid-ball-6-direction", directionOnly=true, type="projectile", pTint={r=0, g=1, b=1, a=0.5}, sTint={r=0, g=1, b=1, a=0.5}, softSmokeName="the-soft-smoke-rampant", damage=70, radius=2.5})
    end
end

return AttackBall
