-- import

local streamUtils = require("StreamUtils")

-- constants

local DISALLOW_FRIENDLY_FIRE = settings.startup["rampant-disallowFriendlyFire"].value

-- imported functions

local makeStream = streamUtils.makeStream

-- dumb acid projectiles
local AttackBall = {}

function AttackBall.createAttackBall(attributes)

    local templateDamage = { amount = attributes.damage, type = attributes.damageType or "acid" }
    local templateArea = {
	type = "area",
	radius = attributes.radius,
	force = (DISALLOW_FRIENDLY_FIRE and "enemy") or nil,
	action_delivery =
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
			type= "create-entity",
			entity_name = attributes.crater or "acid-splash-purple"
		    }
	    }
	}
    }

    local template = {
	name = attributes.name,
	particleTint = attributes.pTint,
	spineAnimationTint = attributes.sTint,
	softSmokeTint = attributes.smTint,
	softSmokeName = attributes.softSmokeName,
	particleVertialAcceleration = attributes.particleVertialAcceleration,
	particleHoizontalSpeed = attributes.particleHoizontalSpeed,
	particleHoizontalSpeedDeviation = attributes.particleHoizontalSpeedDeviation,
	actions = templateActions
    }

    return makeStream(template)
end

function AttackBall.generateLegacy()
    AttackBall.createAttackBall({name="acid-ball", pTint={r=0, g=1, b=1, a=0.5}, sTint={r=0, g=1, b=1, a=0.5}, softSmokeName="the-soft-smoke-rampant", damage=4, radius=1.2})
    AttackBall.createAttackBall({name="acid-ball-1", pTint={r=0, g=1, b=1, a=0.5}, sTint={r=0, g=1, b=1, a=0.5}, softSmokeName="the-soft-smoke-rampant", damage=9, radius=1.3})
    AttackBall.createAttackBall({name="acid-ball-2", pTint={r=0, g=1, b=1, a=0.5}, sTint={r=0, g=1, b=1, a=0.5}, softSmokeName="the-soft-smoke-rampant", damage=14, radius=1.4})
    AttackBall.createAttackBall({name="acid-ball-3", pTint={r=0, g=1, b=1, a=0.5}, sTint={r=0, g=1, b=1, a=0.5}, softSmokeName="the-soft-smoke-rampant", damage=23, radius=1.5})
    AttackBall.createAttackBall({name="wide-acid-ball", pTint={r=0, g=1, b=1, a=0.5}, sTint={r=0, g=1, b=1, a=0.5}, softSmokeName="the-soft-smoke-rampant", damage=18, radius=3})
    AttackBall.createAttackBall({name="acid-ball-4", pTint={r=0, g=1, b=1, a=0.5}, sTint={r=0, g=1, b=1, a=0.5}, softSmokeName="the-soft-smoke-rampant", damage=25, radius=1.75})
    AttackBall.createAttackBall({name="acid-ball-5", pTint={r=0, g=1, b=1, a=0.5}, sTint={r=0, g=1, b=1, a=0.5}, softSmokeName="the-soft-smoke-rampant", damage=50, radius=2})
    AttackBall.createAttackBall({name="acid-ball-6", pTint={r=0, g=1, b=1, a=0.5}, sTint={r=0, g=1, b=1, a=0.5}, softSmokeName="the-soft-smoke-rampant", damage=70, radius=2.5})
end

return AttackBall
