-- import

local streamUtils = require("StreamUtils")
local colorUtils = require("ColorUtils")
local smokeUtils = require("SmokeUtils")


-- imported functions

local makeSmokeSoft = smokeUtils.makeSmokeSoft

local makeStream = streamUtils.makeStream
local makeColor = colorUtils.makeColor

-- dumb acid projectiles
local acidBall = {}

function acidBall.createAcidBall(attributes)

    local templateDamage = { amount = attributes.damage, type = attributes.damageType or "acid" }
    local templateArea = {
	type = "area",
	radius = attributes.radius,
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

    makeStream(template)
end

function acidBall.generateLegacy()
    makeSmokeSoft({name="acid-ball", softSmokeTint=makeColor(0.3, 0.75, 0.3, 0.1)})
    
    acidBall.createAcidBall({name="acid-ball", pTint={r=0, g=1, b=1, a=0.5}, sTint={r=0, g=1, b=1, a=0.5}, softSmokeName="acid-ball-soft-smoke-rampant", damage=4, radius=1.2})
    acidBall.createAcidBall({name="acid-ball-1", pTint={r=0, g=1, b=1, a=0.5}, sTint={r=0, g=1, b=1, a=0.5}, softSmokeName="acid-ball-soft-smoke-rampant", damage=9, radius=1.3})
    acidBall.createAcidBall({name="acid-ball-2", pTint={r=0, g=1, b=1, a=0.5}, sTint={r=0, g=1, b=1, a=0.5}, softSmokeName="acid-ball-soft-smoke-rampant", damage=14, radius=1.4})
    acidBall.createAcidBall({name="acid-ball-3", pTint={r=0, g=1, b=1, a=0.5}, sTint={r=0, g=1, b=1, a=0.5}, softSmokeName="acid-ball-soft-smoke-rampant", damage=23, radius=1.5})
    acidBall.createAcidBall({name="wide-acid-ball", pTint={r=0, g=1, b=1, a=0.5}, sTint={r=0, g=1, b=1, a=0.5}, softSmokeName="acid-ball-soft-smoke-rampant", damage=18, radius=3})
    acidBall.createAcidBall({name="acid-ball-4", pTint={r=0, g=1, b=1, a=0.5}, sTint={r=0, g=1, b=1, a=0.5}, softSmokeName="acid-ball-soft-smoke-rampant", damage=25, radius=1.75})
    acidBall.createAcidBall({name="acid-ball-5", pTint={r=0, g=1, b=1, a=0.5}, sTint={r=0, g=1, b=1, a=0.5}, softSmokeName="acid-ball-soft-smoke-rampant", damage=50, radius=2})
    acidBall.createAcidBall({name="acid-ball-6", pTint={r=0, g=1, b=1, a=0.5}, sTint={r=0, g=1, b=1, a=0.5}, softSmokeName="acid-ball-soft-smoke-rampant", damage=70, radius=2.5})
end

return acidBall
