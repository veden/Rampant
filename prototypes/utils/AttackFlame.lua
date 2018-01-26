local attackFlame = {}

-- imported

local streamUtils = require("StreamUtils")
local colorUtils = require("ColorUtils")
local fireUtils = require("FireUtils")
local stickerUtils = require("StickerUtils")

-- imported functions

local makeColor = colorUtils.makeColor
local makeStream = streamUtils.makeStream
local makeFire = fireUtils.makeFire
local makeSticker = stickerUtils.makeSticker
local makeSpreadEffect = fireUtils.makeSpreadEffect

-- module code

function attackFlame.createAttackFlame(attributes)
    
    local spawnEntityName = makeSpreadEffect({
	    name = attributes.name,
	    smokeWithoutGlowName = attributes.smokeWithoutGlowName,
	    fireDamagePerTick = attributes.fireDamagePerTick,
	    fireDamagePerTickType = attributes.fireDamagePerTickType,
    })
    local fireName = makeFire({
	    name = attributes.name,
	    fireTint = attributes.fTint or {r=0, g=0.9, b=0, a=0.5},
	    smokeWithGlowName = attributes.smokeWithGlowName,
	    smokeAddingFuelName = attributes.smokeAddingFuelName,
	    spawnEntityName = spawnEntityName,
	    fireDamagePerTick = attributes.fireDamagePerTick,
	    fireDamagePerTickType = attributes.fireDamagePerTickType,
	    damageMaxMultipler = attributes.damageMaxMultipler,
	    multiplerIncrease = attributes.multiplerIncrease,
	    multiplerDecrease = attributes.multiplerDecrease
    })
    local stickerName = makeSticker({
	    name = attributes.name,
	    spawnEntityName = spawnEntityName,
	    stickerAnimation = attributes.stickerAnimation,
	    stickerDuration = attributes.stickerDuration,
	    stickerDamagePerTick = attributes.stickerDamagePerTick,
	    stickerDamagePerTick2 = attributes.stickerDamagePerTick2,
	    stickerDamagePerTickType = attributes.stickerDamagePerTickType,
	    stickerDamagePerTickType2 = attributes.stickerDamagePerTickType2,
	    stickerMovementModifier = attributes.stickerMovementModifier,
	    fireSpreadRadius = attributes.fireSpreadRadius
    })
    
    return makeStream({
	    name = attributes.name,
	    particleTint = attributes.pTint or {r=0, g=1, b=1, a=0.5},
	    spineAnimationTint = attributes.sTint or {r=0, g=1, b=1, a=0.5},
	    softSmokeTint = attributes.smTint or makeColor(0.3, 0.75, 0.3, 0.1),
	    softSmokeName = attributes.softSmokeName,
	    particleTimeout = attributes.particleTimeout,
	    actions = {
		{
		    type = "area",
		    radius = attributes.radius or 2.5,
		    action_delivery =
			{
			    type = "instant",
			    target_effects =
				{
				    {
					type = "create-sticker",
					sticker = stickerName
				    },
				    {
					type = "damage",
					damage = { amount = attributes.damage, type = attributes.damageType or "fire" }
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
			    entity_name = fireName
			}
		    }
		}
	    }
    })
end

return attackFlame
