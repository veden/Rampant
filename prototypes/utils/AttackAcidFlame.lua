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

function attackFlame.makeAttackFlame(attributes)

    
    local name = attributes.name .. "-flame-rampant"
    local spawnEntityName = makeSpreadEffect({
	    name = name,
	    smokeWithoutGlowTint = makeColor(0.25,0.75,0.1, 0.25)
    })
    local fireName = makeFire({
	    name = name,
	    fireTint = {r=0, g=0.9, b=0, a=0.5},
	    smokeWithGlowTint = {r=0.2, g=0.8, b=0.2, a=0.25},
	    spawnEntityName = spawnEntityName
    })
    local stickerName = makeSticker({
	    name = name,
	    spawnEntityName = spawnEntityName
    })
    makeStream({
	    name = name,
	    particleTint = {r=0, g=1, b=1, a=0.5},
	    spineAnimationTint = {r=0, g=1, b=1, a=0.5},
	    softSmokeTint = makeColor(0.3, 0.75, 0.3, 0.1),
	    actions = {
		{
		    type = "area",
		    radius = 2.5,
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
					damage = { amount = 1, type = "fire" }
				    },
				    {
					type = "damage",
					damage = { amount = 1, type = "acid" }
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
