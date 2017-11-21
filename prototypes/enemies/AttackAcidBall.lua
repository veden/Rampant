-- import

local streamUtils = require("StreamUtils")
local colorUtils = require("ColorUtils")

-- imported functions

local makeStream = streamUtils.makeStream
local makeColor = colorUtils.makeColor

-- dumb acid projectiles

local templateDamage = { amount = 4, type = "acid" }
local templateArea = {
    type = "area",
    perimeter = 1.2,
    action_delivery =
	{
	    {
		type = "instant",
		target_effects =
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
	    target_effects = {
		type= "create-entity",
		entity_name = "acid-splash-purple"
	    }
	}
    }
}


local template = {
    name = "acid-ball",
    particleTint = {r=0, g=1, b=1, a=0.5},
    spineAnimationTint = {r=0, g=1, b=1, a=0.5},
    softSmokeTint = makeColor(0.3, 0.75, 0.3, 0.1),
    actions = templateActions
}

makeStream(template)

--

template.name = "acid-ball-1"
templateDamage.amount = 9
templateArea.perimeter = 1.3
makeStream(template)

--

template.name = "acid-ball-2"
templateDamage.amount = 14
templateArea.perimeter = 1.4
makeStream(template)

--

template.name = "acid-ball-3"
templateDamage.amount = 23
templateArea.perimeter = 1.5
makeStream(template)

--

template.name = "wide-acid-ball"
templateDamage.amount = 18
templateArea.perimeter = 3
makeStream(template)

--

template.name = "acid-ball-4"
templateDamage.amount = 25
templateArea.perimeter = 1.75
makeStream(template)

--

template.name = "acid-ball-5"
templateDamage.amount = 50
templateArea.perimeter = 2
makeStream(template)

--

template.name = "acid-ball-6"
templateDamage.amount = 70
templateArea.perimeter = 2.5
makeStream(template)
