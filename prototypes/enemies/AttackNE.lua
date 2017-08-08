-- import

local streamUtils = require("StreamUtils")
local colorUtils = require("ColorUtils")

-- imported functions

local makeStream = streamUtils.makeStream
local makeColor = colorUtils.makeColor

-- module code

makeStream({
	name = "ne-infected-unit-ball",
	particleTint = {r=0, g=0.97, b=0.34, a=0.5},
	spineAnimationTint = {r=0, g=0.1, b=1, a=1},
	softSmokeTint = makeColor(0.7, 0.4, 0.2, 0.1),
	actions = {
	    {
		type = "direct",
		action_delivery =
		    {
			type = "instant",
			target_effects =
			    {
				{
				    type = "create-entity",
				    entity_name = "unit-cluster",
				    trigger_created_entity = "true"
				},
				{
				    type = "create-sticker",
				    sticker = "slowdown-sticker",
				},
				{
				    type = "create-entity",
				    entity_name = "Infected-Poison-Cloud"
				},
				{
				    type = "damage",
				    damage = {amount = 10, type = "explosion"}
				},
				{
				    type = "damage",
				    damage = {amount = 25, type = "poison"}
				}
			    }
		    }
	    }
	}
})

--

makeStream({
	name = "ne-mutated-unit-ball",
	particleTint = {r=0.5, g=0.7, b=0.34, a=0.5},
	spineAnimationTint = {r=0.5, g=0.97, b=0.34, a=0.5},
	softSmokeTint = makeColor(0.3, 0.75, 0.3, 0.1),
	actions = {
	    {
		type = "direct",
		action_delivery =
		    {
			type = "instant",
			target_effects =
			    {
				{
				    type = "create-entity",
				    entity_name = "unit-cluster",
				    trigger_created_entity = "true"
				},
				{
				    type = "create-sticker",
				    sticker = "slowdown-sticker",
				},
				{
				    type = "create-entity",
				    entity_name = "acid-splash-purple"
				}
			    }
		    }
	    },
	    {
		type = "area",
		perimeter = 2,
		action_delivery =
		    {
			type = "instant",
			target_effects =
			    {
				{
				    type = "damage",
				    damage = { amount = 5, type = "explosion" }
				},
				{
				    type = "damage",
				    damage = { amount = 15, type = "acid" }
				}
			    }
		    }
	    }
	}
})

--

makeStream({
	name = "ne-infected-ball",
	particleTint = {r=0.5, g=0.7, b=0.34, a=0.5},
	spineAnimationTint = {r=0.5, g=0.97, b=0.34, a=0.5},
	softSmokeTint = makeColor(0.3, 0.75, 0.3, 0.1),
	actions = {
	    {
		type = "direct",
		action_delivery =
		    {
			type = "instant",
			target_effects =
			    {
				{
				    type = "create-entity",
				    entity_name = "Infected-Poison-Cloud"
				}
			    }
		    }
	    },
	    {
		type = "area",
		perimeter = 1.5,
		action_delivery =
		    {
			type = "instant",
			target_effects =
			    {
				{
				    type = "damage",
				    damage = { amount = 3, type = "explosion" }
				},
				{
				    type = "damage",
				    damage = { amount = 7, type = "poison" }
				}
			    }
		    }
	    }
	}
})

--

makeStream({
	name = "ne-mutated-ball",
	particleTint = {r=0.5, g=0.7, b=0.34, a=0.5},
	spineAnimationTint = {r=0.5, g=0.97, b=0.34, a=0.5},
	softSmokeTint = makeColor(0.3, 0.75, 0.3, 0.1),
	actions = {
	    {
		type = "direct",
		action_delivery =
		    {
			type = "instant",
			target_effects =
			    {
				{
				    type = "create-entity",
				    entity_name = "acid-splash-purple"
				},
				{
				    type = "create-entity",
				    entity_name = "acid-splash-mutated"
				}
			    }
		    }
	    },
	    {
		type = "area",
		perimeter = 2,
		action_delivery =
		    {
			type = "instant",
			target_effects =
			    {
				{
				    type = "damage",
				    damage = { amount = 7, type = "explosion" }
				},
				{
				    type = "damage",
				    damage = { amount = 14, type = "acid" }
				}
			    }
		    }
	    }
	}
})
