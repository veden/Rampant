-- import

local streamUtils = require("StreamUtils")

-- imported functions

local makeStream = streamUtils.makeStream

-- module code

local softSmoke = "the-soft-smoke-rampant"

makeStream({
	name = "ne-infected-unit-ball",
	particleTint = {r=0, g=0.97, b=0.34, a=0.5},
	spineAnimationTint = {r=0, g=0.1, b=1, a=1},
	softSmokeName = softSmoke,
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
				    damage = {amount = 24, type = "poison"}
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
	softSmokeName = softSmoke,
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
		radius = 2,
		action_delivery =
		    {
			type = "instant",
			target_effects =
			    {
				{
				    type = "damage",
				    damage = { amount = 8, type = "explosion" }
				},
				{
				    type = "damage",
				    damage = { amount = 18, type = "acid" }
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
	softSmokeName = softSmoke,
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
		radius = 1.5,
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
				    damage = { amount = 12, type = "poison" }
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
	softSmokeName = softSmoke,
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
		radius = 2,
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
				    damage = { amount = 12, type = "acid" }
				}
			    }
		    }
	    }
	}
})
