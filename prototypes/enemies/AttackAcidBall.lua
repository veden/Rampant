-- import

local streamUtils = require("StreamUtils")
local colorUtils = require("ColorUtils")

-- imported functions

local makeStream = streamUtils.makeStream
local makeColor = colorUtils.makeColor

-- dumb acid projectiles

makeStream({
	name = "acid-ball",
	particleTint = {r=0, g=1, b=1, a=0.5},
	spineAnimationTint = {r=0, g=1, b=1, a=0.5},
	softSmokeTint = makeColor(0.3, 0.75, 0.3, 0.1),
	actions = {
	    {
		type = "area",
		perimeter = 1.5,
		action_delivery =
		    {
			{
			    type = "instant",
			    target_effects =
				{
				    {
					type = "damage",
					damage = { amount = 15, type = "acid" }
				    }
				}
			}
		    }
	    },
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
})

--

makeStream({
	name = "acid-ball-1",
	particleTint = {r=0, g=1, b=1, a=0.5},
	spineAnimationTint = {r=0, g=1, b=1, a=0.5},
	softSmokeTint = makeColor(0.3, 0.75, 0.3, 0.1),
	actions = {
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
				    damage = { amount = 22, type = "acid" }
				},
				{
				    type= "create-entity",
				    entity_name = "acid-splash-purple"
				}
			    }
		    }
	    },
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
})

--

makeStream({
	name = "acid-ball-2",
	particleTint = {r=0, g=1, b=1, a=0.5},
	spineAnimationTint = {r=0, g=1, b=1, a=0.5},
	softSmokeTint = makeColor(0.3, 0.75, 0.3, 0.1),
	actions = {
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
				    damage = { amount = 32, type = "acid" }
				},
				{
				    type= "create-entity",
				    entity_name = "acid-splash-purple"
				}
			    }
		    }
	    },
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
})

--

makeStream({
	name = "acid-ball-3",
	particleTint = {r=0, g=1, b=1, a=0.5},
	spineAnimationTint = {r=0, g=1, b=1, a=0.5},
	softSmokeTint = makeColor(0.3, 0.75, 0.3, 0.1),
	actions = {
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
				    damage = { amount = 45, type = "acid" }
				},
				{
				    type= "create-entity",
				    entity_name = "acid-splash-purple"
				}
			    }
		    }
	    },
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
})

--

makeStream({
	name = "wide-acid-ball",
	particleTint = {r=0, g=1, b=1, a=0.5},
	spineAnimationTint = {r=0, g=1, b=1, a=0.5},
	softSmokeTint = makeColor(0.3, 0.75, 0.3, 0.1),
	actions = {
	    {
		type = "area",
		perimeter = 3,
		action_delivery =
		    {
			type = "instant",
			target_effects =
			    {
				{
				    type = "damage",
				    damage = { amount = 35, type = "acid" }
				},
				{
				    type= "create-entity",
				    entity_name = "acid-splash-purple"
				}
			    }
		    }
	    },
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
})

--

makeStream({
	name = "acid-ball-4",
	particleTint = {r=0, g=1, b=1, a=0.5},
	spineAnimationTint = {r=0, g=1, b=1, a=0.5},
	softSmokeTint = makeColor(0.3, 0.75, 0.3, 0.1),
	actions = {
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
				    damage = { amount = 95, type = "acid" }
				},
				{
				    type= "create-entity",
				    entity_name = "acid-splash-purple"
				}
			    }
		    }
	    },
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
})

--

makeStream({
	name = "acid-ball-5",
	particleTint = {r=0, g=1, b=1, a=0.5},
	spineAnimationTint = {r=0, g=1, b=1, a=0.5},
	softSmokeTint = makeColor(0.3, 0.75, 0.3, 0.1),
	actions = {
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
				    damage = { amount = 145, type = "acid" }
				},
				{
				    type= "create-entity",
				    entity_name = "acid-splash-purple"
				}
			    }
		    }
	    },
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
})
