local utils = require("BiterFunctions")

local createFireAttack = utils.createFireAttack
local makeSpitter = utils.makeSpitter
local makeResistance = utils.makeResistance

local smallFireBiterScale = 0.53
local smallFireBiterTint1 = {r=0.8, g=0.0, b=0.20, a=0.8}
local smallFireBiterTint2 = {r=0.7, g=0.0, b=0.30, a=0.4}

data:extend({
	makeSpitter({name = "small-fire-spitter",
		     health = 15,
		     movement = 0.19,
		     distancePerFrame = 0.04,
		     healing = 0.01,
		     scale = smallFireBiterScale,
		     tint1 = smallFireBiterTint1,
		     tint2 = smallFireBiterTint2,
		     explosion = "blood-explosion-small"},
	    createFireAttack({ scale = smallFireBiterScale,
			       tint1 = smallFireBiterTint1,
			       tint2 = smallFireBiterTint2,
			       range = 12,
			       minRange = 1,
			       cooldown = 55,
			       turnRange = 1.0/3.0,
			       firePenalty = 15}),
	    {
		makeResistance("fire", 0, 15)
	})
	
})
