local biterUtils = require("BiterUtils")

local createFireAttack = biterUtils.createFireAttack
local makeSpitter = biterUtils.makeSpitter
local makeResistance = biterUtils.makeResistance

local smallFireBiterScale = 0.53
local smallFireBiterTint1 = {r=0.8, g=0.0, b=0.20, a=0.8}
local smallFireBiterTint2 = {r=0.7, g=0.0, b=0.30, a=0.4}

local mediumFireBiterScale = 0.73
local mediumFireBiterTint1 = {r=0.8, g=0.0, b=0.20, a=0.8}
local mediumFireBiterTint2 = {r=0.7, g=0.0, b=0.30, a=0.4}

local bigFireBiterScale = 1.05
local bigFireBiterTint1 = {r=0.8, g=0.0, b=0.20, a=0.8}
local bigFireBiterTint2 = {r=0.7, g=0.0, b=0.30, a=0.4}

local behemothFireBiterScale = 1.23
local behemothFireBiterTint1 = {r=0.8, g=0.0, b=0.20, a=0.8}
local behemothFireBiterTint2 = {r=0.7, g=0.0, b=0.30, a=0.4}

data:extend({
	makeSpitter({name = "small-fire-spitter-rampant",
		     health = 15,
		     movement = 0.0,
		     distancePerFrame = 0.00,
		     healing = 0.01,
		     scale = smallFireBiterScale,
		     tint1 = smallFireBiterTint1,
		     tint2 = smallFireBiterTint2,
		     corpse = "small-spitter-corpse",
		     explosion = "blood-explosion-small"},
	    createFireAttack({ scale = smallFireBiterScale,
			       tint1 = smallFireBiterTint1,
			       tint2 = smallFireBiterTint2,
			       range = 13,
			       minRange = 5,
			       cooldown = 55,
			       turnRange = 1,
			       firePenalty = 15}, "acid-flame-fire-stream-rampant"),
	    {
		makeResistance("fire", 0, 15)
	}),
	makeSpitter({name = "medium-fire-spitter-rampant",
		     health = 65,
		     movement = 0.165,
		     distancePerFrame = 0.055,
		     healing = 0.01,
		     scale = mediumFireBiterScale,
		     tint1 = mediumFireBiterTint1,
		     tint2 = mediumFireBiterTint2,
		     corpse = "medium-spitter-corpse",
		     explosion = "blood-explosion-small"},
	    createFireAttack({ scale = mediumFireBiterScale,
			       tint1 = mediumFireBiterTint1,
			       tint2 = mediumFireBiterTint2,
			       range = 14,
			       minRange = 5,
			       cooldown = 40,
			       turnRange = 1,
			       firePenalty = 15}, "acid-flame-1-fire-stream-rampant"),
	    {
		makeResistance("fire", 2, 35)
	}),
	makeSpitter({name = "big-fire-spitter-rampant",
		     health = 225,
		     movement = 0.15,
		     distancePerFrame = 0.07,
		     healing = 0.01,
		     scale = bigFireBiterScale,
		     tint1 = bigFireBiterTint1,
		     tint2 = bigFireBiterTint2,
		     corpse = "big-spitter-corpse",
		     explosion = "blood-explosion-big"},
	    createFireAttack({ scale = bigFireBiterScale,
			       tint1 = bigFireBiterTint1,
			       tint2 = bigFireBiterTint2,
			       range = 16,
			       minRange = 5,
			       cooldown = 25,
			       turnRange = 1,
			       firePenalty = 15}, "acid-flame-1-fire-stream-rampant"),
	    {
		makeResistance("fire", 4, 55)
	}),
	makeSpitter({name = "behemoth-fire-spitter-rampant",
		     health = 2250,
		     movement = 0.15,
		     distancePerFrame = 0.084,
		     healing = 0.01,
		     scale = behemothFireBiterScale,
		     tint1 = behemothFireBiterTint1,
		     tint2 = behemothFireBiterTint2,
		     corpse = "behemoth-spitter-corpse",
		     explosion = "blood-explosion-big"},
	    createFireAttack({ scale = behemothFireBiterScale,
			       tint1 = behemothFireBiterTint1,
			       tint2 = behemothFireBiterTint2,
			       range = 17,
			       minRange = 5,
			       cooldown = 15,
			       turnRange = 1,
			       firePenalty = 15}, "acid-flame-2-fire-stream-rampant"),
	    {
		makeResistance("fire", 6, 75)
	})



	
})
