
-- imports

local biterUtils = require("BiterUtils")

-- import functions

local makeBiter = biterUtils.makeBiter
local createSuicideAttack = biterUtils.createSuicideAttack
local makeResistance = biterUtils.makeResistance

-- local

local smallSuicideBiterScale = 0.75
local smallSuicideBiterTint1 = {r=0.0, g=0.0, b=0.70, a=0.8}
local smallSuicideBiterTint2 = {r=0.0, g=0.0, b=0.72, a=0.4}

-- module code

data:extend({
	makeBiter({name = "small-tendril-biter-rampant",
		   health = 30,
		   movement = 0.21,
		   distancePerFrame = 0.1,
		   healing = 0.01,
		   scale = smallSuicideBiterScale,
		   tint1 = smallSuicideBiterTint1,
		   tint2 = smallSuicideBiterTint2,
		   explosion = "blood-explosion-small"},
	    createSuicideAttack({ area = 3.5,
				  damage = 20,
				  explosion = "explosion",
				  scorchmark = "small-scorchmark",
				  explosionCount = 2,
				  explosionDistance = 2,
				  scale = smallSuicideBiterScale,
				  tint1 = smallSuicideBiterTint1,
				  tint2 = smallSuicideBiterTint2}),
	    {makeResistance("explosion", 0, -50),
	     makeResistance("laser", 1, 0),
	     makeResistance("fire", 0, -60)}),
})
