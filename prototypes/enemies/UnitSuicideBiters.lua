local biterUtils = require("BiterUtils")

local makeBiter = biterUtils.makeBiter
local createSuicideAttack = biterUtils.createSuicideAttack
local makeResistance = biterUtils.makeResistance

local smallSuicideBiterScale = 0.55
local smallSuicideBiterTint1 = {r=0.6, g=0.0, b=0.70, a=0.8}
local smallSuicideBiterTint2 = {r=0.7, g=0.0, b=0.72, a=0.4}

local mediumSuicideBiterScale = 0.75
local mediumSuicideBiterTint1 = {r=0.6, g=0.0, b=0.70, a=0.8}
local mediumSuicideBiterTint2 = {r=0.7, g=0.0, b=0.72, a=0.4}

local bigSuicideBiterScale = 1.05
local bigSuicideBiterTint1 = {r=0.6, g=0.0, b=0.70, a=0.8}
local bigSuicideBiterTint2 = {r=0.7, g=0.0, b=0.72, a=0.4}

local behemothSuicideBiterScale = 1.25
local behemothSuicideBiterTint1 = {r=0.6, g=0.0, b=0.70, a=0.8}
local behemothSuicideBiterTint2 = {r=0.7, g=0.0, b=0.72, a=0.4}


data:extend({
	makeBiter({name = "small-suicide-biter-rampant",
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
	makeBiter({name = "medium-suicide-biter-rampant",
		   health = 125,
		   movement = 0.19,
		   distancePerFrame = 0.15,
		   healing = 0.01,
		   scale = mediumSuicideBiterScale,
		   tint1 = mediumSuicideBiterTint1,
		   tint2 = mediumSuicideBiterTint2,
		   explosion = "blood-explosion-small"},
	    createSuicideAttack({ area = 4.5,
				  damage = 28,
				  explosion = "big-explosion",
				  scorchmark = "small-scorchmark",
				  explosionCount = 4,
				  explosionDistance = 2,
				  scale = mediumSuicideBiterScale,
				  tint1 = mediumSuicideBiterTint1,
				  tint2 = mediumSuicideBiterTint2}),
	    {makeResistance("physical", 1, 0),
	     makeResistance("explosion", 0, -40),
	     makeResistance("laser", 1, 20),
	     makeResistance("fire", 0, -50)}),
	makeBiter({name = "big-suicide-biter-rampant",
		   health = 425,
		   movement = 0.18,
		   distancePerFrame = 0.19,
		   scale = bigSuicideBiterScale,
		   tint1 = bigSuicideBiterTint1,
		   tint2 = bigSuicideBiterTint2,
		   explosion = "blood-explosion-big"},
	    createSuicideAttack({ area = 5.5,
				  damage = 63,
				  explosion = "big-explosion",
				  scorchmark = "small-scorchmark",
				  explosionCount = 8,
				  explosionDistance = 3,
				  scale = bigSuicideBiterScale,
				  tint1 = bigSuicideBiterTint1,
				  tint2 = bigSuicideBiterTint2}),
	    {makeResistance("physical", 2, 0),
	     makeResistance("explosion", 0, -30),
	     makeResistance("laser", 3, 25),
	     makeResistance("fire", 0, -30)}),
	makeBiter({name = "behemoth-suicide-biter-rampant",
		   health = 1200,
		   movement = 0.18,
		   distancePerFrame = 0.19,
		   scale = behemothSuicideBiterScale,
		   tint1 = behemothSuicideBiterTint1,
		   tint2 = behemothSuicideBiterTint2,
		   explosion = "blood-explosion-big"},
	    createSuicideAttack({ area = 7.5,
				  damage = 150,
				  explosion = "massive-explosion",
				  scorchmark = "small-scorchmark",
				  explosionCount = 12,
				  explosionDistance = 3,
				  scale = behemothSuicideBiterScale,
				  tint1 = behemothSuicideBiterTint1,
				  tint2 = behemothSuicideBiterTint2}),
	    {makeResistance("physical", 4, 20),
	     makeResistance("explosion", 0, -20),
	     makeResistance("laser", 5, 30),
	     makeResistance("fire", 0, -10)})
})
