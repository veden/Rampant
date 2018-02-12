-- imports

local acidBall = require("utils/AttackBall")
local droneUtils = require("utils/DroneUtils")
local biterUtils = require("utils/BiterUtils")
local swarmUtils = require("SwarmUtils")
package.path = "../libs/?.lua;" .. package.path
local constants = require("Constants")

-- constants

local WASP_UNIT_TIERS = constants.WASP_UNIT_TIERS
local WASP_UNIT_VARIATIONS = constants.WASP_UNIT_VARIATIONS

local WASP_NEST_TIERS = constants.WASP_NEST_TIERS
local WASP_NEST_VARIATIONS = constants.WASP_NEST_VARIATIONS

local WASP_WORM_TIERS = constants.WASP_WORM_TIERS
local WASP_WORM_VARIATIONS = constants.WASP_WORM_VARIATIONS

-- imported functions

local buildUnitSpawner = swarmUtils.buildUnitSpawner
local buildWorm = swarmUtils.buildWorm
local buildUnits = swarmUtils.buildUnits
local createAttackBall = acidBall.createAttackBall
local createProjectileAttack = biterUtils.createProjectileAttack

local biterAttackSounds = biterUtils.biterAttackSounds

local softSmoke = "the-soft-smoke-rampant"

local createCapsuleProjectile = droneUtils.createCapsuleProjectile

local makeUnitAlienLootTable = biterUtils.makeUnitAlienLootTable
local makeSpawnerAlienLootTable = biterUtils.makeSpawnerAlienLootTable
local makeWormAlienLootTable = biterUtils.makeWormAlienLootTable

local biterLoot = makeUnitAlienLootTable("purple")
local spawnerLoot = makeSpawnerAlienLootTable("purple")
local wormLoot = makeWormAlienLootTable("purple")

-- wasp
buildUnits(
    {
	name = "wasp-drone",

	attributes = {
	    followsPlayer = true
	},
	attack = {
	    type = "stream",
	    softSmokeName = softSmoke
	},
	death = function (attributes)
	    return {
		type = "direct",
		action_delivery =
		    {
			type = "instant",
			target_effects =
			    {
				type = "create-entity",
				entity_name = "acid-splash-purple"
			    }
		    }
	    }
	end,
	scales = {
	    [1] = 0.5,
	    [2] = 0.5,
	    [3] = 0.6,
	    [4] = 0.6,
	    [5] = 0.7,
	    [6] = 0.7,
	    [7] = 0.8,
	    [8] = 0.8,
	    [9] = 0.9,
	    [10] = 0.9
	},
	resistances = {},

	type = "drone",
	attackName = "wasp-drone",
	tint = {r=1, g=1, b=0, a=1},
	pTint = {r=0, g=1, b=1, a=0.5},
	sTint = {r=0, g=1, b=1, a=0.5},
	dTint = {r=0, g=0, b=1, a=1}
    },
    function (attack)
	return {
	    type = "projectile",
	    ammo_category = "bullet",
	    cooldown = attack.cooldown or 20,
	    projectile_center = {0, 1},
	    projectile_creation_distance = 0.6,
	    range = attack.range or 15,
	    sound = biterAttackSounds(),
	    ammo_type =
		{
		    category = "bullet",
		    action =
			{
			    type = "direct",
			    action_delivery =
				{
				    type = "stream",
				    stream = createAttackBall(attack),
				    duration = 160
				}
			}
		}
	}
    end,
    {
	{		
	    type = "attribute",
	    name = "health",
	    [1] = 10,
	    [2] = 15,
	    [3] = 20,
	    [4] = 25,
	    [5] = 30,
	    [6] = 35,
	    [7] = 40,
	    [8] = 45,
	    [9] = 50,
	    [10] = 55
	},

	{		
	    type = "attack",
	    name = "cooldown",
	    [1] = 60,
	    [2] = 60,
	    [3] = 55,
	    [4] = 55,
	    [5] = 50,
	    [6] = 50,
	    [7] = 45,
	    [8] = 45,
	    [9] = 40,
	    [10] = 40
	},

	{		
	    type = "attribute",
	    name = "ttl",
	    [1] = 300,
	    [2] = 300,
	    [3] = 350,
	    [4] = 350,
	    [5] = 400,
	    [6] = 400,
	    [7] = 450,
	    [8] = 450,
	    [9] = 500,
	    [10] = 500
	},
	
	{		
	    type = "attack",
	    name = "damage",
	    [1] = 2,
	    [2] = 4,
	    [3] = 7,
	    [4] = 13,
	    [5] = 15,
	    [6] = 18,
	    [7] = 22,
	    [8] = 28,
	    [9] = 35,
	    [10] = 40
	},
	
	{		
	    type = "attribute",
	    name = "healing",
	    [1] = 0.01,
	    [2] = 0.01,
	    [3] = 0.015,
	    [4] = 0.02,
	    [5] = 0.05,
	    [6] = 0.075,
	    [7] = 0.1,
	    [8] = 0.12,
	    [9] = 0.14,
	    [10] = 0.16
	},

	
	{
	    type = "attribute",
	    name = "movement",
	    [1] = 0.03,
	    [2] = 0.03,
	    [3] = 0.04,
	    [4] = 0.04,
	    [5] = 0.05,
	    [6] = 0.05,
	    [7] = 0.06,
	    [8] = 0.06,
	    [9] = 0.07,
	    [10] = 0.07
	},
	{
	    type = "attribute",
	    name = "distancePerFrame",
	    [1] = 0.013,
	    [2] = 0.013,
	    [3] = 0.014,
	    [4] = 0.014,
	    [5] = 0.015,
	    [6] = 0.015,
	    [7] = 0.016,
	    [8] = 0.016,
	    [9] = 0.017,
	    [10] = 0.017
	},

	{
	    type = "attack",
	    name = "rangeFromPlayer",
	    [1] = 18,
	    [2] = 18,
	    [3] = 19,
	    [4] = 19,
	    [5] = 20,
	    [6] = 20,
	    [7] = 21,
	    [8] = 21,
	    [9] = 22,
	    [10] = 22
	},
	
	{
	    type = "attack",
	    name = "range",
	    [1] = 10,
	    [2] = 10,
	    [3] = 11,
	    [4] = 11,
	    [5] = 12,
	    [6] = 12,
	    [7] = 13,
	    [8] = 13,
	    [9] = 14,
	    [10] = 14
	},

	{
	    type = "attack",
	    name = "radius",
	    [1] = 1.2,
	    [2] = 1.3,
	    [3] = 1.4,
	    [4] = 1.5,
	    [5] = 1.6,
	    [6] = 1.7,
	    [7] = 1.8,
	    [8] = 1.9,
	    [9] = 2.0,
	    [10] = 2.5
	},

	{
	    type = "attack",
	    name = "particleVerticalAcceleration",
	    [1] = 0.01,
	    [2] = 0.01,
	    [3] = 0.02,
	    [4] = 0.02,
	    [5] = 0.03,
	    [6] = 0.03,
	    [7] = 0.04,
	    [8] = 0.04,
	    [9] = 0.05,
	    [10] = 0.05
	},

	{
	    type = "attack",
	    name = "particleHoizontalSpeed",
	    [1] = 0.6,
	    [2] = 0.6,
	    [3] = 0.7,
	    [4] = 0.7,
	    [5] = 0.8,
	    [6] = 0.8,
	    [7] = 0.9,
	    [8] = 0.9,
	    [9] = 1,
	    [10] = 1
	},

	{
	    type = "attack",
	    name = "particleHoizontalSpeedDeviation",
	    [1] = 0.0025,
	    [2] = 0.0025,
	    [3] = 0.0024,
	    [4] = 0.0024,
	    [5] = 0.0023,
	    [6] = 0.0023,
	    [7] = 0.0022,
	    [8] = 0.0022,
	    [9] = 0.0021,
	    [10] = 0.0021
	}
    },
    WASP_UNIT_VARIATIONS,
    WASP_UNIT_TIERS
)

buildUnits(
    {
	name = "wasp-worm-drone",

	attributes = {	    
	},
	attack = {
	    type = "stream",
	    softSmokeName = softSmoke
	},
	death = function (attributes)
	    return {
		type = "direct",
		action_delivery =
		    {
			type = "instant",
			target_effects =
			    {
				type = "create-entity",
				entity_name = "acid-splash-purple"
			    }
		    }
	    }
	end,
	scales = {
	    [1] = 0.5,
	    [2] = 0.5,
	    [3] = 0.6,
	    [4] = 0.6,
	    [5] = 0.7,
	    [6] = 0.7,
	    [7] = 0.8,
	    [8] = 0.8,
	    [9] = 0.9,
	    [10] = 0.9
	},
	resistances = {},

	type = "drone",
	attackName = "wasp-worm-drone",
	tint = {r=1, g=1, b=0, a=1},
	pTint = {r=0, g=1, b=1, a=0.5},
	sTint = {r=0, g=1, b=1, a=0.5},
	dTint = {r=0, g=0, b=1, a=1}
    },
    function (attack)
	return {
	    type = "projectile",
	    ammo_category = "bullet",
	    cooldown = attack.cooldown or 20,
	    projectile_center = {0, 1},
	    projectile_creation_distance = 0.6,
	    range = attack.range or 15,
	    sound = biterAttackSounds(),
	    ammo_type =
		{
		    category = "bullet",
		    action =
			{
			    type = "direct",
			    action_delivery =
				{
				    type = "stream",
				    stream = createAttackBall(attack),
				    duration = 160
				}
			}
		}
	}
    end,
    {
	{		
	    type = "attribute",
	    name = "health",
	    [1] = 10,
	    [2] = 15,
	    [3] = 20,
	    [4] = 25,
	    [5] = 30,
	    [6] = 35,
	    [7] = 40,
	    [8] = 45,
	    [9] = 50,
	    [10] = 55
	},
	
	{		
	    type = "attack",
	    name = "cooldown",
	    [1] = 60,
	    [2] = 60,
	    [3] = 55,
	    [4] = 55,
	    [5] = 50,
	    [6] = 50,
	    [7] = 45,
	    [8] = 45,
	    [9] = 40,
	    [10] = 40
	},

	{		
	    type = "attribute",
	    name = "ttl",
	    [1] = 300,
	    [2] = 300,
	    [3] = 350,
	    [4] = 350,
	    [5] = 400,
	    [6] = 400,
	    [7] = 450,
	    [8] = 450,
	    [9] = 500,
	    [10] = 500
	},
	
	{		
	    type = "attack",
	    name = "damage",
	    [1] = 2,
	    [2] = 4,
	    [3] = 7,
	    [4] = 13,
	    [5] = 15,
	    [6] = 18,
	    [7] = 22,
	    [8] = 28,
	    [9] = 35,
	    [10] = 40
	},

	{
	    type = "attack",
	    name = "rangeFromPlayer",
	    [1] = 10,
	    [2] = 10,
	    [3] = 11,
	    [4] = 11,
	    [5] = 12,
	    [6] = 12,
	    [7] = 13,
	    [8] = 13,
	    [9] = 14,
	    [10] = 14
	},
	
	{		
	    type = "attribute",
	    name = "healing",
	    [1] = 0.01,
	    [2] = 0.01,
	    [3] = 0.015,
	    [4] = 0.02,
	    [5] = 0.05,
	    [6] = 0.075,
	    [7] = 0.1,
	    [8] = 0.12,
	    [9] = 0.14,
	    [10] = 0.16
	},

	
	{
	    type = "attribute",
	    name = "movement",
	    [1] = 0.01,
	    [2] = 0.01,
	    [3] = 0.02,
	    [4] = 0.02,
	    [5] = 0.03,
	    [6] = 0.03,
	    [7] = 0.04,
	    [8] = 0.04,
	    [9] = 0.05,
	    [10] = 0.05
	},
	{
	    type = "attribute",
	    name = "distancePerFrame",
	    [1] = 0.013,
	    [2] = 0.013,
	    [3] = 0.014,
	    [4] = 0.014,
	    [5] = 0.015,
	    [6] = 0.015,
	    [7] = 0.016,
	    [8] = 0.016,
	    [9] = 0.017,
	    [10] = 0.017
	},

	{
	    type = "attack",
	    name = "range",
	    [1] = 10,
	    [2] = 10,
	    [3] = 11,
	    [4] = 11,
	    [5] = 12,
	    [6] = 12,
	    [7] = 13,
	    [8] = 13,
	    [9] = 14,
	    [10] = 14
	},

	{
	    type = "attack",
	    name = "radius",
	    [1] = 1.2,
	    [2] = 1.3,
	    [3] = 1.4,
	    [4] = 1.5,
	    [5] = 1.6,
	    [6] = 1.7,
	    [7] = 1.8,
	    [8] = 1.9,
	    [9] = 2.0,
	    [10] = 2.5
	},

	{
	    type = "attack",
	    name = "particleVerticalAcceleration",
	    [1] = 0.01,
	    [2] = 0.01,
	    [3] = 0.02,
	    [4] = 0.02,
	    [5] = 0.03,
	    [6] = 0.03,
	    [7] = 0.04,
	    [8] = 0.04,
	    [9] = 0.05,
	    [10] = 0.05
	},

	{
	    type = "attack",
	    name = "particleHoizontalSpeed",
	    [1] = 0.6,
	    [2] = 0.6,
	    [3] = 0.7,
	    [4] = 0.7,
	    [5] = 0.8,
	    [6] = 0.8,
	    [7] = 0.9,
	    [8] = 0.9,
	    [9] = 1,
	    [10] = 1
	},

	{
	    type = "attack",
	    name = "particleHoizontalSpeedDeviation",
	    [1] = 0.0025,
	    [2] = 0.0025,
	    [3] = 0.0024,
	    [4] = 0.0024,
	    [5] = 0.0023,
	    [6] = 0.0023,
	    [7] = 0.0022,
	    [8] = 0.0022,
	    [9] = 0.0021,
	    [10] = 0.0021
	}
    },
    WASP_UNIT_VARIATIONS,
    WASP_UNIT_TIERS
)


-- wasp spitters
buildUnitSpawner(
    {
	unit = {
	    name = "wasp-spitter",

	    loot = biterLoot,
	    attributes = {
		explosion = "blood-explosion-small"
	    },
	    attack = {
		type = "projectile",
		directionOnly = true,
		collisionBox = {{0,0}, {0,0}},
		softSmokeName = softSmoke
	    },
	    resistances = {},

	    type = "spitter",
	    scales = {
		[1] = 0.5,
		[2] = 0.6,
		[3] = 0.7,
		[4] = 0.8,
		[5] = 0.9,
		[6] = 1,
		[7] = 1.1,
		[8] = 1.2,
		[9] = 1.3,
		[10] = 1.4
	    },
	    attackName = "wasp-drone",
	    tint = {r=1, g=1, b=0, a=1},
	    pTint = {r=0, g=1, b=1, a=0.5},
	    sTint = {r=0, g=1, b=1, a=0.5}
	},

	unitSpawner = {
	    name = "wasp-spitter-nest",

	    loot = spawnerLoot,
	    attributes = {},
	    resistances = {},

	    scales = {
		[1] = 0.5,
		[2] = 0.6,
		[3] = 0.7,
		[4] = 0.8,
		[5] = 0.9,
		[6] = 1.0,
		[7] = 1.1,
		[8] = 1.2,
		[9] = 1.3,
		[10] = 1.4
	    },
	    tint = {r=1, g=1, b=0, a=1}
	}
    },

    {
	unit = {
	    {		
		type = "attribute",
		name = "health",
		[1] = 10,
		[2] = 50,
		[3] = 200,
		[4] = 350,
		[5] = 750,
		[6] = 1000,
		[7] = 1500,
		[8] = 2500,
		[9] = 4500,
		[10] = 7000
	    },

	    {		
		type = "attack",
		name = "cooldown",
		[1] = 100,
		[2] = 100,
		[3] = 97,
		[4] = 97,
		[5] = 95,
		[6] = 95,
		[7] = 93,
		[8] = 93,
		[9] = 90,
		[10] = 90
	    },

	    {		
		type = "attribute",
		name = "spawningTimeModifer",
		[1] = 0,
		[2] = 0,
		[3] = 1,
		[4] = 2,
		[5] = 3,
		[6] = 7,
		[7] = 10,
		[8] = 10,
		[9] = 12,
		[10] = 12
	    },

	    {		
		type = "attribute",
		name = "pollutionToAttack",
		[1] = 200,
		[2] = 750,
		[3] = 1200,
		[4] = 1750,
		[5] = 2500,
		[6] = 5000,
		[7] = 10000,
		[8] = 12500,
		[9] = 15000,
		[10] = 20000
	    },

	    {		
		type = "attack",
		name = "damage",
		[1] = 4,
		[2] = 9,
		[3] = 14,
		[4] = 23,
		[5] = 30,
		[6] = 37,
		[7] = 45,
		[8] = 57,
		[9] = 70,
		[10] = 80
	    },

	    {		
		type = "attribute",
		name = "healing",
		[1] = 0.01,
		[2] = 0.01,
		[3] = 0.015,
		[4] = 0.02,
		[5] = 0.05,
		[6] = 0.075,
		[7] = 0.1,
		[8] = 0.12,
		[9] = 0.14,
		[10] = 0.16
	    },


	    {
		type = "attribute",
		name = "movement",
		[1] = 0.185,
		[2] = 0.18,
		[3] = 0.18,
		[4] = 0.17,
		[5] = 0.17,
		[6] = 0.16,
		[7] = 0.16,
		[8] = 0.15,
		[9] = 0.15,
		[10] = 0.14
	    },
	    {
		type = "attribute",
		name = "distancePerFrame",
		[1] = 0.04,
		[2] = 0.045,
		[3] = 0.050,
		[4] = 0.055,
		[5] = 0.060,
		[6] = 0.065,
		[7] = 0.070,
		[8] = 0.075,
		[9] = 0.08,
		[10] = 0.084
	    },

	    {
		type = "resistance",
		name = "explosion",
		percent = {
		    [1] = 0,
		    [2] = 0,
		    [3] = 10,
		    [4] = 10,
		    [5] = 20,
		    [6] = 20,
		    [7] = 30,
		    [8] = 30,
		    [9] = 40,
		    [10] = 40
		}
	    },

	    {
		type = "attack",
		name = "range",
		[1] = 13,
		[2] = 13,
		[3] = 14,
		[4] = 14,
		[5] = 15,
		[6] = 15,
		[7] = 16,
		[8] = 16,
		[9] = 17,
		[10] = 17
	    },

	    {
		type = "attack",
		name = "radius",
		[1] = 1.2,
		[2] = 1.3,
		[3] = 1.4,
		[4] = 1.5,
		[5] = 1.6,
		[6] = 1.7,
		[7] = 1.8,
		[8] = 1.9,
		[9] = 2.0,
		[10] = 2.5
	    },

	    {
		type = "attack",
		name = "particleVerticalAcceleration",
		[1] = 0.01,
		[2] = 0.01,
		[3] = 0.02,
		[4] = 0.02,
		[5] = 0.03,
		[6] = 0.03,
		[7] = 0.04,
		[8] = 0.04,
		[9] = 0.05,
		[10] = 0.05
	    },

	    {
		type = "attack",
		name = "particleHoizontalSpeed",
		[1] = 0.6,
		[2] = 0.6,
		[3] = 0.7,
		[4] = 0.7,
		[5] = 0.8,
		[6] = 0.8,
		[7] = 0.9,
		[8] = 0.9,
		[9] = 1,
		[10] = 1
	    },

	    {
		type = "attack",
		name = "particleHoizontalSpeedDeviation",
		[1] = 0.0025,
		[2] = 0.0025,
		[3] = 0.0024,
		[4] = 0.0024,
		[5] = 0.0023,
		[6] = 0.0023,
		[7] = 0.0022,
		[8] = 0.0022,
		[9] = 0.0021,
		[10] = 0.0021
	    }
	},

	unitSpawner = {

	    {		
		type = "attribute",
		name = "health",
		[1] = 350,
		[2] = 500,
		[3] = 750,
		[4] = 1500,
		[5] = 2500,
		[6] = 3500,
		[7] = 5000,
		[8] = 7000,
		[9] = 10000,
		[10] = 15000
	    },

	    {		
		type = "attribute",
		name = "healing",
		[1] = 0.02,
		[2] = 0.02,
		[3] = 0.022,
		[4] = 0.024,
		[5] = 0.026,
		[6] = 0.028,
		[7] = 0.03,
		[8] = 0.032,
		[9] = 0.034,
		[10] = 0.036
	    },

	    {		
		type = "attribute",
		name = "spawingCooldownStart",
		[1] = 360,
		[2] = 360,
		[3] = 355,
		[4] = 355,
		[5] = 350,
		[6] = 350,
		[7] = 345,
		[8] = 345,
		[9] = 340,
		[10] = 340
	    },

	    {		
		type = "attribute",
		name = "spawingCooldownEnd",
		[1] = 150,
		[2] = 150,
		[3] = 145,
		[4] = 145,
		[5] = 140,
		[6] = 140,
		[7] = 135,
		[8] = 135,
		[9] = 130,
		[10] = 130
	    },

	    {		
		type = "attribute",
		name = "evolutionRequirement",
		[1] = 0.08,
		[2] = 0.16,
		[3] = 0.24,
		[4] = 0.35,
		[5] = 0.40,
		[6] = 0.45,
		[7] = 0.55,
		[8] = 0.65,
		[9] = 0.75,
		[10] = 0.9
	    },

	    {
		type = "attribute",
		name = "unitsOwned",
		[1] = 7,
		[2] = 7,
		[3] = 8,
		[4] = 8,
		[5] = 9,
		[6] = 9,
		[7] = 10,
		[8] = 10,
		[9] = 11,
		[10] = 11
	    },

	    {		
		type = "attribute",
		name = "unitsToSpawn",
		[1] = 5,
		[2] = 5,
		[3] = 6,
		[4] = 6,
		[5] = 7,
		[6] = 7,
		[7] = 8,
		[8] = 8,
		[9] = 9,
		[10] = 9
	    },

	    {
		type = "resistance",
		name = "physical",
		decrease = {
		    [1] = 2,
		    [2] = 2,
		    [3] = 4,
		    [4] = 4,
		    [5] = 6,
		    [6] = 6,
		    [7] = 10,
		    [8] = 12,
		    [9] = 12,
		    [10] = 14
		},
		percent = {
		    [1] = 15,
		    [2] = 15,
		    [3] = 17,
		    [4] = 17,
		    [5] = 18,
		    [6] = 18,
		    [7] = 19,
		    [8] = 19,
		    [9] = 20,
		    [10] = 20
		}
	    },
	    {
		type = "resistance",
		name = "explosion",
		decrease = {
		    [1] = 5,
		    [2] = 5,
		    [3] = 6,
		    [4] = 6,
		    [5] = 7,
		    [6] = 7,
		    [7] = 8,
		    [8] = 8,
		    [9] = 9,
		    [10] = 9
		},
		percent = {
		    [1] = 15,
		    [2] = 15,
		    [3] = 17,
		    [4] = 17,
		    [5] = 18,
		    [6] = 18,
		    [7] = 19,
		    [8] = 19,
		    [9] = 20,
		    [10] = 20
		}
	    },
	    {
		type = "resistance",
		name = "fire",
		decrease = {
		    [1] = 3,
		    [2] = 3,
		    [3] = 4,
		    [4] = 4,
		    [5] = 6,
		    [6] = 6,
		    [7] = 6,
		    [8] = 6,
		    [9] = 7,
		    [10] = 7
		},
		percent = {
		    [1] = 60,
		    [2] = 60,
		    [3] = 62,
		    [4] = 62,
		    [5] = 63,
		    [6] = 63,
		    [7] = 64,
		    [8] = 64,
		    [9] = 65,
		    [10] = 65
		}
	    }	    
	},

	probabilityTable = {
	    [1] = 1,
	    [2] = 1,
	    [3] = 1,
	    [4] = 1,
	    [5] = 1,
	    [6] = 1,
	    [7] = 1,
	    [8] = 1,
	    [9] = 1,
	    [10] = 1,
	}
    },

    function (attributes)
	return createProjectileAttack(attributes,
				   createCapsuleProjectile(attributes.name,
							   attributes,
							   attributes.name .. "-drone-rampant"),
				   spitterattackanimation(attributes.scale, attributes.tint))
    end,

    {
	unit = WASP_UNIT_VARIATIONS,
	unitSpawner = WASP_NEST_VARIATIONS
    },

    {
	unit = WASP_UNIT_TIERS,
	unitSpawner = WASP_NEST_TIERS
    }
)

-- wasp worms
buildWorm(
    {
	name = "wasp-worm",

	loot = wormLoot,	    
	attributes = {
	},
	attack = {
	    type = "projectile",
	    collisionBox = {{0,0}, {0,0}},
	    softSmokeName = softSmoke
	},
	resistances = {},

	scales = {
	    [1] = 0.5,
	    [2] = 0.6,
	    [3] = 0.7,
	    [4] = 0.8,
	    [5] = 0.9,
	    [6] = 1,
	    [7] = 1.1,
	    [8] = 1.2,
	    [9] = 1.3,
	    [10] = 1.4
	},
	attackName = "wasp-worm-drone",
	tint = {r=1, g=1, b=0, a=1},
	pTint = {r=0, g=1, b=1, a=0.5},
	sTint = {r=0, g=1, b=1, a=0.5}
    },

    {
	{
	    type = "attribute",
	    name = "health",
	    [1] = 200,
	    [2] = 350,
	    [3] = 500,
	    [4] = 750,
	    [5] = 900,
	    [6] = 1000,
	    [7] = 1500,
	    [8] = 3000,
	    [9] = 5000,
	    [10] = 9000
	},

	{    
	    type = "attack",
	    name = "cooldown",
	    [1] = 50,
	    [2] = 50,
	    [3] = 45,
	    [4] = 45,
	    [5] = 40,
	    [6] = 40,
	    [7] = 35,
	    [8] = 35,
	    [9] = 30,
	    [10] = 30
	},

	{    
	    type = "attribute",
	    name = "evolutionRequirement",
	    [1] = 0,
	    [2] = 0.1,
	    [3] = 0.2,
	    [4] = 0.3,
	    [5] = 0.4,
	    [6] = 0.5,
	    [7] = 0.6,
	    [8] = 0.7,
	    [9] = 0.8,
	    [10] = 0.9
	},

	{    
	    type = "attack",
	    name = "damage",
	    [1] = 12,
	    [2] = 20,
	    [3] = 25,
	    [4] = 30,
	    [5] = 35,
	    [6] = 40,
	    [7] = 50,
	    [8] = 60,
	    [9] = 70,
	    [10] = 80
	},

	{    
	    type = "attribute",
	    name = "healing",
	    [1] = 0.01,
	    [2] = 0.01,
	    [3] = 0.015,
	    [4] = 0.02,
	    [5] = 0.05,
	    [6] = 0.075,
	    [7] = 0.1,
	    [8] = 0.12,
	    [9] = 0.14,
	    [10] = 0.16
	},

	{    
	    type = "attribute",
	    name = "prepareRange",
	    [1] = 30,
	    [2] = 30,
	    [3] = 31,
	    [4] = 31,
	    [5] = 32,
	    [6] = 32,
	    [7] = 33,
	    [8] = 33,
	    [9] = 34,
	    [10] = 34
	},

	{    
	    type = "attribute",
	    name = "foldingSpeed",
	    [1] = 0.15,
	    [2] = 0.15,
	    [3] = 0.16,
	    [4] = 0.16,
	    [5] = 0.16,
	    [6] = 0.17,
	    [7] = 0.17,
	    [8] = 0.18,
	    [9] = 0.18,
	    [10] = 0.19
	},

	{    
	    type = "attribute",
	    name = "preparingSpeed",
	    [1] = 0.025,
	    [2] = 0.025,
	    [3] = 0.026,
	    [4] = 0.026,
	    [5] = 0.027,
	    [6] = 0.027,
	    [7] = 0.028,
	    [8] = 0.028,
	    [9] = 0.029,
	    [10] = 0.029
	},

	{
	    type = "resistance",
	    name = "physical",
	    decrease = {
		[1] = 0,
		[2] = 0,
		[3] = 5,
		[4] = 5,
		[5] = 8,
		[6] = 8,
		[7] = 10,
		[8] = 10,
		[9] = 12,
		[10] = 12
	    }
	},

	{
	    type = "resistance",
	    name = "explosion",
	    decrease = {
		[1] = 0,
		[2] = 0,
		[3] = 5,
		[4] = 5,
		[5] = 8,
		[6] = 8,
		[7] = 10,
		[8] = 10,
		[9] = 12,
		[10] = 12
	    },
	    percent = {
		[1] = 0,
		[2] = 0,
		[3] = 10,
		[4] = 10,
		[5] = 20,
		[6] = 20,
		[7] = 30,
		[8] = 30,
		[9] = 40,
		[10] = 40
	    }
	},

	{
	    type = "resistance",
	    name = "fire",
	    decrease = {
		[1] = 3,
		[2] = 3,
		[3] = 4,
		[4] = 4,
		[5] = 6,
		[6] = 6,
		[7] = 6,
		[8] = 6,
		[9] = 7,
		[10] = 7
	    },
	    percent = {
		[1] = 70,
		[2] = 70,
		[3] = 72,
		[4] = 72,
		[5] = 73,
		[6] = 73,
		[7] = 74,
		[8] = 74,
		[9] = 75,
		[10] = 75
	    }
	},

	{
	    type = "attack",
	    name = "range",
	    [1] = 21,
	    [2] = 22,
	    [3] = 23,
	    [4] = 23,
	    [5] = 24,
	    [6] = 26,
	    [7] = 26,
	    [8] = 28,
	    [9] = 30,
	    [10] = 32
	},

	{
	    type = "attack",
	    name = "radius",
	    [1] = 1.5,
	    [2] = 1.6,
	    [3] = 1.7,
	    [4] = 1.8,
	    [5] = 1.9,
	    [6] = 2.0,
	    [7] = 2.2,
	    [8] = 2.3,
	    [9] = 2.5,
	    [10] = 3.0
	},

	{
	    type = "attack",
	    name = "particleVerticalAcceleration",
	    [1] = 0.01,
	    [2] = 0.01,
	    [3] = 0.02,
	    [4] = 0.02,
	    [5] = 0.03,
	    [6] = 0.03,
	    [7] = 0.04,
	    [8] = 0.04,
	    [9] = 0.05,
	    [10] = 0.05
	},

	{	      
	    type = "attack",
	    name = "particleHoizontalSpeed",
	    [1] = 0.6,
	    [2] = 0.6,
	    [3] = 0.7,
	    [4] = 0.7,
	    [5] = 0.8,
	    [6] = 0.8,
	    [7] = 0.9,
	    [8] = 0.9,
	    [9] = 1,
	    [10] = 1
	},

	{	    
	    type = "attack",
	    name = "particleHoizontalSpeedDeviation",
	    [1] = 0.0025,
	    [2] = 0.0025,
	    [3] = 0.0024,
	    [4] = 0.0024,
	    [5] = 0.0023,
	    [6] = 0.0023,
	    [7] = 0.0022,
	    [8] = 0.0022,
	    [9] = 0.0021,
	    [10] = 0.0021
	}
    },

    function (attributes)
	return createProjectileAttack(attributes,
				  createCapsuleProjectile(attributes.name,
							  attributes,
							  attributes.name .. "-drone-rampant"))
    end,

    WASP_WORM_VARIATIONS,
    WASP_WORM_TIERS
)
