local swarmUtils = {}
-- imports

local droneUtils = require("utils/DroneUtils")
local biterUtils = require("utils/BiterUtils")
package.path = "../?.lua;" .. package.path
local mathUtils = require("libs/MathUtils")
local constants = require("libs/Constants")

-- imported functions

local gaussianRandomRangeRG = mathUtils.gaussianRandomRangeRG

local roundToNearest = mathUtils.roundToNearest

local mMax = math.max
local mMin = math.min

local mFloor = math.floor

local deepcopy = util.table.deepcopy

local TIER_UPGRADE_SET_5 = constants.TIER_UPGRADE_SET_5
local TIER_UPGRADE_SET_10 = constants.TIER_UPGRADE_SET_10

local TIER_NAMING_SET_10 = constants.TIER_NAMING_SET_10
local TIER_NAMING_SET_5 = constants.TIER_NAMING_SET_5

local xorRandom = mathUtils.xorRandom(settings.startup["rampant-enemySeed"].value)

local makeBiterCorpse = biterUtils.makeBiterCorpse
local makeUnitSpawnerCorpse = biterUtils.makeUnitSpawnerCorpse
local makeWormCorpse = biterUtils.makeWormCorpse
local makeSpitterCorpse = biterUtils.makeSpitterCorpse

local makeBiter = biterUtils.makeBiter
local makeDrone = droneUtils.makeDrone
local makeSpitter = biterUtils.makeSpitter
local makeWorm = biterUtils.makeWorm
local makeUnitSpawner = biterUtils.makeUnitSpawner

-- module code



local function isMember(fieldType, fieldName, lst)
    for i=1,#lst do
	if (lst[i].type == fieldType) and (lst[i].name == fieldName) then
	    return true
	end
    end
    return false
end

local function pushUpgrade(upgrades, upgrade)
    if not isMember(upgrade.type, upgrade.name, upgrades) then
	upgrades[#upgrades+1] = upgrade
    end
end

local function addDefaultScales(template)
    if not template.scales then
	template.scales = {
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
	}
    end
end

local function addDefaultVerticalAcceleration(upgrades)
    pushUpgrade(upgrades,
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
    })
end

local function addDefaultHoizontalSpeed(upgrades)
    pushUpgrade(upgrades,
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
    })
end

local function addDefaultHoizontalSpeedDeviation(upgrades)
    pushUpgrade(upgrades,
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
    })
end

local function addUnitDefaults(template, upgrades)
    addDefaultScales(template)

    addDefaultVerticalAcceleration(upgrades)
    addDefaultHoizontalSpeed(upgrades)
    addDefaultHoizontalSpeedDeviation(upgrades)

    pushUpgrade(upgrades,
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
    })

    if (template.type == "biter") then

	pushUpgrade(upgrades,
		    {
			type = "attack",
			name = "range",
			[1] = 0.5,
			[2] = 0.5,
			[3] = 0.75,
			[4] = 0.75,
			[5] = 1.0,
			[6] = 1.0,
			[7] = 1.25,
			[8] = 1.50,
			[9] = 1.75,
			[10] = 2.0
	})

	pushUpgrade(upgrades,
		    {
			type = "attribute",
			name = "distancePerFrame",
			[1] = 0.1,
			[2] = 0.125,
			[3] = 0.15,
			[4] = 0.19,
			[5] = 0.195,
			[6] = 0.2,
			[7] = 0.2,
			[8] = 0.2,
			[9] = 0.2,
			[10] = 0.2
	})

	pushUpgrade(upgrades,
		    {
			type = "resistance",
			name = "physical",
			decrease = {
			    [1] = 0,
			    [2] = 0,
			    [3] = 4,
			    [4] = 5,
			    [5] = 6,
			    [6] = 8,
			    [7] = 11,
			    [8] = 13,
			    [9] = 16,
			    [10] = 17
			},
			percent = {
			    [1] = 0,
			    [2] = 0,
			    [3] = 0,
			    [4] = 10,
			    [5] = 12,
			    [6] = 12,
			    [7] = 14,
			    [8] = 16,
			    [9] = 18,
			    [10] = 20
			}
	})

	pushUpgrade(upgrades,
		    {
			type = "resistance",
			name = "explosion",
			decrease = {
			    [1] = 0,
			    [2] = 0,
			    [3] = 0,
			    [4] = 0,
			    [5] = 0,
			    [6] = 10,
			    [7] = 12,
			    [8] = 14,
			    [9] = 16,
			    [10] = 20
			},
			percent = {
			    [1] = 0,
			    [2] = 0,
			    [3] = 0,
			    [4] = 10,
			    [5] = 12,
			    [6] = 13,
			    [7] = 15,
			    [8] = 16,
			    [9] = 17,
			    [10] = 20
			}
	})

	pushUpgrade(upgrades,
		    {
			type = "attribute",
			name = "movement",
			[1] = 0.2,
			[2] = 0.19,
			[3] = 0.185,
			[4] = 0.18,
			[5] = 0.175,
			[6] = 0.17,
			[7] = 0.17,
			[8] = 0.17,
			[9] = 0.17,
			[10] = 0.17
	})

	pushUpgrade(upgrades,
		    {
			type = "attack",
			name = "cooldown",
			[1] = 35,
			[2] = 35,
			[3] = 35,
			[4] = 35,
			[5] = 35,
			[6] = 35,
			[7] = 50,
			[8] = 50,
			[9] = 55,
			[10] = 57
	})

	pushUpgrade(upgrades,
		    {
			type = "attribute",
			name = "health",
			[1] = 15,
			[2] = 75,
			[3] = 150,
			[4] = 250,
			[5] = 400,
			[6] = 750,
			[7] = 1500,
			[8] = 3000,
			[9] = 5000,
			[10] = 10000
	})

	pushUpgrade(upgrades,
		    {
			type = "attack",
			name = "cooldown",
			[1] = 35,
			[2] = 35,
			[3] = 35,
			[4] = 35,
			[5] = 35,
			[6] = 35,
			[7] = 50,
			[8] = 50,
			[9] = 55,
			[10] = 57
	})

	pushUpgrade(upgrades,
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
	})

	pushUpgrade(upgrades,
		    {
			type = "attack",
			name = "damage",
			[1] = 7,
			[2] = 15,
			[3] = 22.5,
			[4] = 35,
			[5] = 45,
			[6] = 60,
			[7] = 75,
			[8] = 90,
			[9] = 150,
			[10] = 200
	})

	pushUpgrade(upgrades,
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
	})

    elseif (template.type == "spitter") then
	pushUpgrade(upgrades,
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
	})

	pushUpgrade(upgrades,
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
	})

        pushUpgrade(upgrades,
		    {
			type = "resistance",
			name = "physical",
			decrease = {
			    [1] = 0,
			    [2] = 0,
			    [3] = 0,
			    [4] = 0,
			    [5] = 2,
			    [6] = 4,
			    [7] = 6,
			    [8] = 8,
			    [9] = 10,
			    [10] = 12
			},
			percent = {
			    [1] = 0,
			    [2] = 0,
			    [3] = 0,
			    [4] = 10,
			    [5] = 12,
			    [6] = 12,
			    [7] = 14,
			    [8] = 14,
			    [9] = 15,
			    [10] = 15
			}
	})

	pushUpgrade(upgrades,
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
	})

	pushUpgrade(upgrades,
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
	})


	pushUpgrade(upgrades,
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
	})

	pushUpgrade(upgrades,
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
	})

	pushUpgrade(upgrades,
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
	})

	pushUpgrade(upgrades,
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
	})

	pushUpgrade(upgrades,
		    {
			type = "attribute",
			name = "spawningTimeModifer",
			[1] = 0,
			[2] = 0,
			[3] = 1,
			[4] = 2,
			[5] = 2,
			[6] = 5,
			[7] = 8,
			[8] = 8,
			[9] = 10,
			[10] = 10
	})

    elseif (template.type == "drone") then
	pushUpgrade(upgrades,
		    {
			type = "attribute",
			name = "health",
			[1] = 100,
			[2] = 100,
			[3] = 110,
			[4] = 110,
			[5] = 120,
			[6] = 120,
			[7] = 130,
			[8] = 130,
			[9] = 140,
			[10] = 140
	})

	pushUpgrade(upgrades,
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
	})
    end
end

local function addUnitSpawnerDefaults(template, upgrades)
    addDefaultScales(template)

    pushUpgrade(upgrades,
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
    })

    pushUpgrade(upgrades,
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
    })

    pushUpgrade(upgrades,
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
    })

    pushUpgrade(upgrades,
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
    })

    pushUpgrade(upgrades,
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
    })

    pushUpgrade(upgrades,
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
    })

    pushUpgrade(upgrades,
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
    })

    pushUpgrade(upgrades,
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
    })

    pushUpgrade(upgrades,
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
    })

    pushUpgrade(upgrades,
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
    })
end

local function addWormDefaults(template, upgrades)
    addDefaultScales(template)

    addDefaultVerticalAcceleration(upgrades)
    addDefaultHoizontalSpeed(upgrades)
    addDefaultHoizontalSpeedDeviation(upgrades)

    pushUpgrade(upgrades,
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
    })

    pushUpgrade(upgrades,
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
    })

    pushUpgrade(upgrades,
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
    })

    pushUpgrade(upgrades,
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
    })

    pushUpgrade(upgrades,
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
    })

    pushUpgrade(upgrades,
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
    })

    pushUpgrade(upgrades,
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
    })

    pushUpgrade(upgrades,
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
    })

    pushUpgrade(upgrades,
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
    })

    pushUpgrade(upgrades,
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
    })

    pushUpgrade(upgrades,
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
    })

    pushUpgrade(upgrades,
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
    })

    pushUpgrade(upgrades,
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
    })
end


local function unitSetToProbabilityTable(upgradeTable, unitSet)
    local dividers = {}

    for i=1,#unitSet do
	dividers[i] = 1
    end

    if upgradeTable then
	local points = #unitSet * 2
	for _=1,points do
	    local index = mFloor(xorRandom() * #unitSet)+1
	    local upgrade = upgradeTable[index]

	    dividers[index] = dividers[index] + upgrade
	end
    end

    local total = 0
    for i=1,#dividers do
	total = total + dividers[i]
    end

    local runningTotal = 0
    for i=1,#dividers do
	runningTotal = runningTotal + (dividers[i] / total)
	dividers[i] = runningTotal
    end

    local stepUnit = 1 / (#unitSet[1] + 1)

    local probabilityTable = {}

    for i=1,#unitSet do
	local result
    	if (i == 1) then
    	    result = {
    		{
    		    0,
    		    stepUnit
    		},
    		{
    		    dividers[i],
    		    0
    		}
    	    }
    	elseif (i == #unitSet) then
    	    result = {
    		{
    		    dividers[i-2],
    		    0
    		},
    		{
    		    1,
    		    stepUnit
    		}
    	    }
    	else
    	    result = {
    		{
    		    ((i - 2) > 0 and dividers[i-2]) or 0,
    		    0
    		},
    		{
    		    dividers[i-1],
    		    stepUnit
    		},
    		{
    		    dividers[i],
    		    0
    		}
    	    }
    	end

	probabilityTable[i] = result
    end

    local result = {}

    for i=1, #probabilityTable do
	local probability = probabilityTable[i]
	for x=1, #unitSet[i] do
	    result[#result+1] = {unitSet[i][x], probability}
	end
    end

    return result
end

local function scaleAttributes (upgrade, entity)
    if (upgrade.type == "attribute") then
	if (entity.type == "biter") then
	    if (upgrade.name == "health") then
		entity.attributes[upgrade.name] = entity.attributes[upgrade.name] * settings.startup["rampant-unitBiterHealthScaler"].value
	    end
	    if (upgrade.name == "movement") then
		entity.attributes[upgrade.name] = entity.attributes[upgrade.name] * settings.startup["rampant-unitBiterSpeedScaler"].value
	    end
	    if (upgrade.name == "distancePerFrame") then
		entity.attributes[upgrade.name] = entity.attributes[upgrade.name] * settings.startup["rampant-unitBiterSpeedScaler"].value
	    end

	elseif (entity.type == "spitter") then
	    if (upgrade.name == "health") then
		entity.attributes[upgrade.name] = entity.attributes[upgrade.name] * settings.startup["rampant-unitSpitterHealthScaler"].value
	    end
	    if (upgrade.name == "movement") then
		entity.attributes[upgrade.name] = entity.attributes[upgrade.name] * settings.startup["rampant-unitSpitterSpeedScaler"].value
	    end
	    if (upgrade.name == "distancePerFrame") then
		entity.attributes[upgrade.name] = entity.attributes[upgrade.name] * settings.startup["rampant-unitSpitterSpeedScaler"].value
	    end
	elseif (entity.type == "drone") then
	    if (upgrade.name == "health") then
		entity.attributes[upgrade.name] = entity.attributes[upgrade.name] * settings.startup["rampant-unitDroneHealthScaler"].value
	    end
	    if (upgrade.name == "movement") then
		entity.attributes[upgrade.name] = entity.attributes[upgrade.name] * settings.startup["rampant-unitDroneSpeedScaler"].value
	    end
	    if (upgrade.name == "distancePerFrame") then
		entity.attributes[upgrade.name] = entity.attributes[upgrade.name] * settings.startup["rampant-unitDroneSpeedScaler"].value
	    end
	elseif (entity.type == "spawner") then
	    if (upgrade.name == "health") then
		entity.attributes[upgrade.name] = entity.attributes[upgrade.name] * settings.startup["rampant-unitSpawnerHealthScaler"].value
	    end
	    if (upgrade.name == "unitsOwned") then
		entity.attributes[upgrade.name] = entity.attributes[upgrade.name] * settings.startup["rampant-unitSpawnerOwnedScaler"].value
	    end
	    if (upgrade.name == "unitsToSpawn") then
		entity.attributes[upgrade.name] = entity.attributes[upgrade.name] * settings.startup["rampant-unitSpawnerSpawnScaler"].value
	    end
	    if (upgrade.name == "spawingCooldownStart") then
		entity.attributes[upgrade.name] = entity.attributes[upgrade.name] * settings.startup["rampant-unitSpawnerRespawnScaler"].value
	    end
	    if (upgrade.name == "spawingCooldownEnd") then
		entity.attributes[upgrade.name] = entity.attributes[upgrade.name] * settings.startup["rampant-unitSpawnerRespawnScaler"].value
	    end
	elseif (entity.type == "worm") then
	    if (upgrade.name == "health") then
		entity.attributes[upgrade.name] = entity.attributes[upgrade.name] * settings.startup["rampant-unitWormHealthScaler"].value
	    end
	end
    end
    if (upgrade.type == "resistance") then
	-- not asked for
    end
    if (upgrade.type == "attack") then
	if (entity.type == "biter") then
	    if (upgrade.name == "damage") then
		entity.attack[upgrade.name] = entity.attack[upgrade.name] * settings.startup["rampant-unitBiterDamageScaler"].value
	    end
	    if (upgrade.name == "range") then
		entity.attack[upgrade.name] = entity.attack[upgrade.name] * settings.startup["rampant-unitBiterRangeScaler"].value
	    end
	elseif (entity.type == "spitter") then
	    if (upgrade.name == "damage") then
		entity.attack[upgrade.name] = entity.attack[upgrade.name] * settings.startup["rampant-unitSpitterDamageScaler"].value
	    end
	    if (upgrade.name == "stickerDamagePerTick") then
		entity.attack[upgrade.name] = entity.attack[upgrade.name] * settings.startup["rampant-unitSpitterDamageScaler"].value
	    end
	    if (upgrade.name == "range") then
		entity.attack[upgrade.name] = entity.attack[upgrade.name] * settings.startup["rampant-unitSpitterRangeScaler"].value
	    end
	elseif (entity.type == "drone") then
	    if (upgrade.name == "damage") then
		entity.attack[upgrade.name] = entity.attack[upgrade.name] * settings.startup["rampant-unitDroneDamageScaler"].value
	    end
	    if (upgrade.name == "range") then
		entity.attack[upgrade.name] = entity.attack[upgrade.name] * settings.startup["rampant-unitDroneRangeScaler"].value
	    end
	elseif (entity.type == "worm") then
	    if (upgrade.name == "damage") then
		entity.attack[upgrade.name] = entity.attack[upgrade.name] * settings.startup["rampant-unitWormDamageScaler"].value
	    end
	    if (upgrade.name == "range") then
		entity.attack[upgrade.name] = entity.attack[upgrade.name] * settings.startup["rampant-unitWormRangeScaler"].value
	    end
	end
    end
end

local function upgradeEntity(entity, upgradeTable, tier)
    if upgradeTable then
	for upgradeIndex=1, #upgradeTable do
	    local upgrade = upgradeTable[upgradeIndex]

	    if (upgrade.type == "attribute") then
		if upgrade.mapping then
		    entity.attributes[upgrade.mapping] = upgrade[tier]
		else
		    local adj = upgrade[tier]
		    local min = upgrade.min or adj * 0.85
		    local max = upgrade.max or adj * 1.30
		    if (adj < 0) then
			local t = min
			min = max
			max = t
		    end
		    adj = roundToNearest(gaussianRandomRangeRG(adj, adj * 0.15, min, max, xorRandom), 0.001)
		    entity.attributes[upgrade.name] = (entity.attributes[upgrade.name] or 0) + adj
		end
		scaleAttributes(upgrade, entity)
	    end
	    if (upgrade.type == "resistance") then
		local field = upgrade.name
		if not entity.resistances[field] then
		    entity.resistances[field] = {}
		end
		local adj
		if upgrade.decrease then
		    adj = upgrade.decrease[tier]
		    local min = upgrade.min or adj * 0.85
		    local max = upgrade.max or adj * 1.30
		    if (adj < 0) then
			local t = min
			min = max
			max = t
		    end
		    adj = roundToNearest(gaussianRandomRangeRG(adj, adj * 0.15, min, max, xorRandom), 0.001)
		    entity.resistances[field].decrease = (entity.resistances[field].decrease or 0) + adj
		end
		if upgrade.percent then
		    adj = upgrade.percent[tier]
		    local min = upgrade.min or adj * 0.85
		    local max = upgrade.max or adj * 1.30
		    if (adj < 0) then
			local t = min
			min = max
			max = t
		    end
		    adj = roundToNearest(gaussianRandomRangeRG(adj, adj * 0.15, min, max, xorRandom), 0.001)
		    entity.resistances[field].percent = mMin((entity.resistances[field].percent or 0) + adj, 100)
		end
	    end
	    if (upgrade.type == "attack") then
		if upgrade.mapping then
		    entity.attack[upgrade.mapping] = upgrade[tier]
		else
		    local adj = upgrade[tier]
		    local min = upgrade.min or adj * 0.85
		    local max = upgrade.max or adj * 1.30
		    if (adj < 0) then
			local t = min
			min = max
			max = t
		    end
		    adj = roundToNearest(gaussianRandomRangeRG(adj, adj * 0.15, min, max, xorRandom), 0.001)
		    entity.attack[upgrade.name] = (entity.attack[upgrade.name] or 0) + adj
		end
		scaleAttributes(upgrade, entity)
	    end
	end
    end
end

local function calculateRGBa(tint, tier)
    local r = gaussianRandomRangeRG(tint.r, tint.r * 0.10 + (0.005 * tier), mMax(tint.r * 0.85 - (0.005 * tier), 0), mMin(tint.r * 1.15, 1), xorRandom)
    local g = gaussianRandomRangeRG(tint.g, tint.g * 0.10 + (0.005 * tier), mMax(tint.g * 0.85 - (0.005 * tier), 0), mMin(tint.g * 1.15, 1), xorRandom)
    local b = gaussianRandomRangeRG(tint.b, tint.b * 0.10 + (0.005 * tier), mMax(tint.b * 0.85 - (0.005 * tier), 0), mMin(tint.b * 1.15, 1), xorRandom)
    local a = gaussianRandomRangeRG(tint.a, tint.a * 0.10 + (0.005 * tier), mMax(tint.a * 0.85 - (0.005 * tier), 0), mMin(tint.a * 1.15, 1), xorRandom)

    return { r=r, g=g, b=b, a=a }
end

local function generateApperance(unit, tier)
    local scale

    if unit.scales then
	local scaleValue = unit.scales[tier]
	scale = gaussianRandomRangeRG(scaleValue, scaleValue * 0.12, scaleValue * 0.60, scaleValue * 1.40, xorRandom)

	unit.scale = scale
	unit.attributes.scale = scale
    end
    if unit.tint then
	local tint = calculateRGBa(unit.tint, tier)

	unit.attributes.tint = tint

	if unit.attack then
	    if scale then
		unit.attack.scale = scale
	    end
	    unit.attack.tint = tint
	end
    end
    if unit.tint1 and unit.tint2 then
	local tint1 = calculateRGBa(unit.tint1, tier)
	local tint2 = calculateRGBa(unit.tint2, tier)

	unit.attributes.tint1 = tint1
	unit.attributes.tint2 = tint2

	if unit.attack then
	    if scale then
		unit.attack.scale = scale
	    end
	    unit.attack.tint1 = tint1
	    unit.attack.tint2 = tint2
	end
    end
    if unit.attack then
	if unit.pTint then
	    unit.attack.pTint = calculateRGBa(unit.pTint, tier)
	end
	if unit.lTint then
	    unit.attack.lTint = calculateRGBa(unit.lTint, tier)
	end
	if unit.sTint then
	    unit.attack.sTint = calculateRGBa(unit.sTint, tier)
	end
	if unit.smTint then
	    unit.attack.smTint = calculateRGBa(unit.smTint, tier)
	end
    end
end

function swarmUtils.buildUnits(template, attackGenerator, upgradeTable, variations, tiers)
    addUnitDefaults(template, upgradeTable)

    local unitSet = {}

    for tier=1, tiers do
	local t = ((tiers == 5) and TIER_NAMING_SET_5[tier]) or TIER_NAMING_SET_10[tier]
	local ut = ((tiers == 5) and TIER_UPGRADE_SET_5[tier]) or TIER_UPGRADE_SET_10[tier]
	local result = {}

	for i=1,variations do
	    local unit = deepcopy(template)
	    unit.name = unit.name .. "-v" .. i .. "-t" .. t
	    unit.attributes.tier = "-v" .. i .. "-t" .. t
	    generateApperance(unit, ut)
	    upgradeEntity(unit, upgradeTable,  ut)

	    if unit.attackName then
		unit.attack.name = unit.attackName .. "-v" .. i .. "-t" .. t
	    end

	    if unit.loot then
		unit.attributes.loot = { unit.loot[ut] }
	    end

	    local entity
	    if (unit.type == "spitter") then
		unit.attributes.corpse = makeSpitterCorpse(unit)
		entity = makeSpitter(unit.name,
				     unit.attributes,
				     attackGenerator(unit.attack, unit.attributes),
				     unit.resistances)
	    elseif (unit.type == "biter") then
		unit.attributes.corpse = makeBiterCorpse(unit)
		entity = makeBiter(unit.name,
				   unit.attributes,
				   attackGenerator(unit.attack, unit.attributes),
				   unit.resistances)
	    elseif (unit.type == "drone") then
		entity = makeDrone(unit.name,
				   unit.attributes,
				   unit.resistances,
				   attackGenerator(unit.attack),
				   unit.death(unit.attack, unit.attributes))
	    end

	    result[#result+1] = entity.name

	    data:extend({entity})
	end

	unitSet[#unitSet+1] = result
    end

    return unitSet
end

function swarmUtils.buildUnitSpawner(templates, upgradeTable, attackGenerator, variations, tiers)
    addUnitDefaults(templates.unit, upgradeTable.unit)
    addUnitSpawnerDefaults(templates.unitSpawner, upgradeTable.unitSpawner)

    if (not upgradeTable.probabilityTable) then
	upgradeTable.probabilityTable = {
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
    end

    local unitSet = swarmUtils.buildUnits(templates.unit,
					  attackGenerator,
					  upgradeTable.unit,
					  variations.unit,
					  tiers.unit)

    for tier=1, tiers.unitSpawner do
	local t = ((tiers.unitSpawner == 5) and TIER_NAMING_SET_5[tier]) or TIER_NAMING_SET_10[tier]
	local ut = ((tiers.unitSpawner == 5) and TIER_UPGRADE_SET_5[tier]) or TIER_UPGRADE_SET_10[tier]
	-- print(tier,t,ut)
	for i=1,variations.unitSpawner do
	    local unitSpawner = deepcopy(templates.unitSpawner)
	    unitSpawner.name = unitSpawner.name .. "-v" .. i .. "-t" .. t
	    local unitTable = unitSetToProbabilityTable(upgradeTable.probabilityTable,
							unitSet)
	    generateApperance(unitSpawner, ut)
	    unitSpawner.type = "spawner"
	    upgradeEntity(unitSpawner, upgradeTable.unitSpawner, ut)

	    if unitSpawner.loot then
		unitSpawner.attributes.loot = { unitSpawner.loot[ut] }
	    end

	    if unitSpawner.autoplace then
		unitSpawner.attributes["autoplace"] = unitSpawner.autoplace[ut]
	    end
	    unitSpawner.attributes.corpse = makeUnitSpawnerCorpse(unitSpawner)
	    data:extend({
		    makeUnitSpawner(unitSpawner.name,
				    unitSpawner.attributes,
				    unitSpawner.resistances,
				    unitTable)
	    })
	end
    end

end

function swarmUtils.buildWorm(template, upgradeTable, attackGenerator, variations, tiers)
    addWormDefaults(template, upgradeTable)

    for tier=1, tiers do
	local t = ((tiers == 5) and TIER_NAMING_SET_5[tier]) or TIER_NAMING_SET_10[tier]
	local ut = ((tiers == 5) and TIER_UPGRADE_SET_5[tier]) or TIER_UPGRADE_SET_10[tier]
	for i=1,variations do
	    local worm = deepcopy(template)
	    worm.name = worm.name .. "-v" .. i .. "-t" .. t
	    generateApperance(worm, ut)
	    worm.type = "worm"
	    upgradeEntity(worm, upgradeTable, ut)

	    if worm.attackName then
		worm.attack.name = worm.attackName .. "-v" .. i .. "-t" .. t
	    end

	    if worm.loot then
		worm.attributes.loot = { worm.loot[ut] }
	    end

	    if worm.autoplace then
		worm.attributes["autoplace"] = worm.autoplace[ut]
	    end
	    worm.attributes.corpse = makeWormCorpse(worm)
	    data:extend({
		    makeWorm(worm.name,
			     worm.attributes,
			     attackGenerator(worm.attack),
			     worm.resistances)
	    })
	end
    end
end

return swarmUtils
