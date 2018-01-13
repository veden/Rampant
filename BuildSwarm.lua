-- imports

local biterUtils = require("prototypes/enemies/BiterUtils")
local mathUtils = require("libs/MathUtils")

-- imported functions

local gaussianRandomRange = mathUtils.gaussianRandomRange

local mRandom = math.random
local mMin = math.min
local mMax = math.max

local deepcopy = util.table.deepcopy

local makeBiter = biterUtils.makeBiter
local makeSpitter = biterUtils.makeSpitter
local makeWorm = biterUtils.makeWorm
local makeUnitSpawner = biterUtils.makeUnitSpawner

local createSuicideAttack = biterUtils.createSuicideAttack

-- module code

local function unitSetToProbabilityTable(points, upgradeTable, unitSet)
    local dividers = {}
    
    for i=1,#unitSet do
	dividers[i] = 1
    end

    while (points > 0) do
	local upgrade = upgradeTable[mRandom(#upgradeTable)]

	local cost = upgrade.cost
	local index = upgrade.index

	dividers[index] = dividers[index] + upgrade.adjustment
	
	points = points - cost
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

    local stepUnit = 1 / #unitSet[1]
   
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

local function buildUnits(startingPoints, template, attackGenerator, upgradeTable, variations, tiers)
    local unitSet = {}

    for t=1, tiers do
	local result = {}

	local allottedPoints = startingPoints * t
	
	for i=1,variations do
	    local unit = deepcopy(template)
	    unit.name = unit.name .. "-v" .. i .. "-t" .. t
	    local remainingPoints = allottedPoints
	    while (remainingPoints > 0) do
		local upgrade = upgradeTable[mRandom(#upgradeTable)]

		local cost = upgrade.cost
		if upgrade.attribute then
		    local attribute = upgrade.attribute
		    unit.attributes[attribute.name] = unit.attributes[attribute.name] + attribute.adjustment
		end
		if upgrade.resistance then
		    local field = upgrade.resistance.name
		    if not unit.resistances[field] then
			unit.resistances[field] = {}
		    end
		    if upgrade.resistance.decrease then
			unit.resistances[field].decrease = (unit.resistances[field].decrease or 0) + upgrade.resistance.decrease
		    end
		    if upgrade.resistance.percentage then
			unit.resistances[field].percentage = (unit.resistances[field].percentage or 0) + upgrade.resistance.percentage
		    end
		end
		if upgrade.attack then
		    local attack = upgrade.attack
		    unit.attack[attack.name] = (unit.attack[attack.name] or 0) + attack.adjustment
		end
		
		remainingPoints = remainingPoints - cost
	    end

	    local scale = gaussianRandomRange(unit.scale, unit.scale * 0.12, unit.scale * 0.60, unit.scale * 1.40) + (0.05 * t)
	    local tint1 = {
		gaussianRandomRange(unit.tint1.r, unit.tint1.r * 0.06 + (0.005 * t), unit.tint1.r * 0.85 - (0.005 * t), unit.tint1.r * 1.15),
		gaussianRandomRange(unit.tint1.g, unit.tint1.g * 0.06 + (0.005 * t), unit.tint1.g * 0.85 - (0.005 * t), unit.tint1.g * 1.15),
		gaussianRandomRange(unit.tint1.b, unit.tint1.b * 0.06 + (0.005 * t), unit.tint1.b * 0.85 - (0.005 * t), unit.tint1.b * 1.15),
		gaussianRandomRange(unit.tint1.a, unit.tint1.a * 0.06 + (0.005 * t), unit.tint1.a * 0.85 - (0.005 * t), unit.tint1.a * 1.15)
	    }
	    local tint2 = {
		gaussianRandomRange(unit.tint2.r, unit.tint2.r * 0.06 + (0.005 * t), unit.tint2.r * 0.85 - (0.005 * t), unit.tint2.r * 1.15),
		gaussianRandomRange(unit.tint2.g, unit.tint2.g * 0.06 + (0.005 * t), unit.tint2.g * 0.85 - (0.005 * t), unit.tint2.g * 1.15),
		gaussianRandomRange(unit.tint2.b, unit.tint2.b * 0.06 + (0.005 * t), unit.tint2.b * 0.85 - (0.005 * t), unit.tint2.b * 1.15),
		gaussianRandomRange(unit.tint2.a, unit.tint2.a * 0.06 + (0.005 * t), unit.tint2.a * 0.85 - (0.005 * t), unit.tint2.a * 1.15)
	    }
	    
	    unit.attributes.scale = scale
	    unit.attributes.tint1 = tint1
	    unit.attributes.tint2 = tint2

	    unit.attack.scale = scale
	    unit.attack.tint1 = tint1
	    unit.attack.tint2 = tint2


	    local entity
	    if (unit.type == "spitter") then
		entity = makeSpitter(unit.name,
				     unit.attributes,
				     attackGenerator(unit.attack),
				     unit.resistances)
	    elseif (unit.type == "biter") then
		entity = makeBiter(unit.name,
				   unit.attributes,
				   attackGenerator(unit.attack),
				   unit.resistances)
	    end

	    result[i] = entity.name
	    
	    data:extend({entity})
	end
	
	unitSet[t] = result
    end
    
    return unitSet
end

local function buildUnitSpawner(startingPoints, template, upgradeTable, unitTable, variations, tiers)
    for t=1, tiers do
	local allottedPoints = startingPoints * t
	
	for i=1,variations do
	    local unitSpawner = deepcopy(template)
	    unitSpawner.name = unitSpawner.name .. "-v" .. i .. "-t" .. t
	    local remainingPoints = allottedPoints
	    while (remainingPoints > 0) do
		local upgrade = upgradeTable[mRandom(#upgradeTable)]

		local cost = upgrade.cost
		if upgrade.attribute then
		    local attribute = upgrade.attribute
		    unitSpawner.attributes[attribute.name] = unitSpawner.attributes[attribute.name] + attribute.adjustment
		end
		if upgrade.resistance then
		    local field = upgrade.resistance.name
		    if not unitSpawner.resistances[field] then
			unitSpawner.resistances[field] = {}
			unitSpawner.resistances[field].type = field
		    end
		    if upgrade.resistance.decrease then
			unitSpawner.resistances[field].decrease = (unitSpawner.resistances[field].decrease or 0) + upgrade.resistance.decrease
		    end
		    if upgrade.resistance.percentage then
			unitSpawner.resistances[field].percentage = (unitSpawner.resistances[field].percentage or 0) + upgrade.resistance.percentage
		    end
		end
		
		remainingPoints = remainingPoints - cost
	    end

	    local scale = gaussianRandomRange(unitSpawner.scale, unitSpawner.scale * 0.12, unitSpawner.scale * 0.60, unitSpawner.scale * 1.40) + (0.05 * t)
	    local tint = {
		gaussianRandomRange(unitSpawner.tint1.r, unitSpawner.tint1.r * 0.06 + (0.005 * t), unitSpawner.tint1.r * 0.85 - (0.005 * t), unitSpawner.tint1.r * 1.15),
		gaussianRandomRange(unitSpawner.tint1.g, unitSpawner.tint1.g * 0.06 + (0.005 * t), unitSpawner.tint1.g * 0.85 - (0.005 * t), unitSpawner.tint1.g * 1.15),
		gaussianRandomRange(unitSpawner.tint1.b, unitSpawner.tint1.b * 0.06 + (0.005 * t), unitSpawner.tint1.b * 0.85 - (0.005 * t), unitSpawner.tint1.b * 1.15),
		gaussianRandomRange(unitSpawner.tint1.a, unitSpawner.tint1.a * 0.06 + (0.005 * t), unitSpawner.tint1.a * 0.85 - (0.005 * t), unitSpawner.tint1.a * 1.15)
	    }
	    
	    unitSpawner.attributes.scale = scale
	    unitSpawner.attributes.tint = tint

	    data:extend({
		    makeUnitSpawner(unitSpawner.name,
				    unitSpawner.attributes,
				    unitSpawner.resistances,
				    unitTable)
	    })
	end
    end
    
end

local function buildWorm(startingPoints, template, attackGenerator, upgradeTable, variations, tiers)
    for t=1, tiers do
	local allottedPoints = startingPoints * t
	
	for i=1,variations do
	    local worm = deepcopy(template)
	    worm.name = worm.name .. "-v" .. i .. "-t" .. t
	    local remainingPoints = allottedPoints
	    while (remainingPoints > 0) do
		local upgrade = upgradeTable[mRandom(#upgradeTable)]

		local cost = upgrade.cost
		if upgrade.attribute then
		    local attribute = upgrade.attribute
		    worm.attributes[attribute.name] = worm.attributes[attribute.name] + attribute.adjustment
		end
		if upgrade.resistance then
		    local field = upgrade.resistance.name
		    if not worm.resistances[field] then
			worm.resistances[field] = {}
			worm.resistances[field].type = field
		    end
		    if upgrade.resistance.decrease then
			worm.resistances[field].decrease = (worm.resistances[field].decrease or 0) + upgrade.resistance.decrease
		    end
		    if upgrade.resistance.percentage then
			worm.resistances[field].percentage = (worm.resistances[field].percentage or 0) + upgrade.resistance.percentage
		    end
		end
		
		remainingPoints = remainingPoints - cost
	    end

	    local scale = gaussianRandomRange(worm.scale, worm.scale * 0.12, worm.scale * 0.60, worm.scale * 1.40) + (0.05 * t)
	    local tint1 = {
		gaussianRandomRange(worm.tint1.r, worm.tint1.r * 0.06 + (0.005 * t), worm.tint1.r * 0.85 - (0.005 * t), worm.tint1.r * 1.15),
		gaussianRandomRange(worm.tint1.g, worm.tint1.g * 0.06 + (0.005 * t), worm.tint1.g * 0.85 - (0.005 * t), worm.tint1.g * 1.15),
		gaussianRandomRange(worm.tint1.b, worm.tint1.b * 0.06 + (0.005 * t), worm.tint1.b * 0.85 - (0.005 * t), worm.tint1.b * 1.15),
		gaussianRandomRange(worm.tint1.a, worm.tint1.a * 0.06 + (0.005 * t), worm.tint1.a * 0.85 - (0.005 * t), worm.tint1.a * 1.15)
	    }
	    local tint2 = {
		gaussianRandomRange(worm.tint2.r, worm.tint2.r * 0.06 + (0.005 * t), worm.tint2.r * 0.85 - (0.005 * t), worm.tint2.r * 1.15),
		gaussianRandomRange(worm.tint2.g, worm.tint2.g * 0.06 + (0.005 * t), worm.tint2.g * 0.85 - (0.005 * t), worm.tint2.g * 1.15),
		gaussianRandomRange(worm.tint2.b, worm.tint2.b * 0.06 + (0.005 * t), worm.tint2.b * 0.85 - (0.005 * t), worm.tint2.b * 1.15),
		gaussianRandomRange(worm.tint2.a, worm.tint2.a * 0.06 + (0.005 * t), worm.tint2.a * 0.85 - (0.005 * t), worm.tint2.a * 1.15)
	    }
	    
	    worm.attributes.scale = scale
	    worm.attributes.tint1 = tint1
	    worm.attributes.tint2 = tint2

	    worm.attack.scale = scale
	    worm.attack.tint1 = tint1
	    worm.attack.tint2 = tint2
	    
	    data:extend({
		    makeWorm(worm.name,
			     worm.attributes,
			     attackGenerator(worm.attack),
			     worm.resistances)
	    })
	end
    end
end

local function createUnitClass(points, templates, upgradeTable, attackGenerator, variations, tiers)
    buildUnitSpawner(points.unitSpawner,
		     templates.unitSpawner,
		     upgradeTable.unitSpawner,
		     variations.unitSpawner,
		     tiers.unitSpawner,
		     unitSetToProbabilityTable(points.probabilityTable,
					       upgradeTable.probabilityTable,
					       buildUnits(points.unit,
							  templates.unit,
							  attackGenerator,
							  upgradeTable.unit,
							  variations.unit,
							  tiers.unit)),
		     tiers.unitSpawner)
end

createUnitClass({
	unit = 10,
	unitSpawner = 5,
	probabilityTable = 5
		},
    {
	unit = {
	    name = "rampant-suicide-biter",
	    attributes = {
		health = 30,
		movement = 0.21,
		distancePerFrame = 0.1,
		healing = 0.01,
		explosion = "blood-explosion-small",
	    },
	    attack = {
		area = 3.5,
		damage = 20,
		explosion = "explosion",
		scorchmark = "small-scorchmark",
		explosionCount = 2,
		explosionDistance = 2,
	    },
	    resistances = {
		explosion = {
		    decrease = 0,
		    percentage = -50
		},
		laser = {
		    decrease = 1,
		    percentage = 0
		},
		fire = {
		    decrease = 0,
		    percentage = -60
		}
	    },
	    type = "biter",
	    scale = 0.55,
	    tint1 = {r=0.6, g=0.0, b=0.70, a=0.8},
	    tint2 = {r=0.7, g=0.0, b=0.72, a=0.4}
	},
	unitSpawner = {

	    scale = 0.55,
	    tint1 = {},
	    tint2 = {}
	}
    },
    {
	unit = {
	    {
		cost = 1,
		attribute = {
		    name = "health",
		    adjustment = 50
		}
	    }
	},
	unitSpawner = {
	    {
		cost = 1,
		attribute = {
		    name = "health",
		    adjustment = 50
		}
	    }
	},
	probabilityTable = {
	    {
		cost = 1,
		index = 1,
		adjustment = 1
	    }
	}
    },
    createSuicideAttack,
    {
	unit = 5,
	unitSpawner = 5
    },
    {
	unit = 7,
	unitSpawner = 7
    }
)

local function dog(a)
    a[1] = 2
end

dog()
