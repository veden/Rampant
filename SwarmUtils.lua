local swarmUtils = {}
-- imports

local biterUtils = require("prototypes/enemies/BiterUtils")
local mathUtils = require("libs/MathUtils")

-- imported functions

local gaussianRandomRangeRG = mathUtils.gaussianRandomRangeRG

local mMax = math.max
local mMin = math.min

local mFloor = math.floor

local deepcopy = util.table.deepcopy

local xorRandom = mathUtils.xorRandom(settings.startup["rampant-enemySeed"].value)

local makeBiter = biterUtils.makeBiter
local makeSpitter = biterUtils.makeSpitter
local makeWorm = biterUtils.makeWorm
local makeUnitSpawner = biterUtils.makeUnitSpawner

-- module code

local function unitSetToProbabilityTable(points, upgradeTable, unitSet)
    local dividers = {}
    
    for i=1,#unitSet do
	dividers[i] = 1
    end

    if upgradeTable then
	while (points > 0) do
	    local upgrade = upgradeTable[mFloor(xorRandom() * #upgradeTable)+1]

	    local cost = upgrade.cost
	    local index = upgrade.index

	    dividers[index] = dividers[index] + upgrade.adjustment
	    
	    points = points - cost
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

local function upgradeEntity(points, entity, upgradeTable, tierMultipler)
    local remainingPoints = points
    if upgradeTable then
	while (remainingPoints > 0) do
	    local upgrade = upgradeTable[mFloor(xorRandom() * #upgradeTable)+1]

	    local cost = upgrade.cost
	    for i=1, #upgrade.bonus do
		local bonus = upgrade.bonus[i]
		
		if (bonus.type == "attribute") then
		    if bonus.mapping then
			entity.attributes[bonus.name] = bonus.mapping[entity.attributes[bonus.name] or "default"]
		    else
			local adj = bonus.adjustment * tierMultipler
			if adj < 0 then
			    adj = -adj
			    adj = -gaussianRandomRangeRG(adj, adj * 0.2, adj * 0.75, adj * 1.25, xorRandom)
			else
			    adj = gaussianRandomRangeRG(adj, adj * 0.2, adj * 0.75, adj * 1.25, xorRandom)
			end
			entity.attributes[bonus.name] = (entity.attributes[bonus.name] or 0) + adj
		    end
		end
		if (bonus.type == "resistance") then
		    local field = bonus.name
		    if not entity.resistances[field] then
			entity.resistances[field] = {}
		    end
		    if bonus.decrease then
			entity.resistances[field].decrease = (entity.resistances[field].decrease or 0) + bonus.decrease
		    end
		    if bonus.percent then
			entity.resistances[field].percent = mMin((entity.resistances[field].percent or 0) + bonus.percent, 100)
		    end
		end
		if (bonus.type == "attack") then
		    if bonus.mapping then
			entity.attack[bonus.name] = bonus.mapping[entity.attack[bonus.name] or "default"]
		    else
			local adj = bonus.adjustment * tierMultipler
			if adj < 0 then
			    adj = -adj
			    adj = -gaussianRandomRangeRG(adj, adj * 0.2, adj * 0.75, adj * 1.25, xorRandom)
			else
			    adj = gaussianRandomRangeRG(adj, adj * 0.2, adj * 0.75, adj * 1.25, xorRandom)
			end
			entity.attack[bonus.name] = (entity.attack[bonus.name] or 0) + adj
		    end
		end
	    end
	    
	    remainingPoints = remainingPoints - cost
	end
    end
end

local function generateApperance(unit, tier)
    local scale = gaussianRandomRangeRG(unit.scale, unit.scale * 0.12, unit.scale * 0.60, unit.scale * 1.40, xorRandom) + (0.05 * tier)

    local r,g,b,a
    
    if unit.tint then
	r = gaussianRandomRangeRG(unit.tint.r, unit.tint.r * 0.10 + (0.005 * tier), mMax(unit.tint.r * 0.85 - (0.005 * tier), 0), mMin(unit.tint.r * 1.15, 1), xorRandom)
	g = gaussianRandomRangeRG(unit.tint.g, unit.tint.g * 0.10 + (0.005 * tier), mMax(unit.tint.g * 0.85 - (0.005 * tier), 0), mMin(unit.tint.g * 1.15, 1), xorRandom)
	b = gaussianRandomRangeRG(unit.tint.b, unit.tint.b * 0.10 + (0.005 * tier), mMax(unit.tint.b * 0.85 - (0.005 * tier), 0), mMin(unit.tint.b * 1.15, 1), xorRandom)
	a = gaussianRandomRangeRG(unit.tint.a, unit.tint.a * 0.10 + (0.005 * tier), mMax(unit.tint.a * 0.85 - (0.005 * tier), 0), mMin(unit.tint.a * 1.15, 1), xorRandom)
	    
	local tint = { r=r, g=g, b=b, a=a }
	
	unit.attributes.scale = scale
	unit.attributes.tint = tint
    else	
	r = gaussianRandomRangeRG(unit.tint1.r, unit.tint1.r * 0.10 + (0.005 * tier), mMax(unit.tint1.r * 0.85 - (0.005 * tier), 0), mMin(unit.tint1.r * 1.15, 1), xorRandom)
	g = gaussianRandomRangeRG(unit.tint1.g, unit.tint1.g * 0.10 + (0.005 * tier), mMax(unit.tint1.g * 0.85 - (0.005 * tier), 0), mMin(unit.tint1.g * 1.15, 1), xorRandom)
	b = gaussianRandomRangeRG(unit.tint1.b, unit.tint1.b * 0.10 + (0.005 * tier), mMax(unit.tint1.b * 0.85 - (0.005 * tier), 0), mMin(unit.tint1.b * 1.15, 1), xorRandom)
	a = gaussianRandomRangeRG(unit.tint1.a, unit.tint1.a * 0.10 + (0.005 * tier), mMax(unit.tint1.a * 0.85 - (0.005 * tier), 0), mMin(unit.tint1.a * 1.15, 1), xorRandom)
	
	local tint1 = { r=r, g=g, b=b, a=a }

	r = gaussianRandomRangeRG(unit.tint2.r, unit.tint2.r * 0.10 + (0.005 * tier), mMax(unit.tint2.r * 0.85 - (0.005 * tier), 0), mMin(unit.tint2.r * 1.15, 1), xorRandom)
	g = gaussianRandomRangeRG(unit.tint2.g, unit.tint2.g * 0.10 + (0.005 * tier), mMax(unit.tint2.g * 0.85 - (0.005 * tier), 0), mMin(unit.tint2.g * 1.15, 1), xorRandom)
	b = gaussianRandomRangeRG(unit.tint2.b, unit.tint2.b * 0.10 + (0.005 * tier), mMax(unit.tint2.b * 0.85 - (0.005 * tier), 0), mMin(unit.tint2.b * 1.15, 1), xorRandom)
	a = gaussianRandomRangeRG(unit.tint2.a, unit.tint2.a * 0.10 + (0.005 * tier), mMax(unit.tint2.a * 0.85 - (0.005 * tier), 0), mMin(unit.tint2.a * 1.15, 1), xorRandom)
	
	local tint2 = { r=r, g=g, b=b, a=a }

	unit.attributes.scale = scale
	unit.attributes.tint1 = tint1
	unit.attributes.tint2 = tint2
	
	unit.attack.scale = scale
	unit.attack.tint1 = tint1
	unit.attack.tint2 = tint2
    end
end

local function buildUnits(startingPoints, template, attackGenerator, upgradeTable, variations, tiers)
    local unitSet = {}
    
    for t=1, tiers do
	local result = {}
	
	for i=1,variations do
	    local unit = deepcopy(template)
	    unit.name = unit.name .. "-v" .. i .. "-t" .. t .. "-rampant"
	    generateApperance(unit, t)
	    upgradeEntity(startingPoints, unit, upgradeTable, tiers)
	    
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

function swarmUtils.buildUnitSpawner(points, templates, upgradeTable, attackGenerator, variations, tiers)
    local unitSet = buildUnits(points.unit,
			       templates.unit,
			       attackGenerator,
			       upgradeTable.unit,
			       variations.unit,
			       tiers.unit)
    
    for t=1, tiers.unitSpawner do
	local allottedPoints = points.unitSpawner * t
	
	for i=1,variations.unitSpawner do
	    local unitSpawner = deepcopy(templates.unitSpawner)
	    unitSpawner.name = unitSpawner.name .. "-v" .. i .. "-t" .. t .. "-rampant"
	    local unitTable = unitSetToProbabilityTable(points.probabilityTable,
							upgradeTable.probabilityTable,
							unitSet)
	    generateApperance(unitSpawner, t)
	    upgradeEntity(allottedPoints, unitSpawner, upgradeTable.unitSpawner, t / tiers)

	    data:extend({
		    makeUnitSpawner(unitSpawner.name,
				    unitSpawner.attributes,
				    unitSpawner.resistances,
				    unitTable)
	    })
	end
    end
    
end

function swarmUtils.buildWorm(startingPoints, template, attackGenerator, upgradeTable, variations, tiers)
    for t=1, tiers do
	local allottedPoints = startingPoints * t
	
	for i=1,variations do
	    local worm = deepcopy(template)
	    worm.name = worm.name .. "-v" .. i .. "-t" .. t .. "-rampant"
	    generateApperance(worm, t)
	    upgradeEntity(allottedPoints, worm, upgradeTable, t / tiers)
	    	    
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
