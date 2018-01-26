local swarmUtils = {}
-- imports

local biterUtils = require("utils/BiterUtils")
package.path = "../?.lua;" .. package.path
local mathUtils = require("libs/MathUtils")

-- imported functions

local gaussianRandomRangeRG = mathUtils.gaussianRandomRangeRG

local roundToNearest = mathUtils.roundToNearest

local mMax = math.max
local mMin = math.min

local mFloor = math.floor

local deepcopy = util.table.deepcopy

local xorRandom = mathUtils.xorRandom(settings.startup["rampant-enemySeed"].value)

local makeBiterCorpse = biterUtils.makeBiterCorpse
local makeUnitSpawnerCorpse = biterUtils.makeUnitSpawnerCorpse
local makeWormCorpse = biterUtils.makeWormCorpse
local makeSpitterCorpse = biterUtils.makeSpitterCorpse

local makeBiter = biterUtils.makeBiter
local makeSpitter = biterUtils.makeSpitter
local makeWorm = biterUtils.makeWorm
local makeUnitSpawner = biterUtils.makeUnitSpawner

-- module code

local function unitSetToProbabilityTable(upgradeTable, unitSet)
    local dividers = {}
    
    for i=1,#unitSet do
	dividers[i] = 1
    end

    if upgradeTable then
	local points = #upgradeTable * 10
	while (points > 0) do
	    local index = mFloor(xorRandom() * #upgradeTable)+1
	    local upgrade = upgradeTable[index]

	    dividers[index] = dividers[index] + upgrade
	    
	    points = points - 1
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

-- local function processUpgradeTable(upgradeTable)

--     for i=1,#upgradeTable do
-- 	local bonuses = upgradeTable[i]
-- 	for ii=1,#bonuses do
-- 	    local bonus = bonuses[ii]

-- 	    if (bonus.type == "attribute") or (bonus.type == "attack") then
-- 		if not bonus.mapping then
-- 		    for x = 1, #bonus do

-- 			bonus[x] = bonus[x] * 0.10
-- 		    end
-- 		end
-- 	    elseif (bonus.type == "resistance") then
-- 		if bonus.decrease then
-- 		    for x = 1, #bonus.decrease do
-- 			bonus.decrease[x] = bonus.decrease[x] * 0.10
-- 		    end
-- 		end
-- 		if bonus.percent then
-- 		    for x = 1, #bonus.percent do
-- 			bonus.percent[x] = bonus.percent[x] * 0.10
-- 		    end
-- 		end
-- 	    end
-- 	end
--     end

--     return upgradeTable
-- end

local function upgradeEntity(entity, upgradeTable, tier)
    if upgradeTable then
	for upgradeIndex=1, #upgradeTable do
	    local upgrade = upgradeTable[upgradeIndex]
	    
	    if (upgrade.type == "attribute") then
		if upgrade.mapping then
		    entity.attributes[upgrade.mapping] = upgrade[tier]
		else
		    local adj = upgrade[tier]
		    local min = upgrade.min or adj * 0.70
		    local max = upgrade.max or adj * 1.30
		    if (adj < 0) then
			local t = min
			min = max
			max = t
		    end
		    adj = roundToNearest(gaussianRandomRangeRG(adj, adj * 0.2, min, max, xorRandom), 0.001)
		    entity.attributes[upgrade.name] = (entity.attributes[upgrade.name] or 0) + adj
		end
	    end
	    if (upgrade.type == "resistance") then
		local field = upgrade.name
		if not entity.resistances[field] then
		    entity.resistances[field] = {}
		end
		local adj
		if upgrade.decrease then
		    adj = upgrade.decrease[tier]
		    local min = upgrade.min or adj * 0.70
		    local max = upgrade.max or adj * 1.30
		    if (adj < 0) then
			local t = min
			min = max
			max = t
		    end
		    adj = roundToNearest(gaussianRandomRangeRG(adj, adj * 0.2, min, max, xorRandom), 0.001)
		    entity.resistances[field].decrease = (entity.resistances[field].decrease or 0) + adj
		end
		if upgrade.percent then
		    adj = upgrade.percent[tier]
		    local min = upgrade.min or adj * 0.70
		    local max = upgrade.max or adj * 1.30
		    if (adj < 0) then
			local t = min
			min = max
			max = t
		    end
		    adj = roundToNearest(gaussianRandomRangeRG(adj, adj * 0.2, min, max, xorRandom), 0.001)
		    entity.resistances[field].percent = mMin((entity.resistances[field].percent or 0) + adj, 100)
		end
	    end
	    if (upgrade.type == "attack") then
		if upgrade.mapping then
		    entity.attack[upgrade.mapping] = upgrade[tier]
		else
		    local adj = upgrade[tier]
		    local min = upgrade.min or adj * 0.70
		    local max = upgrade.max or adj * 1.30
		    if (adj < 0) then
			local t = min
			min = max
			max = t
		    end
		    adj = roundToNearest(gaussianRandomRangeRG(adj, adj * 0.2, min, max, xorRandom), 0.001)
		    entity.attack[upgrade.name] = (entity.attack[upgrade.name] or 0) + adj
		end
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
    local scaleValue = unit.scales[tier]
    local scale = gaussianRandomRangeRG(scaleValue, scaleValue * 0.12, scaleValue * 0.60, scaleValue * 1.40, xorRandom)

    unit.scale = scale
    unit.attributes.scale = scale
    if unit.tint then
	local tint = calculateRGBa(unit.tint, tier)
	
	unit.attributes.tint = tint

	if unit.attack then
	    unit.attack.scale = scale
	    unit.attack.tint = tint
	end
    end
    if unit.tint1 and unit.tint2 then
	local tint1 = calculateRGBa(unit.tint1, tier)
	local tint2 = calculateRGBa(unit.tint2, tier)
	
	unit.attributes.tint1 = tint1
	unit.attributes.tint2 = tint2

	if unit.attack then
	    unit.attack.scale = scale
	    unit.attack.tint1 = tint1
	    unit.attack.tint2 = tint2
	end
    end
    if unit.attack then
	if unit.pTint then
	    unit.attack.pTint = calculateRGBa(unit.pTint, tier)
	end
	if unit.sTint then
	    unit.attack.sTint = calculateRGBa(unit.sTint, tier)
	end
	if unit.smTint then
	    unit.attack.smTint = calculateRGBa(unit.smTint, tier)
	end
    end
end

local function buildUnits(template, attackGenerator, upgradeTable, variations, tiers)
    local unitSet = {}
    
    for t=1, tiers do
	local result = {}
	
	for i=1,variations do
	    local unit = deepcopy(template)
	    unit.name = unit.name .. "-v" .. i .. "-t" .. t
	    generateApperance(unit, t)
	    upgradeEntity(unit, upgradeTable,  t)
	    
	    if unit.attackName then
		unit.attack.name = unit.attackName .. "-v" .. i .. "-t" .. t
	    end
	    
	    local entity
	    if (unit.type == "spitter") then
		unit.attributes.corpse = makeSpitterCorpse(unit)
		entity = makeSpitter(unit.name,
				     unit.attributes,
				     attackGenerator(unit.attack),
				     unit.resistances)
	    elseif (unit.type == "biter") then
		unit.attributes.corpse = makeBiterCorpse(unit)
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

function swarmUtils.buildUnitSpawner(templates, upgradeTable, attackGenerator, variations, tiers)
    
    local unitSet = buildUnits(templates.unit,
			       attackGenerator,
			       upgradeTable.unit,
			       variations.unit,
			       tiers.unit)
    
    for t=1, tiers.unitSpawner do	
	for i=1,variations.unitSpawner do
	    local unitSpawner = deepcopy(templates.unitSpawner)
	    unitSpawner.name = unitSpawner.name .. "-v" .. i .. "-t" .. t
	    local unitTable = unitSetToProbabilityTable(upgradeTable.probabilityTable,
							unitSet)
	    generateApperance(unitSpawner, t)
	    upgradeEntity(unitSpawner, upgradeTable.unitSpawner, t)

	    if unitSpawner.autoplace then
		unitSpawner.attributes["autoplace"] = unitSpawner.autoplace[t]
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
    for t=1, tiers do
	for i=1,variations do
	    local worm = deepcopy(template)
	    worm.name = worm.name .. "-v" .. i .. "-t" .. t
	    generateApperance(worm, t)
	    upgradeEntity(worm, upgradeTable, t)

	    if worm.attackName then
		worm.attack.name = worm.attackName .. "-v" .. i .. "-t" .. t
	    end
	    
	    if worm.autoplace then
		worm.attributes["autoplace"] = worm.autoplace[t]
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
