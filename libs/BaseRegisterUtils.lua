local baseRegisterUtils = {}

-- imports

local constants = require("Constants")

local entityUtils = require("EntityUtils")

-- constants

local NEST_COUNT = constants.NEST_COUNT
local WORM_COUNT = constants.WORM_COUNT

local NEST_BASE = constants.NEST_BASE
local WORM_BASE = constants.WORM_BASE

-- imported functions

local getEntityOverlapChunks = entityUtils.getEntityOverlapChunks

-- module code

function baseRegisterUtils.addEnemyStructureToChunk(chunk, entity, base)
    local indexChunk
    local indexBase
    local countChunk
    if (entity.type == "unit-spawner") then
	indexChunk = chunk[NEST_BASE]
	if base then
	    if base.hives[entity.unit_number] then
		indexBase = base.hives
	    else
		indexBase = base.nests
	    end
	end
	countChunk = NEST_COUNT
    elseif (entity.type == "turret") then
	indexChunk = chunk[WORM_BASE]
	if base then
	    indexBase = base.worms
	end
	countChunk = WORM_COUNT
    end
    chunk[countChunk] = chunk[countChunk] + 1
    if indexBase then
	indexChunk[entity.unit_number] = base
	indexBase[entity.unit_number] = entity
    end
end

function baseRegisterUtils.removeEnemyStructureFromChunk(chunk, entity)
    local indexChunk
    local countChunk
    if (entity.type == "unit-spawner") then
	indexChunk = chunk[NEST_BASE]
	countChunk = NEST_COUNT
    elseif (entity.type == "turret") then
	indexChunk = chunk[WORM_BASE]
	countChunk = WORM_COUNT
    end
    local base = indexChunk[entity.unit_number]
    local indexBase
    if base then
	if (entity.type == "unit-spawner") then
	    if base.hives[entity.unit_number] then
		indexBase = base.hives
	    else
		indexBase = base.nests
	    end
	elseif (entity.type == "turret") then
	    indexBase = base.worms
	end
	indexBase[entity.unit_number] = nil
    end
    chunk[countChunk] = chunk[countChunk] - 1
end

function baseRegisterUtils.registerEnemyBaseStructure(regionMap, entity, base)
    local entityType = entity.type
    if ((entityType == "unit-spawner") or (entityType == "turret")) and (entity.force.name == "enemy") then
	local leftTop, rightTop, leftBottom, rightBottom = getEntityOverlapChunks(regionMap, entity)
	
	if leftTop then
	    baseRegisterUtils.addEnemyStructureToChunk(leftTop, entity, base)
	end
	if rightTop then
	    baseRegisterUtils.addEnemyStructureToChunk(rightTop, entity, base)
	end
	if leftBottom then
	    baseRegisterUtils.addEnemyStructureToChunk(leftBottom, entity, base)
	end
	if rightBottom then
	    baseRegisterUtils.addEnemyStructureToChunk(rightBottom, entity, base)
	end	
    end
end

function baseRegisterUtils.unregisterEnemyBaseStructure(regionMap, entity)
    local entityType = entity.type
    if ((entityType == "unit-spawner") or (entityType == "turret")) and (entity.force.name == "enemy") then
	local leftTop, rightTop, leftBottom, rightBottom = getEntityOverlapChunks(regionMap, entity)
	
	if leftTop then
	    baseRegisterUtils.removeEnemyStructureFromChunk(leftTop, entity)
	end
	if rightTop then
	    baseRegisterUtils.removeEnemyStructureFromChunk(rightTop, entity)
	end
	if leftBottom then
	    baseRegisterUtils.removeEnemyStructureFromChunk(leftBottom, entity)
	end
	if rightBottom then
	    baseRegisterUtils.removeEnemyStructureFromChunk(rightBottom, entity)
	end
    end
end

return baseRegisterUtils
