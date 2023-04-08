-- Copyright (C) 2022  veden

-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <https://www.gnu.org/licenses/>.


if ChunkUtilsG then
    return ChunkUtilsG
end
local ChunkUtils = {}

--

local Universe
local ChunkOverlapArray

-- imports

local BaseUtils = require("BaseUtils")
local Constants = require("Constants")
local MapUtils = require("MapUtils")
local ChunkPropertyUtils = require("ChunkPropertyUtils")
local MathUtils = require("MathUtils")
local Utils = require("Utils")

-- Constants

local BUILDING_HIVE_TIER_LOOKUP = Constants.BUILDING_HIVE_TIER_LOOKUP

local VANILLA_ENTITY_TYPE_LOOKUP = Constants.VANILLA_ENTITY_TYPE_LOOKUP
local BUILDING_HIVE_TYPE_LOOKUP = Constants.BUILDING_HIVE_TYPE_LOOKUP

local DEFINES_WIRE_TYPE_RED = defines.wire_type.red
local DEFINES_WIRE_TYPE_GREEN = defines.wire_type.green

local BASE_AI_STATE_ONSLAUGHT = Constants.BASE_AI_STATE_ONSLAUGHT

local BASE_PHEROMONE = Constants.BASE_PHEROMONE
local PLAYER_PHEROMONE = Constants.PLAYER_PHEROMONE
local RESOURCE_PHEROMONE = Constants.RESOURCE_PHEROMONE
local ENEMY_PHEROMONE = Constants.ENEMY_PHEROMONE
local KAMIKAZE_PHEROMONE = Constants.KAMIKAZE_PHEROMONE
local BUILDING_PHEROMONES = Constants.BUILDING_PHEROMONES

local CHUNK_SIZE = Constants.CHUNK_SIZE
local CHUNK_SIZE_DIVIDER = Constants.CHUNK_SIZE_DIVIDER

local CHUNK_NORTH_SOUTH = Constants.CHUNK_NORTH_SOUTH
local CHUNK_EAST_WEST = Constants.CHUNK_EAST_WEST

local CHUNK_ALL_DIRECTIONS = Constants.CHUNK_ALL_DIRECTIONS
local CHUNK_IMPASSABLE = Constants.CHUNK_IMPASSABLE

local RESOURCE_NORMALIZER = Constants.RESOURCE_NORMALIZER

local CHUNK_TICK = Constants.CHUNK_TICK

local DEV_HIVE_TTL = Constants.DEV_HIVE_TTL
local MAX_HIVE_TTL = Constants.MAX_HIVE_TTL
local MIN_HIVE_TTL = Constants.MIN_HIVE_TTL

local HIVE_MAX_NESTS = Constants.HIVE_MAX_NESTS
local HIVE_MAX_TURRETS = Constants.HIVE_MAX_TURRETS
local HIVE_MAX_HIVES = Constants.HIVE_MAX_HIVES

local GENERATOR_PHEROMONE_LEVEL_1 = Constants.GENERATOR_PHEROMONE_LEVEL_1
local GENERATOR_PHEROMONE_LEVEL_3 = Constants.GENERATOR_PHEROMONE_LEVEL_3
local GENERATOR_PHEROMONE_LEVEL_5 = Constants.GENERATOR_PHEROMONE_LEVEL_5
local GENERATOR_PHEROMONE_LEVEL_6 = Constants.GENERATOR_PHEROMONE_LEVEL_6

-- imported functions

local removeBaseResourceChunk = ChunkPropertyUtils.removeBaseResourceChunk
local addBaseResourceChunk = ChunkPropertyUtils.addBaseResourceChunk

local setAreaInQueryChunkSize = Utils.setAreaInQueryChunkSize
local setAreaXInQuery = Utils.setAreaXInQuery
local setAreaYInQuery = Utils.setAreaYInQuery

local setPlayerBaseGenerator = ChunkPropertyUtils.setPlayerBaseGenerator
local addPlayerBaseGenerator = ChunkPropertyUtils.addPlayerBaseGenerator
local setResourceGenerator = ChunkPropertyUtils.setResourceGenerator
local addResourceGenerator = ChunkPropertyUtils.addResourceGenerator

local addNestCount = ChunkPropertyUtils.addNestCount
local removeNestCount = ChunkPropertyUtils.removeNestCount
local addHiveCount = ChunkPropertyUtils.addHiveCount
local removeHiveCount = ChunkPropertyUtils.removeHiveCount
local addTurretCount = ChunkPropertyUtils.addTurretCount
local removeTurretCount = ChunkPropertyUtils.removeTurretCount
local addUtilityCount = ChunkPropertyUtils.addUtilityCount
local removeUtilityCount = ChunkPropertyUtils.removeUtilityCount

local setChunkBase = ChunkPropertyUtils.setChunkBase

local processNestActiveness = ChunkPropertyUtils.processNestActiveness

local removeChunkBase = ChunkPropertyUtils.removeChunkBase
local getEnemyStructureCount = ChunkPropertyUtils.getEnemyStructureCount

local findNearbyBase = ChunkPropertyUtils.findNearbyBase

local queueUpgrade = BaseUtils.queueUpgrade
local createBase = BaseUtils.createBase
local modifyBaseUnitPoints = BaseUtils.modifyBaseUnitPoints

local euclideanDistancePoints = MathUtils.euclideanDistancePoints
local gaussianRandomRangeRG = MathUtils.gaussianRandomRangeRG
local linearInterpolation = MathUtils.linearInterpolation

local setPassable = ChunkPropertyUtils.setPassable
local setPathRating = ChunkPropertyUtils.setPathRating

local getChunkByXY = MapUtils.getChunkByXY

local mMin = math.min
local mMax = math.max
local mFloor = math.floor

-- module code

local function getEntityOverlapChunks(map, entity)
    local boundingBox = entity.prototype.collision_box or entity.prototype.selection_box

    ChunkOverlapArray[1] = -1 --LeftTop
    ChunkOverlapArray[2] = -1 --RightTop
    ChunkOverlapArray[3] = -1 --LeftBottom
    ChunkOverlapArray[4] = -1 --RightBottom

    if boundingBox then
        local center = entity.position

        local topXOffset = boundingBox.left_top.x
        local topYOffset = boundingBox.left_top.y

        local bottomXOffset = boundingBox.right_bottom.x
        local bottomYOffset = boundingBox.right_bottom.y

        local leftTopChunkX = mFloor((center.x + topXOffset) * CHUNK_SIZE_DIVIDER) * CHUNK_SIZE
        local leftTopChunkY = mFloor((center.y + topYOffset) * CHUNK_SIZE_DIVIDER) * CHUNK_SIZE

        local rightTopChunkX = mFloor((center.x + bottomXOffset) * CHUNK_SIZE_DIVIDER) * CHUNK_SIZE
        local leftBottomChunkY = mFloor((center.y + bottomYOffset) * CHUNK_SIZE_DIVIDER) * CHUNK_SIZE

        ChunkOverlapArray[1] = getChunkByXY(map, leftTopChunkX, leftTopChunkY) -- LeftTop
        if (leftTopChunkX ~= rightTopChunkX) then
            ChunkOverlapArray[2] = getChunkByXY(map, rightTopChunkX, leftTopChunkY) -- RightTop
        end
        if (leftTopChunkY ~= leftBottomChunkY) then
            ChunkOverlapArray[3] = getChunkByXY(map, leftTopChunkX, leftBottomChunkY) -- LeftBottom
        end
        if (leftTopChunkX ~= rightTopChunkX) and (leftTopChunkY ~= leftBottomChunkY) then
            ChunkOverlapArray[4] = getChunkByXY(map, rightTopChunkX, leftBottomChunkY) -- RightBottom
        end
    end
    return ChunkOverlapArray
end

local function scanPaths(chunk, map)
    local surface = map.surface
    local pass = CHUNK_IMPASSABLE

    local x = chunk.x
    local y = chunk.y

    local filteredEntitiesCliffQuery = Universe.spFilteredEntitiesCliffQuery
    local filteredTilesPathQuery = Universe.spFilteredTilesPathQuery
    local count_entities_filtered = surface.count_entities_filtered
    local count_tiles_filtered = surface.count_tiles_filtered

    local passableNorthSouth = false
    local passableEastWest = false

    setAreaYInQuery(filteredEntitiesCliffQuery, y, y + CHUNK_SIZE)

    for xi=x, x + CHUNK_SIZE do
        setAreaXInQuery(filteredEntitiesCliffQuery, xi, xi + 1)
        if (count_entities_filtered(filteredEntitiesCliffQuery) == 0) and
            (count_tiles_filtered(filteredTilesPathQuery) == 0)
        then
            passableNorthSouth = true
            break
        end
    end

    setAreaXInQuery(filteredEntitiesCliffQuery, x, x + CHUNK_SIZE)

    for yi=y, y + CHUNK_SIZE do
        setAreaYInQuery(filteredEntitiesCliffQuery, yi, yi + 1)
        if (count_entities_filtered(filteredEntitiesCliffQuery) == 0) and
            (count_tiles_filtered(filteredTilesPathQuery) == 0)
        then
            passableEastWest = true
            break
        end
    end

    if passableEastWest and passableNorthSouth then
        pass = CHUNK_ALL_DIRECTIONS
    elseif passableEastWest then
        pass = CHUNK_EAST_WEST
    elseif passableNorthSouth then
        pass = CHUNK_NORTH_SOUTH
    end
    return pass
end

local function scorePlayerBuildings(map, chunk)
    local surface = map.surface
    setAreaInQueryChunkSize(Universe.spbHasPlayerStructuresQuery, chunk)
    if surface.count_entities_filtered(Universe.spbHasPlayerStructuresQuery) > 0 then
        return (surface.count_entities_filtered(Universe.spbFilteredEntitiesPlayerQueryLowest) * GENERATOR_PHEROMONE_LEVEL_1) +
            (surface.count_entities_filtered(Universe.spbFilteredEntitiesPlayerQueryLow) * GENERATOR_PHEROMONE_LEVEL_3) +
            (surface.count_entities_filtered(Universe.spbFilteredEntitiesPlayerQueryHigh) * GENERATOR_PHEROMONE_LEVEL_5) +
            (surface.count_entities_filtered(Universe.spbFilteredEntitiesPlayerQueryHighest) * GENERATOR_PHEROMONE_LEVEL_6)
    end
    return 0
end

local registerTypeToAddFn = {
    ["spitter-spawner"] = addNestCount,
    ["biter-spawner"] = addNestCount,
    ["turret"] = addTurretCount,
    ["utility"] = addUtilityCount,
    ["hive"] = addHiveCount,
    ["unit-spawner"] = addNestCount
}

local unregisterTypeToRemoveFn = {
    ["spitter-spawner"] = removeNestCount,
    ["biter-spawner"] = removeNestCount,
    ["turret"] = removeTurretCount,
    ["utility"] = removeUtilityCount,
    ["hive"] = removeHiveCount,
    ["unit-spawner"] = removeNestCount
}

function ChunkUtils.initialScan(chunk, map, tick)
    local surface = map.surface
    setAreaInQueryChunkSize(Universe.isFilteredTilesQuery, chunk)
    local pass = scanPaths(chunk, map)
    local enemyBuildings = surface.find_entities_filtered(Universe.isFilteredEntitiesEnemyStructureQuery)
    local playerObjects = scorePlayerBuildings(map, chunk)

    if (pass ~= CHUNK_IMPASSABLE) or (#enemyBuildings > 0) or (playerObjects > 0) then
        if ((playerObjects > 0) or (#enemyBuildings > 0)) and (pass == CHUNK_IMPASSABLE) then
            pass = CHUNK_ALL_DIRECTIONS
        end

        if (pass ~= CHUNK_IMPASSABLE) then
            local neutralObjects = mMax(0,
                                        mMin(1 - (surface.count_entities_filtered(Universe.isFilteredEntitiesChunkNeutral) * 0.005),
                                             1) * 0.20)

            setPassable(chunk, pass)

            local waterTiles = (1 - (surface.count_tiles_filtered(Universe.isFilteredTilesQuery) * 0.0009765625)) * 0.80
            setPathRating(chunk, waterTiles + neutralObjects)
            setPlayerBaseGenerator(chunk, playerObjects)

            local resources = surface.count_entities_filtered(Universe.isCountResourcesQuery) * RESOURCE_NORMALIZER
            setResourceGenerator(chunk, resources)

            if (#enemyBuildings > 0) then
                local base = findNearbyBase(chunk)
                if not base then
                    base = createBase(map, chunk, tick)
                end

                if Universe.NEW_ENEMIES then
                    local unitList = surface.find_entities_filtered(Universe.isFilteredEntitiesUnitQuery)
                    for i=1,#unitList do
                        local unit = unitList[i]
                        if (unit.valid) then
                            unit.destroy()
                        end
                    end

                    for i = 1, #enemyBuildings do
                        local enemyBuilding = enemyBuildings[i]
                        ChunkUtils.registerEnemyBaseStructure(enemyBuilding, base, tick)
                        local entityName = enemyBuilding.name
                        local isVanilla = VANILLA_ENTITY_TYPE_LOOKUP[entityName]
                        if isVanilla or (not isVanilla and not BUILDING_HIVE_TYPE_LOOKUP[entityName]) then
                            queueUpgrade(enemyBuilding,
                                         base,
                                         nil,
                                         true)
                        end
                    end
                else
                    for i=1,#enemyBuildings do
                        local building = enemyBuildings[i]
                        ChunkUtils.registerEnemyBaseStructure(building, base, tick)
                    end
                end
            end

            return chunk
        end
    end

    return -1
end

function ChunkUtils.chunkPassScan(chunk, map)
    local surface = map.surface
    setAreaInQueryChunkSize(Universe.cpsFilteredTilesQuery, chunk)
    local pass = scanPaths(chunk, map)
    local enemyCount = surface.count_entities_filtered(Universe.cpsFilteredEnemyAnyFound)
    local playerObjects = (chunk.playerBaseGenerator or 0)

    if (pass ~= CHUNK_IMPASSABLE) or (enemyCount > 0) or playerObjects then
        local neutralObjects = mMax(0,
                                    mMin(1 - (surface.count_entities_filtered(Universe.cpsFilteredEntitiesChunkNeutral) * 0.005),
                                         1) * 0.20)
        local waterTiles = (1 - (surface.count_tiles_filtered(Universe.cpsFilteredTilesQuery) * 0.0009765625)) * 0.80

        if ((playerObjects > 0) or (enemyCount > 0)) and (pass == CHUNK_IMPASSABLE) then
            pass = CHUNK_ALL_DIRECTIONS
        end

        setPassable(chunk, pass)
        setPathRating(chunk, waterTiles + neutralObjects)

        return chunk
    end

    return -1
end

function ChunkUtils.mapScanPlayerChunk(chunk, map)
    local playerObjects = scorePlayerBuildings(map, chunk)
    setPlayerBaseGenerator(chunk, playerObjects)
end

function ChunkUtils.mapScanResourceChunk(chunk, map)
    setAreaInQueryChunkSize(Universe.msrcCountResourcesQuery, chunk)
    local surface = map.surface
    local resources = surface.count_entities_filtered(Universe.msrcCountResourcesQuery) * RESOURCE_NORMALIZER
    setResourceGenerator(chunk, resources)
    local waterTiles = (1 - (surface.count_tiles_filtered(Universe.msrcFilteredTilesQuery) * 0.0009765625)) * 0.80
    local neutralObjects = mMax(0,
                                mMin(1 - (surface.count_entities_filtered(Universe.msrcFilteredEntitiesChunkNeutral) * 0.005),
                                     1) * 0.20)
    setPathRating(chunk, waterTiles + neutralObjects)
end

function ChunkUtils.mapScanEnemyChunk(chunk, map, tick)
    setAreaInQueryChunkSize(Universe.msecFilteredEntitiesEnemyStructureQuery, chunk)
    local buildings = map.surface.find_entities_filtered(Universe.msecFilteredEntitiesEnemyStructureQuery)
    if (#buildings > 0) then
        local base = findNearbyBase(chunk)
        if not base then
            base = createBase(map, chunk, tick)
        end
        for i=1,#buildings do
            local building = buildings[i]

            ChunkUtils.registerEnemyBaseStructure(building, base, tick)
        end
    end
end

function ChunkUtils.entityForPassScan(map, entity)
    local overlapArray = getEntityOverlapChunks(map, entity)

    for i=1,#overlapArray do
        local chunk = overlapArray[i]
        if (chunk ~= -1) then
            Universe.chunkToPassScan[chunk.id] = chunk
        end
    end
end

local function newChunkId()
    local id = Universe.chunkId
    Universe.chunkId = Universe.chunkId + 1
    return id
end

function ChunkUtils.createChunk(map, topX, topY)
    local chunk = {
        x = topX,
        y = topY,
        dOrigin = euclideanDistancePoints(topX, topY, 0, 0),
        id = newChunkId(),
        map = map
    }
    chunk[CHUNK_TICK] = 0
    chunk[BASE_PHEROMONE] = 0
    chunk[PLAYER_PHEROMONE] = 0
    chunk[RESOURCE_PHEROMONE] = 0
    chunk[ENEMY_PHEROMONE] = 0
    chunk[KAMIKAZE_PHEROMONE] = 0

    return chunk
end

function ChunkUtils.colorChunk(chunk, surface, color, ttl)
    local lx = math.floor(chunk.x * CHUNK_SIZE_DIVIDER) * CHUNK_SIZE
    local ly = math.floor(chunk.y * CHUNK_SIZE_DIVIDER) * CHUNK_SIZE

    rendering.draw_rectangle({
            color = color or {0.1, 0.3, 0.1, 0.6},
            width = 32 * 32,
            filled = true,
            left_top = {lx, ly},
            right_bottom = {lx+32, ly+32},
            surface = surface,
            time_to_live = ttl or 180,
            draw_on_ground = true,
            visible = true
    })
end

function ChunkUtils.colorXY(x, y, surface, color)
    local lx = math.floor(x * CHUNK_SIZE_DIVIDER) * CHUNK_SIZE
    local ly = math.floor(y * CHUNK_SIZE_DIVIDER) * CHUNK_SIZE

    rendering.draw_rectangle({
            color = color or {0.1, 0.3, 0.1, 0.6},
            width = 32 * 32,
            filled = true,
            left_top = {lx, ly},
            right_bottom = {lx+32, ly+32},
            surface = surface,
            time_to_live = 180,
            draw_on_ground = true,
            visible = true
    })
end

local function registerHive(base, entity, hiveType, tick)
    if hiveType == "hive" then
        local entityUnitNumber = entity.unit_number
        local hiveData = Universe.hives[entityUnitNumber]
        local tier = BUILDING_HIVE_TIER_LOOKUP[entity.name]
        local maxNests = HIVE_MAX_NESTS[tier]
        local maxTurrets = HIVE_MAX_TURRETS[tier]
        local maxHives = 0
        local rollHives = Universe.random()
        if rollHives < 0.05 then
            maxHives = HIVE_MAX_HIVES[tier]
        end

        if not hiveData then
            hiveData = {
                hiveId = entityUnitNumber,
                e = entity,
                position = entity.position,
                base = base,
                hive = 0,
                nest = 0,
                turret = 0,
                maxNests = ((maxNests > 0) and (Universe.random(maxNests) - 1)) or 0,
                maxTurrets = ((maxTurrets > 0) and (Universe.random(maxTurrets) - 1)) or 0,
                maxHives = ((maxHives > 0) and (Universe.random(maxHives) - 1)) or 0,
                tier = tier,
                tick = tick + gaussianRandomRangeRG(
                    linearInterpolation(Universe.evolutionLevel, MAX_HIVE_TTL, MIN_HIVE_TTL),
                    DEV_HIVE_TTL,
                    MIN_HIVE_TTL,
                    MAX_HIVE_TTL,
                    Universe.random
                                                   )
            }
            Universe.hives[entityUnitNumber] = hiveData
            Universe.activeHives[entityUnitNumber] = hiveData
        else
            hiveData.hiveId = entityUnitNumber
            hiveData.e = entity
            hiveData.position = entity.position
            hiveData.maxNests = ((maxNests > 0) and (Universe.random(maxNests) - 1)) or 0
            hiveData.maxTurrets = ((maxTurrets > 0) and (Universe.random(maxTurrets) - 1)) or 0
            hiveData.maxHives = ((maxHives > 0) and (Universe.random(maxHives) - 1)) or 0
            hiveData.tier = tier
            Universe.activeHives[entityUnitNumber] = hiveData
        end
    end
end

function ChunkUtils.registerEnemyBaseStructure(entity, base, tick, skipCount)
    local name = entity.name
    local hiveType = BUILDING_HIVE_TYPE_LOOKUP[name]

    local addFunc =
        registerTypeToAddFn[hiveType]
        or registerTypeToAddFn[entity.type]
        or addNestCount

    local added = false
    local entityUnitNumber = entity.unit_number
    local chunks = getEntityOverlapChunks(base.map, entity)
    for i=1,#chunks do
        local chunk = chunks[i]
        if (chunk ~= -1) then
            if addFunc(chunk, entityUnitNumber) then
                added = true
                setChunkBase(chunk, base)
                addBaseResourceChunk(base, chunk)
                if (hiveType == "spitter-spawner") or (hiveType == "biter-spawner") then
                    processNestActiveness(chunk, tick)
                end
            end
        end
    end
    registerHive(base, entity, hiveType, tick)
    if added and (not skipCount) then
        base.builtEnemyBuilding = base.builtEnemyBuilding + 1
    end
end

local function unregisterHive(entityUnitNumber, hiveType)
    if Universe.hives[entityUnitNumber] then
        Universe.hives[entityUnitNumber] = nil
        Universe.activeHives[entityUnitNumber] = nil
        if Universe.hiveIterator == entityUnitNumber then
            Universe.hiveIterator = nil
        end
    else
        local hiveData = Universe.hiveData[entityUnitNumber]
        if hiveData then
            if Universe.hiveDataIterator == entityUnitNumber then
                Universe.hiveDataIterator = nil
            end
            if not Universe.hives[hiveData.hiveId] then
                Universe.hiveData[entityUnitNumber] = nil
                return
            end

            Universe.activeHives[hiveData.hiveId] = hiveData
            local adjustedHiveType = (
                (
                    (hiveType == "spitter-spawner")
                    or (hiveType == "biter-spawner")
                )
                and "nest"
            ) or hiveType
            hiveData[adjustedHiveType] = hiveData[adjustedHiveType] - 1
            if hiveData[adjustedHiveType] < 0 then
                hiveData[adjustedHiveType] = 0
            end
            Universe.hiveData[entityUnitNumber] = nil
        end
    end
end

function ChunkUtils.unregisterEnemyBaseStructure(map, entity, damageTypeName, skipCount)
    local hiveType = BUILDING_HIVE_TYPE_LOOKUP[entity.name]

    local removeFunc =
        unregisterTypeToRemoveFn[hiveType]
        or unregisterTypeToRemoveFn[entity.type]
        or removeNestCount

    local entityUnitNumber = entity.unit_number
    local usedBases = {}
    local chunks = getEntityOverlapChunks(map, entity)
    for i=1,#chunks do
        local chunk = chunks[i]
        if (chunk ~= -1) then
            if removeFunc(chunk, entityUnitNumber) then
                local base = chunk.base
                local baseId = base.id
                if not usedBases[baseId] then
                    usedBases[baseId] = true
                    if damageTypeName then
                        base.damagedBy[damageTypeName] = (base.damagedBy[damageTypeName] or 0) + 3
                        base.deathEvents = base.deathEvents + 3
                    end
                    if (not skipCount) then
                        base.lostEnemyBuilding = base.lostEnemyBuilding + 1
                    end
                end
                if (getEnemyStructureCount(chunk) <= 0) then
                    removeBaseResourceChunk(base, chunk)
                    removeChunkBase(chunk, base)
                end
            end
        end
    end
    unregisterHive(entityUnitNumber, hiveType)
end

function ChunkUtils.accountPlayerEntity(entity, map, addObject, base)
    if (BUILDING_PHEROMONES[entity.type] ~= nil) and (entity.force.name ~= "enemy") then
        local entityValue = BUILDING_PHEROMONES[entity.type]
        local overlapArray = getEntityOverlapChunks(map, entity)
        if not addObject then
            if base then
                local pointValue = entityValue
                if pointValue == GENERATOR_PHEROMONE_LEVEL_1 then
                    pointValue = 0
                end
                base.destroyPlayerBuildings = base.destroyPlayerBuildings + 1
                if (base.stateAI ~= BASE_AI_STATE_ONSLAUGHT) then
                    pointValue = pointValue * 0.12
                end
                modifyBaseUnitPoints(base, pointValue, "Structure Kill")
            end
            entityValue = -entityValue
        end

        for i=1,#overlapArray do
            local chunk = overlapArray[i]
            if (chunk ~= -1) then
                local amount = addPlayerBaseGenerator(chunk, entityValue)
                if (amount == 0) then
                    chunk[BASE_PHEROMONE] = 0
                end
            end
        end
    end
    return entity
end

function ChunkUtils.unregisterResource(entity, map)
    if entity.prototype.infinite_resource then
        return
    end
    local overlapArray = getEntityOverlapChunks(map, entity)

    for i=1,#overlapArray do
        local chunk = overlapArray[i]
        if (chunk ~= -1) then
            addResourceGenerator(chunk, -RESOURCE_NORMALIZER)
        end
    end
end

function ChunkUtils.registerResource(entity, map)
    local overlapArray = getEntityOverlapChunks(map, entity)

    for i=1,#overlapArray do
        local chunk = overlapArray[i]
        if (chunk ~= -1) then
            addResourceGenerator(chunk, RESOURCE_NORMALIZER)
        end
    end
end

function ChunkUtils.makeImmortalEntity(surface, entity)
    local repairPosition = entity.position
    local repairName = entity.name
    local repairForce = entity.force
    local repairDirection = entity.direction

    local wires
    if (entity.type == "electric-pole") then
        wires = entity.neighbours
    end
    entity.destroy()
    local newEntity = surface.create_entity({position=repairPosition,
                                             name=repairName,
                                             direction=repairDirection,
                                             force=repairForce})
    if wires then
        for _,v in pairs(wires.copper) do
            if (v.valid) then
                newEntity.connect_neighbour(v);
            end
        end
        for _,v in pairs(wires.red) do
            if (v.valid) then
                newEntity.connect_neighbour({wire = DEFINES_WIRE_TYPE_RED, target_entity = v});
            end
        end
        for _,v in pairs(wires.green) do
            if (v.valid) then
                newEntity.connect_neighbour({wire = DEFINES_WIRE_TYPE_GREEN, target_entity = v});
            end
        end
    end

    newEntity.destructible = false
end

function ChunkUtils.init(universe)
    Universe = universe
    ChunkOverlapArray = universe.chunkOverlapArray
end

ChunkUtilsG = ChunkUtils
return ChunkUtils
