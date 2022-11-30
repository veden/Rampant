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


if chunkUtilsG then
    return chunkUtilsG
end
local chunkUtils = {}

-- imports

local baseUtils = require("BaseUtils")
local constants = require("Constants")
local mapUtils = require("MapUtils")
local chunkPropertyUtils = require("ChunkPropertyUtils")
local mathUtils = require("MathUtils")
local queryUtils = require("QueryUtils")

-- constants

local VANILLA_ENTITY_TYPE_LOOKUP = constants.VANILLA_ENTITY_TYPE_LOOKUP
local BUILDING_HIVE_TYPE_LOOKUP = constants.BUILDING_HIVE_TYPE_LOOKUP
local HIVE_BUILDINGS_TYPES = constants.HIVE_BUILDINGS_TYPES

local DEFINES_WIRE_TYPE_RED = defines.wire_type.red
local DEFINES_WIRE_TYPE_GREEN = defines.wire_type.green

local CHUNK_PASS_THRESHOLD = constants.CHUNK_PASS_THRESHOLD

local BASE_AI_STATE_ONSLAUGHT = constants.BASE_AI_STATE_ONSLAUGHT

local BASE_PHEROMONE = constants.BASE_PHEROMONE
local PLAYER_PHEROMONE = constants.PLAYER_PHEROMONE
local RESOURCE_PHEROMONE = constants.RESOURCE_PHEROMONE
local ENEMY_PHEROMONE = constants.ENEMY_PHEROMONE
local BUILDING_PHEROMONES = constants.BUILDING_PHEROMONES

local CHUNK_SIZE = constants.CHUNK_SIZE
local CHUNK_SIZE_DIVIDER = constants.CHUNK_SIZE_DIVIDER

local CHUNK_NORTH_SOUTH = constants.CHUNK_NORTH_SOUTH
local CHUNK_EAST_WEST = constants.CHUNK_EAST_WEST

local CHUNK_ALL_DIRECTIONS = constants.CHUNK_ALL_DIRECTIONS
local CHUNK_IMPASSABLE = constants.CHUNK_IMPASSABLE

local RESOURCE_NORMALIZER = constants.RESOURCE_NORMALIZER

local CHUNK_TICK = constants.CHUNK_TICK

local GENERATOR_PHEROMONE_LEVEL_1 = constants.GENERATOR_PHEROMONE_LEVEL_1
local GENERATOR_PHEROMONE_LEVEL_3 = constants.GENERATOR_PHEROMONE_LEVEL_3
local GENERATOR_PHEROMONE_LEVEL_5 = constants.GENERATOR_PHEROMONE_LEVEL_5
local GENERATOR_PHEROMONE_LEVEL_6 = constants.GENERATOR_PHEROMONE_LEVEL_6

-- imported functions

local setAreaInQueryChunkSize = queryUtils.setAreaInQueryChunkSize
local setAreaXInQuery = queryUtils.setAreaXInQuery
local setAreaYInQuery = queryUtils.setAreaYInQuery

local setPlayerBaseGenerator = chunkPropertyUtils.setPlayerBaseGenerator
local addPlayerBaseGenerator = chunkPropertyUtils.addPlayerBaseGenerator
local setResourceGenerator = chunkPropertyUtils.setResourceGenerator
local addResourceGenerator = chunkPropertyUtils.addResourceGenerator

local addNestCount = chunkPropertyUtils.addNestCount
local removeNestCount = chunkPropertyUtils.removeNestCount
local addHiveCount = chunkPropertyUtils.addHiveCount
local removeHiveCount = chunkPropertyUtils.removeHiveCount
local addTrapCount = chunkPropertyUtils.addTrapCount
local removeTrapCount = chunkPropertyUtils.removeTrapCount
local addTurretCount = chunkPropertyUtils.addTurretCount
local removeTurretCount = chunkPropertyUtils.removeTurretCount
local addUtilityCount = chunkPropertyUtils.addUtilityCount
local removeUtilityCount = chunkPropertyUtils.removeUtilityCount

local getPlayerBaseGenerator = chunkPropertyUtils.getPlayerBaseGenerator
local setRaidNestActiveness = chunkPropertyUtils.setRaidNestActiveness
local setNestActiveness = chunkPropertyUtils.setNestActiveness
local getChunkById = mapUtils.getChunkById

local processNestActiveness = chunkPropertyUtils.processNestActiveness

local removeChunkBase = chunkPropertyUtils.removeChunkBase
local getEnemyStructureCount = chunkPropertyUtils.getEnemyStructureCount

local findNearbyBase = chunkPropertyUtils.findNearbyBase

local createBase = baseUtils.createBase
local upgradeEntity = baseUtils.upgradeEntity
local modifyBasePoints = baseUtils.modifyBasePoints

local euclideanDistancePoints = mathUtils.euclideanDistancePoints

local getChunkBase = chunkPropertyUtils.getChunkBase
local setChunkBase = chunkPropertyUtils.setChunkBase
local setPassable = chunkPropertyUtils.setPassable
local setPathRating = chunkPropertyUtils.setPathRating

local getChunkByXY = mapUtils.getChunkByXY

local mMin = math.min
local mMax = math.max
local mFloor = math.floor

-- module code

local function getEntityOverlapChunks(map, entity)
    local boundingBox = entity.prototype.collision_box or entity.prototype.selection_box
    local overlapArray = map.universe.chunkOverlapArray

    overlapArray[1] = -1 --LeftTop
    overlapArray[2] = -1 --RightTop
    overlapArray[3] = -1 --LeftBottom
    overlapArray[4] = -1 --RightBottom

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

        overlapArray[1] = getChunkByXY(map, leftTopChunkX, leftTopChunkY) -- LeftTop
        if (leftTopChunkX ~= rightTopChunkX) then
            overlapArray[2] = getChunkByXY(map, rightTopChunkX, leftTopChunkY) -- RightTop
        end
        if (leftTopChunkY ~= leftBottomChunkY) then
            overlapArray[3] = getChunkByXY(map, leftTopChunkX, leftBottomChunkY) -- LeftBottom
        end
        if (leftTopChunkX ~= rightTopChunkX) and (leftTopChunkY ~= leftBottomChunkY) then
            overlapArray[4] = getChunkByXY(map, rightTopChunkX, leftBottomChunkY) -- RightBottom
        end
    end
    return overlapArray
end

local function scanPaths(chunk, map)
    local surface = map.surface
    local pass = CHUNK_IMPASSABLE

    local x = chunk.x
    local y = chunk.y

    local universe = map.universe
    local filteredEntitiesCliffQuery = universe.spFilteredEntitiesCliffQuery
    local filteredTilesPathQuery = universe.spFilteredTilesPathQuery
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
    local universe = map.universe
    setAreaInQueryChunkSize(universe.spbHasPlayerStructuresQuery, chunk)
    if surface.count_entities_filtered(universe.spbHasPlayerStructuresQuery) > 0 then
        return (surface.count_entities_filtered(universe.spbFilteredEntitiesPlayerQueryLowest) * GENERATOR_PHEROMONE_LEVEL_1) +
            (surface.count_entities_filtered(universe.spbFilteredEntitiesPlayerQueryLow) * GENERATOR_PHEROMONE_LEVEL_3) +
            (surface.count_entities_filtered(universe.spbFilteredEntitiesPlayerQueryHigh) * GENERATOR_PHEROMONE_LEVEL_5) +
            (surface.count_entities_filtered(universe.spbFilteredEntitiesPlayerQueryHighest) * GENERATOR_PHEROMONE_LEVEL_6)
    end
    return 0
end

function chunkUtils.initialScan(chunk, map, tick)
    local surface = map.surface
    local universe = map.universe
    setAreaInQueryChunkSize(universe.isFilteredTilesQuery, chunk)
    local waterTiles = (1 - (surface.count_tiles_filtered(universe.isFilteredTilesQuery) * 0.0009765625)) * 0.80
    local enemyBuildings = surface.find_entities_filtered(universe.isFilteredEntitiesEnemyStructureQuery)

    if (waterTiles >= CHUNK_PASS_THRESHOLD) or (#enemyBuildings > 0) then
        local pass = scanPaths(chunk, map)

        local playerObjects = scorePlayerBuildings(map, chunk)

        if ((playerObjects > 0) or (#enemyBuildings > 0)) and (pass == CHUNK_IMPASSABLE) then
            pass = CHUNK_ALL_DIRECTIONS
        end

        if (pass ~= CHUNK_IMPASSABLE) then
            local neutralObjects = mMax(0,
                                        mMin(1 - (surface.count_entities_filtered(universe.isFilteredEntitiesChunkNeutral) * 0.005),
                                             1) * 0.20)

            setPassable(map, chunk, pass)
            setPathRating(map, chunk, waterTiles + neutralObjects)
            setPlayerBaseGenerator(map, chunk, playerObjects)

            local resources = surface.count_entities_filtered(universe.isCountResourcesQuery) * RESOURCE_NORMALIZER
            setResourceGenerator(map, chunk, resources)

            local counts = map.chunkScanCounts
            for i=1,#HIVE_BUILDINGS_TYPES do
                counts[HIVE_BUILDINGS_TYPES[i]] = 0
            end

            if (#enemyBuildings > 0) then
                local base = findNearbyBase(map, chunk)
                if not base then
                    base = createBase(map, chunk, tick)
                end

                if universe.NEW_ENEMIES then
                    local unitList = surface.find_entities_filtered(universe.isFilteredEntitiesUnitQuery)
                    for i=1,#unitList do
                        local unit = unitList[i]
                        if (unit.valid) then
                            unit.destroy()
                        end
                    end

                    for i = 1, #enemyBuildings do
                        local enemyBuilding = enemyBuildings[i]
                        chunkUtils.registerEnemyBaseStructure(map, enemyBuilding, base)
                        local entityName = enemyBuilding.name
                        local isVanilla = VANILLA_ENTITY_TYPE_LOOKUP[entityName]
                        if isVanilla or (not isVanilla and not BUILDING_HIVE_TYPE_LOOKUP[entityName]) then
                            upgradeEntity(enemyBuilding, base, map, nil, true)
                        end
                    end
                else
                    for i=1,#enemyBuildings do
                        local building = enemyBuildings[i]
                        chunkUtils.registerEnemyBaseStructure(map, building, base)
                    end
                end
            end

            return chunk
        end
    end

    return -1
end

function chunkUtils.chunkPassScan(chunk, map)
    local surface = map.surface
    local universe = map.universe
    setAreaInQueryChunkSize(universe.cpsFilteredTilesQuery, chunk)
    local waterTiles = (1 - (surface.count_tiles_filtered(universe.cpsFilteredTilesQuery) * 0.0009765625)) * 0.80
    local enemyCount = surface.count_entities_filtered(universe.cpsFilteredEnemyAnyFound)

    if (waterTiles >= CHUNK_PASS_THRESHOLD) or (enemyCount > 0) then
        local neutralObjects = mMax(0,
                                    mMin(1 - (surface.count_entities_filtered(universe.cpsFilteredEntitiesChunkNeutral) * 0.005),
                                         1) * 0.20)
        local pass = scanPaths(chunk, map)

        local playerObjects = getPlayerBaseGenerator(map, chunk)

        if ((playerObjects > 0) or (enemyCount > 0)) and (pass == CHUNK_IMPASSABLE) then
            pass = CHUNK_ALL_DIRECTIONS
        end

        setPassable(map, chunk, pass)
        setPathRating(map, chunk, waterTiles + neutralObjects)

        if pass == CHUNK_IMPASSABLE then
            return -1
        end

        return chunk
    end

    return -1
end

function chunkUtils.mapScanPlayerChunk(chunk, map)
    local playerObjects = scorePlayerBuildings(map, chunk)
    setPlayerBaseGenerator(map, chunk, playerObjects)
end

function chunkUtils.mapScanResourceChunk(chunk, map)
    local universe = map.universe
    setAreaInQueryChunkSize(universe.msrcCountResourcesQuery, chunk)
    local surface = map.surface
    local resources = surface.count_entities_filtered(universe.msrcCountResourcesQuery) * RESOURCE_NORMALIZER
    setResourceGenerator(map, chunk, resources)
    local waterTiles = (1 - (surface.count_tiles_filtered(universe.msrcFilteredTilesQuery) * 0.0009765625)) * 0.80
    local neutralObjects = mMax(0,
                                mMin(1 - (surface.count_entities_filtered(universe.msrcFilteredEntitiesChunkNeutral) * 0.005),
                                     1) * 0.20)
    setPathRating(map, chunk, waterTiles + neutralObjects)
end

function chunkUtils.mapScanEnemyChunk(chunk, map, tick)
    local universe = map.universe
    setAreaInQueryChunkSize(universe.msecFilteredEntitiesEnemyStructureQuery, chunk)
    local buildings = map.surface.find_entities_filtered(universe.msecFilteredEntitiesEnemyStructureQuery)
    local counts = map.chunkScanCounts
    for i=1,#HIVE_BUILDINGS_TYPES do
        counts[HIVE_BUILDINGS_TYPES[i]] = 0
    end
    if (#buildings > 0) then
        local base = findNearbyBase(map, chunk)
        if not base then
            base = createBase(map, chunk, tick)
        end
        for i=1,#buildings do
            local building = buildings[i]

            chunkUtils.registerEnemyBaseStructure(map, building, base)
        end
    end
end

function chunkUtils.addBasesToAllEnemyStructures(universe, tick)
    for chunkId, chunkPack in pairs(universe.chunkToNests) do
        local map = chunkPack.map
        if map.surface.valid then
            local chunk = getChunkById(map, chunkId)
            local base = findNearbyBase(map, chunk)
            if not base then
                base = createBase(map, chunk, tick)
            end
            setChunkBase(map, chunk, base)
        end
    end
    for _, map in pairs(universe.maps) do
        if map.surface.valid then
            for chunkId in pairs(map.chunkToTurrets) do
                local chunk = getChunkById(map, chunkId)
                local base = findNearbyBase(map, chunk)
                if not base then
                    base = createBase(map, chunk, tick)
                end
                setChunkBase(map, chunk, base)
            end
            for chunkId in pairs(map.chunkToHives) do
                local chunk = getChunkById(map, chunkId)
                local base = findNearbyBase(map, chunk)
                if not base then
                    base = createBase(map, chunk, tick)
                end
                setChunkBase(map, chunk, base)
            end
            for chunkId in pairs(map.chunkToUtilities) do
                local chunk = getChunkById(map, chunkId)
                local base = findNearbyBase(map, chunk)
                if not base then
                    base = createBase(map, chunk, tick)
                end
                setChunkBase(map, chunk, base)
            end
            for chunkId in pairs(map.chunkToTraps) do
                local chunk = getChunkById(map, chunkId)
                local base = findNearbyBase(map, chunk)
                if not base then
                    base = createBase(map, chunk, tick)
                end
                setChunkBase(map, chunk, base)
            end
        end
    end
end

function chunkUtils.entityForPassScan(map, entity)
    local overlapArray = getEntityOverlapChunks(map, entity)

    for i=1,#overlapArray do
        local chunk = overlapArray[i]
        if (chunk ~= -1) and not map.universe.chunkToPassScan[chunk.id] then
            map.universe.chunkToPassScan[chunk.id] = {
                map = map,
                chunk = chunk
            }
        end
    end
end

local function newChunkId(universe)
    local id = universe.chunkId
    universe.chunkId = universe.chunkId + 1
    return id
end

function chunkUtils.createChunk(map, topX, topY)
    local chunk = {
        x = topX,
        y = topY,
        dOrigin = euclideanDistancePoints(topX, topY, 0, 0),
        id = newChunkId(map.universe)
    }
    chunk[BASE_PHEROMONE] = 0
    chunk[PLAYER_PHEROMONE] = 0
    chunk[RESOURCE_PHEROMONE] = 0
    chunk[ENEMY_PHEROMONE] = 0
    chunk[CHUNK_TICK] = 0

    return chunk
end

function chunkUtils.colorChunk(chunk, surface, color, ttl)
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

function chunkUtils.colorXY(x, y, surface, color)
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

function chunkUtils.registerEnemyBaseStructure(map, entity, base, skipCount)
    local entityType = entity.type

    local addFunc
    local hiveType = BUILDING_HIVE_TYPE_LOOKUP[entity.name]
    if (hiveType == "spitter-spawner") or (hiveType == "biter-spawner") then
        addFunc = addNestCount
    elseif (hiveType == "turret") then
        addFunc = addTurretCount
    elseif (hiveType == "trap") then
        addFunc = addTrapCount
    elseif (hiveType == "utility") then
        addFunc = addUtilityCount
    elseif (hiveType == "hive") then
        addFunc = addHiveCount
    else
        if (entityType == "turret") then
            addFunc = addTurretCount
        else
            addFunc = addNestCount
        end
    end

    local added = false
    local entityUnitNumber = entity.unit_number
    local chunks = getEntityOverlapChunks(map, entity)
    for i=1,#chunks do
        local chunk = chunks[i]
        if (chunk ~= -1) then
            if addFunc(map, chunk, entityUnitNumber) then
                added = true
                setChunkBase(map, chunk, base)
            end
            if (hiveType == "spitter-spawner") or (hiveType == "biter-spawner") then
                processNestActiveness(map, chunk)
            end
        end
    end
    if added and (not skipCount) then
        base.builtEnemyBuilding = base.builtEnemyBuilding + 1
    end
end

function chunkUtils.unregisterEnemyBaseStructure(map, entity, damageTypeName, skipCount)
    local entityType = entity.type

    local removeFunc
    local hiveType = BUILDING_HIVE_TYPE_LOOKUP[entity.name]
    if (hiveType == "spitter-spawner") or (hiveType == "biter-spawner") then
        removeFunc = removeNestCount
    elseif (hiveType == "turret") then
        removeFunc = removeTurretCount
    elseif (hiveType == "trap") then
        removeFunc = removeTrapCount
    elseif (hiveType == "utility") then
        removeFunc = removeUtilityCount
    elseif (hiveType == "hive") then
        removeFunc = removeHiveCount
    else
        if (entityType == "turret") then
            removeFunc = removeTurretCount
        else
            hiveType = "biter-spawner"
            removeFunc = removeNestCount
        end
    end

    local entityUnitNumber = entity.unit_number
    local usedBases = {}
    local chunks = getEntityOverlapChunks(map, entity)
    for i=1,#chunks do
        local chunk = chunks[i]
        if (chunk ~= -1) then
            local base = getChunkBase(map, chunk)
            if (hiveType == "spitter-spawner") or (hiveType == "biter-spawner") then
                setRaidNestActiveness(map, chunk, 0, base)
                setNestActiveness(map, chunk, 0, base)
            end
            if removeFunc(map, chunk, entityUnitNumber) then
                if not usedBases[base.id] then
                    usedBases[base.id] = true
                    if damageTypeName then
                        base.damagedBy[damageTypeName] = (base.damagedBy[damageTypeName] or 0) + 3
                        base.deathEvents = base.deathEvents + 3
                    end
                    if (not skipCount) and (hiveType ~= "trap") then
                        base.lostEnemyBuilding = base.lostEnemyBuilding + 1
                    end
                end
                if (getEnemyStructureCount(map, chunk) <= 0) then
                    removeChunkBase(map, chunk, base)
                end
            end
        end
    end
end

function chunkUtils.accountPlayerEntity(entity, map, addObject, base)
    if (BUILDING_PHEROMONES[entity.type] ~= nil) and (entity.force.name ~= "enemy") then
        local universe = map.universe
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
                modifyBasePoints(base, pointValue, "Structure Kill")
            end
            entityValue = -entityValue
        end

        for i=1,#overlapArray do
            local chunk = overlapArray[i]
            if (chunk ~= -1) then
                addPlayerBaseGenerator(map, chunk, entityValue)
            end
        end
    end
    return entity
end

function chunkUtils.unregisterResource(entity, map)
    if entity.prototype.infinite_resource then
        return
    end
    local overlapArray = getEntityOverlapChunks(map, entity)

    for i=1,#overlapArray do
        local chunk = overlapArray[i]
        if (chunk ~= -1) then
            addResourceGenerator(map, chunk, -RESOURCE_NORMALIZER)
        end
    end
end

function chunkUtils.registerResource(entity, map)
    local overlapArray = getEntityOverlapChunks(map, entity)

    for i=1,#overlapArray do
        local chunk = overlapArray[i]
        if (chunk ~= -1) then
            addResourceGenerator(map, chunk, RESOURCE_NORMALIZER)
        end
    end
end

function chunkUtils.makeImmortalEntity(surface, entity)
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

chunkUtilsG = chunkUtils
return chunkUtils
