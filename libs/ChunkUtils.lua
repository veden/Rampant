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

local HIVE_BUILDINGS_TYPES = constants.HIVE_BUILDINGS_TYPES

local DEFINES_WIRE_TYPE_RED = defines.wire_type.red
local DEFINES_WIRE_TYPE_GREEN = defines.wire_type.green

local CHUNK_PASS_THRESHOLD = constants.CHUNK_PASS_THRESHOLD

local AI_STATE_ONSLAUGHT = constants.AI_STATE_ONSLAUGHT

local BASE_PHEROMONE = constants.BASE_PHEROMONE
local PLAYER_PHEROMONE = constants.PLAYER_PHEROMONE
local RESOURCE_PHEROMONE = constants.RESOURCE_PHEROMONE
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

local setAreaInQuery = queryUtils.setAreaInQuery

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
local getNestCount = chunkPropertyUtils.getNestCount
local setRaidNestActiveness = chunkPropertyUtils.setRaidNestActiveness
local setNestActiveness = chunkPropertyUtils.setNestActiveness

local processNestActiveness = chunkPropertyUtils.processNestActiveness

local removeChunkBase = chunkPropertyUtils.removeChunkBase
local getEnemyStructureCount = chunkPropertyUtils.getEnemyStructureCount

local findNearbyBase = baseUtils.findNearbyBase
local createBase = baseUtils.createBase

local upgradeEntity = baseUtils.upgradeEntity

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
    local filteredEntitiesCliffQuery = universe.filteredEntitiesCliffQuery
    local filteredTilesPathQuery = universe.filteredTilesPathQuery
    local count_entities_filtered = surface.count_entities_filtered
    local count_tiles_filtered = surface.count_tiles_filtered

    local passableNorthSouth = false
    local passableEastWest = false

    local topPosition = filteredEntitiesCliffQuery.area[1]
    local bottomPosition = filteredEntitiesCliffQuery.area[2]
    topPosition[2] = y
    bottomPosition[2] = y + 32

    for xi=x, x + 32 do
        topPosition[1] = xi
        bottomPosition[1] = xi + 1
        if (count_entities_filtered(filteredEntitiesCliffQuery) == 0) and
            (count_tiles_filtered(filteredTilesPathQuery) == 0)
        then
            passableNorthSouth = true
            break
        end
    end

    topPosition[1] = x
    bottomPosition[1] = x + 32

    for yi=y, y + 32 do
        topPosition[2] = yi
        bottomPosition[2] = yi + 1
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

local function scorePlayerBuildings(map)
    local surface = map.surface
    local universe = map.universe
    if surface.count_entities_filtered(universe.hasPlayerStructuresQuery) > 0 then
        return (surface.count_entities_filtered(universe.filteredEntitiesPlayerQueryLowest) * GENERATOR_PHEROMONE_LEVEL_1) +
            (surface.count_entities_filtered(universe.filteredEntitiesPlayerQueryLow) * GENERATOR_PHEROMONE_LEVEL_3) +
            (surface.count_entities_filtered(universe.filteredEntitiesPlayerQueryHigh) * GENERATOR_PHEROMONE_LEVEL_5) +
            (surface.count_entities_filtered(universe.filteredEntitiesPlayerQueryHighest) * GENERATOR_PHEROMONE_LEVEL_6)
    end
    return 0
end

function chunkUtils.initialScan(chunk, map, tick)
    local surface = map.surface
    local universe = map.universe
    local waterTiles = (1 - (surface.count_tiles_filtered(universe.filteredTilesQuery) * 0.0009765625)) * 0.80
    local enemyBuildings = surface.find_entities_filtered(universe.filteredEntitiesEnemyStructureQuery)

    if (waterTiles >= CHUNK_PASS_THRESHOLD) or (#enemyBuildings > 0) then
        local neutralObjects = mMax(0,
                                    mMin(1 - (surface.count_entities_filtered(universe.filteredEntitiesChunkNeutral) * 0.005),
                                         1) * 0.20)
        local pass = scanPaths(chunk, map)

        local playerObjects = scorePlayerBuildings(map)

        if ((playerObjects > 0) or (#enemyBuildings > 0)) and (pass == CHUNK_IMPASSABLE) then
            pass = CHUNK_ALL_DIRECTIONS
        end

        if (pass ~= CHUNK_IMPASSABLE) then
            local resources = surface.count_entities_filtered(universe.countResourcesQuery) * RESOURCE_NORMALIZER

            local buildingHiveTypeLookup = universe.buildingHiveTypeLookup
            local counts = map.chunkScanCounts
            for i=1,#HIVE_BUILDINGS_TYPES do
                counts[HIVE_BUILDINGS_TYPES[i]] = 0
            end

            if (#enemyBuildings > 0) then
                if universe.NEW_ENEMIES then
                    local base = findNearbyBase(map, chunk)
                    if not base then
                        base = createBase(map, chunk, tick)
                    end

                    local unitList = surface.find_entities_filtered(universe.filteredEntitiesUnitQuery)
                    for i=1,#unitList do
                        local unit = unitList[i]
                        if (unit.valid) then
                            unit.destroy()
                        end
                    end

                    for i = 1, #enemyBuildings do
                        local enemyBuilding = enemyBuildings[i]

                        chunkUtils.registerEnemyBaseStructure(map, enemyBuilding, tick, base)
                        if not buildingHiveTypeLookup[enemyBuilding.name] then
                            upgradeEntity(enemyBuilding, base, map, nil, true)
                        end
                    end
                else
                    for i=1,#enemyBuildings do
                        local building = enemyBuildings[i]
                        chunkUtils.registerEnemyBaseStructure(map, building, tick)
                    end
                end
            end

            setPlayerBaseGenerator(map, chunk, playerObjects)
            setResourceGenerator(map, chunk, resources)

            setPassable(map, chunk, pass)
            setPathRating(map, chunk, waterTiles + neutralObjects)

            return chunk
        end
    end

    return -1
end

function chunkUtils.chunkPassScan(chunk, map)
    local surface = map.surface
    local universe = map.universe
    local waterTiles = (1 - (surface.count_tiles_filtered(universe.filteredTilesQuery) * 0.0009765625)) * 0.80

    if (waterTiles >= CHUNK_PASS_THRESHOLD) then
        local neutralObjects = mMax(0,
                                    mMin(1 - (surface.count_entities_filtered(universe.filteredEntitiesChunkNeutral) * 0.005),
                                         1) * 0.20)
        local pass = scanPaths(chunk, map)

        local playerObjects = getPlayerBaseGenerator(map, chunk)

        local nests = getNestCount(map, chunk)

        if ((playerObjects > 0) or (nests > 0)) and (pass == CHUNK_IMPASSABLE) then
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
    local playerObjects = scorePlayerBuildings(map)
    setPlayerBaseGenerator(map, chunk, playerObjects)
end

function chunkUtils.mapScanResourceChunk(chunk, map)
    local surface = map.surface
    local universe = map.universe
    local resources = surface.count_entities_filtered(universe.countResourcesQuery) * RESOURCE_NORMALIZER
    setResourceGenerator(map, chunk, resources)
    local waterTiles = (1 - (surface.count_tiles_filtered(universe.filteredTilesQuery) * 0.0009765625)) * 0.80
    local neutralObjects = mMax(0,
                                mMin(1 - (surface.count_entities_filtered(universe.filteredEntitiesChunkNeutral) * 0.005),
                                     1) * 0.20)
    setPathRating(map, chunk, waterTiles + neutralObjects)
end

function chunkUtils.mapScanEnemyChunk(chunk, map, tick)
    local universe = map.universe
    local query = universe.filteredEntitiesEnemyStructureQuery
    setAreaInQuery(query, chunk, CHUNK_SIZE)
    local buildings = map.surface.find_entities_filtered(query)
    local counts = map.chunkScanCounts
    for i=1,#HIVE_BUILDINGS_TYPES do
        counts[HIVE_BUILDINGS_TYPES[i]] = 0
    end
    local base
    if universe.NEW_ENEMIES then
        base = findNearbyBase(map, chunk)
        if not base then
            base = createBase(map, chunk, tick)
        end
    end
    for i=1,#buildings do
        local building = buildings[i]

        chunkUtils.registerEnemyBaseStructure(map, building, tick, base)
    end
end

function chunkUtils.entityForPassScan(map, entity)
    local overlapArray = getEntityOverlapChunks(map, entity)

    for i=1,#overlapArray do
        local chunk = overlapArray[i]
        if (chunk ~= -1) then
            map.chunkToPassScan[chunk.id] = chunk
        end
    end
end

function chunkUtils.newChunkId(map)
    local id = map.chunkId
    map.chunkId = map.chunkId + 1
    return id
end

function chunkUtils.createChunk(map, topX, topY)
    local chunk = {
        x = topX,
        y = topY,
        dOrigin = euclideanDistancePoints(topX, topY, 0, 0),
        id = chunkUtils.newChunkId(map)
    }
    chunk[BASE_PHEROMONE] = 0
    chunk[PLAYER_PHEROMONE] = 0
    chunk[RESOURCE_PHEROMONE] = 0
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

function chunkUtils.registerEnemyBaseStructure(map, entity, tick, incomingBase)
    local entityType = entity.type

    local addFunc
    local universe = map.universe
    local hiveTypeLookup = universe.buildingHiveTypeLookup
    local hiveType = hiveTypeLookup[entity.name]
    if (hiveType == "spitter-spawner") or (hiveType == "biter-spawner") then
        map.builtEnemyBuilding = map.builtEnemyBuilding + 1
        addFunc = addNestCount
    elseif (hiveType == "turret") then
        map.builtEnemyBuilding = map.builtEnemyBuilding + 1
        addFunc = addTurretCount
    elseif (hiveType == "trap") then
        addFunc = addTrapCount
    elseif (hiveType == "utility") then
        map.builtEnemyBuilding = map.builtEnemyBuilding + 1
        addFunc = addUtilityCount
    elseif (hiveType == "hive") then
        map.builtEnemyBuilding = map.builtEnemyBuilding + 1
        addFunc = addHiveCount
    else
        if (entityType == "turret") then
            map.builtEnemyBuilding = map.builtEnemyBuilding + 1
            addFunc = addTurretCount
        else
            map.builtEnemyBuilding = map.builtEnemyBuilding + 1
            addFunc = addNestCount
        end
    end

    local entityUnitNumber = entity.unit_number
    local chunks = getEntityOverlapChunks(map, entity)
    for i=1,#chunks do
        local chunk = chunks[i]
        if (chunk ~= -1) then
            if universe.NEW_ENEMIES then
                local base = incomingBase
                if not base then
                    base = findNearbyBase(map, chunk)
                    if not base then
                        base = createBase(map, chunk, tick)
                    end
                end
                setChunkBase(map, chunk, base)
            end
            addFunc(map, chunk, entityUnitNumber)
            if (hiveType == "spitter-spawner") or (hiveType == "biter-spawner") then
                processNestActiveness(map, chunk)
            end
        end
    end
end

function chunkUtils.unregisterEnemyBaseStructure(map, entity, damageType)
    local entityType = entity.type

    local removeFunc
    local hiveTypeLookup = map.universe.buildingHiveTypeLookup
    local hiveType = hiveTypeLookup[entity.name]
    if (hiveType == "spitter-spawner") or (hiveType == "biter-spawner") then
        map.lostEnemyBuilding = map.lostEnemyBuilding + 1
        removeFunc = removeNestCount
    elseif (hiveType == "turret") then
        map.lostEnemyBuilding = map.lostEnemyBuilding + 1
        removeFunc = removeTurretCount
    elseif (hiveType == "trap") then
        removeFunc = removeTrapCount
    elseif (hiveType == "utility") then
        map.lostEnemyBuilding = map.lostEnemyBuilding + 1
        removeFunc = removeUtilityCount
    elseif (hiveType == "hive") then
        map.lostEnemyBuilding = map.lostEnemyBuilding + 1
        removeFunc = removeHiveCount
    else
        if (entityType == "turret") then
            map.lostEnemyBuilding = map.lostEnemyBuilding + 1
            removeFunc = removeTurretCount
        else
            hiveType = "biter-spawner"
            map.lostEnemyBuilding = map.lostEnemyBuilding + 1
            removeFunc = removeNestCount
        end
    end

    local entityUnitNumber = entity.unit_number
    local usedBases = {}
    local chunks = getEntityOverlapChunks(map, entity)
    for i=1,#chunks do
        local chunk = chunks[i]
        if (chunk ~= -1) then
            if (hiveType == "spitter-spawner") or (hiveType == "biter-spawner") then
                setRaidNestActiveness(map, chunk, 0)
                setNestActiveness(map, chunk, 0)
            end
            removeFunc(map, chunk, entityUnitNumber)
            if map.universe.NEW_ENEMIES then
                local base = getChunkBase(map, chunk)
                if damageType and usedBases[base.id] then
                    usedBases[base.id] = true
                    local damageTypeName = damageType.name
                    base.damagedBy[damageTypeName] = (base.damagedBy[damageTypeName] or 0) + 3
                    base.deathEvents = base.deathEvents + 3
                end
                if (getEnemyStructureCount(map, chunk) <= 0) then
                    removeChunkBase(map, chunk, base)
                end
            end
        end
    end
end

function chunkUtils.accountPlayerEntity(entity, map, addObject, creditNatives)
    if (BUILDING_PHEROMONES[entity.type] ~= nil) and (entity.force.name ~= "enemy") then
        local universe = map.universe
        local entityValue = BUILDING_PHEROMONES[entity.type]
        local overlapArray = getEntityOverlapChunks(map, entity)
        if not addObject then
            if creditNatives then
                map.destroyPlayerBuildings = map.destroyPlayerBuildings + 1
                if (map.state == AI_STATE_ONSLAUGHT) then
                    map.points = map.points + entityValue
                    if universe.aiPointsPrintGainsToChat then
                        game.print(map.surface.name .. ": Points: +" .. math.floor(entityValue) .. ". [Structure Kill] Total: " .. string.format("%.2f", map.points))
                    end
                else
                    map.points = map.points + (entityValue * 0.12)
                    if universe.aiPointsPrintGainsToChat then
                        game.print(map.surface.name .. ": Points: +" .. math.floor(entityValue * 0.12) .. ". [Structure Kill] Total: " .. string.format("%.2f", map.points))
                    end
                end
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
