-- imports

local chunkPropertyUtils = require("libs/ChunkPropertyUtils")
local unitUtils = require("libs/UnitUtils")
local baseUtils = require("libs/BaseUtils")
local mapUtils = require("libs/MapUtils")
local mathUtils = require("libs/MathUtils")
local unitGroupUtils = require("libs/UnitGroupUtils")
local chunkProcessor = require("libs/ChunkProcessor")
local mapProcessor = require("libs/MapProcessor")
local constants = require("libs/Constants")
local pheromoneUtils = require("libs/PheromoneUtils")
local squadDefense = require("libs/SquadDefense")
local squadAttack = require("libs/SquadAttack")
local aiAttackWave = require("libs/AIAttackWave")
local aiPlanning = require("libs/AIPlanning")
-- local interop = require("libs/Interop")
local tests = require("tests")
local chunkUtils = require("libs/ChunkUtils")
local upgrade = require("Upgrade")
local config = require("config")
local aiPredicates = require("libs/AIPredicates")

-- constants

local AI_SQUAD_COST = constants.AI_SQUAD_COST
local AI_SETTLER_COST = constants.AI_SETTLER_COST

local RECOVER_NEST_COST = constants.RECOVER_NEST_COST
local RECOVER_WORM_COST = constants.RECOVER_WORM_COST

local RETREAT_GRAB_RADIUS = constants.RETREAT_GRAB_RADIUS

local RETREAT_SPAWNER_GRAB_RADIUS = constants.RETREAT_SPAWNER_GRAB_RADIUS

local DEFINES_WIRE_TYPE_RED = defines.wire_type.red
local DEFINES_WIRE_TYPE_GREEN = defines.wire_type.green

local ENERGY_THIEF_CONVERSION_TABLE = constants.ENERGY_THIEF_CONVERSION_TABLE
local ENERGY_THIEF_LOOKUP = constants.ENERGY_THIEF_LOOKUP

-- imported functions

local canMigrate = aiPredicates.canMigrate

local convertTypeToDrainCrystal = unitUtils.convertTypeToDrainCrystal

local squadDispatch = squadAttack.squadDispatch

local cleanUpMapTables = mapProcessor.cleanUpMapTables

local positionToChunkXY = mapUtils.positionToChunkXY

local temperamentPlanner = aiPlanning.temperamentPlanner

local processVengence = mapProcessor.processVengence
local processSpawners = mapProcessor.processSpawners

local processStaticMap = mapProcessor.processStaticMap

local disperseVictoryScent = pheromoneUtils.disperseVictoryScent

local getChunkByPosition = mapUtils.getChunkByPosition

local entityForPassScan = chunkUtils.entityForPassScan

local processPendingChunks = chunkProcessor.processPendingChunks
local processScanChunks = chunkProcessor.processScanChunks

local processMap = mapProcessor.processMap
local processPlayers = mapProcessor.processPlayers
local scanEnemyMap = mapProcessor.scanEnemyMap
local scanPlayerMap = mapProcessor.scanPlayerMap
local scanResourceMap = mapProcessor.scanResourceMap

local processNests = mapProcessor.processNests

local planning = aiPlanning.planning

local rallyUnits = aiAttackWave.rallyUnits

local recycleBases = baseUtils.recycleBases

local deathScent = pheromoneUtils.deathScent
local victoryScent = pheromoneUtils.victoryScent

local createSquad = unitGroupUtils.createSquad

local createBase = baseUtils.createBase
local findNearbyBase = baseUtils.findNearbyBase

local processActiveNests = mapProcessor.processActiveNests

local getDeathGenerator = chunkPropertyUtils.getDeathGenerator

local retreatUnits = squadDefense.retreatUnits

local accountPlayerEntity = chunkUtils.accountPlayerEntity
local unregisterEnemyBaseStructure = chunkUtils.unregisterEnemyBaseStructure
local registerEnemyBaseStructure = chunkUtils.registerEnemyBaseStructure
local makeImmortalEntity = chunkUtils.makeImmortalEntity

local registerResource = chunkUtils.registerResource
local unregisterResource = chunkUtils.unregisterResource

local cleanSquads = squadAttack.cleanSquads

local upgradeEntity = baseUtils.upgradeEntity
local rebuildNativeTables = baseUtils.rebuildNativeTables

local mRandom = math.random

local tRemove = table.remove

local sFind = string.find

-- local references to global

local maps -- manages the chunks that make up the game world
local queriesAndCommands -- manages all the queries and commands that will be used
local natives -- manages the enemy units, structures, and ai

-- hook functions

local function onIonCannonFired(event)
    --[[
        event.force, event.surface, event.player_index, event.position, event.radius
    --]]
    local surface = event.surface
    local surfaceIndex = surface.index
    local map = maps.map[surfaceIndex]
    local native = natives[surfaceIndex]
    native.ionCannonBlasts = native.ionCannonBlasts + 1
    native.points = native.point + 4000

    local chunk = getChunkByPosition(map, event.position)
    if (chunk ~= -1) then
        rallyUnits(chunk, map, event.tick)
    end
end

local function hookEvents()
    if config.ionCannonPresent then
        script.on_event(remote.call("orbital_ion_cannon", "on_ion_cannon_fired"),
                        onIonCannonFired)
    end
end

local function onLoad()
    maps = global.maps
    queriesAndCommands = global.queriesAndCommands
    natives = global.natives

    hookEvents()
end

local function onChunkGenerated(event)
    -- queue generated chunk for delayed processing, queuing is required because
    -- some mods (RSO) mess with chunk as they are generated, which messes up the
    -- scoring.
    maps.map[event.surface.index].pendingChunks[event] = true
end

local function onModSettingsChange(event)

    if event and ((string.sub(event.setting, 1, 7) ~= "rampant") or
        (string.sub(event.setting, 1, 15) == "rampant-arsenal") or
        (string.sub(event.setting, 1, 17) == "rampant-resources") or
        (string.sub(event.setting, 1, 17) == "rampant-evolution") or
        (string.sub(event.setting, 1, 19) == "rampant-maintenance") or
        (string.sub(event.setting, 1, 16) == "rampant-industry"))
    then
        return false
    end

    upgrade.compareTable(natives,
                         "safeBuildings",
                         settings.global["rampant-safeBuildings"].value)
    upgrade.compareTable(natives.safeEntities,
                         "curved-rail",
                         settings.global["rampant-safeBuildings-curvedRail"].value)
    upgrade.compareTable(natives.safeEntities,
                         "straight-rail",
                         settings.global["rampant-safeBuildings-straightRail"].value)
    upgrade.compareTable(natives.safeEntities,
                         "rail-signal",
                         settings.global["rampant-safeBuildings-railSignals"].value)
    upgrade.compareTable(natives.safeEntities,
                         "rail-chain-signal",
                         settings.global["rampant-safeBuildings-railChainSignals"].value)
    upgrade.compareTable(natives.safeEntities,
                         "train-stop",
                         settings.global["rampant-safeBuildings-trainStops"].value)
    upgrade.compareTable(natives.safeEntities,
                         "lamp",
                         settings.global["rampant-safeBuildings-lamps"].value)

    local changed, newValue = upgrade.compareTable(natives.safeEntities,
                                                   "big-electric-pole",
                                                   settings.global["rampant-safeBuildings-bigElectricPole"].value)
    if changed then
        natives.safeEntities["big-electric-pole"] = newValue
        natives.safeEntities["big-electric-pole-2"] = newValue
        natives.safeEntities["big-electric-pole-3"] = newValue
        natives.safeEntities["big-electric-pole-4"] = newValue
        natives.safeEntities["lighted-big-electric-pole-4"] = newValue
        natives.safeEntities["lighted-big-electric-pole-3"] = newValue
        natives.safeEntities["lighted-big-electric-pole-2"] = newValue
        natives.safeEntities["lighted-big-electric-pole"] = newValue
    end

    upgrade.compareTable(natives,
                         "deadZoneFrequency",
                         settings.global["rampant-deadZoneFrequency"].value)
    upgrade.compareTable(natives,
                         "raidAIToggle",
                         settings.global["rampant-raidAIToggle"].value)
    upgrade.compareTable(natives,
                         "siegeAIToggle",
                         settings.global["rampant-siegeAIToggle"].value)

    upgrade.compareTable(natives,
                         "attackPlayerThreshold",
                         settings.global["rampant-attackPlayerThreshold"].value)
    upgrade.compareTable(natives,
                         "attackUsePlayer",
                         settings.global["rampant-attackWaveGenerationUsePlayerProximity"].value)

    upgrade.compareTable(natives,
                         "attackWaveMaxSize",
                         settings.global["rampant-attackWaveMaxSize"].value)
    upgrade.compareTable(natives,
                         "aiNocturnalMode",
                         settings.global["rampant-permanentNocturnal"].value)
    upgrade.compareTable(natives,
                         "aiPointsScaler",
                         settings.global["rampant-aiPointsScaler"].value)

    natives.enabledMigration = natives.expansion and settings.global["rampant-enableMigration"].value

    upgrade.compareTable(natives,
                         "AI_MAX_SQUAD_COUNT",
                         settings.global["rampant-maxNumberOfSquads"].value)
    upgrade.compareTable(natives,
                         "AI_MAX_BUILDER_COUNT",
                         settings.global["rampant-maxNumberOfBuilders"].value)

    return true
end

local function prepWorld(surface)
    surface.print("Rampant - Indexing chunks, please wait.")

    local surfaceIndex = surface.index

    if not maps.map then
        maps.map = {}
    end

    local map = maps.map[surfaceIndex]
    if not map then
        map = {}
        maps.map[surfaceIndex] = map
        if not maps.registeredMaps then
            maps.registeredMaps = {}
        end
        local add = true
        for _, v in pairs(maps.registeredMaps) do
            if surfaceIndex == v then
                add = false
                break
            end
        end
        if add then
            maps.registeredMaps[#maps.registeredMaps+1] = surfaceIndex
        end
    end

    map.totalChunks = 0
    map.processedChunks = 0
    map.mapIterator = nil
    map.processQueue = {}
    map.processIndex = 1
    map.cleanupIndex = 1
    map.scanPlayerIndex = 1
    map.scanResourceIndex = 1
    map.scanEnemyIndex = 1
    map.processStaticIndex = 1
    map.outgoingScanWave = true
    map.outgoingStaticScanWave = true

    map.pendingChunks = {}
    map.chunkToBase = {}
    map.chunkToNests = {}
    map.chunkToTurrets = {}
    map.chunkToTraps = {}
    map.chunkToUtilities = {}
    map.chunkToHives = {}
    map.chunkToPlayerBase = {}
    map.chunkToResource = {}
    map.chunkToPlayerCount = {}
    map.playerToChunk = {}

    map.chunkToPassScan = {}
    map.chunkToSquad = {}

    map.chunkToRetreats = {}
    map.chunkToRallys = {}

    map.chunkToPassable = {}
    map.chunkToPathRating = {}
    map.chunkToDeathGenerator = {}
    map.chunkToDrained = {}
    map.chunkToVictory = {}
    map.chunkToActiveNest = {}
    map.chunkToActiveRaidNest = {}

    map.nextChunkSort = 0
    map.nextChunkSortTick = 0

    map.squadIterator = nil
    map.regroupIterator = nil
    map.deployVengenceIterator = nil
    map.recycleBaseIterator = nil
    map.processActiveSpawnerIterator = nil
    map.processActiveRaidSpawnerIterator = nil
    map.processMigrationIterator = nil
    map.processNestIterator = nil
    map.victoryScentIterator = nil

    map.chunkScanCounts = {}

    map.enemiesToSquad = {}
    map.enemiesToSquad.len = 0
    map.chunkRemovals = {}
    map.processActiveNest = {}
    map.tickActiveNest = {}

    map.emptySquadsOnChunk = {}

    local native = natives[surface.index]
    if not native then
        native = {}
        natives[surface.index] = native
    end
    map.native = native
    native.map = map
    map.surface = surface
    native.surface = surface
    map.queriesAndCommands = queriesAndCommands
    native.queriesAndCommands = queriesAndCommands

    -- queue all current chunks that wont be generated during play
    local tick = game.tick
    local position = {0,0}
    native.nextChunkSort = 0
    for chunk in surface.get_chunks() do
        local x = chunk.x
        local y = chunk.y
        position[1] = x
        position[2] = y
        if surface.is_chunk_generated(position) then
            onChunkGenerated({ surface = surface,
                               area = { left_top = { x = x * 32,
                                                     y = y * 32}}})
        end
    end

    processPendingChunks(map, tick)
end

local function onConfigChanged()
    if upgrade.attempt(natives, queriesAndCommands) then
        onModSettingsChange(nil)
    end

    upgrade.compareTable(natives,
                         "ENEMY_SEED",
                         settings.startup["rampant-enemySeed"].value)
    upgrade.compareTable(natives,
                         "ENEMY_VARIATIONS",
                         settings.startup["rampant-newEnemyVariations"].value)
    upgrade.compareTable(natives,
                         "NEW_ENEMIES",
                         settings.startup["rampant-newEnemies"].value)

    if natives.NEW_ENEMIES then
        rebuildNativeTables(natives, game.create_random_generator(natives.ENEMY_SEED))
    else
        natives.buildingHiveTypeLookup = {}
        natives.buildingHiveTypeLookup["biter-spawner"] = "biter-spawner"
        natives.buildingHiveTypeLookup["spitter-spawner"] = "spitter-spawner"
        natives.buildingHiveTypeLookup["small-worm-turret"] = "turret"
        natives.buildingHiveTypeLookup["medium-worm-turret"] = "turret"
        natives.buildingHiveTypeLookup["big-worm-turret"] = "turret"
        natives.buildingHiveTypeLookup["behemoth-worm-turret"] = "turret"
    end
end

local function onBuild(event)
    local entity = event.created_entity or event.entity
    if entity.valid then
        local native = natives[entity.surface.index]
        local map = native.map
        if (entity.type == "resource") and (entity.force.name == "neutral") then
            registerResource(entity, map)
        else
            accountPlayerEntity(entity, native, true, false)
            if natives.safeBuildings then
                if natives.safeEntities[entity.type] or natives.safeEntities[entity.name] then
                    entity.destructible = false
                end
            end
        end
    end
end

local function onMine(event)
    local entity = event.entity
    if entity.valid then
        local native = natives[entity.surface.index]
        local map = native.map
        if (entity.type == "resource") and (entity.force.name == "neutral") then
            if (entity.amount == 0) then
                unregisterResource(entity, map)
            end
        else
            accountPlayerEntity(entity, native, false, false)
        end
    end
end

local function onDeath(event)
    local entity = event.entity
    if entity.valid then
        local surface = entity.surface
        local native = natives[surface.index]
        local map = native.map
        local entityPosition = entity.position
        local chunk = getChunkByPosition(map, entityPosition)
        local cause = event.cause
        local tick = event.tick
        local entityType = entity.type
        if (entity.force.name == "enemy") then

            local artilleryBlast = (cause and
                                    ((cause.type == "artillery-wagon") or (cause.type == "artillery-turret")))

            if artilleryBlast then
                native.artilleryBlasts = native.artilleryBlasts + 1
            end

            if (entityType == "unit") then
                if (chunk ~= -1) and event.force and (event.force.name ~= "enemy") then
                    -- drop death pheromone where unit died
                    deathScent(map, chunk)

                    if (-getDeathGenerator(map, chunk) < -native.retreatThreshold) and cause and cause.valid then
                        retreatUnits(chunk,
                                     cause,
                                     map,
                                     tick,
                                     (artilleryBlast and RETREAT_SPAWNER_GRAB_RADIUS) or RETREAT_GRAB_RADIUS)
                    end

                    native.lostEnemyUnits = native.lostEnemyUnits + 1

                    if (mRandom() < native.rallyThreshold) and not surface.peaceful_mode then
                        rallyUnits(chunk, map, tick)
                    end
                end
            elseif event.force and (event.force.name ~= "enemy") and
                ((entityType == "unit-spawner") or (entityType == "turret"))
            then
                native.points = native.points +
                    (((entityType == "unit-spawner") and RECOVER_NEST_COST) or RECOVER_WORM_COST)

                unregisterEnemyBaseStructure(map, entity)

                if (chunk ~= -1) then
                    rallyUnits(chunk, map, tick)

                    if cause and cause.valid then
                        retreatUnits(chunk,
                                     cause,
                                     map,
                                     tick,
                                     RETREAT_SPAWNER_GRAB_RADIUS)
                    end
                end
            else
                local entityUnitNumber = entity.unit_number
                local pair = native.drainPylons[entityUnitNumber]
                if pair then
                    local target = pair[1]
                    local pole = pair[2]
                    if target == entity then
                        native.drainPylons[entityUnitNumber] = nil
                        if pole.valid then
                            native.drainPylons[pole.unit_number] = nil
                            pole.die()
                        end
                    elseif (pole == entity) then
                        native.drainPylons[entityUnitNumber] = nil
                        if target.valid then
                            native.drainPylons[target.unit_number] = nil
                            target.destroy()
                        end
                    end
                end
            end

        elseif (entity.force.name ~= "enemy") then
            local creditNatives = false
            if (event.force ~= nil) and (event.force.name == "enemy") then
                creditNatives = true
                if (chunk ~= -1) then
                    victoryScent(map, chunk, entityType)
                end

                local drained = (entityType == "electric-turret") and map.chunkToDrained[chunk]
                if cause or (drained and (drained - tick) > 0) then
                    if ((cause and ENERGY_THIEF_LOOKUP[cause.name]) or (not cause)) then
                        local conversion = ENERGY_THIEF_CONVERSION_TABLE[entityType]
                        if conversion then
                            local newEntity = surface.create_entity({
                                    position=entity.position,
                                    name=convertTypeToDrainCrystal(entity.force.evolution_factor, conversion),
                                    direction=entity.direction
                            })
                            if (conversion == "pole") then
                                local targetEntity = surface.create_entity({
                                        position=entity.position,
                                        name="pylon-target-rampant",
                                        direction=entity.direction
                                })
                                targetEntity.backer_name = ""
                                local pair = {targetEntity, newEntity}
                                native.drainPylons[targetEntity.unit_number] = pair
                                native.drainPylons[newEntity.unit_number] = pair
                                local wires = entity.neighbours
                                if wires then
                                    for _,v in pairs(wires.copper) do
                                        if (v.valid) then
                                            newEntity.connect_neighbour(v);
                                        end
                                    end
                                    for _,v in pairs(wires.red) do
                                        if (v.valid) then
                                            newEntity.connect_neighbour({
                                                    wire = DEFINES_WIRE_TYPE_RED,
                                                    target_entity = v
                                            });
                                        end
                                    end
                                    for _,v in pairs(wires.green) do
                                        if (v.valid) then
                                            newEntity.connect_neighbour({
                                                    wire = DEFINES_WIRE_TYPE_GREEN,
                                                    target_entity = v
                                            });
                                        end
                                    end
                                end
                            elseif newEntity.backer_name then
                                newEntity.backer_name = ""
                            end
                        end
                    end
                end
            elseif (entity.type == "resource") and (entity.force.name == "neutral") then
                if (entity.amount == 0) then
                    unregisterResource(entity, map)
                end
            end
            if creditNatives and natives.safeBuildings and
                (natives.safeEntities[entityType] or natives.safeEntities[entity.name])
            then
                makeImmortalEntity(surface, entity)
            else
                accountPlayerEntity(entity, native, false, creditNatives)
            end
        end
    end
end

local function onEnemyBaseBuild(event)
    local entity = event.entity
    if entity.valid then
        local native = natives[entity.surface.index]
        local map = native.map
        local chunk = getChunkByPosition(map, entity.position)
        if (chunk ~= -1) then
            local base
            if natives.NEW_ENEMIES then
                base = findNearbyBase(map, chunk)
                if not base then
                    base = createBase(native,
                                      chunk,
                                      event.tick)
                end
                entity = upgradeEntity(entity,
                                       base.alignment,
                                       native,
                                       nil,
                                       true)
            end
            if entity and entity.valid then
                event.entity = registerEnemyBaseStructure(map, entity, base)
            end
        end
    end
end

local function onSurfaceTileChange(event)
    local surfaceIndex = event.surface_index or (event.robot and event.robot.surface and event.robot.surface.index)
    local map = maps.map[surfaceIndex]
    local surface = map.surface
    local chunks = {}
    local tiles = event.tiles
    if event.tile then
        if ((event.tile.name == "landfill") or sFind(event.tile.name, "water")) then
            for i=1,#tiles do
                local position = tiles[i].position
                local chunk = getChunkByPosition(map, position)

                if (chunk ~= -1) then
                    map.chunkToPassScan[chunk] = true
                else
                    local x,y = positionToChunkXY(position)
                    local addMe = true
                    for ci=1,#chunks do
                        local c = chunks[ci]
                        if (c.x == x) and (c.y == y) then
                            addMe = false
                            break
                        end
                    end
                    if addMe then
                        local chunkXY = {x=x,y=y}
                        chunks[#chunks+1] = chunkXY
                        onChunkGenerated({area = { left_top = chunkXY },
                                          surface = surface})
                    end
                end
            end
        end
    else
        for i=1,#tiles do
            local tile = tiles[i]
            if (tile.name == "landfill") or sFind(tile.name, "water") then
                local position = tile.position
                local chunk = getChunkByPosition(map, position)

                if (chunk ~= -1) then
                    map.chunkToPassScan[chunk] = true
                else
                    local x,y = positionToChunkXY(position)
                    local addMe = true
                    for ci=1,#chunks do
                        local c = chunks[ci]
                        if (c.x == x) and (c.y == y) then
                            addMe = false
                            break
                        end
                    end
                    if addMe then
                        local chunkXY = {x=x,y=y}
                        chunks[#chunks+1] = chunkXY
                        onChunkGenerated({area = { left_top = chunkXY },
                                          surface = surface})
                    end
                end
            end
        end
    end
end

local function onResourceDepleted(event)
    local entity = event.entity
    if entity.valid then
        local map = maps.map[entity.surface.index]
        unregisterResource(entity, map)
    end
end

local function onRobotCliff(event)
    local entity = event.robot
    if entity.valid then
        local map = maps.map[entity.surface.index]
        if (event.item.name == "cliff-explosives") then
            entityForPassScan(map, event.cliff)
        end
    end
end

local function onUsedCapsule(event)
    local surface = game.players[event.player_index].surface
    local map = maps.map[surface.index]
    if (event.item.name == "cliff-explosives") then
        map.position2Top.x = event.position.x-0.75
        map.position2Top.y = event.position.y-0.75
        map.position2Bottom.x = event.position.x+0.75
        map.position2Bottom.y = event.position.y+0.75
        local cliffs = surface.find_entities_filtered(map.cliffQuery)
        for i=1,#cliffs do
            entityForPassScan(map, cliffs[i])
        end
    end
end

local function onRocketLaunch(event)
    local entity = event.rocket_silo or event.rocket
    if entity.valid then
        local native = natives[entity.surface.index]
        native.rocketLaunched = native.rocketLaunched + 1
        native.points = native.points + 5000
    end
end

local function onTriggerEntityCreated(event)
    local entity = event.entity
    if entity.valid and (entity.name  == "drain-trigger-rampant") then
        local map = maps.map[entity.surface.index]
        local chunk = getChunkByPosition(map, entity.position)
        if (chunk ~= -1) then
            map.chunkToDrained[chunk] = event.tick + 60
        end
        entity.destroy()
    end
end

local function onInit()
    global.maps = {}
    global.natives = {}
    global.queriesAndCommands = {}

    maps = global.maps
    queriesAndCommands = global.queriesAndCommands
    natives = global.natives

    hookEvents()
    onConfigChanged()
    prepWorld(game.surfaces["nauvis"])
end

local function onEntitySpawned(event)
    local entity = event.mine
    if natives.NEW_ENEMIES and entity.valid then
        local native = natives[entity.surface.index]
        local map = native.map
        if natives.buildingHiveTypeLookup[entity.name] then
            local disPos = mathUtils.distortPosition(entity.position, 8)

            local chunk = getChunkByPosition(map, disPos)
            if (chunk ~= -1) then
                local base = findNearbyBase(map, chunk)
                if not base then
                    base = createBase(native,
                                      chunk,
                                      event.tick)
                end

                entity = upgradeEntity(entity,
                                       base.alignment,
                                       native,
                                       disPos)

                if entity and entity.valid then
                    event.entity = registerEnemyBaseStructure(map, entity, base)
                end
            else
                entity.destroy()
            end
        end
    end
end

local function onUnitGroupCreated(event)
    local group = event.group
    if (group.force.name == "enemy") then
        local surface = group.surface
        local squad
        if not group.is_script_driven then
            local native = natives[surface.index]
            if not natives.aiNocturnalMode then
                local settler = mRandom() < 0.25 and
                    canMigrate(native) and
                    (natives.builderCount < natives.AI_MAX_BUILDER_COUNT)

                if not settler and natives.squadCount > natives.AI_MAX_SQUAD_COUNT then
                    group.destroy()
                    native.points = native.points + AI_SQUAD_COST
                    return
                end

                squad = createSquad(nil, nil, group, settler)
                native.groupNumberToSquad[group.group_number] = squad

                if settler then
                    natives.builderCount = natives.builderCount + 1
                else
                    natives.squadCount = natives.squadCount + 1
                end
            else
                if not (surface.darkness > 0.65) then
                    native.points = native.points + AI_SQUAD_COST
                    group.destroy()
                    return
                end

                local settler = mRandom() < 0.25 and
                    canMigrate(native) and
                    (natives.builderCount < natives.AI_MAX_BUILDER_COUNT)

                if not settler and natives.squadCount > natives.AI_MAX_SQUAD_COUNT then
                    group.destroy()
                    native.points = native.points + AI_SQUAD_COST
                    return
                end

                squad = createSquad(nil, nil, group, settler)
                natives.groupNumberToSquad[group.group_number] = squad
                if settler then
                    natives.builderCount = natives.builderCount + 1
                else
                    natives.squadCount = natives.squadCount + 1
                end
            end
        end
    end
end

local function onGroupFinishedGathering(event)
    local group = event.group
    if group.valid and (group.force.name == "enemy") then
        local unitNumber = group.group_number
        local native = natives[group.surface.index]
        local map = native.map
        local squad = native.groupNumberToSquad[unitNumber]
        if squad then
            if squad.settler then
                if (natives.builderCount < natives.AI_MAX_BUILDER_COUNT) then
                    squadDispatch(map, squad, unitNumber)
                else
                    group.destroy()
                    native.points = native.points + AI_SETTLER_COST
                end
            else
                if (natives.squadCount < natives.AI_MAX_SQUAD_COUNT) then
                    squadDispatch(map, squad, unitNumber)
                else
                    group.destroy()
                    native.points = native.points + AI_SQUAD_COST
                end
            end
        else
            local settler = mRandom() < 0.25 and
                canMigrate(native) and
                (natives.builderCount < natives.AI_MAX_BUILDER_COUNT)

            if not settler and natives.squadCount > natives.AI_MAX_SQUAD_COUNT then
                group.destroy()
                native.points = native.points + AI_SQUAD_COST
                return
            end

            squad = createSquad(nil, nil, group, settler)
            native.groupNumberToSquad[group.group_number] = squad
            if settler then
                natives.builderCount = natives.builderCount + 1
            else
                natives.squadCount = natives.squadCount + 1
            end
            squadDispatch(map, squad, unitNumber)
        end
    end
end

local function onForceCreated(event)
    queriesAndCommands.activePlayerForces[#queriesAndCommands.activePlayerForces+1] = event.force.name
end

local function onForceMerged(event)
    for i=#queriesAndCommands.activePlayerForces,1,-1 do
        if (queriesAndCommands.activePlayerForces[i] == event.source_name) then
            tRemove(queriesAndCommands.activePlayerForces, i)
            break
        end
    end
end

local function onSurfaceCreated(event)

end

local function onSurfaceCleared(event)
    local surfaceIndex = event.surface_index
    -- maps.map[surfaceIndex] = nil
    -- natives[surfaceIndex] = nil
end

local function onSurfaceDeleted(event)
    local surfaceIndex = event.surface_index
    maps.map[surfaceIndex] = nil
    natives[surfaceIndex] = nil
end

local function onBuilderArrived(event)
    local builder = event.group
    if not (builder and builder.valid) then
        builder = event.unit
        if not (builder and builder.valid and builder.force.name == "enemy") then
            return
        end
    elseif (builder.force.name ~= "enemy") then
        return
    end
    local targetPosition = queriesAndCommands.position
    targetPosition.x = builder.position.x
    targetPosition.y = builder.position.y

    builder.surface.create_entity(queriesAndCommands.createBuildCloudQuery)
end

-- hooks

script.on_event(defines.events.on_tick,
                function ()
                    local gameRef = game
                    local tick = gameRef.tick
                    local pick = tick % 7

                    -- local profiler = game.create_profiler()
                    local map = maps.activeMap
                    if (not map) or (map.processedChunks > map.totalChunks) then
                        maps.mapIterator, map = next(maps.map, maps.mapIterator)
                        maps.activeMap = map
                        print("initial", map, maps.mapIterator)
                    end
                    if not map then
                        print("after", map, maps.mapIterator)
                        maps.mapIterator, map = next(maps.map, maps.mapIterator)
                        if not map then
                            return
                        end
                    end
                    local native = map.native

                    if (pick == 0) then
                        processPendingChunks(map, tick)
                        processScanChunks(map)
                    elseif (pick == 1) then
                        processPlayers(gameRef.connected_players, map, tick)
                        cleanUpMapTables(map, tick)
                    elseif (pick == 2) then
                        processMap(map, tick)
                        planning(natives, native, gameRef.forces.enemy.evolution_factor, tick)
                        if natives.NEW_ENEMIES then
                            recycleBases(native, tick)
                        end
                    elseif (pick == 3) then
                        processStaticMap(map)
                        disperseVictoryScent(map)
                        cleanSquads(natives, native)
                    elseif (pick == 4) then
                        scanResourceMap(map, tick)
                        processVengence(map)
                    elseif (pick == 5) then
                        scanEnemyMap(map, tick)
                        processSpawners(map, tick)
                        temperamentPlanner(native)
                    elseif (pick == 6) then
                        scanPlayerMap(map, tick)
                        processNests(map, tick)
                    end

                    processActiveNests(map, tick)
                    -- game.print({"", "--dispatch4 ", profiler, ", ", game.tick, "       ", mRandom()})
end)

script.on_event(defines.events.on_surface_cleared, onSurfaceCleared)
script.on_event(defines.events.on_surface_deleted, onSurfaceDeleted)

script.on_init(onInit)
script.on_load(onLoad)
script.on_event(defines.events.on_runtime_mod_setting_changed, onModSettingsChange)
script.on_configuration_changed(onConfigChanged)

script.on_event(defines.events.on_resource_depleted, onResourceDepleted)
script.on_event({defines.events.on_player_built_tile,
                 defines.events.on_robot_built_tile,
                 defines.events.script_raised_set_tiles}, onSurfaceTileChange)

script.on_event(defines.events.on_player_used_capsule, onUsedCapsule)

script.on_event(defines.events.on_trigger_created_entity, onTriggerEntityCreated)

script.on_event(defines.events.on_pre_robot_exploded_cliff, onRobotCliff)

script.on_event(defines.events.on_biter_base_built, onEnemyBaseBuild)
script.on_event({defines.events.on_player_mined_entity,
                 defines.events.on_robot_mined_entity}, onMine)

script.on_event({defines.events.on_built_entity,
                 defines.events.on_robot_built_entity,
                 defines.events.script_raised_built,
                 defines.events.script_raised_revive}, onBuild)

script.on_event(defines.events.on_land_mine_armed, onEntitySpawned)

script.on_event(defines.events.on_rocket_launched, onRocketLaunch)
script.on_event({defines.events.on_entity_died,
                 defines.events.script_raised_destroy}, onDeath)
script.on_event(defines.events.on_chunk_generated, onChunkGenerated)
script.on_event(defines.events.on_unit_group_created, onUnitGroupCreated)
script.on_event(defines.events.on_force_created, onForceCreated)
script.on_event(defines.events.on_forces_merged, onForceMerged)
script.on_event(defines.events.on_unit_group_finished_gathering, onGroupFinishedGathering)

script.on_event(defines.events.on_build_base_arrived, onBuilderArrived)

remote.add_interface("rampantTests",
                     {
                         pheromoneLevels = tests.pheromoneLevels,
                         activeSquads = tests.activeSquads,
                         entitiesOnPlayerChunk = tests.entitiesOnPlayerChunk,
                         findNearestPlayerEnemy = tests.findNearestPlayerEnemy,
                         morePoints = tests.morePoints,
                         aiStats = tests.aiStats,
                         dumpEnvironment = tests.dumpEnvironment,
                         fillableDirtTest = tests.fillableDirtTest,
                         tunnelTest = tests.tunnelTest,
                         dumpNatives = tests.dumpatives,
                         createEnemy = tests.createEnemy,
                         attackOrigin = tests.attackOrigin,
                         cheatMode = tests.cheatMode,
                         gaussianRandomTest = tests.gaussianRandomTest,
                         reveal = tests.reveal,
                         showMovementGrid = tests.showMovementGrid,
                         showBaseGrid = tests.showBaseGrid,
                         baseStats = tests.baseStats,
                         mergeBases = tests.mergeBases,
                         clearBases = tests.clearBases,
                         getOffsetChunk = tests.getOffsetChunk,
                         registeredNest = tests.registeredNest,
                         colorResourcePoints = tests.colorResourcePoints,
                         entityStats = tests.entityStats,
                         stepAdvanceTendrils = tests.stepAdvanceTendrils,
                         unitGroupBuild = tests.unitGroupBuild,
                         exportAiState = tests.exportAiState(nil),
                         createEnergyTest = tests.createEnergyTest,
                         killActiveSquads = tests.killActiveSquads,
                         scanChunkPaths = tests.scanChunkPaths
                     }
)

-- remote.add_interface("rampant", interop)
