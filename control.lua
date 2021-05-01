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
local stringUtils = require("libs/StringUtils")

-- constants

local AI_SQUAD_COST = constants.AI_SQUAD_COST
local AI_SETTLER_COST = constants.AI_SETTLER_COST

local RECOVER_NEST_COST = constants.RECOVER_NEST_COST
local RECOVER_WORM_COST = constants.RECOVER_WORM_COST

local RETREAT_GRAB_RADIUS = constants.RETREAT_GRAB_RADIUS

local RETREAT_SPAWNER_GRAB_RADIUS = constants.RETREAT_SPAWNER_GRAB_RADIUS

local PROCESS_QUEUE_SIZE = constants.PROCESS_QUEUE_SIZE

local DEFINES_WIRE_TYPE_RED = defines.wire_type.red
local DEFINES_WIRE_TYPE_GREEN = defines.wire_type.green

local ENERGY_THIEF_CONVERSION_TABLE = constants.ENERGY_THIEF_CONVERSION_TABLE
local ENERGY_THIEF_LOOKUP = constants.ENERGY_THIEF_LOOKUP

-- imported functions

local isRampantSetting = stringUtils.isRampantSetting

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

local universe -- manages the chunks that make up the game universe

-- hook functions

local function onIonCannonFired(event)
    --[[
        event.force, event.surface, event.player_index, event.position, event.radius
    --]]
    local map = universe.maps[event.surface.index]
    map.ionCannonBlasts = map.ionCannonBlasts + 1
    map.points = map.points + 4000

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
    universe = global.universe

    hookEvents()
end

local function onChunkGenerated(event)
    -- queue generated chunk for delayed processing, queuing is required because
    -- some mods (RSO) mess with chunk as they are generated, which messes up the
    -- scoring.
    universe.maps[event.surface.index].pendingChunks[event] = true
end

local function onModSettingsChange(event)

    if not isRampantSetting(event.setting) then
        return
    end

    upgrade.compareTable(universe,
                         "safeBuildings",
                         settings.global["rampant--safeBuildings"].value)
    upgrade.compareTable(universe.safeEntities,
                         "curved-rail",
                         settings.global["rampant--safeBuildings-curvedRail"].value)
    upgrade.compareTable(universe.safeEntities,
                         "straight-rail",
                         settings.global["rampant--safeBuildings-straightRail"].value)
    upgrade.compareTable(universe.safeEntities,
                         "rail-signal",
                         settings.global["rampant--safeBuildings-railSignals"].value)
    upgrade.compareTable(universe.safeEntities,
                         "rail-chain-signal",
                         settings.global["rampant--safeBuildings-railChainSignals"].value)
    upgrade.compareTable(universe.safeEntities,
                         "train-stop",
                         settings.global["rampant--safeBuildings-trainStops"].value)
    upgrade.compareTable(universe.safeEntities,
                         "lamp",
                         settings.global["rampant--safeBuildings-lamps"].value)

    local changed, newValue = upgrade.compareTable(universe.safeEntities,
                                                   "big-electric-pole",
                                                   settings.global["rampant--safeBuildings-bigElectricPole"].value)
    if changed then
        universe.safeEntities["big-electric-pole"] = newValue
        universe.safeEntities["big-electric-pole-2"] = newValue
        universe.safeEntities["big-electric-pole-3"] = newValue
        universe.safeEntities["big-electric-pole-4"] = newValue
        universe.safeEntities["lighted-big-electric-pole-4"] = newValue
        universe.safeEntities["lighted-big-electric-pole-3"] = newValue
        universe.safeEntities["lighted-big-electric-pole-2"] = newValue
        universe.safeEntities["lighted-big-electric-pole"] = newValue
    end

    upgrade.compareTable(universe,
                         "temperamentRateModifier",
                         settings.global["rampant--temperamentRateModifier"].value)

    upgrade.compareTable(universe,
                         "deadZoneFrequency",
                         settings.global["rampant--deadZoneFrequency"].value)
    upgrade.compareTable(universe,
                         "raidAIToggle",
                         settings.global["rampant--raidAIToggle"].value)
    upgrade.compareTable(universe,
                         "siegeAIToggle",
                         settings.global["rampant--siegeAIToggle"].value)

    upgrade.compareTable(universe,
                         "attackPlayerThreshold",
                         settings.global["rampant--attackPlayerThreshold"].value)
    upgrade.compareTable(universe,
                         "attackUsePlayer",
                         settings.global["rampant--attackWaveGenerationUsePlayerProximity"].value)

    upgrade.compareTable(universe,
                         "attackWaveMaxSize",
                         settings.global["rampant--attackWaveMaxSize"].value)
    upgrade.compareTable(universe,
                         "aiNocturnalMode",
                         settings.global["rampant--permanentNocturnal"].value)
    upgrade.compareTable(universe,
                         "aiPointsScaler",
                         settings.global["rampant--aiPointsScaler"].value)

    universe.enabledMigration = universe.expansion and settings.global["rampant--enableMigration"].value
    universe.peacefulAIToggle = settings.global["rampant--peacefulAIToggle"].value
    universe.printAIStateChanges = settings.global["rampant--printAIStateChanges"].value
    universe.debugTemperament = settings.global["rampant--debugTemperament"].value

    upgrade.compareTable(universe,
                         "AI_MAX_SQUAD_COUNT",
                         settings.global["rampant--maxNumberOfSquads"].value)
    upgrade.compareTable(universe,
                         "AI_MAX_BUILDER_COUNT",
                         settings.global["rampant--maxNumberOfBuilders"].value)

    return true
end

local function prepMap(surface)
    surface.print("Rampant - Indexing surface:" .. tostring(surface.index) .. ", please wait.")

    local surfaceIndex = surface.index

    if not universe.maps then
        universe.maps = {}
    end

    local map = universe.maps[surfaceIndex]
    if not map then
        map = {}
        universe.maps[surfaceIndex] = map
    end

    map.processedChunks = 0
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

    map.surface = surface
    map.universe = universe

    map.vengenceQueue = {}
    map.bases = {}
    map.baseIndex = 1
    map.baseIncrement = 0
    map.points = 0
    map.state = constants.AI_STATE_AGGRESSIVE
    map.baseId = 0
    map.squads = nil
    map.pendingAttack = nil
    map.building = nil

    map.evolutionLevel = game.forces.enemy.evolution_factor
    map.canAttackTick = 0
    map.drainPylons = {}
    map.groupNumberToSquad = {}
    map.activeRaidNests = 0
    map.activeNests = 0
    map.destroyPlayerBuildings = 0
    map.lostEnemyUnits = 0
    map.lostEnemyBuilding = 0
    map.rocketLaunched = 0
    map.builtEnemyBuilding = 0
    map.ionCannonBlasts = 0
    map.artilleryBlasts = 0

    map.temperament = 0.5
    map.temperamentScore = 0
    map.stateTick = 0

    -- queue all current chunks that wont be generated during play
    local tick = game.tick
    local position = {0,0}
    map.nextChunkSort = 0
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

    processPendingChunks(map, tick, true)
end

local function onConfigChanged()
    local version = upgrade.attempt(universe)
    if version then
        if not universe then
            universe = global.universe
        end
    end

    onModSettingsChange({setting="rampant--"})

    upgrade.compareTable(universe,
                         "ENEMY_SEED",
                         settings.startup["rampant--enemySeed"].value)
    upgrade.compareTable(universe,
                         "ENEMY_VARIATIONS",
                         settings.startup["rampant--newEnemyVariations"].value)
    upgrade.compareTable(universe,
                         "NEW_ENEMIES",
                         settings.startup["rampant--newEnemies"].value)

    if universe.NEW_ENEMIES then
        rebuildNativeTables(universe, game.create_random_generator(universe.ENEMY_SEED))
    else
        universe.buildingHiveTypeLookup = {}
        universe.buildingHiveTypeLookup["biter-spawner"] = "biter-spawner"
        universe.buildingHiveTypeLookup["spitter-spawner"] = "spitter-spawner"
        universe.buildingHiveTypeLookup["small-worm-turret"] = "turret"
        universe.buildingHiveTypeLookup["medium-worm-turret"] = "turret"
        universe.buildingHiveTypeLookup["big-worm-turret"] = "turret"
        universe.buildingHiveTypeLookup["behemoth-worm-turret"] = "turret"
    end

    for _,surface in pairs(game.surfaces) do
        if not universe.maps then
            universe.maps = {}
        end
        if not universe.maps[surface.index] then
            prepMap(surface)
        end
    end
end

local function onBuild(event)
    local entity = event.created_entity or event.entity
    if entity.valid then
        local map = universe.maps[entity.surface.index]
        if (entity.type == "resource") and (entity.force.name == "neutral") then
            registerResource(entity, map)
        else
            accountPlayerEntity(entity, map, true, false)
            if universe.safeBuildings then
                if universe.safeEntities[entity.type] or universe.safeEntities[entity.name] then
                    entity.destructible = false
                end
            end
        end
    end
end

local function onMine(event)
    local entity = event.entity
    if entity.valid then
        local map = universe.maps[entity.surface.index]
        if (entity.type == "resource") and (entity.force.name == "neutral") then
            if (entity.amount == 0) then
                unregisterResource(entity, map)
            end
        else
            accountPlayerEntity(entity, map, false, false)
        end
    end
end

local function onDeath(event)
    local entity = event.entity
    if entity.valid then
        local surface = entity.surface
        local map = universe.maps[surface.index]
        local entityPosition = entity.position
        local chunk = getChunkByPosition(map, entityPosition)
        local cause = event.cause
        local tick = event.tick
        local entityType = entity.type
        if (entity.force.name == "enemy") then

            local artilleryBlast = (cause and
                                    ((cause.type == "artillery-wagon") or (cause.type == "artillery-turret")))

            if artilleryBlast then
                map.artilleryBlasts = map.artilleryBlasts + 1
            end

            if (entityType == "unit") then
                if (chunk ~= -1) and event.force and (event.force.name ~= "enemy") then

                    -- local group = entity.unit_group
                    -- if group then
                    --     local damageType = event.damage_type
                    --     local squad = map.groupNumberToSquad[group.group_number]
                    --     if squad then
                    --         local base = squad.base
                    --         if base then
                    --             base.damagedBy[damageType] = (base.damagedBy[damageType] or 0) + 0.01
                    --             base.deathEvents = base.deathEvents + 1
                    --         end
                    --     end
                    -- end

                    -- drop death pheromone where unit died
                    deathScent(map, chunk)

                    if (-getDeathGenerator(map, chunk) < -universe.retreatThreshold) and cause and cause.valid then
                        retreatUnits(chunk,
                                     cause,
                                     map,
                                     tick,
                                     (artilleryBlast and RETREAT_SPAWNER_GRAB_RADIUS) or RETREAT_GRAB_RADIUS)
                    end

                    map.lostEnemyUnits = map.lostEnemyUnits + 1

                    if (mRandom() < universe.rallyThreshold) and not surface.peaceful_mode then
                        rallyUnits(chunk, map, tick)
                    end
                end
            elseif event.force and (event.force.name ~= "enemy") and
                ((entityType == "unit-spawner") or (entityType == "turret"))
            then
                map.points = map.points +
                    (((entityType == "unit-spawner") and RECOVER_NEST_COST) or RECOVER_WORM_COST)

                unregisterEnemyBaseStructure(map, entity, event.damage_type)

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
                local pair = map.drainPylons[entityUnitNumber]
                if pair then
                    local target = pair[1]
                    local pole = pair[2]
                    if target == entity then
                        map.drainPylons[entityUnitNumber] = nil
                        if pole.valid then
                            map.drainPylons[pole.unit_number] = nil
                            pole.die()
                        end
                    elseif (pole == entity) then
                        map.drainPylons[entityUnitNumber] = nil
                        if target.valid then
                            map.drainPylons[target.unit_number] = nil
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
                                map.drainPylons[targetEntity.unit_number] = pair
                                map.drainPylons[newEntity.unit_number] = pair
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
            if creditNatives and universe.safeBuildings and
                (universe.safeEntities[entityType] or universe.safeEntities[entity.name])
            then
                makeImmortalEntity(surface, entity)
            else
                accountPlayerEntity(entity, map, false, creditNatives)
            end
        end
    end
end

local function onEnemyBaseBuild(event)
    local entity = event.entity
    if entity.valid then
        local map = universe.maps[entity.surface.index]
        local chunk = getChunkByPosition(map, entity.position)
        if (chunk ~= -1) then
            local base
            if universe.NEW_ENEMIES then
                base = findNearbyBase(map, chunk)
                if not base then
                    base = createBase(map,
                                      chunk,
                                      event.tick)
                end
                entity = upgradeEntity(entity,
                                       base.alignment,
                                       map,
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
    local map = universe.maps[surfaceIndex]
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
        unregisterResource(entity, universe.maps[entity.surface.index])
    end
end

local function onRobotCliff(event)
    local entity = event.robot
    if entity.valid then
        local map = universe.maps[entity.surface.index]
        if (event.item.name == "cliff-explosives") then
            entityForPassScan(map, event.cliff)
        end
    end
end

local function onUsedCapsule(event)
    local surface = game.players[event.player_index].surface
    local map = universe.maps[surface.index]
    if (event.item.name == "cliff-explosives") then
        local position2Top = universe.position2Top
        local position2Bottom = universe.position2Bottom
        position2Top.x = event.position.x-0.75
        position2Top.y = event.position.y-0.75
        position2Bottom.x = event.position.x+0.75
        position2Bottom.y = event.position.y+0.75
        local cliffs = surface.find_entities_filtered(universe.cliffQuery)
        for i=1,#cliffs do
            entityForPassScan(map, cliffs[i])
        end
    end
end

local function onRocketLaunch(event)
    local entity = event.rocket_silo or event.rocket
    if entity.valid then
        local map = universe.maps[entity.surface.index]
        map.rocketLaunched = map.rocketLaunched + 1
        map.points = map.points + 5000
    end
end

local function onTriggerEntityCreated(event)
    local entity = event.entity
    if entity.valid and (entity.name  == "drain-trigger-rampant") then
        local map = universe.maps[entity.surface.index]
        local chunk = getChunkByPosition(map, entity.position)
        if (chunk ~= -1) then
            map.chunkToDrained[chunk] = event.tick + 60
        end
        entity.destroy()
    end
end

local function onInit()
    global.universe = {}

    universe = global.universe

    hookEvents()
    onConfigChanged()
end

local function onEntitySpawned(event)
    local entity = event.mine
    if universe.NEW_ENEMIES and entity.valid then
        local map = universe.maps[entity.surface.index]
        if universe.buildingHiveTypeLookup[entity.name] then
            local disPos = mathUtils.distortPosition(entity.position, 8)

            local chunk = getChunkByPosition(map, disPos)
            if (chunk ~= -1) then
                local base = findNearbyBase(map, chunk)
                if not base then
                    base = createBase(map,
                                      chunk,
                                      event.tick)
                end

                entity = upgradeEntity(entity,
                                       base.alignment,
                                       map,
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
            local map = universe.maps[surface.index]
            if not universe.aiNocturnalMode then
                local settler = mRandom() < 0.25 and
                    canMigrate(map) and
                    (universe.builderCount < universe.AI_MAX_BUILDER_COUNT)

                if not settler and universe.squadCount > universe.AI_MAX_SQUAD_COUNT then
                    group.destroy()
                    map.points = map.points + AI_SQUAD_COST
                    return
                end

                squad = createSquad(nil, nil, group, settler)
                map.groupNumberToSquad[group.group_number] = squad

                -- if universe.NEW_ENEMIES then
                --     local chunk = getChunkByPosition(map, group.position)
                --     if (chunk ~= -1) then
                --         squad.base = findNearbyBase(map, chunk)
                --     end
                -- end

                if settler then
                    universe.builderCount = universe.builderCount + 1
                else
                    universe.squadCount = universe.squadCount + 1
                end
            else
                if not (surface.darkness > 0.65) then
                    map.points = map.points + AI_SQUAD_COST
                    group.destroy()
                    return
                end

                local settler = mRandom() < 0.25 and
                    canMigrate(map) and
                    (universe.builderCount < universe.AI_MAX_BUILDER_COUNT)

                if not settler and universe.squadCount > universe.AI_MAX_SQUAD_COUNT then
                    group.destroy()
                    map.points = map.points + AI_SQUAD_COST
                    return
                end

                squad = createSquad(nil, nil, group, settler)
                map.groupNumberToSquad[group.group_number] = squad

                -- if universe.NEW_ENEMIES then
                --     local chunk = getChunkByPosition(group.position)
                --     if (chunk ~= -1) then
                --         squad.base = findNearbyBase(map, chunk)
                --     end
                -- end

                if settler then
                    universe.builderCount = universe.builderCount + 1
                else
                    universe.squadCount = universe.squadCount + 1
                end
            end
        end
    end
end

local function onGroupFinishedGathering(event)
    local group = event.group
    if group.valid and (group.force.name == "enemy") then
        local map = universe.maps[group.surface.index]
        local squad = map.groupNumberToSquad[group.group_number]
        if squad then
            if squad.settler then
                if (universe.builderCount < universe.AI_MAX_BUILDER_COUNT) then
                    squadDispatch(map, squad)
                else
                    group.destroy()
                    map.points = map.points + AI_SETTLER_COST
                end
            else
                if (universe.squadCount < universe.AI_MAX_SQUAD_COUNT) then
                    squadDispatch(map, squad)
                else
                    group.destroy()
                    map.points = map.points + AI_SQUAD_COST
                end
            end
        else
            local settler = mRandom() < 0.25 and
                canMigrate(map) and
                (universe.builderCount < universe.AI_MAX_BUILDER_COUNT)

            if not settler and universe.squadCount > universe.AI_MAX_SQUAD_COUNT then
                group.destroy()
                map.points = map.points + AI_SQUAD_COST
                return
            end

            squad = createSquad(nil, nil, group, settler)
            map.groupNumberToSquad[group.group_number] = squad
            if settler then
                universe.builderCount = universe.builderCount + 1
            else
                universe.squadCount = universe.squadCount + 1
            end
            squadDispatch(map, squad)
        end
    end
end

local function onForceCreated(event)
    universe.activePlayerForces[#universe.activePlayerForces+1] = event.force.name
end

local function onForceMerged(event)
    for i=#universe.activePlayerForces,1,-1 do
        if (universe.activePlayerForces[i] == event.source_name) then
            tRemove(universe.activePlayerForces, i)
            break
        end
    end
end

local function onSurfaceCreated(event)
    prepMap(game.surfaces[event.surface_index])
end

local function onSurfaceDeleted(event)
    local surfaceIndex = event.surface_index
    if (universe.mapIterator == surfaceIndex) then
        universe.mapIterator, universe.activeMap = next(universe.maps, universe.mapIterator)
    end
    universe.maps[surfaceIndex] = nil
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
    local targetPosition = universe.position
    targetPosition.x = builder.position.x
    targetPosition.y = builder.position.y

    builder.surface.create_entity(universe.createBuildCloudQuery)
end

-- hooks

script.on_event(defines.events.on_tick,
                function ()
                    local gameRef = game
                    local tick = gameRef.tick
                    local pick = tick % 7

                    -- local profiler = game.create_profiler()

                    local map = universe.activeMap
                    if (not map) or (universe.processedChunks > #map.processQueue) then
                        universe.mapIterator, map = next(universe.maps, universe.mapIterator)
                        if not map then
                            universe.mapIterator, map = next(universe.maps, universe.mapIterator)
                        end
                        universe.processedChunks = 0
                        universe.activeMap = map
                    end

                    if (pick == 0) then
                        processPendingChunks(map, tick)
                        processScanChunks(map)
                        universe.processedChunks = universe.processedChunks + PROCESS_QUEUE_SIZE
                        planning(map, gameRef.forces.enemy.evolution_factor, tick)
                        if universe.NEW_ENEMIES then
                            recycleBases(map, tick)
                        end
                        cleanUpMapTables(map, tick)
                    elseif (pick == 1) then
                        processPlayers(gameRef.connected_players, map, tick)
                    elseif (pick == 2) then
                        processMap(map, tick)
                    elseif (pick == 3) then
                        processStaticMap(map)
                        disperseVictoryScent(map)
                        processVengence(map)
                    elseif (pick == 4) then
                        scanResourceMap(map, tick)
                    elseif (pick == 5) then
                        scanEnemyMap(map, tick)
                        processSpawners(map, tick)
                    elseif (pick == 6) then
                        scanPlayerMap(map, tick)
                        processNests(map, tick)
                        temperamentPlanner(map)
                    end

                    processActiveNests(map, tick)
                    cleanSquads(map)

                    -- game.print({"", "--dispatch4 ", profiler, ", ", pick, ", ", game.tick, "       ", mRandom()})
end)

script.on_event(defines.events.on_surface_deleted, onSurfaceDeleted)
script.on_event(defines.events.on_surface_cleared, onSurfaceCreated)
script.on_event(defines.events.on_surface_created, onSurfaceCreated)

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

-- testing

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
