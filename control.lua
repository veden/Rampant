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
local tests = require("tests")
local chunkUtils = require("libs/ChunkUtils")
local upgrade = require("Upgrade")
local aiPredicates = require("libs/AIPredicates")
local stringUtils = require("libs/StringUtils")
local queryUtils = require("libs/QueryUtils")

-- constants

local TICKS_A_MINUTE = constants.TICKS_A_MINUTE
local COMMAND_TIMEOUT = constants.COMMAND_TIMEOUT
local AI_SQUAD_COST = constants.AI_SQUAD_COST
local AI_SETTLER_COST = constants.AI_SETTLER_COST

local RECOVER_NEST_COST = constants.RECOVER_NEST_COST
local RECOVER_WORM_COST = constants.RECOVER_WORM_COST

local RETREAT_GRAB_RADIUS = constants.RETREAT_GRAB_RADIUS

local RETREAT_SPAWNER_GRAB_RADIUS = constants.RETREAT_SPAWNER_GRAB_RADIUS

-- imported functions

local setPointAreaInQuery = queryUtils.setPointAreaInQuery
local setPositionInQuery = queryUtils.setPositionInQuery

local nextMap = mapUtils.nextMap

local distortPosition = mathUtils.distortPosition
local prepMap = upgrade.prepMap

local processMapAIs = aiPlanning.processMapAIs

local registerEnemyBaseStructure = chunkUtils.registerEnemyBaseStructure

local queueGeneratedChunk = mapUtils.queueGeneratedChunk
local isRampantSetting = stringUtils.isRampantSetting

local processPendingUpgrades = chunkProcessor.processPendingUpgrades
local canMigrate = aiPredicates.canMigrate

local squadDispatch = squadAttack.squadDispatch

local cleanUpMapTables = mapProcessor.cleanUpMapTables

local positionToChunkXY = mapUtils.positionToChunkXY

local processVengence = mapProcessor.processVengence
local processAttackWaves = mapProcessor.processAttackWaves

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

local rallyUnits = aiAttackWave.rallyUnits

local recycleBases = baseUtils.recycleBases

local deathScent = pheromoneUtils.deathScent
local victoryScent = pheromoneUtils.victoryScent

local createSquad = unitGroupUtils.createSquad

local createBase = baseUtils.createBase
local findNearbyBase = baseUtils.findNearbyBase

local processActiveNests = mapProcessor.processActiveNests

local removeDrainPylons = chunkPropertyUtils.removeDrainPylons
local getDrainPylonPair = chunkPropertyUtils.getDrainPylonPair

local createDrainPylon = unitUtils.createDrainPylon

local isDrained = chunkPropertyUtils.isDrained
local setDrainedTick = chunkPropertyUtils.setDrainedTick
local getDeathGenerator = chunkPropertyUtils.getDeathGenerator

local retreatUnits = squadDefense.retreatUnits

local accountPlayerEntity = chunkUtils.accountPlayerEntity
local unregisterEnemyBaseStructure = chunkUtils.unregisterEnemyBaseStructure
local makeImmortalEntity = chunkUtils.makeImmortalEntity

local registerResource = chunkUtils.registerResource
local unregisterResource = chunkUtils.unregisterResource

local cleanSquads = squadAttack.cleanSquads

local upgradeEntity = baseUtils.upgradeEntity
local rebuildNativeTables = baseUtils.rebuildNativeTables

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
    if not map then
        return
    end
    map.ionCannonBlasts = map.ionCannonBlasts + 1
    map.points = map.points + 4000
    if universe.aiPointsPrintGainsToChat then
        game.print(map.surface.name .. ": Points: +" .. 4000 .. ". [Ion Cannon] Total: " .. string.format("%.2f", map.points))
    end

    local chunk = getChunkByPosition(map, event.position)
    if (chunk ~= -1) then
        rallyUnits(chunk, map, event.tick)
    end
end

local function onAbandonedRuins(event)
    local entity = event.entity
    if entity.valid and (entity.force.name ~= "enemy") then
        local map = universe.maps[entity.surface.index]
        if not map then
            return
        end
        accountPlayerEntity(entity, map, true, false)
    end
end

local function hookEvents()
    if settings.startup["ion-cannon-radius"] ~= nil then
        script.on_event(remote.call("orbital_ion_cannon", "on_ion_cannon_fired"),
                        onIonCannonFired)
    end
    if settings.global["AbandonedRuins-set"] ~= nil then
        script.on_event(remote.call("AbandonedRuins", "get_on_entity_force_changed_event"),
                        onAbandonedRuins)
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
    queueGeneratedChunk(universe, event)
end

local function onModSettingsChange(event)

    if not isRampantSetting(event.setting) then
        return
    end

    universe.safeEntities["curved-rail"] = settings.global["rampant--safeBuildings-curvedRail"].value
    universe.safeEntities["straight-rail"] = settings.global["rampant--safeBuildings-straightRail"].value
    universe.safeEntities["rail-signal"] = settings.global["rampant--safeBuildings-railSignals"].value
    universe.safeEntities["rail-chain-signal"] = settings.global["rampant--safeBuildings-railChainSignals"].value
    universe.safeEntities["train-stop"] = settings.global["rampant--safeBuildings-trainStops"].value
    universe.safeEntities["lamp"] = settings.global["rampant--safeBuildings-lamps"].value

    universe.safeEntities["big-electric-pole"] = settings.global["rampant--safeBuildings-bigElectricPole"].value

    universe.safeEntities["big-electric-pole"] = universe.safeEntities["big-electric-pole"]
    universe.safeEntities["big-electric-pole-2"] = universe.safeEntities["big-electric-pole"]
    universe.safeEntities["big-electric-pole-3"] = universe.safeEntities["big-electric-pole"]
    universe.safeEntities["big-electric-pole-4"] = universe.safeEntities["big-electric-pole"]
    universe.safeEntities["lighted-big-electric-pole-4"] = universe.safeEntities["big-electric-pole"]
    universe.safeEntities["lighted-big-electric-pole-3"] = universe.safeEntities["big-electric-pole"]
    universe.safeEntities["lighted-big-electric-pole-2"] = universe.safeEntities["big-electric-pole"]
    universe.safeEntities["lighted-big-electric-pole"] = universe.safeEntities["big-electric-pole"]

    universe["temperamentRateModifier"] = settings.global["rampant--temperamentRateModifier"].value
    universe["baseDistanceModifier"] = settings.global["rampant--baseDistanceModifier"].value
    universe["printBaseAdaptation"] = settings.global["rampant--printBaseAdaptation"].value
    universe["adaptationModifier"] = settings.global["rampant--adaptationModifier"].value

    universe["raidAIToggle"] = settings.global["rampant--raidAIToggle"].value
    universe["siegeAIToggle"] = settings.global["rampant--siegeAIToggle"].value
    universe["attackPlayerThreshold"] = settings.global["rampant--attackPlayerThreshold"].value
    universe["attackUsePlayer"] = settings.global["rampant--attackWaveGenerationUsePlayerProximity"].value

    universe["attackWaveMaxSize"] = settings.global["rampant--attackWaveMaxSize"].value
    universe["aiNocturnalMode"] = settings.global["rampant--permanentNocturnal"].value
    universe["aiPointsScaler"] = settings.global["rampant--aiPointsScaler"].value

    universe["aiPointsPrintGainsToChat"] = settings.global["rampant--aiPointsPrintGainsToChat"].value
    universe["aiPointsPrintSpendingToChat"] = settings.global["rampant--aiPointsPrintSpendingToChat"].value
    universe["printBaseUpgrades"] = settings.global["rampant--printBaseUpgrades"].value

    universe["enabledMigration"] = universe.expansion and settings.global["rampant--enableMigration"].value
    universe["peacefulAIToggle"] = settings.global["rampant--peacefulAIToggle"].value
    universe["printAIStateChanges"] = settings.global["rampant--printAIStateChanges"].value
    universe["debugTemperament"] = settings.global["rampant--debugTemperament"].value

    universe["AI_MAX_SQUAD_COUNT"] = settings.global["rampant--maxNumberOfSquads"].value
    universe["AI_MAX_BUILDER_COUNT"] = settings.global["rampant--maxNumberOfBuilders"].value
    universe["MAX_BASE_MUTATIONS"] = settings.global["rampant--max-base-mutations"].value

    universe["initialPeaceTime"] = settings.global["rampant--initialPeaceTime"].value * TICKS_A_MINUTE
    universe["printAwakenMessage"] = settings.global["rampant--printAwakenMessage"].value

    return true
end

local function onConfigChanged()
    local version = upgrade.attempt(universe)
    if version then
        if not universe then
            universe = global.universe
        end
    end

    onModSettingsChange({setting="rampant--"})

    universe["ENEMY_SEED"] = settings.startup["rampant--enemySeed"].value
    universe["ENEMY_VARIATIONS"] = settings.startup["rampant--newEnemyVariations"].value
    universe["NEW_ENEMIES"] = settings.startup["rampant--newEnemies"].value

    if universe.NEW_ENEMIES then
        rebuildNativeTables(universe, universe.random)
    else
        universe.buildingHiveTypeLookup = {}
        universe.buildingHiveTypeLookup["biter-spawner"] = "biter-spawner"
        universe.buildingHiveTypeLookup["spitter-spawner"] = "spitter-spawner"
        universe.buildingHiveTypeLookup["small-worm-turret"] = "turret"
        universe.buildingHiveTypeLookup["medium-worm-turret"] = "turret"
        universe.buildingHiveTypeLookup["big-worm-turret"] = "turret"
        universe.buildingHiveTypeLookup["behemoth-worm-turret"] = "turret"
    end

    -- not a completed implementation needs if checks to use all forces
    -- both in the data stage, commands, and if then logic
    local enemyForces = {
        ["enemy"]=true
    }

    local npcForces = {
        ["neutral"]=true
    }

    if game.active_mods["AbandonedRuins"] then
        npcForces["AbandonedRuins:enemy"] = true
    end

    upgrade.setCommandForces(universe, npcForces, enemyForces)

    if not universe.maps then
        universe.maps = {}
    end
    for _,surface in pairs(game.surfaces) do
        if not universe.maps[surface.index] then
            prepMap(universe, surface)
        end
    end
end

local function onBuild(event)
    local entity = event.created_entity or event.entity
    if entity.valid then
        local map = universe.maps[entity.surface.index]
        if not map then
            return
        end
        if (entity.type == "resource") and (entity.force.name == "neutral") then
            registerResource(entity, map)
        else
            accountPlayerEntity(entity, map, true, false)
            if universe.safeEntities[entity.type] or universe.safeEntities[entity.name] then
                entity.destructible = false
            end
        end
    end
end

local function onMine(event)
    local entity = event.entity
    if entity.valid then
        local map = universe.maps[entity.surface.index]
        if not map then
            return
        end
        accountPlayerEntity(entity, map, false, false)
    end
end

local function onDeath(event)
    local entity = event.entity
    if not entity.valid then
        return
    end
    local surface = entity.surface
    local map = universe.maps[surface.index]
    if not map then
        return
    end
    if (entity.force.name == "neutral") then
        if (entity.name == "cliff") then
            entityForPassScan(map, entity)
        end
        return
    end
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

                local group = entity.unit_group
                if group then
                    local damageType = event.damage_type
                    local squad = universe.groupNumberToSquad[group.group_number]
                    if damageType and squad then
                        local base = squad.base
                        if base then
                            local damageTypeName = damageType.name
                            base.damagedBy[damageTypeName] = (base.damagedBy[damageTypeName] or 0) + 0.01
                            base.deathEvents = base.deathEvents + 1
                        end
                    end
                end

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

                if (universe.random() < universe.rallyThreshold) and not surface.peaceful_mode then
                    rallyUnits(chunk, map, tick)
                end
            end

        elseif getDrainPylonPair(map, entity.unit_number) then
            removeDrainPylons(map, entity.unit_number)
        else
            if event.force and (event.force.name ~= "enemy") then
                local rally = false
                if (entityType == "unit-spawner") then
                    map.points = map.points + RECOVER_NEST_COST
                    if universe.aiPointsPrintGainsToChat then
                        game.print(map.surface.name .. ": Points: +" .. RECOVER_NEST_COST .. ". [Nest Lost] Total: " .. string.format("%.2f", map.points))
                    end
                    rally = true
                elseif (entityType == "turret") then
                    map.points = map.points + RECOVER_WORM_COST
                    if universe.aiPointsPrintGainsToChat then
                        game.print(map.surface.name .. ": Points: +" .. RECOVER_WORM_COST .. ". [Worm Lost] Total: " .. string.format("%.2f", map.points))
                    end
                    rally = true
                end
                if (chunk ~= -1) and rally then
                    rallyUnits(chunk, map, tick)

                    if cause and cause.valid then
                        retreatUnits(chunk,
                                     cause,
                                     map,
                                     tick,
                                     RETREAT_SPAWNER_GRAB_RADIUS)
                    end
                end
            end

            if universe.buildingHiveTypeLookup[entity.name] or
                (entityType == "unit-spawner") or
                (entityType == "turret")
            then
                unregisterEnemyBaseStructure(map, entity, event.damage_type)
            end
        end
    else
        local creditNatives = false
        if (event.force ~= nil) and (event.force.name == "enemy") then
            creditNatives = true
            local drained = false
            if (chunk ~= -1) then
                victoryScent(map, chunk, entityType)
                drained = (entityType == "electric-turret") and isDrained(map, chunk, tick)
            end

            if cause or drained then
                createDrainPylon(map, cause, entity, entityType)
            end
        end
        if creditNatives and (universe.safeEntities[entityType] or universe.safeEntities[entity.name])
        then
            makeImmortalEntity(surface, entity)
        else
            accountPlayerEntity(entity, map, false, creditNatives)
        end
    end
end

local function onEnemyBaseBuild(event)
    local entity = event.entity
    if entity.valid then
        local map = universe.maps[entity.surface.index]
        if not map then
            return
        end
        map.activeSurface = true
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

                registerEnemyBaseStructure(map, entity, event.tick, base)

                upgradeEntity(entity,
                              base,
                              map,
                              nil,
                              true,
                              true)
            end
        else
            local x,y = positionToChunkXY(entity.position)
            onChunkGenerated({
                    surface = entity.surface,
                    tick = event.tick,
                    area = {
                        left_top = {
                            x = x,
                            y = y
                        }
                    }
            })
        end
    end
end

local function processSurfaceTile(map, position, chunks, tick)
    local chunk = getChunkByPosition(map, position)

    if (chunk ~= -1) then
        if not map.universe.chunkToPassScan[chunk.id] then
            map.universe.chunkToPassScan[chunk.id] = {
                map=map,
                chunk=chunk
            }
        end
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
                              tick = tick,
                              surface = map.surface})
        end
    end
end

local function onSurfaceTileChange(event)
    local surfaceIndex = event.surface_index or (event.robot and event.robot.surface and event.robot.surface.index)
    local map = universe.maps[surfaceIndex]
    if not map then
        return
    end
    local chunks = {}
    local tiles = event.tiles
    if event.tile then
        if ((event.tile.name == "landfill") or sFind(event.tile.name, "water")) then
            for i=1,#tiles do
                processSurfaceTile(map, tiles[i].position, chunks, event.tick)
            end
        end
    else
        for i=1,#tiles do
            local tile = tiles[i]
            if (tile.name == "landfill") or sFind(tile.name, "water") then
                processSurfaceTile(map, tiles[i].position, chunks, event.tick)
            end
        end
    end
end

local function onResourceDepleted(event)
    local entity = event.entity
    if entity.valid then
        local map = universe.maps[entity.surface.index]
        if not map then
            return
        end
        unregisterResource(entity, map)
    end
end

local function onRobotCliff(event)
    local entity = event.robot
    if entity.valid then
        local map = universe.maps[entity.surface.index]
        if not map then
            return
        end
        if (event.item.name == "cliff-explosives") then
            entityForPassScan(map, event.cliff)
        end
    end
end

local function onUsedCapsule(event)
    local surface = game.players[event.player_index].surface
    local map = universe.maps[surface.index]
    if not map then
        return
    end
    if (event.item.name == "cliff-explosives") then
        setPointAreaInQuery(universe.oucCliffQuery, event.position, 0.75)
        local cliffs = surface.find_entities_filtered(universe.oucCliffQuery)
        for i=1,#cliffs do
            entityForPassScan(map, cliffs[i])
        end
    end
end

local function onRocketLaunch(event)
    local entity = event.rocket_silo or event.rocket
    if entity.valid then
        local map = universe.maps[entity.surface.index]
        if not map then
            return
        end
        map.rocketLaunched = map.rocketLaunched + 1
        map.points = map.points + 5000
        if universe.aiPointsPrintGainsToChat then
            game.print(map.surface.name .. ": Points: +" .. 5000 .. ". [Rocket Launch] Total: " .. string.format("%.2f", map.points))
        end
    end
end

local function onTriggerEntityCreated(event)
    if (event.effect_id == "rampant-drain-trigger") then
        local entity = event.target_entity
        if (entity and entity.valid) then
            local map = universe.maps[event.surface_index]
            if not map then
                return
            end
            local chunk = getChunkByPosition(map, entity.position)
            if (chunk ~= -1) then
                setDrainedTick(map, chunk, event.tick)
            end
        elseif (event.target_position) then
            local map = universe.maps[event.surface_index]
            if not map then
                return
            end
            local chunk = getChunkByPosition(map, event.target_position)
            if (chunk ~= -1) then
                setDrainedTick(map, chunk, event.tick)
            end
        end
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
        if not map then
            return
        end
        if universe.buildingHiveTypeLookup[entity.name] then
            map.activeSurface = true
            local disPos = distortPosition(universe.random, entity.position, 8)

            local chunk = getChunkByPosition(map, disPos)
            if (chunk ~= -1) then
                local base = findNearbyBase(map, chunk)
                if not base then
                    base = createBase(map,
                                      chunk,
                                      event.tick)
                end

                registerEnemyBaseStructure(map, entity, event.tick, base, true)

                upgradeEntity(entity,
                              base,
                              map,
                              disPos,
                              true,
                              true)
            else
                local x,y = positionToChunkXY(entity.position)
                onChunkGenerated({
                        surface = entity.surface,
                        tick = event.tick,
                        area = {
                            left_top = {
                                x = x,
                                y = y
                            }
                        }
                })
                entity.destroy()
            end
        end
    end
end

local function onUnitGroupCreated(event)
    local group = event.group
    if (group.force.name ~= "enemy") then
        return
    end
    local surface = group.surface
    local squad
    if group.is_script_driven then
        return
    end
    local map = universe.maps[surface.index]
    if not map then
        return
    end
    map.activeSurface = true
    if not universe.aiNocturnalMode then
        local settler = canMigrate(map) and
            (universe.builderCount < universe.AI_MAX_BUILDER_COUNT) and
            (universe.random() < 0.25)

        if not settler and (universe.squadCount > universe.AI_MAX_SQUAD_COUNT) then
            group.destroy()
            map.points = map.points + AI_SQUAD_COST
            if universe.aiPointsPrintGainsToChat then
                game.print(map.surface.name .. ": Points: +" .. AI_SQUAD_COST .. ". [Squad Refund] Total: " .. string.format("%.2f", map.points))
            end
            return
        end

        squad = createSquad(nil, map, group, settler)
        universe.groupNumberToSquad[group.group_number] = squad

        if universe.NEW_ENEMIES then
            local chunk = getChunkByPosition(map, group.position)
            if (chunk ~= -1) then
                squad.base = findNearbyBase(map, chunk)
            end
        end

        if settler then
            universe.builderCount = universe.builderCount + 1
        else
            universe.squadCount = universe.squadCount + 1
        end
    else
        if not (surface.darkness > 0.65) then
            group.destroy()
            return
        end

        local settler = canMigrate(map) and
            (universe.builderCount < universe.AI_MAX_BUILDER_COUNT) and
            (universe.random() < 0.25)

        if not settler and (universe.squadCount > universe.AI_MAX_SQUAD_COUNT) then
            group.destroy()
            map.points = map.points + AI_SQUAD_COST
            if universe.aiPointsPrintGainsToChat then
                game.print(map.surface.name .. ": Points: +" .. AI_SQUAD_COST .. ". [Squad Refund] Total: " .. string.format("%.2f", map.points))
            end
            return
        end

        squad = createSquad(nil, map, group, settler)
        universe.groupNumberToSquad[group.group_number] = squad

        if universe.NEW_ENEMIES then
            local chunk = getChunkByPosition(group.position)
            if (chunk ~= -1) then
                squad.base = findNearbyBase(map, chunk)
            end
        end

        if settler then
            universe.builderCount = universe.builderCount + 1
        else
            universe.squadCount = universe.squadCount + 1
        end
    end
end

local function onGroupFinishedGathering(event)
    local group = event.group
    if not group.valid or (group.force.name ~= "enemy") then
        return
    end
    local map = universe.maps[group.surface.index]
    if not map then
        return
    end
    map.activeSurface = true
    local squad = universe.groupNumberToSquad[group.group_number]
    if squad then
        if squad.settler then
            if (universe.builderCount < universe.AI_MAX_BUILDER_COUNT) then
                squadDispatch(map, squad, event.tick)
            else
                group.destroy()
                map.points = map.points + AI_SETTLER_COST
                if universe.aiPointsPrintGainsToChat then
                    game.print(map.surface.name .. ": Points: +" .. AI_SETTLER_COST .. ". [Settler Refund] Total: " .. string.format("%.2f", map.points))
                end
            end
        else
            if (universe.squadCount < universe.AI_MAX_SQUAD_COUNT) then
                squadDispatch(map, squad, event.tick)
            else
                group.destroy()
                map.points = map.points + AI_SQUAD_COST
                if universe.aiPointsPrintGainsToChat then
                    game.print(map.surface.name .. ": Points: +" .. AI_SQUAD_COST .. ". [Squad Refund] Total: " .. string.format("%.2f", map.points))
                end
            end
        end
    else
        local settler = canMigrate(map) and
            (universe.builderCount < universe.AI_MAX_BUILDER_COUNT) and
            (universe.random() < 0.25)

        if not settler and (universe.squadCount > universe.AI_MAX_SQUAD_COUNT) then
            group.destroy()
            map.points = map.points + AI_SQUAD_COST
            if universe.aiPointsPrintGainsToChat then
                game.print(map.surface.name .. ": Points: +" .. AI_SQUAD_COST .. ". [Squad Refund] Total: " .. string.format("%.2f", map.points))
            end
            return
        end

        squad = createSquad(nil, map, group, settler)
        universe.groupNumberToSquad[group.group_number] = squad
        if settler then
            universe.builderCount = universe.builderCount + 1
        else
            universe.squadCount = universe.squadCount + 1
        end
        squadDispatch(map, squad, event.tick)
    end
end

local function onForceCreated(event)
    universe.playerForces[#universe.playerForces+1] = event.force.name
end

local function onForceMerged(event)
    for i=#universe.playerForces,1,-1 do
        if (universe.playerForces[i] == event.source_name) then
            tRemove(universe.playerForces, i)
            break
        end
    end
end

local function onSurfaceCreated(event)
    prepMap(universe, game.surfaces[event.surface_index])
end

local function onSurfaceDeleted(event)
    local surfaceIndex = event.surface_index
    if (universe.mapIterator == surfaceIndex) then
        universe.mapIterator, universe.activeMap = next(universe.maps, universe.mapIterator)
    end
    if (universe.processMapAIIterator == surfaceIndex) then
        universe.processMapAIIterator = nil
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

    local map = universe.maps[builder.surface.index]
    if not map then
        return
    end
    map.activeSurface = true
    local squad = universe.groupNumberToSquad[builder.group_number]
    squad.commandTick = event.tick + COMMAND_TIMEOUT * 10
    if universe.aiPointsPrintSpendingToChat then
        game.print("Settled: [gps=" .. builder.position.x .. "," .. builder.position.y .."]")
    end
    setPositionInQuery(universe.obaCreateBuildCloudQuery, builder.position)
    map.surface.create_entity(universe.obaCreateBuildCloudQuery)
end

-- hooks

script.on_event(defines.events.on_tick,
                function ()
                    local gameRef = game
                    local tick = gameRef.tick
                    local pick = tick % 8

                    -- local profiler = game.create_profiler()

                    local map = universe.activeMap
                    if (not map) or (universe.processedChunks > (#map.processQueue * 0.05)) then
                        universe.processedChunks = 0
                        map = nextMap(universe)
                        universe.activeMap = map
                    end

                    if (pick == 0) then
                        processPendingChunks(universe, tick)
                        processMapAIs(universe, gameRef.forces.enemy.evolution_factor, tick)
                        if map and universe.NEW_ENEMIES then
                            recycleBases(map)
                        end
                        cleanUpMapTables(universe, tick)
                    elseif (pick == 1) then
                        processPlayers(gameRef.connected_players, universe, tick)
                    elseif (pick == 2) then
                        if map then
                            processMap(map, tick)
                        end
                    elseif (pick == 3) then
                        if map then
                            processStaticMap(map)
                        end
                        disperseVictoryScent(universe)
                        processVengence(universe)
                    elseif (pick == 4) then
                        if map then
                            scanResourceMap(map, tick)
                            scanEnemyMap(map, tick)
                        end
                    elseif (pick == 5) then
                        processAttackWaves(universe)
                        if map then
                            scanEnemyMap(map, tick)
                        end
                    elseif (pick == 6) then
                        if map then
                            scanPlayerMap(map, tick)
                        end
                        processNests(universe, tick)
                    elseif (pick == 7) then
                        processPendingChunks(universe, tick)
                        processScanChunks(universe)
                    end

                    processActiveNests(universe, tick)
                    processPendingUpgrades(universe, tick)
                    processPendingUpgrades(universe, tick)
                    cleanSquads(universe, tick)

                    -- game.print({"", "--dispatch4 ", profiler, ", ", pick, ", ", game.tick, "       ", universe.random()})
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

script.on_event(defines.events.on_script_trigger_effect, onTriggerEntityCreated)

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
                         scanChunkPaths = tests.scanChunkPaths,
                         scanEnemy = tests.scanEnemy,
                         getEnemyStructureCount = tests.getEnemyStructureCount
                     }
)

local function rampantSetAIState(event)
    local surfaceIndex = game.players[event.player_index].surface.index
    local map = universe.maps[surfaceIndex]
    if not map then
        return
    end

    game.print(map.surface.name .. " is in " .. constants.stateEnglish[map.state])

    if event.parameter then
        local target = tonumber(event.parameter)

        if (target == nil) then
            game.print("invalid param")
            return
        end

        if (target ~= constants.AI_STATE_PEACEFUL and target ~= constants.AI_STATE_AGGRESSIVE and target ~= constants.AI_STATE_RAIDING and target ~= constants.AI_STATE_MIGRATING and target ~= constants.AI_STATE_SIEGE and target ~= constants.AI_STATE_ONSLAUGHT) then
            game.print(target .. " is not a valid state")
            return
        else
            map.state = target
            game.print(map.surface.name .. " is now in " .. constants.stateEnglish[map.state])
        end
    end
end

commands.add_command('rampantSetAIState', "", rampantSetAIState)
