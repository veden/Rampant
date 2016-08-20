-- imports

local chunkUtils = require("libs/ChunkUtils")
local mapUtils = require("libs/MapUtils")
local unitGroupUtils = require("libs/UnitGroupUtils")
local chunkProcessor = require("libs/ChunkProcessor")
local mapProcessor = require("libs/MapProcessor")
local constants = require("libs/Constants")
local pheromoneUtils = require("libs/PheromoneUtils")
local aiDefense = require("libs/AIDefense")
local aiAttack = require("libs/AIAttack")
local aiBuilding = require("libs/AIBuilding")
local tests = require("Tests")

-- global state references

local regionMap -- chunk based map
local pendingChunks -- pending chunks to be processed
local natives -- units that are being commanded

-- initialization

chunkProcessor.install(chunkUtils.checkChunkPassability)
chunkProcessor.install(chunkUtils.scoreChunk)

mapProcessor.install(pheromoneUtils.enemyBaseScent, 0, 1)
mapProcessor.install(pheromoneUtils.playerDefenseScent, 0, 1)
mapProcessor.install(pheromoneUtils.playerBaseScent, 0, 1)
mapProcessor.install(aiBuilding.sendScouts, 0.05, 0.10)
mapProcessor.install(aiBuilding.formSquads, 0.11, 0.25)
mapProcessor.install(pheromoneUtils.processPheromone, 0, 1)

-- constants

local DEATH_PHEROMONE_GENERATOR_AMOUNT = constants.DEATH_PHEROMONE_GENERATOR_AMOUNT

-- imported functions

local processPendingChunks = chunkProcessor.processPendingChunks

local processMap = mapProcessor.processMap

local accumulatePoints = aiBuilding.accumulatePoints
local removeScout = aiBuilding.removeScout
local scouting = aiBuilding.scouting

local playerScent = pheromoneUtils.playerScent
local deathScent = pheromoneUtils.deathScent

local regroupSquads = unitGroupUtils.regroupSquads
local convertUnitGroupToSquad = unitGroupUtils.convertUnitGroupToSquad

local squadAttackPlayer = aiAttack.squadAttackPlayer
local squadAttackLocation = aiAttack.squadAttackLocation
local squadBeginAttack = aiAttack.squadBeginAttack

local retreatUnits = aiDefense.retreatUnits

local addRemoveObject = mapUtils.addRemoveObject

local mRandom = math.random

-- hook functions

function onInit()
    -- print("init")
    global.regionMap = {}
    global.pendingChunks = {}
    global.natives = {}
    
    regionMap = global.regionMap
    pendingChunks = global.pendingChunks
    regionMap.pQ = {{}} -- processing queue
    regionMap.pI = 1 -- insertion location for chunk processing
    regionMap.pP = 1 -- index for the chunk set to process
    regionMap.pR = -1 -- current processing roll
    natives = global.natives
    natives.squads = {}
    natives.scouts = {}
    natives.points = 0
    
    game.forces.player.research_all_technologies()
    
    -- queue all current chunks that wont be generated during play
    local surface = game.surfaces[1]
    for chunk in surface.get_chunks() do
        onChunkGenerated({ surface = surface, 
                           area = { left_top = { x = chunk.x * 32,
                                                 y = chunk.y * 32 }}})
    end
end

function onLoad()
    -- print("load")
    regionMap = global.regionMap
    pendingChunks = global.pendingChunks
    natives = global.natives
end

function onChunkGenerated(event)
    -- queue generated chunk for delayed processing, queuing is required because some mods (RSO) mess with chunk as they
    -- are generated, which messes up the scoring.
    if (event.surface.index == 1) then
        pendingChunks[#pendingChunks+1] = event
    end
end

function onTick(event)
    if (event.tick % 20 == 0) then
        local surface = game.surfaces[1]
        
        if (event.tick % 40 == 0) then
            if not game.players[1].cheat_mode then
                game.players[1].cheat_mode = true
            end
            
            accumulatePoints(natives)
            
            -- put down player pheromone for player hunters
            playerScent(regionMap, game.players)
            
            regroupSquads(natives)
                    
            scouting(regionMap, surface, natives)
            
            squadAttackPlayer(regionMap, surface, natives, game.players)
            
            squadBeginAttack(natives)
            squadAttackLocation(regionMap, surface, natives)
        end
        processPendingChunks(regionMap, surface, natives, pendingChunks)
        
        processMap(regionMap, surface, natives, game.evolution_factor)
    end
end

function onBuild(event)
    addRemoveObject(regionMap, event.created_entity, natives, true)
end

function onPickUp(event)
    addRemoveObject(regionMap, event.entity, natives, false)
end

function onDeath(event)
    local entity = event.entity
    if (entity.force.name == "enemy") then
        if (entity.type == "unit") then
            local entityPosition = entity.position
            -- drop death pheromone where unit died
            local squad = convertUnitGroupToSquad(natives, entity.unit_group)
            
            local surface = game.surfaces[1]
            
            deathScent(regionMap,
                       surface,
                       entityPosition.x,
                       entityPosition.y,
                       squad,
                       DEATH_PHEROMONE_GENERATOR_AMOUNT)
            
            if (event.force ~= nil) and (event.force.name == "player") then
                retreatUnits(entityPosition, squad, regionMap, surface, natives)
            end
            
            removeScout(entity, natives)
        elseif (entity.type == "unit-spawner") then
            addRemoveObject(regionMap, entity, natives, false)
        end
    elseif (entity.force.name == "player") then
        addRemoveObject(regionMap, entity, natives, false)
    end
end

-- hooks

script.on_init(onInit)
script.on_load(onLoad)

script.on_event({defines.events.on_preplayer_mined_item,
                 defines.events.on_robot_pre_mined}, 
                onPickUp)
script.on_event({defines.events.on_built_entity,
                 defines.events.on_robot_built_entity}, 
                onBuild)
                
script.on_event(defines.events.on_entity_died, onDeath)
script.on_event(defines.events.on_tick, onTick)
script.on_event(defines.events.on_chunk_generated, onChunkGenerated)

remote.add_interface("rampant", {
                                    test1 = tests.test1,
                                    test2 = tests.test2,
                                    test3 = tests.test3,
                                    test4 = tests.test4,
                                    test5 = tests.test5
                                })
