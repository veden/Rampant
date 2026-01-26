-- Test fixtures for Rampant mod testing
-- Provides reusable fixtures for Universe, Map, Base, Chunk, and Surface

local Mocks = require("tests/mocks")
local MathUtils = require("libs/MathUtils")

local Fixtures = {}

--- Create a minimal Universe table structure
---@return table
function Fixtures.makeUniverse()
    return {
        tables = {
            maps = {},
            activeMaps = {},
            bases = {},
            structures = {},
            entities = {},
            pendingChunks = {},
            chunkToVictory = {},
            chunkToPassScan = {},
            activeStructures = {},
            settlePurpleCloudQueue = {},
            vengenceQueue = {},
            pendingUpgrades = {},
            safeEntities = {},
            excludedSurfaces = {},
            groupNumberToSquad = {},
            entityToHive = {},
        },
        iterators = {
            squadIterator = nil,
            processMapAIIterator = nil,
            currentMap = nil,
            mapIterator = nil,
            baseAIIterator = nil,
            recycleBaseIterator = nil,
            structureIterator = nil,
            activeStructureIterator = nil,
        },
        ids = {
            upgradeIds = 0,
            eventIds = 0,
            chunkIds = 0,
            baseIds = 0,
        },
        flushPendingChunks = false,
        builderCount = 0,
        squadCount = 0,
        awake = false,
        enabledMigration = true,
        peacefulAIToggle = false,
        printAIStateChanges = false,
        debugTemperament = false,
        newEnemies = false,
        baseDistanceModifier = 1.0,
        random = MathUtils.xorRandom(0), -- Proper random generator for gaussian functions
        forces = {
            enemy = {
                name = "enemy",
            },
            neutral = {
                name = "neutral",
            },
        },
    }
end

--- Create a mock surface for testing
---@param name string
---@return table
function Fixtures.makeSurface(name)
    return Mocks.makeSurface(name)
end

--- Create a RampantMap structure with mocked surface
---@param surface table
---@return table
function Fixtures.makeMap(surface)
    return {
        id = 1,
        surface = surface,
        chunkRegions = {},
        chunks = {},
        entities = {},
        activeStructures = {},
        structures = {},
        players = {},
        playerGenerators = {},
        movementGenerators = {},
        pendingUpgrades = {},
        resourceGenerators = {},
        chunksByXY = {},
        baseRegions = {},
        bases = {},
        chunkToVictory = {},
        chunkToPassScan = {},
        settlePurpleCloudQueue = {},
        activeNestCount = 0,
        baseAI = {
            expansion = true,
        },
        evolutionLevel = 0.5,
    }
end

--- Create a RampantChunk structure
---@param map table
---@param x integer
---@param y integer
---@return table
function Fixtures.makeChunk(map, x, y)
    return {
        map = map,
        id = math.random(1000000), -- Give chunk a unique ID for victory scent tracking
        x = x,
        y = y,
        enemyAndResourcePheromone = 0,
        playerPheromone = 0,
        enemyPheromone = 0,
        victoryPheromone = 0,
        deathPheromone = 0,
        settled = false,
        scanned = false,
        scannedPlayer = false,
        scannedPass = false,
        activeStructures = {},
        structures = {},
        entities = {},
        nestCount = 0,
        wormCount = 0,
        hiveCount = 0,
        turrets = {},
        hives = {},
        worms = {},
        nests = {},
    }
end

--- Create a RampantBase structure
---@param map table
---@param position table
---@return table
function Fixtures.makeBase(map, position)
    local baseId = map.universe.ids.baseIds + 1
    if not map.universe.ids then map.universe = { ids = { baseIds = 0 } } end
    map.universe.ids.baseIds = baseId

    return {
        id = baseId,
        position = position,
        map = map,
        surface = map.surface,
        nests = {},
        turrets = {},
        hives = {},
        worms = {},
        activeNests = 0,
        totalNests = 0,
        totalTurrets = 0,
        totalWorms = 0,
        totalHives = 0,
        aiState = "peaceful",
        aiStateChangeTick = 0,
        generationState = "dormant",
        points = 0,
        pointsChange = 0,
        temperament = 0.5,
        targetTemperature = 0.5,
        ionCannonBlasts = 0,
        rallyCount = 0,
        rallyStrength = 0,
        ignoreCloudGeneration = false,
        activeStructureCount = 0,
        totalStructures = 0,
        lastProcessed = 0,
        commandGroups = {},
        settleCloud = nil,
        settlingPosition = nil,
        settlingTick = 0,
        settlingTimeout = 0,
        settleChunk = nil,
        retreatThreshold = 0,
        attackThreshold = 0,
        buildingCost = nil,
        buildingTick = 0,
    }
end

--- Register a chunk in the map's chunkRegions
---@param map table
---@param chunk table
function Fixtures.registerChunk(map, chunk)
    local x, y = chunk.x, chunk.y
    map.chunkRegions[x] = map.chunkRegions[x] or {}
    map.chunkRegions[x][y] = chunk
    map.chunksByXY[x .. "," .. y] = chunk
    return chunk
end

--- Create and register a chunk in one step
---@param map table
---@param x integer
---@param y integer
---@return table
function Fixtures.createAndRegisterChunk(map, x, y)
    local chunk = Fixtures.makeChunk(map, x, y)
    Fixtures.registerChunk(map, chunk)
    return chunk
end

--- Add an entity to the surface and register in chunk
---@param surface table
---@param chunk table
---@param entityProps table
---@return table
function Fixtures.addEntityToChunk(surface, chunk, entityProps)
    local entity = Mocks.makeEntity(entityProps)
    entity.position = entityProps.position or { x = chunk.x, y = chunk.y }
    entity.surface = surface
    surface.add_entity(surface, entity)
    table.insert(chunk.entities, entity)
    return entity
end

return Fixtures