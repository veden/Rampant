--[[
    WorldBuilder - Fluent API for test setup

    Provides a chainable API for constructing test worlds
    with surfaces, chunks, entities, bases, and players.
]]

local Surface = require("tests/sim.Surface")
local Entity = require("tests/sim.Entity")
local UnitGroup = require("tests/sim.UnitGroup")
local TickSystem = require("tests/sim.TickSystem")

local WorldBuilder = {}
WorldBuilder.__index = WorldBuilder

--- Create a new WorldBuilder
-- @return WorldBuilder: New builder instance
function WorldBuilder.create()
    local self = setmetatable({}, WorldBuilder)

    self.surface = nil
    self.chunks = {}
    self.entities = {}
    self.bases = {}
    self.players = {}
    self.tick = 0

    -- Configuration
    self._surfaceName = "nauvis"
    self._chunkPositions = {}  -- List of {x, y}
    self._basePositions = {}   -- List of {x, y}
    self._playerPositions = {} -- List of {x, y}
    self._enemyEntities = {}   -- List of {type, pos}
    self._playerBuildings = {} -- List of {type, pos}

    return self
end

--- Set the surface name
-- @param name string: Surface name
-- @return WorldBuilder: self for chaining
function WorldBuilder:withSurface(name)
    self._surfaceName = name
    return self
end

--- Add a chunk at position
-- @param x number: Chunk X coordinate
-- @param y number: Chunk Y coordinate
-- @return WorldBuilder: self for chaining
function WorldBuilder:withChunk(x, y)
    table.insert(self._chunkPositions, {x = x, y = y})
    return self
end

--- Add a player at position
-- @param pos table: Position {x, y}
-- @return WorldBuilder: self for chaining
function WorldBuilder:withPlayer(pos)
    table.insert(self._playerPositions, {x = pos.x, y = pos.y})
    return self
end

--- Add an enemy base at position
-- @param pos table: Position {x, y}
-- @return WorldBuilder: self for chaining
function WorldBuilder:withBase(pos)
    table.insert(self._basePositions, {x = pos.x, y = pos.y})
    return self
end

--- Add an enemy entity
-- @param entityType string: Entity type (e.g., "small-biter")
-- @param pos table: Position {x, y}
-- @return WorldBuilder: self for chaining
function WorldBuilder:withEnemyEntity(entityType, pos)
    table.insert(self._enemyEntities, {type = entityType, pos = pos})
    return self
end

--- Add a player building
-- @param buildingType string: Building type (e.g., "gun-turret")
-- @param pos table: Position {x, y}
-- @return WorldBuilder: self for chaining
function WorldBuilder:withPlayerBuilding(buildingType, pos)
    table.insert(self._playerBuildings, {type = buildingType, pos = pos})
    return self
end

--- Build the world
-- @return World: Constructed world object
function WorldBuilder:build()
    -- Create surface
    local surface = Surface.new(self._surfaceName)
    self.surface = surface

    -- Create chunks
    for _, chunkPos in ipairs(self._chunkPositions) do
        surface:getOrCreateChunk(chunkPos.x, chunkPos.y)
    end

    -- If no chunks specified, create default chunk at origin
    if #self._chunkPositions == 0 then
        surface:getOrCreateChunk(0, 0)
    end

    -- Update all chunk neighbors (both directions)
    for _, chunk in pairs(surface.chunks) do
        for _, neighborChunk in pairs(chunk) do
            surface:_updateChunkNeighbors(neighborChunk)
        end
    end

    -- Create player(s)
    for _, playerPos in ipairs(self._playerPositions) do
        local player = Entity.new({
            type = Entity.TYPES.PLAYER,
            name = "player",
            position = playerPos,
            force = "player",
            health = 100,
            surface = surface
        })
        table.insert(self.players, player)
        table.insert(self.entities, player)

        -- Register with game.forces
        if _G.game and _G.game.forces and _G.game.forces.player then
            table.insert(_G.game.forces.player.players, player)
        end
    end

    -- Create enemy entities (individual units, not bases)
    for _, enemy in ipairs(self._enemyEntities) do
        local entity = Entity.new({
            type = Entity.TYPES.UNIT,
            name = enemy.type,
            position = enemy.pos,
            force = "enemy",
            health = 100,
            surface = surface
        })
        table.insert(self.entities, entity)
    end

    -- Create enemy bases (spawners)
    for _, basePos in ipairs(self._basePositions) do
        local spawner = Entity.new({
            type = Entity.TYPES.STRUCTURE,
            name = "biter-spawner",
            position = basePos,
            force = "enemy",
            health = 500,
            surface = surface
        })
        table.insert(self.bases, spawner)
        table.insert(self.entities, spawner)
    end

    -- Create player buildings
    for _, building in ipairs(self._playerBuildings) do
        local entity = Entity.new({
            type = Entity.TYPES.BUILDING,
            name = building.type,
            position = building.pos,
            force = "player",
            health = 100,
            surface = surface
        })
        table.insert(self.entities, entity)
    end

    -- Create World wrapper with helper methods
    local world = {
        surface = surface,
        entities = self.entities,
        bases = self.bases,
        players = self.players,
        tick = 0,

        -- Advance time
        advanceTicks = function(ticks)
            TickSystem.advance(ticks)
            self.tick = TickSystem.now()
        end,
        setTick = function(t)
            TickSystem.set(t)
            self.tick = t
        end,
        getTick = function()
            return TickSystem.now()
        end,

        -- Entity queries
        addBuilding = function(name, pos)
            local entity = Entity.new({
                type = Entity.TYPES.BUILDING,
                name = name,
                position = {x = pos.x, y = pos.y},
                force = "player",
                health = 100,
                surface = surface
            })
            table.insert(self.entities, entity)
            return entity
        end,

        addEnemy = function(name, pos)
            local entity = Entity.new({
                type = Entity.TYPES.UNIT,
                name = name,
                position = {x = pos.x, y = pos.y},
                force = "enemy",
                health = 100,
                surface = surface
            })
            table.insert(self.entities, entity)
            return entity
        end,

        killEntity = function(name, pos)
            -- Find entity by position and name
            for _, entity in ipairs(self.entities) do
                if entity.name == name and entity.position.x == pos.x and entity.position.y == pos.y then
                    entity.alive = false
                    entity.health = 0
                    return entity
                end
            end
            return nil
        end,

        destroyEntity = function(entity)
            if entity.destroy then
                entity:destroy()
            end
        end,

        -- Squad/group queries
        getSquadCount = function()
            return #UnitGroup.all()
        end,

        getFirstSquad = function()
            local groups = UnitGroup.all()
            return groups[1]
        end,

        getSquads = function()
            return UnitGroup.all()
        end,

        getSquadDistanceToPlayer = function()
            local groups = UnitGroup.all()
            if #groups == 0 then return nil end

            local squad = groups[1]
            if #self.players == 0 then return nil end

            local player = self.players[1]
            return squad:distanceTo(player.position)
        end,

        spawnSquad = function(pos)
            local group = UnitGroup.new({
                position = pos,
                surface = surface
            })
            return group
        end,

        addToSquad = function(squad, unitName, unitPos)
            local unit = Entity.new({
                type = Entity.TYPES.UNIT,
                name = unitName,
                position = {x = unitPos.x, y = unitPos.y},
                force = "enemy",
                health = 100,
                surface = surface
            })
            squad:addMember(unit)
            return unit
        end,

        -- Base queries
        getBaseCount = function()
            return #self.bases
        end,

        getBases = function()
            return self.bases
        end,

        getBaseDistanceToPlayer = function()
            if #self.bases == 0 or #self.players == 0 then return nil end
            local base = self.bases[1]
            local player = self.players[1]
            return base:distanceTo(player.position)
        end,

        -- Pheromone queries
        getPheromoneAt = function(pos)
            local chunk = surface:getChunkAtPosition(pos)
            if chunk then
                return {
                    player = chunk.playerPheromone,
                    death = chunk.deathPheromone,
                    victory = chunk.victoryScent,
                    base = chunk.basePheromone,
                    ignore = chunk.ignorePheromone
                }
            end
            return nil
        end,

        -- Player queries
        getPlayerCount = function()
            return #self.players
        end,

        getFirstPlayer = function()
            return self.players[1]
        end,

        -- Entity queries
        findEnemyInRadius = function(pos, radius)
            return surface:findInRadius(pos, radius)
        end,

        countEnemies = function()
            return surface:countByType(Entity.TYPES.UNIT)
        end,

        -- Reset helpers
        reset = function()
            Entity.clearAll()
            UnitGroup.clearAll()
            TickSystem.reset()
            self.tick = 0
        end
    }

    return world
end

return WorldBuilder