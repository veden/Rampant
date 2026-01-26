--[[
    Surface - Surface managing chunks and entities

    Represents a game surface (like nauvis) containing chunks and entities.
    Provides spatial queries for entity lookup.
]]

local Chunk = require("tests/sim.Chunk")
local TickSystem = require("tests/sim.TickSystem")

local Surface = {}
Surface.__index = Surface

-- Chunk size constant
Surface.CHUNK_SIZE = 32

--- Create a new surface
-- @param name string: Surface name (e.g., "nauvis")
-- @return Surface: New surface instance
function Surface.new(name)
    local self = setmetatable({}, Surface)
    self.name = name
    self.chunks = {}  -- Map<chunkX, Map<chunkY, Chunk>>
    self.entities = {}  -- All entities on surface

    -- Entity lookup indexes
    self._byName = {}  -- Map<entityName, List<Entity>>
    self._byType = {}  -- Map<entityType, List<Entity>>
    self._byForce = {}  -- Map<forceName, List<Entity>>

    return self
end

--- Create or get a chunk at coordinates
-- @param x number: Chunk coordinate X
-- @param y number: Chunk coordinate Y
-- @return Chunk: The chunk
function Surface:getOrCreateChunk(x, y)
    if not self.chunks[x] then
        self.chunks[x] = {}
    end

    local chunk = self.chunks[x][y]
    if not chunk then
        chunk = Chunk.new(x, y)
        self.chunks[x][y] = chunk

        -- Set up neighbors
        self:_updateChunkNeighbors(chunk)
    end

    return chunk
end

--- Get a chunk at coordinates (returns nil if not exists)
-- @param x number: Chunk coordinate X
-- @param y number: Chunk coordinate Y
-- @return Chunk|nil: The chunk or nil
function Surface:getChunk(x, y)
    if self.chunks[x] then
        return self.chunks[x][y]
    end
    return nil
end

--- Get the chunk containing a position
-- @param pos table: Position {x, y}
-- @return Chunk: The containing chunk
function Surface:getChunkAtPosition(pos)
    local cx = math.floor(pos.x / Surface.CHUNK_SIZE) * Surface.CHUNK_SIZE
    local cy = math.floor(pos.y / Surface.CHUNK_SIZE) * Surface.CHUNK_SIZE
    return self:getOrCreateChunk(cx, cy)
end

--- Check if a chunk has been generated
-- @param pos table: Position or chunk coordinates
-- @return boolean: True if chunk exists
function Surface:isChunkGenerated(pos)
    local x = pos.x or pos[1]
    local y = pos.y or pos[2]
    return self.chunks[x] and self.chunks[x][y] ~= nil
end

--- Find entities in an area
-- @param area table: Area with {left_top = {x, y}, right_bottom = {x, y}}
-- @return table: List of entities in area
function Surface:findInArea(area)
    local result = {}
    local left = area.left_top.x
    local top = area.left_top.y
    local right = area.right_bottom.x
    local bottom = area.right_bottom.y

    for _, entity in ipairs(self.entities) do
        if entity.position.x >= left and entity.position.x <= right
           and entity.position.y >= top and entity.position.y <= bottom then
            table.insert(result, entity)
        end
    end

    return result
end

--- Find entities in a radius around a position
-- @param pos table: Center position {x, y}
-- @param radius number: Search radius
-- @return table: List of entities within radius
function Surface:findInRadius(pos, radius)
    local result = {}
    local radiusSq = radius * radius

    for _, entity in ipairs(self.entities) do
        local distSq = (entity.position.x - pos.x)^2 + (entity.position.y - pos.y)^2
        if distSq <= radiusSq then
            table.insert(result, entity)
        end
    end

    return result
end

--- Count entities by type
-- @param entityType string: Type to count
-- @return number: Count of entities
function Surface:countByType(entityType)
    local count = 0
    for _, entity in ipairs(self.entities) do
        if entity.type == entityType then
            count = count + 1
        end
    end
    return count
end

--- Count entities by name
-- @param name string: Entity prototype name
-- @return number: Count of entities
function Surface:countByName(name)
    local count = 0
    for _, entity in ipairs(self.entities) do
        if entity.name == name then
            count = count + 1
        end
    end
    return count
end

--- Find entity by unit number
-- @param unitNumber number: Unit number to find
-- @return Entity|nil: Found entity or nil
function Surface:findByUnitNumber(unitNumber)
    for _, entity in ipairs(self.entities) do
        if entity.unit_number == unitNumber then
            return entity
        end
    end
    return nil
end

--- Get all entities of a force
-- @param forceName string: Force name
-- @return table: List of entities
function Surface:getEntitiesByForce(forceName)
    local result = {}
    for _, entity in ipairs(self.entities) do
        if entity.force == forceName then
            table.insert(result, entity)
        end
    end
    return result
end

--- Create an entity on this surface
-- @param options table: Entity options (see Entity.new)
-- @return Entity: Created entity
function Surface:createEntity(options)
    options.surface = self
    return require("tests/sim.Entity").new(options)
end

--- Register an entity (internal)
-- @param entity Entity: Entity to register
function Surface:_registerEntity(entity)
    table.insert(self.entities, entity)

    -- Update indexes
    if not self._byName[entity.name] then
        self._byName[entity.name] = {}
    end
    table.insert(self._byName[entity.name], entity)

    if not self._byType[entity.type] then
        self._byType[entity.type] = {}
    end
    table.insert(self._byType[entity.type], entity)

    if not self._byForce[entity.force] then
        self._byForce[entity.force] = {}
    end
    table.insert(self._byForce[entity.force], entity)

    -- Add to chunk
    local chunk = self:getChunkAtPosition(entity.position)
    chunk:addEntity(entity)
end

--- Update neighbor references for a chunk
-- @param chunk Chunk: Chunk to update neighbors for
function Surface:_updateChunkNeighbors(chunk)
    -- Check all 8 directions
    for dir = 1, 8 do
        local offset = TickSystem.NEIGHBOR_OFFSETS[dir]
        local nx = chunk.position.x + offset.x
        local ny = chunk.position.y + offset.y

        local neighbor = self:getChunk(nx, ny)
        if neighbor then
            chunk:addNeighbor(neighbor)
        end
    end
end

--- Clear the surface (for test reset)
function Surface:clear()
    self.chunks = {}
    self.entities = {}
    self._byName = {}
    self._byType = {}
    self._byForce = {}
end

--- Find entities using Factorio-style filtered query
-- @param query table: Query with optional fields:
--   - area: {left_top: {x, y}, right_bottom: {x, y}}
--   - radius: number (center position, alternative to area)
--   - position: {x, y} (center for radius search)
--   - force: string (filter by force name)
--   - type: string (filter by entity type)
--   - name: string (filter by entity prototype name)
-- @return table: List of matching entities
function Surface:findEntitiesFiltered(query)
    local result = {}

    -- Helper to check if entity matches filter
    local function matches(entity)
        -- Filter by force
        if query.force and entity.force ~= query.force then
            return false
        end
        -- Filter by type
        if query.type and entity.type ~= query.type then
            return false
        end

        -- Filter by name
        if query.name and entity.name ~= query.name then
            return false
        end

        -- Filter by area
        if query.area then
            local left = query.area.left_top.x
            local top = query.area.left_top.y
            local right = query.area.right_bottom.x
            local bottom = query.area.right_bottom.y
            if entity.position.x < left or entity.position.x > right
               or entity.position.y < top or entity.position.y > bottom then
                return false
            end
        end

        -- Filter by radius
        if query.radius and query.position then
            local dx = entity.position.x - query.position.x
            local dy = entity.position.y - query.position.y
            if (dx * dx + dy * dy) > (query.radius * query.radius) then
                return false
            end
        end

        return true
    end

    for _, entity in ipairs(self.entities) do
        if matches(entity) then
            table.insert(result, entity)
        end
    end

    return result
end

--- Set multi-command (spawns units for squad)
-- This is a Factorio-style command to spawn multiple units at once
-- @param command table: Command options:
--   - unit_count: number of units to spawn
--   - unit_template: string (entity prototype name)
--   - position: {x, y} spawn position
--   - force: string force name
--   - group: UnitGroup (optional, to add units to existing group)
-- @return table: List of spawned entities
function Surface:setMultiCommand(command)
    local spawned = {}

    for i = 1, (command.unit_count or 1) do
        -- Add slight random offset to position for realism
        local offsetX = (math.random() - 0.5) * 2
        local offsetY = (math.random() - 0.5) * 2

        local entity = require("tests/sim.Entity").new({
            type = "unit",
            name = command.unit_template or "small-biter",
            position = {
                x = command.position.x + offsetX,
                y = command.position.y + offsetY
            },
            force = command.force or "enemy",
            health = 100,
            surface = self
        })
        -- Note: Entity.new() automatically registers with surface via _registerEntity

        table.insert(spawned, entity)

        -- Add to group if specified
        if command.group then
            command.group:addMember(entity)
        end
    end

    return spawned
end

--- Get dark level at position (for nocturnal AI)
-- @param pos table: Position {x, y}
-- @return number: Darkness level (0-1, where 1 is darkest)
function Surface:getDarkness(pos)
    -- Simplified: returns 0.5 during simulation
    -- Real Factorio uses time-of-day calculation
    return 0.5
end

--- Check if surface is valid
-- @return boolean: True (always valid in simulation)
function Surface:isValid()
    return true
end

return Surface