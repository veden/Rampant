--[[
    Entity - Base entity with position and type

    Represents any game entity (units, structures, buildings).
    Supports spatial queries and entity state management.
]]

local Entity = {}
Entity.__index = Entity

-- Entity type constants
Entity.TYPES = {
    UNIT = "unit",
    STRUCTURE = "structure",
    BUILDING = "building",
    PLAYER = "player",
    TILE = "tile",
    TRASH = "trash"
}

-- Track all entities for global queries
Entity._all = {}
Entity._nextId = 1

--- Create a new entity
-- @param options table: Entity configuration
-- @option options type string: Entity type (UNIT, STRUCTURE, BUILDING, PLAYER)
-- @option options name string: Entity prototype name (e.g., "small-biter")
-- @option options position table: Position {x, y}
-- @option options force string: Force name ("enemy", "player", "neutral")
-- @option options health number: Health value (default 100)
-- @option options surface Surface: Surface to register on
-- @return Entity: New entity instance
function Entity.new(options)
    local self = setmetatable({}, Entity)

    self.type = options.type or Entity.TYPES.UNIT
    self.name = options.name or "unknown"
    self.position = {
        x = options.position.x,
        y = options.position.y
    }
    self.force = options.force or "neutral"
    self.health = options.health or 100
    self.maxHealth = self.health
    self.unit_number = Entity._nextId
    Entity._nextId = Entity._nextId + 1

    -- State
    self.alive = true
    self._chunk = nil
    self._surface = options.surface

    -- Unit-specific
    self.speed = options.speed or 0.5
    self.active = true

    -- Building-specific
    if options.stockpile then
        self.stockpile = options.stockpile
    end

    -- Register
    table.insert(Entity._all, self)
    if self._surface then
        self._surface:_registerEntity(self)
    end

    return self
end

--- Get the chunk containing this entity
-- @return Chunk|nil: Containing chunk or nil if not on surface
function Entity:chunk()
    if self._chunk then
        return self._chunk
    end
    if self._surface then
        return self._surface:getChunkAtPosition(self.position)
    end
    return nil
end

--- Calculate distance to another entity or position
-- @param other table: Entity or position {x, y}
-- @return number: Distance in tiles
function Entity:distanceTo(other)
    local ox = other.x or other.position.x
    local oy = other.y or other.position.y
    local dx = self.position.x - ox
    local dy = self.position.y - oy
    return math.sqrt(dx * dx + dy * dy)
end

--- Move entity to new position
-- @param newPos table: New position {x, y}
function Entity:moveTo(newPos)
    local oldChunk = self:chunk()
    self.position.x = newPos.x
    self.position.y = newPos.y

    -- Update chunk registration
    local newChunk = self:chunk()
    if newChunk and newChunk ~= oldChunk then
        if oldChunk then
            oldChunk:removeEntity(self)
        end
        newChunk:addEntity(self)
    end
end

--- Damage the entity
-- @param amount number: Damage amount
-- @param force string: Force dealing damage
-- @return boolean: True if entity died
function Entity:damage(amount)
    self.health = self.health - amount
    if self.health <= 0 then
        self.alive = false
        self.health = 0
        return true  -- Died
    end
    return false  -- Still alive
end

--- Heal the entity
-- @param amount number: Amount to heal
function Entity:heal(amount)
    self.health = math.min(self.health + amount, self.maxHealth)
end

--- Check if entity is in valid state
-- @return boolean: True if alive and on surface
function Entity:valid()
    return self.alive and self._surface ~= nil
end

--- Destroy entity (remove from game)
function Entity:destroy()
    self.alive = false
    if self._chunk then
        self._chunk:removeEntity(self)
    end
    self._surface = nil

    -- Remove from global list
    for i, e in ipairs(Entity._all) do
        if e == self then
            table.remove(Entity._all, i)
            break
        end
    end
end

--- Find all entities matching criteria
-- @param predicate function: Test function(entity) -> boolean
-- @return table: List of matching entities
function Entity.find(predicate)
    local result = {}
    for _, entity in ipairs(Entity._all) do
        if predicate(entity) then
            table.insert(result, entity)
        end
    end
    return result
end

--- Find entities of a specific type
-- @param entityType string: Type to find
-- @return table: List of entities
function Entity.findByType(entityType)
    return Entity.find(function(e)
        return e.type == entityType
    end)
end

--- Find entities by force
-- @param forceName string: Force to find
-- @return table: List of entities
function Entity.findByForce(forceName)
    return Entity.find(function(e)
        return e.force == forceName
    end)
end

--- Clear all entities (for test reset)
function Entity.clearAll()
    Entity._all = {}
    Entity._nextId = 1
end

return Entity