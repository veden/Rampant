--[[
    Chunk - Chunk with neighbors and spatial queries

    Represents a 32x32 tile chunk in the Factorio world.
    Stores entities, path ratings, and neighbor references.
]]

local TickSystem = require("tests/sim.TickSystem")

local Chunk = {}
Chunk.__index = Chunk

--- Create a new chunk
-- @param x number: Chunk coordinate X
-- @param y number: Chunk coordinate Y
-- @return Chunk: New chunk instance
function Chunk.new(x, y)
    local self = setmetatable({}, Chunk)
    self.position = {x = x, y = y}
    self.center = {x = x + 16, y = y + 16}  -- Center of 32x32 chunk
    self.entities = {}
    self.pathRating = 1.0  -- 1.0 = fully passable
    self.neighborOffsets = {
        [-32] = {[-32] = nil, [0] = nil, [32] = nil},
        [0]   = {[-32] = nil, [0] = nil, [32] = nil},
        [32]  = {[-32] = nil, [0] = nil, [32] = nil}
    }
    self._neighbors = {}  -- Actual neighbor chunks

    -- Pheromone values (for AI)
    self.playerPheromone = 0
    self.deathPheromone = 0
    self.victoryScent = 0
    self.basePheromone = 0
    self.ignorePheromone = 0

    -- Processing
    self.tick = 0
    self.processed = false

    return self
end

--- Get a neighbor chunk by direction (1-8)
-- @param direction number: Direction constant
-- @return Chunk|nil: Neighbor chunk or nil if not exists
function Chunk:getNeighbor(direction)
    local offset = TickSystem.NEIGHBOR_OFFSETS[direction]
    if offset then
        return self._neighbors[offset.x .. "," .. offset.y]
    end
    return nil
end

--- Check if a neighbor exists in a direction
-- @param direction number: Direction constant
-- @return boolean: True if neighbor exists
function Chunk:hasNeighbor(direction)
    local offset = TickSystem.NEIGHBOR_OFFSETS[direction]
    if offset then
        return self._neighbors[offset.x .. "," .. offset.y] ~= nil
    end
    return false
end

--- Register a neighbor chunk
-- @param neighbor Chunk: Neighbor chunk to register
function Chunk:addNeighbor(neighbor)
    local key = neighbor.position.x .. "," .. neighbor.position.y
    self._neighbors[key] = neighbor
end

--- Add an entity to this chunk
-- @param entity table: Entity to add
function Chunk:addEntity(entity)
    table.insert(self.entities, entity)
    entity._chunk = self
end

--- Remove an entity from this chunk
-- @param entity table: Entity to remove
-- @return boolean: True if entity was removed
function Chunk:removeEntity(entity)
    for i, e in ipairs(self.entities) do
        if e == entity then
            table.remove(self.entities, i)
            entity._chunk = nil
            return true
        end
    end
    return false
end

--- Find entities in this chunk matching a predicate
-- @param predicate function: Test function(entity) -> boolean
-- @return table: List of matching entities
function Chunk:findEntities(predicate)
    local result = {}
    for _, entity in ipairs(self.entities) do
        if predicate(entity) then
            table.insert(result, entity)
        end
    end
    return result
end

--- Count entities of a specific type
-- @param entityType string: Entity type to count
-- @return number: Count of matching entities
function Chunk:countByType(entityType)
    local count = 0
    for _, entity in ipairs(self.entities) do
        if entity.type == entityType then
            count = count + 1
        end
    end
    return count
end

--- Check if position is within this chunk
-- @param pos table: Position {x, y}
-- @return boolean: True if position is in this chunk
function Chunk:contains(pos)
    return pos.x >= self.position.x and pos.x < self.position.x + 32
       and pos.y >= self.position.y and pos.y < self.position.y + 32
end

--- Get distance from chunk center to a position
-- @param pos table: Position {x, y}
-- @return number: Distance in tiles
function Chunk:distanceTo(pos)
    local dx = self.center.x - pos.x
    local dy = self.center.y - pos.y
    return math.sqrt(dx * dx + dy * dy)
end

return Chunk