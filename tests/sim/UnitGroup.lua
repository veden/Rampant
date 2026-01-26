--[[
    UnitGroup - Unit group with AI state

    Represents a group of units moving and attacking together.
    Used by Rampant AI for squad-based behavior.
]]

local UnitGroup = {}
UnitGroup.__index = UnitGroup

-- Group state constants
UnitGroup.STATES = {
    GATHERING = "gathering",
    MOVING = "moving",
    ATTACKING = "attacking",
    FLEEING = "fleeing",
    STATIONARY = "stationary"
}

-- Track all groups
UnitGroup._all = {}
UnitGroup._nextId = 1

--- Create a new unit group
-- @param options table: Configuration options
-- @option options position table: Initial position {x, y}
-- @option options surface Surface: Surface to register on
-- @option options state string: Initial state (default "gathering")
-- @option options target table: Movement target {x, y}
-- @return UnitGroup: New unit group
function UnitGroup.new(options)
    local self = setmetatable({}, UnitGroup)

    self.id = UnitGroup._nextId
    UnitGroup._nextId = UnitGroup._nextId + 1

    self.position = {
        x = options.position.x,
        y = options.position.y
    }
    self.surface = options.surface
    self.state = options.state or UnitGroup.STATES.GATHERING
    self.target = options.target or {x = 0, y = 0}

    self.members = {}  -- List of unit entities
    self.memberSet = {}  -- Set for O(1) lookup

    -- AI properties
    self.thisOwner = options.thisOwner or nil
    self.ticksSinceLastMove = 0
    self.destination = nil
    self.lastPosition = {x = self.position.x, y = self.position.y}

    -- Register
    table.insert(UnitGroup._all, self)

    return self
end

--- Check if a unit is a member of this group
-- @param entity table: Unit entity
-- @return boolean: True if unit is member
function UnitGroup:hasMember(entity)
    return self.memberSet[entity.unit_number] == true
end

--- Add a unit to this group
-- @param entity table: Unit entity to add
function UnitGroup:addMember(entity)
    if not self:hasMember(entity) then
        table.insert(self.members, entity)
        self.memberSet[entity.unit_number] = true
        -- Update group position to average of members
        self:_recalcPosition()
    end
end

--- Remove a unit from this group
-- @param entity table: Unit entity to remove
-- @return boolean: True if unit was removed
function UnitGroup:removeMember(entity)
    if self:hasMember(entity) then
        self.memberSet[entity.unit_number] = nil
        for i, m in ipairs(self.members) do
            if m == entity then
                table.remove(self.members, i)
                break
            end
        end
        self:_recalcPosition()
        return true
    end
    return false
end

--- Set movement command
-- @param target table: Target position {x, y}
function UnitGroup:setCommand(target)
    self.target = target
    if target then
        self.state = UnitGroup.STATES.MOVING
        self.destination = {x = target.x, y = target.y}
    end
end

--- Set attacking state
-- @param target table: Attack target position
function UnitGroup:setCommandAttack(target)
    self.target = target
    self.state = UnitGroup.STATES.ATTACKING
end

--- Calculate and update group position as average of members
function UnitGroup:_recalcPosition()
    if #self.members == 0 then
        return
    end

    local sumX, sumY = 0, 0
    for _, member in ipairs(self.members) do
        sumX = sumX + member.position.x
        sumY = sumY + member.position.y
    end

    self.position.x = sumX / #self.members
    self.position.y = sumY / #self.members
end

--- Move the group (simulation of group movement)
-- @param dx number: Delta X
-- @param dy number: Delta Y
function UnitGroup:move(dx, dy)
    self.lastPosition = {x = self.position.x, y = self.position.y}
    self.position.x = self.position.x + dx
    self.position.y = self.position.y + dy
    self.ticksSinceLastMove = 0

    -- Move all members
    for _, member in ipairs(self.members) do
        member.position.x = member.position.x + dx
        member.position.y = member.position.y + dy
    end
end

--- Set the group state
-- @param state string: New state
function UnitGroup:setState(state)
    self.state = state
end

--- Get the number of members
-- @return number: Member count
function UnitGroup:memberCount()
    return #self.members
end

--- Check if group is active (has members and not destroyed)
-- @return boolean: True if active
function UnitGroup:isActive()
    return #self.members > 0 and self.surface ~= nil
end

--- Get distance to a position
-- @param pos table: Target position {x, y}
-- @return number: Distance in tiles
function UnitGroup:distanceTo(pos)
    local dx = self.position.x - pos.x
    local dy = self.position.y - pos.y
    return math.sqrt(dx * dx + dy * dy)
end

--- Find all groups
-- @return table: List of all groups
function UnitGroup.all()
    return UnitGroup._all
end

--- Find groups on a surface
-- @param surface Surface: Surface to search
-- @return table: List of groups on surface
function UnitGroup.findBySurface(surface)
    local result = {}
    for _, group in ipairs(UnitGroup._all) do
        if group.surface == surface then
            table.insert(result, group)
        end
    end
    return result
end

--- Clear all groups (for test reset)
function UnitGroup.clearAll()
    UnitGroup._all = {}
    UnitGroup._nextId = 1
end

return UnitGroup