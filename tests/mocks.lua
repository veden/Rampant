-- Mock objects for Factorio APIs to enable headless testing
-- This module provides comprehensive mocks for running tests without Factorio

local MockObject = function(trace, hname, x)
    local r = x or {}
    r.hname = hname
    setmetatable(r, {
        __index = function(_, key)
            local data = r._data or {}
            local calls = r._calls or {}
            local infinite = r._infinite or {}
            local consts = r._consts or {}
            local invokers = r._invokers or {}

            if key == "_data" then return data end
            if key == "_calls" then return calls end
            if key == "_infinite" then return infinite end
            if key == "_invokers" then return invokers end
            if key == "_consts" then return consts end
            if key == "_hname" then return hname end

            local fn = invokers[key]
            if fn then return fn end

            if consts[key] then return consts[key] end

            return function(...)
                local result
                local entries = data[key]
                if infinite[key] then
                    result = infinite[key]
                elseif not entries then
                    result = nil
                else
                    local lastEntryIndex = #entries
                    local entry = entries[lastEntryIndex]
                    entries[lastEntryIndex] = nil
                    if lastEntryIndex == 1 then data[key] = nil end
                    result = entry
                end
                if trace then
                    print("mock:", hname, "Function call:", key)
                    print("args:", ...)
                    print("Result:", result)
                end
                return result
            end
        end,
        __tostring = function() return hname end,
    })
    return r
end

local Mocks = {}

--- Create a mock surface with entities
---@param name string
---@return table
function Mocks.makeSurface(name)
    local entities = {}
    local chunks = {}

    local surface = MockObject(false, "surface:" .. name, {
        _entities = entities,
        _chunks = chunks,
        name = name,
        index = 1,
    })

    surface.set_data = function(_, key, value)
        surface[key] = value
    end

    surface.get_data = function(_, key)
        return surface[key]
    end

    surface.find_entities_filtered = function(_, opts)
        local result = {}
        local area = opts.area
        local position = opts.position
        local radius = opts.radius
        local etype = opts.type
        local ename = opts.name
        local eforce = opts.force

        for _, entity in pairs(entities) do
            if not entity.valid then goto continue end

            -- Check position/radius
            if position and radius then
                local ex, ey = entity.position.x, entity.position.y
                local px, py = position.x or position[1], position.y or position[2]
                local dist = ((ex - px) ^ 2 + (ey - py) ^ 2) ^ 0.5
                if dist > radius then goto continue end
            end

            -- Check area
            if area then
                local ax1, ay1 = area[1][1], area[1][2]
                local ax2, ay2 = area[2][1], area[2][2]
                local ex, ey = entity.position.x, entity.position.y
                if ex < ax1 or ex > ax2 or ey < ay1 or ey > ay2 then goto continue end
            end

            -- Check type
            if etype then
                if type(etype) == "table" then
                    local found = false
                    for _, t in ipairs(etype) do
                        if entity.type == t then found = true break end
                    end
                    if not found then goto continue end
                elseif entity.type ~= etype then goto continue end
            end

            -- Check name
            if ename then
                if type(ename) == "table" then
                    local found = false
                    for _, n in ipairs(ename) do
                        if entity.name == n then found = true break end
                    end
                    if not found then goto continue end
                elseif entity.name ~= ename then goto continue end
            end

            -- Check force
            if eforce then
                if type(eforce) == "table" then
                    local found = false
                    for _, f in ipairs(eforce) do
                        if entity.force.name == f then found = true break end
                    end
                    if not found then goto continue end
                elseif entity.force.name ~= eforce then goto continue end
            end

            table.insert(result, entity)
            ::continue::
        end
        return result
    end

    surface.count_entities_filtered = function(_, opts)
        return #surface.find_entities_filtered(_, opts)
    end

    surface.get_chunks = function()
        local result = {}
        for _, chunk in pairs(chunks) do
            table.insert(result, chunk)
        end
        local i = 0
        return function()
            i = i + 1
            return result[i]
        end
    end

    surface.is_chunk_generated = function(_, position)
        return chunks[position.x] and chunks[position.x][position.y] ~= nil
    end

    surface.add_entity = function(_, entity)
        entity.surface = surface
        entity.valid = true
        entities[entity] = entity
        if not entity.position then
            entity.position = { x = 0, y = 0 }
        end
        return entity
    end

    surface.remove_entity = function(_, entity)
        entities[entity] = nil
    end

    surface.register_for_generation = function(_, chunk)
        chunk.surface = surface
        chunks[chunk.x] = chunks[chunk.x] or {}
        chunks[chunk.x][chunk.y] = chunk
    end

    return surface
end

--- Create a mock entity
---@param props table
---@return table
function Mocks.makeEntity(props)
    props = props or {}
    local entity = MockObject(false, "entity", {
        valid = props.valid ~= false,
        name = props.name or "entity",
        type = props.type or "unit",
        position = props.position or { x = 0, y = 0 },
        force = props.force or { name = "enemy" },
        surface = props.surface,
        health = props.health or 100,
    })

    entity.destroy = function(_)
        entity.valid = false
        if entity._mock and entity._mock.on_destroy then
            entity._mock.on_destroy(entity)
        end
    end

    return entity
end

--- Create a mock force
---@param name string
---@return table
function Mocks.makeForce(name)
    local evolution = 0.5
    return MockObject(false, "force:" .. name, {
        name = name,
        evolution_factor = evolution,
        get_evolution_factor = function() return evolution end,
    })
end

--- Create a mock random generator
---@param values table
---@return function
function Mocks.makeRandomGenerator(values)
    local idx = 0
    return function()
        idx = idx + 1
        return values[idx] or 0.5
    end
end

return Mocks