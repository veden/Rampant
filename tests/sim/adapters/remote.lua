--[[
    Remote Adapter - Factorio `remote` global mock

    Implements Factorio's `remote` global for inter-mod communication.
]]

local remote = {}
remote._interfaces = {}

-- Add an interface
function remote.add_interface(name, api)
    remote._interfaces[name] = api
end

-- Get an interface
function remote.get_interface(name)
    return remote._interfaces[name]
end

-- Check if interface exists
function remote.interfaces()
    local names = {}
    for name, _ in pairs(remote._interfaces) do
        table.insert(names, name)
    end
    return names
end

-- Remove an interface
function remote.remove_interface(name)
    remote._interfaces[name] = nil
end

-- Clear all interfaces (for test reset)
function remote._clear()
    remote._interfaces = {}
end

-- Register as global
_G.remote = remote

return remote