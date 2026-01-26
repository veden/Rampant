--[[
    Script Adapter - Factorio `script` global mock

    Implements Factorio's `script` global for event handling in simulation.
]]

local script = {}
script._eventHandlers = {}
script._on_init_handlers = {}
script._on_load_handlers = {}
script._on_configuration_change_handlers = {}
script._on_nth_tick_handlers = {}

-- Get or create defines global (don't overwrite if already exists)
if not _G.defines then
    _G.defines = {}
end
local defines = _G.defines

-- Ensure events table exists
if not defines.events then
    defines.events = {}
end

-- Merge in event IDs (only if not already set)
local eventIds = {
    on_init = 1,
    on_load = 2,
    on_configuration_change = 3,
    on_tick = 4,
    on_built_entity = 5,
    on_robot_built_entity = 6,
    on_entity_died = 7,
    on_chunk_generated = 8,
    on_area_clicked = 9,
    on_player_created = 10,
    on_player_mined_item = 11,
    on_chunk_charted = 12,
    on_force_created = 13,
    on_forces_merged = 14,
    on_entity_destroyed = 15,
    on_combat_robot_expired = 16,
    on_cancelled_deconstruction = 17,
    on_cancelled_upgrade = 18,
    on_marked_for_deconstruction = 19,
    on_marked_for_upgrade = 20,
    on_player_cursor_opened = 21,
    on_player_closed_gui = 22,
    on_entity_status_text_changed = 23,
    on_entity_status_changed = 24,
    on_dolly_moved = 25,
    on_player_toggled_map_editor = 26,
    on_player_changed_force = 27,
    on_robot_pre_built = 28,
}
for k, v in pairs(eventIds) do
    if not defines.events[k] then
        defines.events[k] = v
    end
end

-- Event IDs (simplified subset)
script.events = {
    on_init = defines.events.on_init,
    on_load = defines.events.on_load,
    on_configuration_change = defines.events.on_configuration_change,
    on_tick = defines.events.on_tick,
    on_built_entity = defines.events.on_built_entity,
    on_robot_built_entity = defines.events.on_robot_built_entity,
    on_entity_died = defines.events.on_entity_died,
    on_chunk_generated = defines.events.on_chunk_generated,
    on_area_clicked = defines.events.on_area_clicked,
    on_player_created = defines.events.on_player_created,
    on_player_mined_item = defines.events.on_player_mined_item,
    on_chunk_charted = defines.events.on_chunk_charted,
    on_force_created = defines.events.on_force_created,
    on_forces_merged = defines.events.on_forces_merged,
    on_entity_destroyed = defines.events.on_entity_destroyed,
    on_combat_robot_expired = defines.events.on_combat_robot_expired,
    on_cancelled_deconstruction = defines.events.on_cancelled_deconstruction,
    on_cancelled_upgrade = defines.events.on_cancelled_upgrade,
    on_marked_for_deconstruction = defines.events.on_marked_for_deconstruction,
    on_marked_for_upgrade = defines.events.on_marked_for_upgrade,
    on_player_cursor_opened = defines.events.on_player_cursor_opened,
    on_player_closed_gui = defines.events.on_player_closed_gui,
    on_entity_status_text_changed = defines.events.on_entity_status_text_changed,
    on_entity_status_changed = defines.events.on_entity_status_changed,
    on_dolly_moved = defines.events.on_dolly_moved,
    on_player_toggled_map_editor = defines.events.on_player_toggled_map_editor,
    on_player_changed_force = defines.events.on_player_changed_force,
    on_robot_pre_built = defines.events.on_robot_pre_built,
}

-- Defines stub (for event ID lookups)
script.defines = {
    events = script.events
}

-- Register event handler
function script.on_event(eventId, handler)
    if not script._eventHandlers[eventId] then
        script._eventHandlers[eventId] = {}
    end
    table.insert(script._eventHandlers[eventId], handler)
end

-- Register on_init handler
function script.on_init(handler)
    table.insert(script._on_init_handlers, handler)
end

-- Register on_load handler
function script.on_load(handler)
    table.insert(script._on_load_handlers, handler)
end

-- Register configuration change handler
function script.on_configuration_change(handler)
    table.insert(script._on_configuration_change_handlers, handler)
end

-- Register nth tick handler
function script.on_nth_tick(tickNumber, handler)
    script._on_nth_tick_handlers[tickNumber] = handler
end

-- Fire an event (used by TickSystem)
function script._fire_event(eventId, eventData)
    local handlers = script._eventHandlers[eventId]
    if handlers then
        for _, handler in ipairs(handlers) do
            handler(eventData)
        end
    end
end

-- Run initialization
function script._run_init()
    for _, handler in ipairs(script._on_init_handlers) do
        handler()
    end
end

-- Run load handlers
function script._run_load()
    for _, handler in ipairs(script._on_load_handlers) do
        handler()
    end
end

-- Run configuration change
function script._run_configuration_change(event)
    for _, handler in ipairs(script._on_configuration_change_handlers) do
        handler(event)
    end
end

-- Run nth tick handlers
function script._run_nth_tick(tick)
    local handler = script._on_nth_tick_handlers[tick]
    if handler then
        handler()
    end
end

-- Clear all handlers (for test reset)
function script._clear()
    script._eventHandlers = {}
    script._on_init_handlers = {}
    script._on_load_handlers = {}
    script._on_configuration_change_handlers = {}
    script._on_nth_tick_handlers = {}
end

-- Register as global
_G.script = script

return script