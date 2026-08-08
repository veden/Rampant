-- Copyright (C) 2022  veden

-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <https://www.gnu.org/licenses/>

--[[
    Executor - Applies AI commands against Factorio / storage state.

    Impure module. Touches game.*, storage.*, defines.*.

    Dispatch table: one handler per command type from libs/command.lua.
    Handlers resolve references (chunks, squads, bases) from the Universe
    using the same registries the legacy code uses (groupNumberToSquad,
    MapUtils.getChunkById), so command-driven and legacy paths stay coherent.

    Note: requires are inside handlers to avoid circular deps at load time.
    They return cached modules at event time (no file I/O).
]]

local Executor = {}

local _Universe
local _lastCreatedBaseId

local Handlers = {}

-- Squad lifecycle

Handlers.CREATE_SQUAD = function(cmd)
    local group = cmd.groupRef
    local base = _Universe.bases[cmd.baseId]
    if group and group.valid and base then
        local squad = SquadG.createSquad(nil, nil, group, cmd.settler, base)
        _Universe.groupNumberToSquad[squad.id] = squad
        if cmd.settler then
            _Universe.builderCount = (_Universe.builderCount or 0) + 1
        else
            _Universe.squadCount = (_Universe.squadCount or 0) + 1
        end
    end
end

Handlers.DESTROY_GROUP = function(cmd)
    if MetricsG then
        MetricsG.inc("group.reject." .. (cmd.reason or "unknown"))
    end
    if cmd.groupRef and cmd.groupRef.valid then
        cmd.groupRef.destroy()
    end
end

Handlers.DESTROY_SQUAD = function(cmd)
    local squad = _Universe.groupNumberToSquad[cmd.squadId]
    if squad and squad.group and squad.group.valid then
        squad.group.destroy()
    end
    _Universe.groupNumberToSquad[cmd.squadId] = nil
end

Handlers.DISPATCH_SQUAD = function(cmd)
    local squad = _Universe.groupNumberToSquad[cmd.squadId]
    if squad then
        squad.status = cmd.status
    end
end

Handlers.RALLY_SQUADS = function(cmd)
    local chunk = MapUtilsG.getChunkById(cmd.chunkId, _Universe)
    if chunk then
        SquadG.rallyUnits(chunk, cmd.tick or game.tick)
    end
end

Handlers.RETREAT_UNITS = function(cmd)
    local chunk = MapUtilsG.getChunkById(cmd.chunkId, _Universe)
    if chunk then
        SquadG.retreatUnits(chunk, cmd.cause or "outnumbered",
                            cmd.tick or game.tick, nil, cmd.radius or 40)
    end
end

Handlers.DECOMPRESS_SQUAD = function(cmd)
    local squad = _Universe.groupNumberToSquad[cmd.squadId]
    if squad then
        SquadG.decompressSquad(squad, cmd.tick or game.tick)
    end
end

-- Base lifecycle

Handlers.CREATE_BASE = function(cmd)
    local chunk = MapUtilsG.getChunkById(cmd.chunkId, _Universe)
    if chunk then
        local base = BaseUtilsG.createBase(chunk.map, chunk, cmd.tick or game.tick)
        cmd.baseId = base.id
        _lastCreatedBaseId = base.id
    end
end

Handlers.CHANGE_BASE_STATE = function(cmd)
    local base = _Universe.bases[cmd.baseId]
    if base then
        base.stateAI = cmd.newState
    end
end

Handlers.RECYCLE_BASE = function()
    BaseUtilsG.recycleBases()
end

Handlers.MODIFY_UNIT_POINTS = function(cmd)
    local base = _Universe.bases[cmd.baseId]
    if base then
        BaseUtilsG.modifyBaseUnitPoints(base, cmd.amount, cmd.source or "executor")
    end
end

Handlers.QUEUE_UPGRADE = function(cmd)
    if not cmd.entityRef or not cmd.entityRef.valid then return end
    local base = _Universe.bases[cmd.baseId]
    if base then
        BaseUtilsG.queueUpgrade(cmd.entityRef, base, nil, cmd.evolve)
    end
end

-- Chunk / structure

Handlers.REGISTER_STRUCTURE = function(cmd)
    if not cmd.entityRef or not cmd.entityRef.valid then return end
    local base = _Universe.bases[cmd.baseId]
    if base then
        ChunkUtilsG.registerEnemyBaseStructure(cmd.entityRef, base, cmd.tick or game.tick)
    end
end

Handlers.UNREGISTER_STRUCTURE = function(cmd)
    if not cmd.entityRef or not cmd.entityRef.valid then return end
    local map = cmd.mapRef
    if not map then
        local surface = cmd.entityRef.surface
        map = surface and _Universe.maps[surface.index]
    end
    if map then
        ChunkUtilsG.unregisterEnemyBaseStructure(map, cmd.entityRef, cmd.damageType)
    end
end

Handlers.MARK_CHUNK_DIRTY = function(cmd)
    MapUtilsG.markChunkDirty(cmd.chunkId, cmd.priority, cmd.tick or game.tick, _Universe)
end

Handlers.TRIGGER_CHUNK_GENERATION = function(cmd)
    local map = cmd.mapRef
    if not map or not map.surface then return end
    MapUtilsG.queueGeneratedChunk({
        surface = map.surface,
        tick = cmd.tick or game.tick,
        area = { left_top = { x = cmd.chunkX, y = cmd.chunkY } }
    })
end

function Executor.init(universe)
    _Universe = universe
end

--- Execute a single command or a table of commands.
function Executor.run(commands)
    if not commands then return end
    _lastCreatedBaseId = nil

    -- Single command
    if commands.type then
        if MetricsG then
            MetricsG.inc("cmd." .. commands.type)
        end
        local handler = Handlers[commands.type]
        if handler then
            handler(commands)
        end
        return
    end

    -- Table of commands
    for _, cmd in ipairs(commands) do
        -- Propagate newly created baseId to commands that need it
        if not cmd.baseId and _lastCreatedBaseId then
            cmd.baseId = _lastCreatedBaseId
        end
        if MetricsG then
            MetricsG.inc("cmd." .. cmd.type)
        end
        local handler = Handlers[cmd.type]
        if handler then
            handler(cmd)
        end
    end
end

return Executor
