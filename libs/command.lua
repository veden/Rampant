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
    Command - AI decision types and factory functions.

    Pure module. Zero Factorio dependencies.

    Every AI decision produces one or more command tables.
    An executor applies them against Factorio / storage state.
]]

local Command = {}
-- Command tables are one-per-decision and dropped by the executor right after its
-- handler runs, so they are recycled through a free list. release() clears every
-- key, because the shapes differ per type. A command still waiting in
-- Executor.queue keeps its table until it drains, so release happens only after
-- the handler.
local CommandFree = {}
local CommandFreeCount = 0

function Command.acquire()
    if CommandFreeCount > 0 then
        local cmd = CommandFree[CommandFreeCount]
        CommandFree[CommandFreeCount] = nil
        CommandFreeCount = CommandFreeCount - 1
        return cmd
    end
    return {}
end

function Command.release(cmd)
    if not cmd or type(cmd) ~= "table" then return end
    for key in pairs(cmd) do cmd[key] = nil end
    CommandFreeCount = CommandFreeCount + 1
    CommandFree[CommandFreeCount] = cmd
end


-- Squad lifecycle

function Command.createSquad(groupRef, baseId, settler, reason)
    local cmd = Command.acquire()
    cmd.type = "CREATE_SQUAD"
    cmd.groupRef = groupRef
    cmd.baseId = baseId
    cmd.settler = settler or false
    cmd.reason = reason or "vanilla_spawn"
    return cmd
end

function Command.destroySquad(squadId, reason)
    local cmd = Command.acquire()
    cmd.type = "DESTROY_SQUAD"
    cmd.squadId = squadId
    cmd.reason = reason or "cleanup"
    return cmd
end

function Command.destroyGroup(reason)
    local cmd = Command.acquire()
    cmd.type = "DESTROY_GROUP"
    cmd.reason = reason or "adopted"
    return cmd
end

function Command.dispatchSquad(squadId, status, targetChunk, reason)
    local cmd = Command.acquire()
    cmd.type = "DISPATCH_SQUAD"
    cmd.squadId = squadId
    cmd.status = status-- RAIDING, SETTLING, RETREATING, BUILDING, GUARDING
    cmd.targetChunk = targetChunk
    cmd.reason = reason or "pheromone_guided"
    return cmd
end

function Command.rallySquads(chunkId, cause, radius)
    local cmd = Command.acquire()
    cmd.type = "RALLY_SQUADS"
    cmd.chunkId = chunkId
    cmd.cause = cause or "death_cry"
    cmd.radius = radius or 0
    return cmd
end

function Command.retreatUnits(chunkId, reason, radius)
    local cmd = Command.acquire()
    cmd.type = "RETREAT_UNITS"
    cmd.chunkId = chunkId
    cmd.reason = reason or "outnumbered"
    cmd.radius = radius or 0
    return cmd
end

function Command.decompressSquad(squadId)
    local cmd = Command.acquire()
    cmd.type = "DECOMPRESS_SQUAD"
    cmd.squadId = squadId
    return cmd
end

-- Base lifecycle

function Command.createBase(chunkId, chunkX, chunkY)
    local cmd = Command.acquire()
    cmd.type = "CREATE_BASE"
    cmd.chunkId = chunkId
    cmd.chunkX = chunkX
    cmd.chunkY = chunkY
    return cmd
end

function Command.changeBaseState(baseId, newState, reason)
    local cmd = Command.acquire()
    cmd.type = "CHANGE_BASE_STATE"
    cmd.baseId = baseId
    cmd.newState = newState-- RAIDING, SIEGE, MIGRATION, ONSLAUGHT, PEACEFUL
    cmd.reason = reason or "evolution_threshold"
    return cmd
end

function Command.recycleBase(baseId, reason)
    local cmd = Command.acquire()
    cmd.type = "RECYCLE_BASE"
    cmd.baseId = baseId
    cmd.reason = reason or "inactive"
    return cmd
end

function Command.modifyUnitPoints(baseId, amount, source)
    local cmd = Command.acquire()
    cmd.type = "MODIFY_UNIT_POINTS"
    cmd.baseId = baseId
    cmd.amount = amount
    cmd.source = source or "unknown"
    return cmd
end

function Command.queueUpgrade(entityName, baseId, evolve)
    local cmd = Command.acquire()
    cmd.type = "QUEUE_UPGRADE"
    cmd.entityName = entityName
    cmd.baseId = baseId
    cmd.evolve = evolve or false
    return cmd
end

-- Chunk / structure

function Command.registerStructure(chunkId, baseId, entityName)
    local cmd = Command.acquire()
    cmd.type = "REGISTER_STRUCTURE"
    cmd.chunkId = chunkId
    cmd.baseId = baseId
    cmd.entityName = entityName
    return cmd
end

function Command.unregisterStructure(chunkId, entityName, damageType)
    local cmd = Command.acquire()
    cmd.type = "UNREGISTER_STRUCTURE"
    cmd.chunkId = chunkId
    cmd.entityName = entityName
    cmd.damageType = damageType
    return cmd
end

function Command.triggerChunkGeneration(chunkX, chunkY)
    local cmd = Command.acquire()
    cmd.type = "TRIGGER_CHUNK_GENERATION"
    cmd.chunkX = chunkX
    cmd.chunkY = chunkY
    return cmd
end

return Command
