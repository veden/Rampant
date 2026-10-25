-- Copyright (C) 2022  veden
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <https://www.gnu.org/licenses/>

--[[
    Structured event telemetry.

    The per-tick timings say how expensive the mod is; this says WHAT it did,
    in an order that can be replayed by eye when a change breaks behaviour:
    every command executed, every point grant and spend, every upgrade, every
    player build/mine, every resource event, every field solve.

    One line per event, pipe separated, key=value pairs:

        t=12345 | cmd.DISPATCH_SQUAD | base=4 squad=31 members=12 pos=1024,-96

    Sinks, both optional and off by default (startup setting
    `rampant--eventTelemetry`):

      * file: script-output/rampant/events.jsonl, appended in batches with
        helpers.write_file. Point the game's write-data at /dev/shm (or symlink
        script-output there) to keep it in RAM instead of on disk.
      * udp: each batch sent to helpers.send_udp(port) for a live listener.

    Flushing is batched: one write per FLUSH_TICKS, or sooner when the buffer
    is full, so the telemetry itself does not become the spike it is meant to
    explain.
]]

if TelemetryG then
    return TelemetryG
end

local Telemetry = {}

local Universe
local enabled = false
-- Runtime override: the startup setting is the default, but an operator can
-- switch the stream on for a session without a restart (and without editing
-- mod-settings.dat) via remote Rampant.eventTelemetry(true).
local forced = nil
local buffer = {}
local buffered = 0
local FLUSH_TICKS = 300
local FLUSH_LINES = 500
local file = "rampant/events.jsonl"
local udpPort = nil
local totalLines = 0
local lastFlushTick = 0

local function fmt(x)
    local t = type(x)
    if t == "number" then
        if x == math.floor(x) then return tostring(x) end
        return string.format("%.3f", x)
    elseif t == "string" or t == "boolean" then
        return tostring(x)
    end
    return nil
end

function Telemetry.init(universe)
    Universe = universe
    enabled = settings.startup["rampant--eventTelemetry"]
        and settings.startup["rampant--eventTelemetry"].value or false
    local port = settings.startup["rampant--eventTelemetryUdpPort"]
    udpPort = port and port.value
    if udpPort and udpPort <= 0 then udpPort = nil end
    lastFlushTick = 0
end

function Telemetry.isEnabled()
    if forced ~= nil then return forced end
    return enabled
end

function Telemetry.setEnabled(on)
    forced = on == true
    return Telemetry.isEnabled()
end

-- kind: short event name. fields: table of key -> string/number/boolean.
-- Nil and non-primitive values are skipped, so callers can pass optional
-- fields without guarding.
function Telemetry.emit(kind, fields)
    if not Telemetry.isEnabled() then return end
    local parts = { "t=" .. tostring(game.tick), "|", kind }
    if fields then
        for key, value in pairs(fields) do
            local s = fmt(value)
            if s then
                parts[#parts + 1] = "|"
                parts[#parts + 1] = key
                parts[#parts + 1] = "="
                parts[#parts + 1] = s
            end
        end
    end
    buffered = buffered + 1
    buffer[buffered] = table.concat(parts)
end

function Telemetry.flush(tick)
    if not Telemetry.isEnabled() then return end
    tick = tick or (game and game.tick) or 0
    if buffered == 0 and (tick - lastFlushTick) < FLUSH_TICKS then return end
    if buffered > 0 then
        local payload = table.concat(buffer, "\n") .. "\n"
        helpers.write_file(file, payload, true)
        if udpPort then
            -- One datagram per batch; a listener can split on newlines.
            helpers.send_udp(udpPort, payload)
        end
        totalLines = totalLines + buffered
        buffer = {}
        buffered = 0
    end
    lastFlushTick = tick
end

function Telemetry.maybeFlush(tick)
    if not Telemetry.isEnabled() then return end
    if buffered >= FLUSH_LINES or (tick - lastFlushTick) >= FLUSH_TICKS then
        Telemetry.flush(tick)
    end
end

function Telemetry.summary()
    return {
        enabled = Telemetry.isEnabled(),
        buffered = buffered,
        written = totalLines,
        file = file,
        udpPort = udpPort,
    }
end

TelemetryG = Telemetry
return Telemetry
