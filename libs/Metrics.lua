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
    Metrics - Telemetry for development and verification.

    Collection-only instrumentation: counters, ring-buffer samples,
    high-water marks. NEVER influences simulation decisions.

    IMPORTANT constraint: Factorio's Lua has NO wall clock (`os` is nil,
    no game.uptime, LuaProfiler raw times are not readable from Lua).
    Therefore all in-game metrics are TICK-DOMAIN:
      counters  - cumulative event/command/system counts
      samples   - ring buffer of tick-domain values (queue size, work per
                  tick, ...)
      highWater - running maxima
      latest    - most recent sample
    Wall-clock performance (UPS) is measured EXTERNALLY via RCON
    (tools/rcon_client.py ups) and gated by the test runner.

    Gating: the whole subsystem is disabled when Universe.metricsEnabled is
    false (startup setting rampant--telemetryEnabled, default true for
    development; toggleable at runtime via remote "Rampant".setTelemetry).
    When disabled, every public function returns after one branch.
]]

if MetricsG then
    return MetricsG
end
local Metrics = {}

local Universe

local MAX_SAMPLES = 600

local function active()
    return Universe ~= nil and Universe.metricsEnabled == true
end

local function fresh()
    return {
        counters = {},
        samples = {},
        highWater = {},
        latest = {},
    }
end

function Metrics.init(universe)
    Universe = universe
    -- Authoritative gate: read the startup setting here (runs on every
    -- load path via initializeLibraries). Default true for development;
    -- flip the setting default for release. Note: settings.startup is NOT
    -- accessible in scenario context, so fall back to enabled.
    -- Create-if-absent: on_load must never mutate storage (CRC check)
    if Universe.metricsEnabled == nil then
        local telemetrySetting = settings.startup and settings.startup["rampant--telemetryEnabled"]
        Universe.metricsEnabled = (telemetrySetting and telemetrySetting.value ~= false) or true
    end
    if not Universe.metrics then
        Universe.metrics = fresh()
    end
end

function Metrics.setEnabled(enabled)
    if Universe then
        Universe.metricsEnabled = enabled == true
    end
end

function Metrics.isEnabled()
    return active()
end

--- Clear all metrics (scenario phase boundaries, test setup).
function Metrics.reset()
    if not active() then return end
    Universe.metrics = fresh()
end

--- Increment a named counter.
function Metrics.inc(name, amount)
    if not active() then return end
    local c = Universe.metrics.counters
    c[name] = (c[name] or 0) + (amount or 1)
end

--- Record a sample: ring buffer + latest + high-water.
function Metrics.sample(name, value)
    if not active() then return end
    local m = Universe.metrics
    local list = m.samples[name]
    if not list then
        list = {}
        m.samples[name] = list
    end
    list[#list + 1] = value
    if #list > MAX_SAMPLES then
        table.remove(list, 1)
    end
    m.latest[name] = value
    local hw = m.highWater[name]
    if not hw or value > hw then
        m.highWater[name] = value
    end
end

--- Record a high-water value without a sample series.
function Metrics.highWater(name, value)
    if not active() then return end
    local hw = Universe.metrics.highWater[name]
    if not hw or value > hw then
        Universe.metrics.highWater[name] = value
    end
end

--- Sample the dirty-queue size (sum of the 5 priority lists).
function Metrics.sampleDirtyQueue(dirtyCount)
    if not active() then return end
    Metrics.sample("dirtyQueueSize", dirtyCount or 0)
end

--- Pull snapshot: counters, samples, highWater, latest + diagnostics.
function Metrics.get()
    local diagnostics = {
        universeRef = Universe ~= nil,
        metricsEnabled = Universe and Universe.metricsEnabled,
        metricsTable = Universe and Universe.metrics ~= nil,
    }
    if not active() then
        diagnostics.enabled = false
        return diagnostics
    end
    local m = Universe.metrics
    diagnostics.enabled = true
    diagnostics.counters = m.counters
    diagnostics.samples = m.samples
    diagnostics.highWater = m.highWater
    diagnostics.latest = m.latest
    return diagnostics
end

--- Summary snapshot for external pull (no raw ring buffers).
function Metrics.getSummary()
    if not active() then
        return { enabled = false }
    end
    local m = Universe.metrics
    local sampleStats = {}
    for name, list in pairs(m.samples) do
        if #list > 0 then
            local sum = 0
            local max = list[1]
            for i = 1, #list do
                sum = sum + list[i]
                if list[i] > max then max = list[i] end
            end
            sampleStats[name] = { n = #list, mean = sum / #list, max = max }
        end
    end
    return {
        enabled = true,
        counters = m.counters,
        latest = m.latest,
        highWater = m.highWater,
        sampleStats = sampleStats,
    }
end

MetricsG = Metrics
return Metrics
