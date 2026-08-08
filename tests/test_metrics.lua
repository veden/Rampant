-- Metrics module tests: gating, counters, ring buffer, high-water

local TestCollection = require("tests/framework")
local my_collection = TestCollection:new()

local function makeUniverse(enabled)
    local universe = {
        metricsEnabled = enabled,
    }
    local Metrics = require("libs/Metrics")
    package.loaded["libs/Metrics"] = nil
    Metrics = require("libs/Metrics")
    Metrics.init(universe)
    -- Metrics.init reads the startup setting authoritatively; the tests
    -- control the gate explicitly
    Metrics.setEnabled(enabled)
    return universe, Metrics
end

my_collection:add('disabled_gate_blocks_all_recording', function()
    local universe, Metrics = makeUniverse(false)
    Metrics.inc("test.counter")
    Metrics.sample("test.series", 42)
    Metrics.highWater("test.hw", 99)
    local m = Metrics.get()
    assert(m.enabled == false, "get should report disabled")
    assert(m.counters == nil, "no counters when disabled")
end)

my_collection:add('enabled_records_counters', function()
    local universe, Metrics = makeUniverse(true)
    Metrics.inc("test.counter")
    Metrics.inc("test.counter", 4)
    local m = Metrics.get()
    assert(m.counters["test.counter"] == 5, "counter should accumulate")
end)

my_collection:add('sample_ring_buffer_and_latest', function()
    local universe, Metrics = makeUniverse(true)
    for i = 1, 100 do
        Metrics.sample("test.series", i)
    end
    local m = Metrics.get()
    assert(#m.samples["test.series"] == 100, "should keep all 100 samples")
    assert(m.latest["test.series"] == 100, "latest should be last value")
    assert(m.highWater["test.series"] == 100, "high-water should be max")
    -- push beyond capacity
    for i = 101, 700 do
        Metrics.sample("test.series", i)
    end
    m = Metrics.get()
    assert(#m.samples["test.series"] == 600, "ring buffer should cap at 600")
    assert(m.samples["test.series"][1] == 101, "oldest sample should drop")
end)

my_collection:add('high_water_tracks_max', function()
    local universe, Metrics = makeUniverse(true)
    Metrics.highWater("test.hw", 5)
    Metrics.highWater("test.hw", 3)
    Metrics.highWater("test.hw", 9)
    local m = Metrics.get()
    assert(m.highWater["test.hw"] == 9, "high-water should be max")
end)

my_collection:add('reset_clears_everything', function()
    local universe, Metrics = makeUniverse(true)
    Metrics.inc("test.counter")
    Metrics.sample("test.series", 1)
    Metrics.reset()
    local m = Metrics.get()
    assert(m.counters["test.counter"] == nil, "counters cleared")
    assert(m.samples["test.series"] == nil, "samples cleared")
end)

my_collection:add('dirty_queue_size_sampled', function()
    local universe, Metrics = makeUniverse(true)
    Metrics.sampleDirtyQueue({ priority1 = {1, 2}, priority2 = {3}, priority3 = {}, priority4 = {}, priority5 = {} })
    local m = Metrics.get()
    assert(m.latest["dirtyQueueSize"] == 3, "queue size should sum priority lists")
end)

my_collection:add('set_enabled_toggles_gate', function()
    local universe, Metrics = makeUniverse(false)
    Metrics.setEnabled(true)
    assert(Metrics.isEnabled() == true, "should be enabled after setEnabled(true)")
    Metrics.inc("test.counter")
    local m = Metrics.get()
    assert(m.counters["test.counter"] == 1, "counter recorded while enabled")
    Metrics.setEnabled(false)
    assert(Metrics.isEnabled() == false, "should be disabled after setEnabled(false)")
end)

return my_collection
