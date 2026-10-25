-- Engine query keys: Factorio 2.1's find_entities_filtered accepts `type`
-- (string or array). `types` is silently ignored, so a query written with
-- `types = {"unit-spawner"}` matches EVERYTHING in the area -- characters,
-- fire, smoke, units. That let initialScan treat crash-site smoke as an enemy
-- base and build a spawner at the crash site on every new game.

local TestCollection = require("tests/framework")
local my_collection = TestCollection:new()

local function readAll(path)
    local f = io.open(path, "r")
    if not f then return nil end
    local s = f:read("*a")
    f:close()
    return s
end

local function offendingLines(path)
    local s = readAll(path)
    if not s then return nil end
    local hits = {}
    local line = 1
    for l in s:gmatch("[^\n]*") do
        if l:match("^%s*%-%-") == nil and l:match("%f[%w_]types%s*=") then
            hits[#hits + 1] = path .. ":" .. line
        end
        line = line + 1
    end
    return hits
end

my_collection:add('no_query_uses_the_ignored_types_key', function()
    local files = { "libs/Upgrade.lua", "libs/ChunkUtils.lua", "libs/Squad.lua",
                    "libs/MapUtils.lua", "libs/Observability.lua", "libs/Processor.lua" }
    local problems = {}
    for _, path in ipairs(files) do
        local hits = offendingLines(path)
        assert(hits, "cannot read " .. path)
        for _, hit in ipairs(hits) do problems[#problems + 1] = hit end
    end
    assert(#problems == 0,
        "query tables must use `type =`, not `types =` (ignored in 2.1): "
        .. table.concat(problems, ", "))
end)

return my_collection
