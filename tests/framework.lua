-- define a TestCollection class
local TestCollection = {}
TestCollection.__index = TestCollection
function TestCollection:new()
    local instance = {
        tests = {}, -- array of functions representing tests
        setup_fn = nil,
        teardown_fn = nil,
        failed_tests = {}, -- table to store failed tests
    }
    setmetatable(instance, TestCollection)
    return instance
end

-- add a test to the collection
function TestCollection:add(test_name, test_fn) self.tests[test_name] = test_fn end

-- add a setup function to the collection
function TestCollection:set_setup(setup_fn) self.setup_fn = setup_fn end

-- add a teardown function to the collection
function TestCollection:set_teardown(teardown_fn) self.teardown_fn = teardown_fn end

-- get the list of failed tests
function TestCollection:get_failures() return self.failed_tests end

-- run all tests in the collection
function TestCollection:run_all()
    for name, test in pairs(self.tests) do
        if self.setup_fn then self.setup_fn() end

        local success, err = pcall(test)
        if not success then
            self.failed_tests[name] = err
            print("Failed! Error message: " .. err)
        else
            print("Passed.")
        end

        if self.teardown_fn then self.teardown_fn() end
    end
end

return TestCollection
