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
-- along with this program.  If not, see <https://www.gnu.org/licenses/>.


if stringUtilsG then
    return stringUtilsG
end
local stringUtils = {}

local sSub = string.sub
local sGMatch = string.gmatch

function stringUtils.isRampantSetting(str)
    return sSub(str, 1, #"rampant--") == "rampant--"
end

function stringUtils.split(str)
    local result = {}
    for i in sGMatch(str, "[a-zA-Z-]+") do
        result[#result+1] = i
    end
    return result
end

function stringUtils.isMember(str, set)
    for _,s in pairs(set) do
        if str == s then
            return true
        end
    end
    return false
end

function stringUtils.intersection(set1, set2)
    local result = {}
    for s1 in pairs(set1) do
        for s2 in pairs(set2) do
            if s1 == s2 then
                result[#result+1] = s1
                break
            end
        end
    end
    return result
end

stringUtilsG = stringUtils
return stringUtils
