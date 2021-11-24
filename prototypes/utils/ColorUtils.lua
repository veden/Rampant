local colorUtils = {}

-- module code

function colorUtils.makeColor(r_,g_,b_,a_)
    return { r = r_ * a_, g = g_ * a_, b = b_ * a_, a = a_ }
end

return colorUtils
