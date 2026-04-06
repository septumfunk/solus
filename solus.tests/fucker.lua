local trials = 20
local iterations = 100000

local total = 0.0
for t = 1, trials do
    local h = ""
    local start = os.clock()
    function hadd(s)
        h = h .. s
    end

    for i = 1, iterations do
        hadd("a")
    end

    local elapsed = os.clock() - start
    total = total + elapsed
end

print(total / trials)