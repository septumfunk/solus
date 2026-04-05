local h = ''
local start = os.clock()
for i = 1, 100000 do
    h = h .. 'a'
end
print(os.clock() - start)