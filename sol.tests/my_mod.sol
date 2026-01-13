let mod = obj.new();

mod.baby = [](var) {
    let x = math.randi(0, 3);
    _g.io.println("(>OwO)>" + string(var));
};

return mod;