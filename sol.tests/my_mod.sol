let mod = obj.new();

mod.baby = [](var) {
    let x = 0;//math.randi(0, 3);
    if x == 0 {
        io.println("(>OwO)>" + string(var));
    }
    if x == 1 {
        io.println("(>XwX)>" + string(var));
    }
    if x == 2 {
        io.println("(>TwT)>" + string(var));
    }
    if x == 3 {
        io.println("(>UwU)>" + string(var));
    }
};

return mod;