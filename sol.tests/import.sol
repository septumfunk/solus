let mod = panic(eval(panic(io.fread("sol.tests/my_mod.sol"))));
let x = 0;
while x < 99999 {
    mod.baby(x);
    x = x + 1;
}