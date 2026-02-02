let start = io.time();

let s = "";
let i = 0;
while i < 20000: {
    s += "A";
    i += 1;
}

if i == 2: {
    io.println(1);
    eval(4);
} else: {
    io.println(4);
}

io.println("Time: " + str(io.time() - start));