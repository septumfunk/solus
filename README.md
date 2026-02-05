# solus 
  solus is an embeddable scripting language with a register based VM and a focus on doing everything exactly the way *I* want to. The design of the compiler and VM are both heavily inspired by Lua's, alongside numerous other language semantics. Solus aims to reduce the amount of code required for me to bind my projects' C functions and structs with a high-level dynamically typed scripting language for both rapid prototyping and ease of use for the end user.
### [Documentation](https://solus.septumfunk.com/)

# Features
### Error Handling
Instead of returning nil on operations that fail, you can return `err` types that include a panic string, that is printed when you call panic on them, or by converting an `err` to a string. Most language operations such as member accesses will also return `err`s on failure, so watch out for that.
```
let exists = f.path;
if type(exists) == "err": {
  io.println(err);
}
exists = unwrap_or(exists, "test.txt");
let contents = unwrap(io.fread(exists));
```
### Assembly Functions
Using the `asm` keyword you can define a function as an assembly function, which uses pure VM instructions (and a little compile time magic), allowing you to make your own optimizations if you don't think my compiler is quite good enough for your needs.
```
let asm_fun = asm(2) [](x) {
    GUPO 1 0 "io";
    LOAD 2 "println";
    GET  1 1 2;
    CALL 2 1 0;
    RET  2;
};
return asm_fun("yay!");
```
# Examples
## Class
```
let cat = {
    name = "Jessica"
    fav_food = "Meow Mix"
    color = "Brown"
    babies = 0

    give_birth = [self]() {
        self.babies += 1;
        io.println("A kitten is born! That's " + string(self.babies) + " babies!");
    }

    describe = [self]() {
        io.println("My name is " + self.name + ", my fav food is " + self.fav_food +
            ", and my color is " + self.color + "!");
        io.println("I've had " + string(self.babies) + " babies so far.");
    }
};

let x = 0;
while x < 3: {
    cat.give_birth();
    x += 1;
}
cat.describe();
return cat;
```
**Output**
```
A kitten is born! That's 1 babies!
A kitten is born! That's 2 babies!
A kitten is born! That's 3 babies!
My name is Jessica, my fav food is Meow Mix, and my color is Brown!
I've had 3 babies so far.
Returned: (obj) 0xca90080a8
```
## Errors
```
let cat = unwrap(import("class.sol"));
let meow = unwrap_or(cat.meow, "meeeow!"); // Non existent member
io.println(meow);

let mod = attempt(
    []() { return import("doesnt-exist.sol"); },
    [](err) { io.println(err); return {}; }
);
```
