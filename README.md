# solus 
  solus is an embeddable scripting language with a register based VM and a focus on doing everything exactly the way *I* want to. The design of the compiler and VM are both heavily inspired by Lua's, alongside numerous other language semantics. Solus aims to reduce the amount of code required for me to bind my projects' C functions and structs with a high-level dynamically typed scripting language for both rapid prototyping and ease of use for the end user.
### [Documentation](https://solus.septumfunk.com/)

# Features
### Error Handling
Instead of returning nil on operations that fail, you can return `err` types that include a panic string, that is printed when you call panic on them, or by converting an `err` to a string. Most language operations such as member accesses will also return `err`s on failure, so watch out for that.
```c
var exists = f.path;
if type(exists) == "err" {
  io.println(err);
}
exists = unwrap_or(exists, "test.txt");
val contents = unwrap(io.fread(exists));
```
# Examples
## Interactive
```c
var should_exit = false;
exit = [should_exit]() {
    should_exit = true;
};

io.println("solus " + str(solus.version) + " interactive prompt script");
io.println("call 'exit()' to leave");

var old_i = "\n";
while !should_exit {
    var i = io.input("> ");
    if i == "!\n"
        i = old_i;
    if i != "\n"
        io.println(str(catch([i]() { eval(i) })));
    old_i = i;
}
```
## Class
```
{
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
}
```
## Errors
```
val cat = unwrap(import("class.sol"));
val meow = unwrap_or(cat.meow, "meeeow!"); # Non existent member
io.println(meow);

val mod = attempt(
    []() { return import("doesnt-exist.sol"); },
    [](err) { io.println(err); return {}; }
);
```
