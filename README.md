# Solus
  Solus is an embeddable scripting language with a register based VM and a focus on doing everything exactly the way *I* want to. The design of the compiler and VM are both heavily inspired by Lua's, alongside numerous other language semantics. Solus aims to reduce the amount of code required for me to bind my projects' C functions and structs with a high-level dynamically typed scripting language for both rapid prototyping and ease of use for the end user.

# Syntax
### Statements
```
let x = 4;
if x == 4 {
  io.println("four");
} else {
  panic(err("Expected four, found " + string(x)));
}

while x == 4 {
  x = x + 1;
}
```
### Core Types
```
// Primitive
let x = 4; // i64
let y = 3.14; // f64
let b = true; // bool
// Dynamic
let z = "string"; // str
let o = obj.new(); // obj
let f = [o](x) { o = x; return o; }; // fun [captures](args)
let e = err("Oh no!"); // err
```
# Features
### Error Handling
Instead of returning nil on operations that fail, you can return `err` types that include a panic string, that is printed when you call panic on them, or by converting an `err` to a string. Most language operations such as member accesses will also return `err`s on failure, so watch out for that.
```
let exists = o.member;
if type(exists) == "err" {
  io.println(err);
}
panic(io.fread("test.txt"));
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
# Standard Library
### Builtin
```
string(any); // str
err(str); // err
panic(value); // any ? panic
assert(con); // ? panic
type(value); // str
```
### IO
```
io.print(str);
io.println(str);
io.time(); // f64
io.fread(path); // ? err
io.fwrite(path); // ? err
```
### OBJ
```
obj.new(); // obj
obj.get(obj, key); // any ? err
obj.set(obj, key, value);
```
