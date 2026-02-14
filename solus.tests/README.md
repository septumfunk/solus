# Solus Esoteric Test Suite

A comprehensive collection of 20 strange, esoteric, and edge-case tests for the Solus programming language (v0.9.1).

## Test Overview

### 01_metamorphic_object.solu
**Tests:** Metaobject programming, self-modifying objects, obj.usemeta, obj.meta
- Creates objects that modify themselves through metaobject hooks
- Tests _str, _call, _get, _set metamethods
- Validates metaobject state tracking and history

### 02_y_combinator.solu
**Tests:** Fixed-point combinator, anonymous recursion, higher-order functions
- Implements Y-combinator for lambda calculus
- Tests factorial, fibonacci, and Ackermann functions
- Validates deep closure capture and recursive closures

### 03_string_torture.solu
**Tests:** string.len, string.sub, string.repeat, string.join, string.split
- Palindrome detection algorithm
- Nested string repetition patterns
- Complex string splitting and joining
- Substring edge cases

### 04_error_labyrinth.solu
**Tests:** err, catch, attempt, unwrap, unwrap_or, panic
- Complex error handling chains
- Triple-nested catch blocks
- Error propagation through call stacks
- Conditional unwrapping and recovery

### 05_hybrid_object.solu
**Tests:** obj.len, obj.members, obj.foreach, obj.pairs
- Objects with both array and member components
- Separate iteration over arrays vs members
- Object joining and merging
- Dynamic object construction

### 06_closure_madness.solu
**Tests:** Closure capture, upvalue lifetime, nested closures
- Multi-level closure factories
- Cascading capture chains
- Mutual closure capture
- State machine implementations

### 07_do_block_alchemy.solu
**Tests:** do expressions, trailing returns, block evaluation
- Do blocks as expressions
- Nested do blocks
- Conditional do blocks
- Do blocks in object construction

### 08_self_reference.solu
**Tests:** self keyword, object methods, method chaining
- Self-referential object methods
- Recursive object methods
- Method chaining patterns
- Builder pattern with self

### 09_short_circuit.solu
**Tests:** and/or short-circuit, side effects, evaluation order
- Short-circuit evaluation validation
- Safe division with error handling
- Complex boolean expression chains
- Ternary-like patterns with and/or

### 10_math_chaos.solu
**Tests:** math.randi, math.randf, math.mini, math.maxi, math.minf, math.maxf
- Random number generation
- Min/max operations
- Statistical testing
- Clamping and bounds checking

### 11_type_transmutation.solu
**Tests:** type, i64, f64, str, automatic coercion
- Type checking and conversion
- String to number conversion
- Number to string conversion
- Automatic type coercion in operations
- Polymorphic functions based on type

### 12_control_flow_maze.solu
**Tests:** Nested if/else, for loops, while loops, complex control flow
- Triple-nested loops
- Mixed loop types
- Early termination patterns
- Pyramid builders
- Collatz conjecture

### 13_code_sorcery.solu
**Tests:** eval, dynamic code generation, string-based metaprogramming
- Basic eval usage
- Dynamic function generation
- Code templates
- Recursive function generation
- Class generation from strings

### 14_template_wizardry.solu
**Tests:** obj.template, lazy evaluation, computed properties
- Template-based object members
- Lazy computation
- Nested templates
- Dynamic formatting

### 15_functional_paradigm.solu
**Tests:** Map, filter, reduce, composition, currying
- Functional programming patterns
- Function composition
- Currying and partial application
- Pipeline processing
- Higher-order functions

### 16_object_alchemy.solu
**Tests:** obj.get, obj.set, obj.pairs, non-identifier keys
- Dynamic key generation
- Non-identifier key names
- Object cloning and merging
- Object filtering and transformation
- Deep nested object access

### 17_operator_labyrinth.solu
**Tests:** Operator precedence, associativity, complex expressions
- Arithmetic precedence
- Comparison precedence
- Logical operator precedence
- Mixed operator expressions
- Unary operators

### 18_immutability_guard.solu
**Tests:** val declarations, constant semantics, immutability
- Basic val usage
- Val with objects (reference immutability)
- Val in closures
- Val in different scopes
- Val with complex expressions

### 19_recursion_abyss.solu
**Tests:** Deep recursion, mutual recursion, recursive algorithms
- Factorial and fibonacci
- Mutual recursion (is_even/is_odd)
- Tree traversal
- Recursive list processing
- GCD algorithm
- Tail-call patterns

### 20_boundary_breaker.solu
**Tests:** Edge cases, boundary conditions, empty collections
- Empty arrays and objects
- nil handling
- Zero values
- Single element collections
- Boundary math operations
- Large number handling

## Running the Tests

Each test is a standalone .solu file that can be run independently:

```bash
solus 01_metamorphic_object.solu
solus 02_y_combinator.solu
# ... etc
```

All tests should output "TEST PASSED" if successful and use assertions to validate behavior.

## Test Categories

### Language Features
- Closures and upvalues: 02, 06, 08
- Control flow: 12, 19
- Type system: 11, 18
- Operators: 09, 17

### Standard Library
- String module: 03
- Object module: 05, 14, 16
- Math module: 10
- Builtin functions: 04, 13

### Advanced Patterns
- Metaprogramming: 01, 14
- Functional programming: 15
- Dynamic code: 13
- Recursion: 19

### Edge Cases
- Boundary conditions: 20
- Error handling: 04
- Type coercion: 11

## Notes

- All tests are designed to be self-contained
- Tests use assertions to validate behavior
- Each test thoroughly exercises specific language features
- Tests include both common patterns and edge cases
- Output includes descriptive messages for debugging

## Version

These tests are designed for Solus version 0.9.1 and may need updates for future versions.
