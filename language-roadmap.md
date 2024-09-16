# Language Roadmap

Stuff that's planned for the language itself but is totally unimplemented or outside of any existing work I've started

closures:
- syntax for manually capturing parts of the environment (move/copy only?)
- type level support for passing closures
- use closures for iteration?

unsafe:
- functions can be declared unsafe
- you can have unsafe blocks (or statements?)
- you can only call unsafe functions from within unsafe blocks

extern:
- opaque extern pointers
- extern functions are unsafe
- externs can only do C FFI stuff, for ease of wasm backend

syntax niceties:
- keyword arguments
- remove ; requirement?
- revisit , as a separator in struct, interface declarations
- separate syntax for i64 / f64 literals
- ambiguity between dereference / multiplication in if { body } * a += 1;
- revisit variable shadowing

static functions:
- static functions can be defined on structs or unions

units of measure:
- design some system to separate units of measure into various domains (e.g. a "screen pixel" different from a "logical pixel")

errors:
- functions can define `throws`
- throw operator creates and returns an error
- try operator returns an error if one exists
- functions may optionally define a union that they throw, and all `throw`s and `try`s in the function must conform to that union
- allow discriminating on errors with a try/catch type construct or something similar for non-throws functions
- ! operator to unwrap null values

allocator:
- intrinsic bump allocator?

static data:
- add some syntax for declaring global state
- handle global state in the HIR
- handle global state in the LIR
- handle global state in the interpreter

anonymous types:
- anonymous struct / union (would this require structural structs / unions?)
- anonymous module
- anonymous interface

type parameters:
- non-reified / non-monomorphizing:
    - generic type syntax
    - types can be parameterized
    - nested type parameters are correctly resolved
- monomorphizing
- generic types can be constrained by interfaces

destructuring:
- destructuring syntax in let-statement: store vec of paths and bindings
- destructuring semantics: node that takes the thing that's being destructured and then a list of assignments, just a marker for the borrowchecker to know that it all happens at once
- (maybe) allow destructuring borrows (and maybe destructuring existing borrows?)
- sugar: allow destructuring in parameter, case statements

linear types (requires destructuring):
- types can be declared linear
- only a linear type can contain a linear type (including collections, no RC'ed linears)
- linear value reaching end of a function unused is a compile error
- linear value conditionally being dropped is a compile error
- linear value re-assigned without dropping is a compile error

partial compilation / error recovery:
- allow partial parsing with error recovery
- allow partial typechecking with error recovery

operator overloading?

case ergonomics:
- let / else equivalent?
- case statement support for nullables

metaprogramming:
- proc macros? comptime? compile-time reflection (what is that even)? no metaprogramming?
- use whatever mechanism above for ser/des

tooling:
- LSP
    - go-to-definition
        - variable
        - type
        - field access
    - find references
    - hover
    - rename (probably going to be very hard)
    - autocomplete
    - provenance range is incorrect on function declarations
- pretty-printer
- REPL
    - read
    - evaluate
    - print
    - loop
