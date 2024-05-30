# My Toy Programming Language

This repo is a work-in-progress programming language inspired by Without Boats' ["Notes on a Smaller Rust"](https://without.boats/blog/notes-on-a-smaller-rust/), Graydon Hoare's ["The Rust I Wanted Had No Future"](https://graydon2.dreamwidth.org/307291.html), the [Austral programming language](https://austral-lang.org/), and to some degree Typescript/Go (for introducing me to the joy of structural interfaces).

Its principle goal is to be enjoyable to use for writing games and toy software. To that end, it should:

1. Be fun to write (implementation simplicity drives design decisions; no LLVM backend)
2. Allow for very fast iteration (compile very quickly, run reasonably quickly; provide facility for easy testing)
3. Prevent frustrating errors (no null pointers; no aliased mutability; sum types)
4. Trade a little performance for a little convenience, where necessary (reference-counting; easy copy/clones, etc)

It's a long way off from usable for even writing toys, though there is a small-but-growing collection of test programs.

You may notice all the sub-projects are named "brick"; this was the original name I had for the project. Unfortunately there's an existing PL project with that name! So this repo goes nameless for now.

## Sub-Crates

- `brick/`: Core language code (parse, typeheck, de-sugar, borrowcheck, low-level IR, and a basic tree-walking interpreter)
- `brick-wasm-backend/`: Emits WebAssembly, plus a wasmtime-based test harness. Not yet capable of producing linked binaries with `rust-runtime`
- `brick-runtime/`: Rust-based runtime for the language, linked into both the interpreter and the wasmtime test harness
- `brick-lsp/`: A proof-of-concept implementation of the Language Server Protocol, currently all it can do is go-to-definition for functions
- `data-test-driver/`: Helper project to run both interpreter and wasm tests over a few hundred test scripts in `tests/`. Originally they were written via the Rust `#[test]` handlers but that was slowing compile and testing across two different implementations was difficult
