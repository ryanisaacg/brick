# My Toy Programming Language

This repo is a work-in-progress programming language inspired by Without Boats' ["Notes on a Smaller Rust"](https://without.boats/blog/notes-on-a-smaller-rust/), Graydon Hoare's ["The Rust I Wanted Had No Future"](https://graydon2.dreamwidth.org/307291.html), the [Austral programming language](https://austral-lang.org/), and to some degree Typescript/Go (for introducing me to the joy of structural interfaces).

Its principle goal is to be enjoyable to use for writing games and toy software. To that end, it should:

1. Be fun to write (implementation simplicity drives design decisions; no LLVM backend)
2. Allow for very fast iteration (compile very quickly, run reasonably quickly; provide facility for easy testing)
3. Prevent frustrating errors (no null pointers; no aliased mutability; sum types)
4. Trade a little performance for a little convenience, where necessary (reference-counting; easy copy/clones, etc)

It's just barely ready to write toys in, if you're willing to get your hands dirty: no standard library, no built-in IO of any kind, barely any collections, and a big pile of known bugs.

You may notice all the sub-projects are named "brick"; this was the original name I had for the project. Unfortunately there's an existing PL project with that name! So this repo goes nameless for now.

## Sub-Crates

- `brick/`: Core language code (parse, typeheck, de-sugar, borrowcheck, low-level IR, and a basic tree-walking interpreter)
- `brick-wasm-backend/`: Emits WebAssembly, plus a wasmtime-based test harness. Not yet capable of producing linked binaries with `rust-runtime`
- `brick-runtime/`: Rust-based runtime for the language, linked into both the interpreter and the wasmtime test harness
- `brick-wasmtime/`: Helper to create a linked wasmtime module that includes the brick runtime
- `brick-lsp/`: A proof-of-concept implementation of the Language Server Protocol, currently all it can do is go-to-definition for functions
- `data-test-driver/`: Helper project to run both interpreter and wasm tests over a few hundred test scripts in `tests/`. Originally they were written via the Rust `#[test]` handlers but that was slowing compile and testing across two different implementations was difficult
- `brick-fmt/`: A proof-of-concept autoformatter. It does produce *an* output but currently strips comments and whitespace, doesn't break lines, and has many quirks
- `brick-browser-runtime/`: Literally just `brick-runtime/` but re-exported as a `cdylib` so it can produce a wasm binary.
- `brick-wasmc/`: Currently it just compiles a given set of source files into a wasm file, and then dumps the runtime wasm next to it. Ideally it will link to [`wasm-merge`](https://github.com/WebAssembly/binaryen?tab=readme-ov-file#tools) and link the given source files with the runtime, and produce a single linked wasm module.


## Setup

This repo requires a recent-ish version of Rust; I've been running it on Rust 1.77.1. You should be able to `cargo test` from the root and all tests should pass. If you want to play around with a potential change to the compiler, the test suite should be a pretty good guide.

Things are a little blurrier when actually trying to write programs. Out of the box, your best bet is using `brick-wasmtime` as a scaffold. You can either copy its source or declare it as a dependency. Add whatever external bindings your program might want from its environment and you should be good to go. Currently the browser-deployment process is a little more cumbersome which I'd like to address before writing docs about it.


