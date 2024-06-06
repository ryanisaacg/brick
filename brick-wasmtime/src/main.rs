use std::env;

use brick_wasm_backend::compile;
use brick_wasmtime::add_runtime_functions;
use wasmtime::{Engine, Linker, Module, Store};

fn main() -> anyhow::Result<()> {
    let mut args = env::args();
    args.next();
    let filename = args.next().expect("pass a filename");
    let contents = std::fs::read_to_string(&filename)?;
    let binary = compile("main", String::leak(filename), contents, true)?;

    let engine = Engine::default();
    let mut store = Store::new(&engine, ());
    let mut linker = Linker::new(&engine);
    add_runtime_functions(&mut linker)?;
    linker.func_wrap("bindings", "print", move |val: i32| {
        println!("{val}");
    })?;

    let module = Module::from_binary(&engine, binary.as_slice())?;
    linker.instantiate(&mut store, &module)?;

    Ok(())
}
