use anyhow::Result;
use brick::compile_source;
use wasmtime::*;

pub fn source_to_module(program: &str) -> Result<(Engine, Module)> {
    let binary = compile_source("testing", program).unwrap();
    let engine = Engine::default();
    let module = Module::from_binary(&engine, binary.as_slice())?;

    Ok((engine, module))
}

pub fn run_test<Args: WasmParams, R: WasmResults>(program: &str, args: Args) -> Result<R> {
    let (engine, module) = source_to_module(program)?;

    let mut store = Store::new(&engine, ());
    let imports = [];
    let instance = Instance::new(&mut store, &module, &imports)?;
    let run: TypedFunc<Args, R> = instance.get_typed_func(&mut store, "test")?;

    run.call(&mut store, args)
}
