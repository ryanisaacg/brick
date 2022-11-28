use anyhow::Result;
use brick::compile;
use wasmtime::*;

struct MyState {}

pub fn run_test<Args: WasmParams, R: WasmResults>(program: &str, args: Args) -> Result<R> {
    let binary = compile("testing", program)?;
    let engine = Engine::default();
    let module = Module::from_binary(&engine, binary.as_slice())?;

    let mut store = Store::new(&engine, MyState {});
    let imports = [];
    let instance = Instance::new(&mut store, &module, &imports)?;
    let run: TypedFunc<Args, R> = instance.get_typed_func(&mut store, "test")?;

    run.call(&mut store, args)
}
