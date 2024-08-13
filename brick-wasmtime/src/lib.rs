use std::sync::OnceLock;

use wasmtime::{Linker, Module, Store};

static RUNTIME_WASM: &[u8] =
    include_bytes!("../../target/wasm32-unknown-unknown/release/brick_browser_runtime.wasm");
static RUNTIME_MOD: OnceLock<Vec<u8>> = OnceLock::new();

pub fn add_runtime_functions(linker: &mut Linker<()>, store: &mut Store<()>) -> anyhow::Result<()> {
    let runtime_module = unsafe {
        let runtime_module =
            RUNTIME_MOD.get_or_init(|| linker.engine().precompile_module(RUNTIME_WASM).unwrap());
        Module::deserialize(linker.engine(), runtime_module)
    }
    .unwrap();
    linker.module(store, "brick-runtime", &runtime_module)?;

    Ok(())
}
