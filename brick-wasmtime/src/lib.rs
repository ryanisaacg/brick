use std::sync::OnceLock;

use wasmtime::{Linker, Module, Store};

static COMPILE_RUNTIME_MODULE: OnceLock<Vec<u8>> = OnceLock::new();

pub fn add_runtime_functions(linker: &mut Linker<()>, store: &mut Store<()>) -> anyhow::Result<()> {
    let runtime_module = unsafe {
        let runtime_module = COMPILE_RUNTIME_MODULE.get_or_init(|| {
            linker
                .engine()
                .precompile_module(runtime_binary::wasm_runtime())
                .unwrap()
        });
        Module::deserialize(linker.engine(), runtime_module)
    }
    .unwrap();
    linker.module(store, "brick-runtime", &runtime_module)?;

    Ok(())
}
