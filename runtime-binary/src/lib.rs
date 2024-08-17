use std::sync::OnceLock;

pub fn wasm_runtime() -> &'static [u8] {
    static RUNTIME_WASM_BINARY: &[u8] = include_bytes!(
        "../../brick-runtime/target/wasm32-unknown-unknown/release/brick_runtime.wasm"
    );
    static MODIFIED_BINARY: OnceLock<Vec<u8>> = OnceLock::new();

    MODIFIED_BINARY.get_or_init(|| {
        rust_wasm_exporter::export_dynamic_lib_globals(RUNTIME_WASM_BINARY).unwrap()
    })
}
