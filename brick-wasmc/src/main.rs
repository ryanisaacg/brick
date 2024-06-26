use std::env;

use brick::SourceFile;
use brick_wasm_backend::compile;

static RUNTIME_WASM: &[u8] =
    include_bytes!("../../target/wasm32-unknown-unknown/release/brick_browser_runtime.wasm");

fn main() {
    let mut args = env::args();
    args.next(); // skip binary name
    let sources: Vec<_> = args
        .map(|arg| SourceFile::from_filename(String::leak(arg) as &'static str).unwrap())
        .collect();

    let module = compile(sources, true).unwrap();
    std::fs::write("out.wasm", module.as_slice()).unwrap();
    std::fs::write("runtime.wasm", RUNTIME_WASM).unwrap();
}
