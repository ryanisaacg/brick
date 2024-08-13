use std::env;

use brick::SourceFile;
use brick_wasm_backend::compile;

fn main() {
    let mut args = env::args();
    args.next(); // skip binary name
    let sources: Vec<_> = args
        .map(|arg| SourceFile::from_filename(String::leak(arg) as &'static str).unwrap())
        .collect();

    match compile(sources, true) {
        Ok(module) => {
            std::fs::write("out.wasm", module.as_slice()).unwrap();
            std::fs::write("runtime.wasm", runtime_binary::RUNTIME_WASM_BINARY).unwrap();
        }
        Err(e) => {
            println!("{e}");
        }
    }
}
