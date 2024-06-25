use std::env;

use brick::SourceFile;
use brick_wasm_backend::compile;

fn main() {
    let mut args = env::args();
    args.next(); // skip binary name
    let sources: Vec<_> = args
        .map(|arg| SourceFile::from_filename(String::leak(arg) as &'static str).unwrap())
        .collect();

    let module = compile(sources, true).unwrap();
    std::fs::write("out.wasm", module.as_slice()).unwrap();
}
