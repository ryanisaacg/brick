use std::env;

use brick::SourceFile;
use brick_ld::InputModule;
use brick_wasm_backend::compile;

fn main() {
    let mut args = env::args();
    args.next(); // skip binary name
    let sources: Vec<_> = args
        .map(|arg| SourceFile::from_filename(String::leak(arg) as &'static str).unwrap())
        .collect();

    match compile(sources, true) {
        Ok(module) => {
            match brick_ld::link(&[
                InputModule {
                    name: "main",
                    definition: module.as_slice(),
                    public_exports: true,
                    is_start: true,
                },
                InputModule {
                    name: "brick-runtime",
                    definition: runtime_binary::wasm_runtime(),
                    public_exports: false,
                    is_start: false,
                },
            ]) {
                Ok(module) => {
                    std::fs::write("out.wasm", module).unwrap();
                }
                Err(err) => {
                    println!("internal compiler error: {err}");
                }
            }
        }
        Err(e) => {
            println!("{e}");
        }
    }
}
