use brick::compile_file;
use std::fs;

fn main() {
    let binary = compile_file("main").unwrap();
    fs::write("out.wasm", binary).expect("Unable to write file");
}
