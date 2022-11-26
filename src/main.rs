use brick::{backend::compile, parser::parse_str, typecheck::typecheck};
use std::fs;

fn main() {
    let (statement, arena) = parse_str("1 + 2").unwrap();
    let (statement, arena) = typecheck(&statement, &arena).unwrap();
    let binary = compile(statement, &arena);
    fs::write("out.wasm", binary).expect("Unable to write file");
}
