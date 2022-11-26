use brick::{backend::compile, parser::parse, tokenizer::tokenize, typecheck::typecheck};
use std::fs;

fn main() {
    let tokens = tokenize("hardcoded", "a:=5.0 - (1.0 - 2.0);a".to_string());
    let (statement, arena) = parse(tokens).unwrap();
    let (statement, arena) = typecheck(&statement, &arena).unwrap();
    println!("{:?}, {:?}", statement, arena);
    let binary = compile(statement, &arena);
    fs::write("out.wasm", binary).expect("Unable to write file");
}
