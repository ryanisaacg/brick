use brick::{backend::compile, parser::parse, typecheck::typecheck, tokenizer::tokenize};
use std::fs;

fn main() {
    let tokens = tokenize("a:=1.0 + 2.0;a=0.5+a;a");
    let (statement, arena) = parse(tokens).unwrap();
    let (statement, arena) = typecheck(&statement, &arena).unwrap();
    println!("{:?}, {:?}", statement, arena);
    let binary = compile(statement, &arena);
    fs::write("out.wasm", binary).expect("Unable to write file");
}
