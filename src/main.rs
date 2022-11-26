use brick::{
    backend::compile,
    parser::{parse, tokenize},
};
use std::fs;

fn main() {
    let binary = compile(parse(tokenize("1 + 2")).unwrap());
    fs::write("out.wasm", binary).expect("Unable to write file");
}
