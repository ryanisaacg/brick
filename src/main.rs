use brick::{
    backend::compile,
    parser::parse,
    tokenizer::tokenize,
    typecheck::{typecheck, IRContext},
};
use std::fs;

fn main() {
    let tokens = tokenize(
        "hardcoded",
        r#"
    let a = 0;
    while a < 5 {
        a = a + 1;
    }
    a"#
        .to_string(),
    );
    let (statement, arena) = parse(tokens).unwrap();
    let mut ir_context = IRContext {
        statements: Vec::new(),
        expressions: Vec::new(),
    };
    let ir = typecheck(statement.iter(), &mut ir_context, &arena, &[]).unwrap();
    let binary = compile(ir, &ir_context);
    fs::write("out.wasm", binary).expect("Unable to write file");
}
