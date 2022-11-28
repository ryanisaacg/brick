use brick::compile;
use std::fs;

fn main() {
    let binary = compile(
        "hardcoded",
        r#"
fn f(a: i64, b: i64): i64 {
    a + b
}"#,
    )
    .unwrap();
    fs::write("out.wasm", binary).expect("Unable to write file");
}
