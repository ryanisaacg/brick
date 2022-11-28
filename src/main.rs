use brick::compile;
use std::fs;

fn main() {
    let binary = compile(
        "hardcoded",
        r#"
    let a = 0;
    while a < 5 {
        a = a + 1;
    }
    a"#,
    )
    .unwrap();
    fs::write("out.wasm", binary).expect("Unable to write file");
}
