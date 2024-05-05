use brickc::compile;

fn main() {
    compile(
        "main",
        "example.brick",
        std::fs::read_to_string("example.brick").unwrap(),
    )
    .unwrap();
}
