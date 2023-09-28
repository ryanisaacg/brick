use brick::compile_file;
use std::fs::read_to_string;

fn main() {
    compile_file(
        "example.brick",
        read_to_string("example.brick").expect("file should be readable"),
    )
    .unwrap();
}
