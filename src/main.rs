use brick::interpret_code;
use std::fs::read_to_string;

fn main() {
    println!(
        "{:?}",
        interpret_code(
            "example.brick",
            read_to_string("example.brick").expect("file should be readable"),
        )
        .unwrap()
    );
}
