use brick::interpret_code;
use std::fs::read_to_string;

fn main() {
    interpret_code(
        "example.brick",
        read_to_string("example.brick").expect("file should be readable"),
        vec![(
            "print",
            Box::new(|_, values| {
                println!("{:?}", values);
                None
            }),
        )],
    )
    .unwrap();
}
