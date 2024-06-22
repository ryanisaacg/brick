use brick::{interpret_code, SourceFile};
use std::fs::read_to_string;

fn main() {
    interpret_code(
        vec![SourceFile {
            filename: "example.brick",
            module_name: "main",
            contents: read_to_string("example.brick").expect("file should be readable"),
        }],
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
