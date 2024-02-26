use brick::{interpret_code, ExternBinding, Value};
use std::{collections::HashMap, fs::read_to_string, io::stdin, sync::Arc};

fn main() {
    let mut bindings: HashMap<String, Arc<ExternBinding>> = HashMap::new();
    bindings.insert(
        "read".to_string(),
        Arc::new(|_, _| {
            let mut input = String::new();
            stdin()
                .read_line(&mut input)
                .expect("should be able to read from stdin");
            Some(Value::Int32(
                input.trim().parse().expect("should be an int"),
            ))
        }),
    );
    bindings.insert(
        "write".to_string(),
        Arc::new(|_, values| {
            let Value::Int32(input) = values[0] else {
                panic!("expected int");
            };
            println!("{}", input);
            None
        }),
    );

    println!(
        "{:?}",
        interpret_code(
            "example.brick",
            read_to_string("example.brick").expect("file should be readable"),
            bindings
        )
        .unwrap()
    );
}
