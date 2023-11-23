use brick::{interpret_code, interpreter::ExternBinding, Value};
use std::{collections::HashMap, fs::read_to_string};

fn main() {
    let mut bindings: HashMap<String, &ExternBinding> = HashMap::new();
    bindings.insert("connect".to_string(), &|_| vec![Value::Bool(false)]);

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
