use brick::{interpret_code, interpreter::ExternBinding, Value};
use std::{collections::HashMap, fs::read_to_string};

fn main() {
    let mut bindings: HashMap<String, &ExternBinding> = HashMap::new();
    bindings.insert("print".to_string(), &|values| {
        let Value::Float(input) = values[0] else {
            panic!("expected float");
        };
        println!("{}", input);
        vec![]
    });

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
