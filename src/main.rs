use brick::{bind_fn, linear_interpret_code, Value};
use std::{collections::HashMap, fs::read_to_string, io::stdin};

#[tokio::main(flavor = "current_thread")]
async fn main() {
    let mut bindings = HashMap::new();
    bindings.insert(
        "read".to_string(),
        bind_fn(|_| async {
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
        bind_fn(|values| async move {
            let Value::Int32(input) = values[0] else {
                panic!("expected int");
            };
            println!("{}", input);
            None
        }),
    );
    bindings.insert(
        "prompt".to_string(),
        bind_fn(|values| async move {
            let Value::String(input) = &values[0] else {
                panic!("expected string");
            };
            println!("{}", input);
            let mut input = String::new();
            stdin()
                .read_line(&mut input)
                .expect("should be able to read from stdin");
            Some(Value::String(input.trim().to_string()))
        }),
    );
    bindings.insert(
        "print".to_string(),
        bind_fn(|values| async move {
            let Value::String(input) = &values[0] else {
                panic!("expected string");
            };
            println!("{}", input);
            None
        }),
    );

    println!(
        "{:?}",
        linear_interpret_code(
            "example.brick",
            read_to_string("example.brick").expect("file should be readable"),
            bindings
        )
        .await
        .unwrap()
    );
}
