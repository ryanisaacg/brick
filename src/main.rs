use brick::{bind_fn, interpret_code, Value};
use std::{collections::HashMap, fs::read_to_string, io::stdin};

#[tokio::main(flavor = "current_thread")]
async fn main() {
    let mut bindings = HashMap::new();
    bindings.insert(
        "read".to_string(),
        bind_fn(|_, _| async {
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
        bind_fn(|_, values| async move {
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
        .await
        .unwrap()
    );
}
