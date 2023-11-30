use brick::{interpret_code, interpreter::ExternBinding, Value};
use std::{collections::HashMap, fs::read_to_string, future::Future, io::stdin, sync::Arc};

#[tokio::main(flavor = "current_thread")]
async fn main() {
    let mut bindings = HashMap::new();
    bindings.insert(
        "read".to_string(),
        ext_fn(|_| async {
            let mut input = String::new();
            stdin()
                .read_line(&mut input)
                .expect("should be able to read from stdin");
            vec![Value::Int(input.trim().parse().expect("should be an int"))]
        }),
    );
    bindings.insert(
        "write".to_string(),
        ext_fn(|values| async move {
            let Value::Int(input) = values[0] else {
                panic!("expected int");
            };
            println!("{}", input);
            vec![]
        }),
    );
    bindings.insert(
        "prompt".to_string(),
        ext_fn(|values| async move {
            let Value::String(input) = &values[0] else {
                panic!("expected string");
            };
            println!("{}", input);
            let mut input = String::new();
            stdin()
                .read_line(&mut input)
                .expect("should be able to read from stdin");
            vec![Value::String(input.trim().to_string())]
        }),
    );
    bindings.insert(
        "print".to_string(),
        ext_fn(|values| async move {
            let Value::String(input) = &values[0] else {
                panic!("expected string");
            };
            println!("{}", input);
            vec![]
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

fn ext_fn<F>(closure: impl Fn(Vec<Value>) -> F + Send + Sync + 'static) -> Arc<ExternBinding>
where
    F: Future<Output = Vec<Value>> + Send + Sync + 'static,
{
    Arc::new(move |x| Box::pin(closure(x)))
}
