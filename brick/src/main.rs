use brick::{interpret_code, ExternBinding, Value};
use std::{collections::HashMap, fs::read_to_string};

fn main() {
    let mut bindings: HashMap<String, ExternBinding> = HashMap::new();
    bindings.insert(
        "set_fill".to_string(),
        Box::new(|_, values| {
            let Value::Float32(r) = values[0] else {
                panic!("expected float");
            };
            let Value::Float32(g) = values[1] else {
                panic!("expected float");
            };
            let Value::Float32(b) = values[2] else {
                panic!("expected float");
            };
            println!("filling {r} {g} {b}");
            None
        }),
    );
    bindings.insert(
        "draw_rect".to_string(),
        Box::new(|_, values| {
            let Value::Float32(x) = values[0] else {
                panic!("expected float");
            };
            let Value::Float32(y) = values[1] else {
                panic!("expected float");
            };
            let Value::Float32(w) = values[2] else {
                panic!("expected float");
            };
            let Value::Float32(h) = values[3] else {
                panic!("expected float");
            };
            println!("rect at {x} {y} {w} {h}");
            None
        }),
    );
    bindings.insert(
        "run".to_string(),
        Box::new(|vm, mut values| {
            let generator = values.remove(0);
            for _ in 0..10 {
                vm.resume_generator(generator.clone()).unwrap();
            }
            None
        }),
    );
    bindings.insert(
        "print".to_string(),
        Box::new(|_vm, values| {
            println!("{:?}", values);
            None
        }),
    );

    interpret_code(
        "example.brick",
        read_to_string("example.brick").expect("file should be readable"),
        bindings,
    )
    .unwrap();
}
