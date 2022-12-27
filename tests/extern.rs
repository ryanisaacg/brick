use std::sync::{Arc, Mutex};
use wasmtime::*;

pub mod common;
use common::source_to_module;

#[test]
fn arithmetic() {
    let state = Arc::new(Mutex::new(0));
    let inner = state.clone();
    let mutate_state = move |value: i32| {
        let mut state = inner.lock().unwrap();
        *state = value;
    };
    let (engine, module) = source_to_module(
        r#"
fn test(): void{
    mutate(5);
}

extern fn mutate(value: i32): void;
"#,
    )
    .unwrap();

    let mut store = Store::new(&engine, ());
    let imports = [Extern::Func(Func::wrap(&mut store, mutate_state))];
    let instance = Instance::new(&mut store, &module, &imports).unwrap();
    dbg!(instance.get_func(&mut store, "test").unwrap());
    let run: TypedFunc<(), ()> = instance.get_typed_func(&mut store, "test").unwrap();

    run.call(&mut store, ()).unwrap();

    assert_eq!(5, *state.lock().unwrap());
}
