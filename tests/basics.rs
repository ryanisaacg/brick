use anyhow::Result;
use brick::compile;
use wasmtime::*;

struct MyState {}

fn run_basic(program: &str) -> Result<i64> {
    let binary = compile("testing", program)?;
    let engine = Engine::default();
    let module = Module::from_binary(&engine, binary.as_slice())?;

    let mut store = Store::new(&engine, MyState {});
    let imports = [];
    let instance = Instance::new(&mut store, &module, &imports)?;
    let run: TypedFunc<(), i64> = instance.get_typed_func(&mut store, "f")?;

    run.call(&mut store, ())
}

#[test]
fn arithmetic() {
    assert_eq!(2, run_basic("1 + 2 + 3 - 4").unwrap());
}

#[test]
fn assignment() {
    assert_eq!(
        3,
        run_basic(
            r#"
let a = 0;
a = 3;
a
    "#
        )
        .unwrap()
    );
}

#[test]
fn branching() {
    assert_eq!(
        6,
        run_basic(
            r#"
let a = 0;
if a < 2 {
    a = 6;
}
if a > 7 {
    a = 0;
}
a
    "#
        )
        .unwrap()
    );
}

#[test]
fn looping() {
    assert_eq!(
        5,
        run_basic(
            r#"
let a = 0;
while a < 5 {
    a = a + 1;
}
a
    "#
        )
        .unwrap()
    );
}
