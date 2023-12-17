use std::collections::HashMap;

use assert_matches::assert_matches;
use brick::{bind_fn, eval_both, linear_interpret_code, Value};

#[tokio::test]
async fn add() {
    let result = eval_both(
        r#"
fn add(a: i32, b: i32): i32 {
    a + b
}

add(1, 2)
"#,
    )
    .await
    .unwrap();
    assert_matches!(&result[..], [Value::Int32(3)]);
}

#[tokio::test]
async fn recursion() {
    let result = eval_both(
        r#"
fn fib(input: i32): i32 {
    if input < 1 {
        return 0;
    }
    if input == 1 {
        return 1;
    }

    fib(input - 1) + fib(input - 2)
}

fib(5)
"#,
    )
    .await
    .unwrap();
    assert_matches!(&result[..], [Value::Int32(5)]);
}

#[tokio::test]
#[should_panic]
async fn return_mismatch() {
    eval_both(
        r#"
fn function(): i32 {
    "not an i32"
}
"#,
    )
    .await
    .unwrap();
}

static mut INCR_VALUE: i32 = 0;

#[tokio::test]
async fn extern_binding() {
    let mut bindings = HashMap::new();

    bindings.insert(
        "next".to_string(),
        bind_fn(|_| async move {
            let x = unsafe { INCR_VALUE };
            unsafe {
                INCR_VALUE += 1;
            }
            Some(Value::Int32(x))
        }),
    );

    let result = linear_interpret_code(
        "",
        r#"
extern fn next(): i32;
let x = next();
x = next();
x = next();
x
"#
        .to_string(),
        bindings,
    )
    .await
    .unwrap();
    assert_matches!(&result[..], [Value::Int32(2)]);
}
