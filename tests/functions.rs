use std::collections::HashMap;

use assert_matches::assert_matches;
use brick::{bind_fn, eval, eval_with_bindings, interpret_code, Value};

#[tokio::test]
async fn add() {
    let result = eval(
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
    let result = eval(
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
    eval(
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
        bind_fn(|_, _| async move {
            let x = unsafe { INCR_VALUE };
            unsafe {
                INCR_VALUE += 1;
            }
            Some(Value::Int32(x))
        }),
    );

    let result = interpret_code(
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

#[tokio::test]
async fn complex_call() {
    let result = eval(
        r#"
fn add(a: i32, b: i32): i32 {
    a + b
}

add({
    let x = 5;
    x
}, if 3 > 2 {
    10
} else {
    -10
})
"#,
    )
    .await
    .unwrap();
    assert_matches!(&result[..], [Value::Int32(15)]);
}

#[tokio::test]
async fn extern_pointer() {
    let mut funcs = HashMap::new();
    funcs.insert(
        "increment".to_string(),
        bind_fn(|memory, mut stack| {
            std::future::ready({
                let Value::Size(pointer) = stack.pop().unwrap() else {
                    unreachable!()
                };
                let size = std::mem::size_of::<i32>();
                let mut value: i32 = *bytemuck::from_bytes(&memory[pointer..(pointer + size)]);
                value += 1;
                memory[pointer..(pointer + size)].copy_from_slice(bytemuck::bytes_of(&value));

                None
            })
        }),
    );
    let result = eval_with_bindings(
        r#"
extern fn increment(a: unique i32);
let x = 10;
increment(unique x);
x
"#,
        funcs,
    )
    .await
    .unwrap();
    assert_matches!(&result[..], [Value::Int32(11)]);
}

#[tokio::test]
#[should_panic]
async fn illegal_return() {
    eval(
        r#"
fn add(a: i32, b: i32): i32 {
    if a > b {
        return 0.0;
    }
    a + b
}
"#,
    )
    .await
    .unwrap();
}

#[tokio::test]
async fn doesnt_converge() {
    eval(
        r#"
fn cmp(a: i32, b: i32): i32 {
    if a > b {
        return 1;
    } else {
        if a < b {
            return -1;
        } else {
            return 0;
        }
    }
}
"#,
    )
    .await
    .unwrap();
}

#[tokio::test]
async fn void_return() {
    let result = eval(
        r#"
fn incr_if_positive(a: unique i32) {
    if *a <= 0 {
        return;
    };
    *a += 1;
}

let x = -3;
incr_if_positive(unique x);
x
"#,
    )
    .await
    .unwrap();
    assert_matches!(&result[..], [Value::Int32(-3)]);
}
