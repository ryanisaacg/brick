use std::collections::HashMap;
use std::sync::Arc;

use assert_matches::assert_matches;
use brick::{eval, eval_with_bindings, interpret_code, ExternBinding, Value};

#[test]
fn add() {
    let result = eval(
        r#"
// test test test

fn add(a: i32, b: i32): i32 {
    a + b
}

add(1, 2)
"#,
    )
    .unwrap();
    assert_matches!(&result[..], [Value::Int32(3)]);
}

#[test]
fn recursion() {
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
    .unwrap();
    assert_matches!(&result[..], [Value::Int32(5)]);
}

#[should_panic]
#[test]
fn return_mismatch() {
    eval(
        r#"
fn function(): i32 {
    "not an i32"
}
"#,
    )
    .unwrap();
}

static mut INCR_VALUE: i32 = 0;

#[test]
fn extern_binding() {
    let mut bindings: HashMap<String, Arc<ExternBinding>> = HashMap::new();

    bindings.insert(
        "next".to_string(),
        Arc::new(|_, _| {
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
    .unwrap();
    assert_matches!(&result[..], [Value::Int32(2)]);
}

#[test]
fn complex_call() {
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
    .unwrap();
    assert_matches!(&result[..], [Value::Int32(15)]);
}

#[test]
fn extern_pointer() {
    let mut funcs: HashMap<String, Arc<ExternBinding>> = HashMap::new();
    funcs.insert(
        "increment".to_string(),
        Arc::new(|memory, mut stack| {
            let Value::Size(pointer) = stack.pop().unwrap() else {
                unreachable!()
            };
            let size = std::mem::size_of::<i32>();
            let mut value: i32 = *bytemuck::from_bytes(&memory[pointer..(pointer + size)]);
            value += 1;
            memory[pointer..(pointer + size)].copy_from_slice(bytemuck::bytes_of(&value));

            None
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
    .unwrap();
    assert_matches!(&result[..], [Value::Int32(11)]);
}

#[should_panic]
#[test]
fn illegal_return() {
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
    .unwrap();
}

#[test]
fn doesnt_converge() {
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
    .unwrap();
}

#[test]
fn void_return() {
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
    .unwrap();
    assert_matches!(&result[..], [Value::Int32(-3)]);
}
