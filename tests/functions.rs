use assert_matches::assert_matches;
use brick::{eval_both, Value};

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
