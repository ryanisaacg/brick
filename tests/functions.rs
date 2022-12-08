mod common;
use common::run_test;

#[test]
fn arguments() {
    let a = 5i64;
    let b = 10i64;
    assert_eq!(
        a + b,
        run_test(
            r#"
fn test(a: i64, b: i64): i64 {
    a + b
}"#,
            (a, b)
        )
        .unwrap()
    );
}

#[test]
fn call_function() {
    assert_eq!(
        -1i64,
        run_test(
            r#"
fn test(): i64 {
    difference(2, 3)
}

fn difference(a: i64, b: i64): i64 {
    a - b
}
"#,
            ()
        )
        .unwrap()
    );
}

#[test]
fn call_and_return() {
    assert_eq!(
        2i64,
        run_test(
            r#"
fn test(): i64 {
    let a = 2
    let b = difference(2, 3)
    a
}

fn difference(a: i64, b: i64): i64 {
    a - b
}
"#,
            ()
        )
        .unwrap()
    );
}

#[test]
#[should_panic]
fn call_function_with_incorrect_args() {
    assert_eq!(
        -1i64,
        run_test(
            r#"
fn difference(a: f64, b: f64): f64 {
    a - b
}

fn test(): i64 {
    difference(2.0, 3)
}
"#,
            ()
        )
        .unwrap()
    );
}

#[test]
fn return_values() {
    assert_eq!(
        5i64,
        run_test(
            r#"
fn test(): i64 {
    let result = 9000
    result = returns_five()
    result
}

fn returns_five(): i64 {
    let x = 5
    x
}
"#,
            ()
        )
        .unwrap()
    );
}

fn recursive_sum(n: i64) -> i64 {
    run_test(
        r#"
fn test(n: i64): i64 {
    let result = 9000
    result = sum(n)
    result
}

fn sum(n: i64): i64 {
    let result = 0
    if n > 0 {
        result = 1
        result = n + sum(n - 1)
    }
    result
}
"#,
        n,
    )
    .unwrap()
}

#[test]
fn test_recursive_sum() {
    assert_eq!(recursive_sum(4), 10);
}

fn fibonacci(n: i64) -> i64 {
    run_test(
        r#"
fn test(n: i64): i64 {
    fib(n)
}

fn fib(n: i64): i64 {
    let result = 1
    if n > 1 {
        result = fib(n - 1) + fib(n - 2)
    }
    result
}
"#,
        n,
    )
    .unwrap()
}

#[test]
fn test_fibonacci() {
    assert_eq!(fibonacci(0), 1);
    assert_eq!(fibonacci(1), 1);
    assert_eq!(fibonacci(2), 2);
    assert_eq!(fibonacci(3), 3);
    assert_eq!(fibonacci(4), 5);
    assert_eq!(fibonacci(5), 8);
}
