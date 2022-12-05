mod common;
use common::run_test;

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
fn arguments() {
    assert_eq!(fibonacci(0), 1);
    assert_eq!(fibonacci(1), 1);
    assert_eq!(fibonacci(2), 2);
    assert_eq!(fibonacci(3), 3);
    assert_eq!(fibonacci(4), 5);
    assert_eq!(fibonacci(5), 8);
}
