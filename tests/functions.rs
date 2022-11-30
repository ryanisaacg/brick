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
fn difference(a: i64, b: i64): i64 {
    a - b
}

fn test(): i64 {
    difference(2, 3)
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
