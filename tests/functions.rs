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
