mod common;
use common::run_test;

#[test]
fn arithmetic() {
    assert_eq!(
        2i64,
        run_test(
            r#"
fn test(): i64 {
    1 + 2 + 3 - 4
}"#,
            ()
        )
        .unwrap()
    );
}

#[test]
fn assignment() {
    assert_eq!(
        3i64,
        run_test(
            r#"
fn test(): i64 {
    let a = 0;
    a = 3;
    a
}
    "#,
            ()
        )
        .unwrap()
    );
}

#[test]
fn branching() {
    assert_eq!(
        6i64,
        run_test(
            r#"
fn test(): i64 {
    let a = 0;
    if a < 2 {
        a = 6;
    }
    if a > 7 {
        a = 0;
    }
    a
}
    "#,
            ()
        )
        .unwrap()
    );
}

#[test]
fn looping() {
    assert_eq!(
        5i64,
        run_test(
            r#"
fn test(): i64 {
    let a = 0;
    while a < 5 {
        a = a + 1;
    }
    a
}
    "#,
            ()
        )
        .unwrap()
    );
}

#[test]
fn floating_point() {
    assert_eq!(
        2.5f64,
        run_test(
            r#"
fn test(): f64 {
    1.0 + 1.5
}"#,
            ()
        )
        .unwrap()
    );
}
