mod common;
use common::run_test;

#[test]
fn arithmetic() {
    assert_eq!(
        2,
        run_test(
            r#"
fn test(): i32 {
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
        3,
        run_test(
            r#"
fn test(): i32 {
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
        6,
        run_test(
            r#"
fn test(): i32 {
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
        5,
        run_test(
            r#"
fn test(): i32 {
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
        2.5,
        run_test::<(), f32>(
            r#"
fn test(): f32 {
    1.0 + 1.5
}"#,
            ()
        )
        .unwrap()
    );
}

#[test]
fn multiple_vars() {
    assert_eq!(
        10,
        run_test(
            r#"
fn test(): i32 {
    let a = 1;
    let b = a + 1;
    let c = b + 1;
    let d = c + 1;
    a + b + c + d
}"#,
            ()
        )
        .unwrap()
    );
}

#[test]
#[should_panic]
fn require_declarations() {
    run_test::<_, ()>(
        r#"
    fn test(): void {
        a = 1
    }
    "#,
        (),
    )
    .unwrap();
}
