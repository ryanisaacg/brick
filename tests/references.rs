mod common;
use common::run_test;

#[test]
fn auto_dereference() {
    assert_eq!(
        10,
        run_test(
            r#"
fn test(): i32 {
    let a = 5
    let b = shared a
    let c = a + b
    c
}
"#,
            ()
        )
        .unwrap()
    );
}

#[test]
fn mutation_auto_dereference() {
    assert_eq!(
        6,
        run_test(
            r#"
fn test(): i32 {
    let a = 5
    increment(unique a)
    a
}

fn increment(val: unique i32): void {
    val = val + 1
}

"#,
            ()
        )
        .unwrap()
    );
}
