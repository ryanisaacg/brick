mod common;
use common::run_test;

#[test]
#[should_panic] // TODO: restore this test
fn auto_dereference() {
    assert_eq!(
        6i64,
        run_test(
            r#"
fn test(): i64 {
    let a = allocai64(5)
    incremented(a)
}

fn incremented(x: shared i64): i64 {
    x + 1
}
"#,
            ()
        )
        .unwrap()
    );
}
