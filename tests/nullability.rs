use assert_matches::assert_matches;
use brick::{eval, eval_types, Value};

#[test]
#[should_panic]
fn must_specify_null_type() {
    eval_types(
        r#"
let x = null;
"#,
    )
    .unwrap();
}
