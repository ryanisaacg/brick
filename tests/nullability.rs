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

#[tokio::test]
async fn basic_null() {
    let result = eval(
        r#"
let x: i32? = null;
x
"#,
    )
    .await
    .unwrap();
    assert_matches!(&result[..], [Value::Null]);
}

#[tokio::test]
async fn basic_non_null_nullable() {
    let result = eval(
        r#"
let x: i32? = 70;
x
"#,
    )
    .await
    .unwrap();
    assert_matches!(&result[..], [Value::Int32(70)]);
}
