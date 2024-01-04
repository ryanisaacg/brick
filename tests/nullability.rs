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
    assert_matches!(&result[..], [Value::Bool(false)]);
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
    assert_matches!(&result[..], [Value::Int32(70), Value::Bool(true)]);
}

#[tokio::test]
async fn null_coalesce() {
    let result = eval(
        r#"
let x: i32? = 18;
let y = x ?? 8;
x = null;
let z = x ?? 13;
y + z
"#,
    )
    .await
    .unwrap();
    assert_matches!(&result[..], [Value::Int32(31)]);
}
