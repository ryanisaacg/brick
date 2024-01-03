use assert_matches::assert_matches;
use brick::{eval, Value};

#[tokio::test]
async fn basic_construction() {
    let result = eval(
        r#"
union Number {
    int(i32),
    float(f32),
}

let num = Number { int: 12 };
num.int
"#,
    )
    .await
    .unwrap();
    assert_matches!(&result[..], [Value::Int32(12)]);
}
