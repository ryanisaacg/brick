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
num.int ?? 0
"#,
    )
    .await
    .unwrap();
    assert_matches!(&result[..], [Value::Int32(12)]);
}

#[tokio::test]
async fn incorrect_access() {
    let result = eval(
        r#"
union Number {
    a(i32),
    b(i32),
}

let num = Number { a: 12 };
num.b ?? -50
"#,
    )
    .await
    .unwrap();
    assert_matches!(&result[..], [Value::Int32(-50)]);
}
