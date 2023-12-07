use assert_matches::assert_matches;
use brick::{eval, Value};

#[tokio::test]
async fn basic_construction() {
    let result = eval(r#"
struct Triple {
    a: i32,
    b: i32,
    c: i32
}

let tri = Triple { a: 1, b: 2, c: 3 };

tri.a + tri.b + tri.c
"#).await.unwrap();
    assert_matches!(&result[..], [Value::Int(6)]);
}

