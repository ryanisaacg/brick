use assert_matches::assert_matches;
use brick::{eval, Value};

#[tokio::test]
async fn basic_precedence() {
    let result = eval("1 + 2 * 3").await.unwrap();
    assert_matches!(&result[..], [Value::Int32(7)]);
}

#[tokio::test]
async fn parens() {
    let result = eval("(1 + 2) * 3").await.unwrap();
    assert_matches!(&result[..], [Value::Int32(9)]);
}

#[tokio::test]
async fn auto_cast() {
    let result = eval("1 + 2.0").await.unwrap();
    assert_eq!(result[0], Value::Float32(3.0));
}
