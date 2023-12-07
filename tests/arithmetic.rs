use assert_matches::assert_matches;
use brick::{eval, Value};

#[tokio::test]
async fn basic_precedence() {
    let result = eval("1 + 2 * 3").await.unwrap();
    assert_matches!(&result[..], [Value::Int(7)]);
}

#[tokio::test]
async fn parens() {
    let result = eval("(1 + 2) * 3").await.unwrap();
    assert_matches!(&result[..], [Value::Int(9)]);
}
