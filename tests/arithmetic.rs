use assert_matches::assert_matches;
use brick::{eval_both, Value};

#[tokio::test]
async fn basic_precedence() {
    let result = eval_both("1 + 2 * 3").await.unwrap();
    assert_matches!(&result[..], [Value::Int32(7)]);
}

#[tokio::test]
async fn parens() {
    let result = eval_both("(1 + 2) * 3").await.unwrap();
    assert_matches!(&result[..], [Value::Int32(9)]);
}
