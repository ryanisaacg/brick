use assert_matches::assert_matches;
use brick::{eval, Value};

#[test]
fn basic_precedence() {
    let result = eval("1 + 2 * 3").unwrap();
    assert_matches!(&result[..], [Value::Int32(7)]);
}

#[test]
fn parens() {
    let result = eval("(1 + 2) * 3").unwrap();
    assert_matches!(&result[..], [Value::Int32(9)]);
}

#[test]
fn auto_cast() {
    let result = eval("1 + 2.0").unwrap();
    assert_eq!(result[0], Value::Float32(3.0));
}
