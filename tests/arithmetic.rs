use assert_matches::assert_matches;
use brick::{eval, Value};

// TODO: assert matches?

#[test]
fn basic_precedence() {
    let result = eval("1 + 2 * 3").unwrap();
    assert_matches!(&result[..], [Value::Int(7)]);
}

#[test]
fn parens() {
    let result = eval("(1 + 2) * 3").unwrap();
    assert_matches!(&result[..], [Value::Int(9)]);
}
