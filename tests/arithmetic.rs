use brick::{interpret_code, Value};

// TODO: assert matches?

fn eval_expr(contents: &str) -> Vec<Value> {
    interpret_code("test", format!("fn main() {{ {} }}", contents)).unwrap()
}

#[test]
fn basic_precedence() {
    let result = eval_expr("1 + 2 * 3");
    assert!(matches!(&result[..], [Value::Int(7)]));
}

#[test]
fn parens() {
    let result = eval_expr("(1 + 2) * 3");
    assert!(matches!(&result[..], [Value::Int(9)]));
}
