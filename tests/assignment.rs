use assert_matches::assert_matches;
use brick::{eval, Value};

#[test]
fn assignment() {
    let result = eval(
        r#"
        let x = 1;
        x = x + 2;
        x
    "#,
    )
    .unwrap();
    assert_matches!(&result[..], [Value::Int(3)]);
}

#[test]
#[should_panic]
fn declaration_not_assignment() {
    eval(
        r#"
        let x += 1;
    "#,
    )
    .unwrap();
}

#[test]
fn numeric_assignment() {
    let result = eval(
        r#"
    let x = 1;
    x += 2;
    x *= 3;
    x -= 1;
    x /= 2;
    x
    "#,
    )
    .unwrap();
    assert_matches!(&result[..], [Value::Int(4)]);
}
