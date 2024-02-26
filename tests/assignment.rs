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
    assert_matches!(&result[..], [Value::Int32(3)]);
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
    assert_matches!(&result[..], [Value::Int32(4)]);
}

#[test]
fn multiple_variables() {
    let result = eval(
        r#"
let a = 5;
let b = 3 + a;
let c = b / 2;
a - c
"#,
    )
    .unwrap();
    assert_matches!(&result[..], [Value::Int32(1)]);
}

#[test]
fn type_hint() {
    let result = eval(
        r#"
let x: i32 = 50;
x
"#,
    )
    .unwrap();
    assert_matches!(&result[..], [Value::Int32(50)]);
}

#[test]
#[should_panic]
fn illegal_type_hint() {
    eval(
        r#"
struct Test {}
let x: Test= 50;
x
"#,
    )
    .unwrap();
}

#[test]
#[should_panic]
fn illegal_lvalue() {
    eval(
        r#"
let x = 1;
if true { x } else { x } = 5;
"#,
    )
    .unwrap();
}

#[test]
fn conditional() {
    let result = eval(
        r#"
let x = 1;
if 1 + 1 == 2 {
    x = 1000;
} else {
    x = -1000;
}
x
    "#,
    )
    .unwrap();
    assert_eq!(result[0], Value::Int32(1000));
}
