use assert_matches::assert_matches;
use brick::{check_types, eval, Value};

#[test]
#[should_panic]
fn must_specify_null_type() {
    check_types(
        r#"
let x = null;
"#,
    )
    .unwrap();
}

#[test]
fn basic_null() {
    let result = eval(
        r#"
let x: i32? = null;
x
"#,
    )
    .unwrap();
    assert_matches!(&result[..], [Value::Byte(0)]);
}

#[test]
fn basic_non_null_nullable() {
    let result = eval(
        r#"
let x: i32? = 70;
x
"#,
    )
    .unwrap();
    assert_matches!(&result[..], [Value::Int32(70), Value::Byte(1)]);
}

#[test]
fn null_coalesce() {
    let result = eval(
        r#"
let x: i32? = 18;
let y = x ?? 8;
x = null;
let z = x ?? 13;
y + z
"#,
    )
    .unwrap();
    assert_matches!(&result[..], [Value::Int32(31)]);
}

#[test]
fn null_traverse_is_null() {
    let result = eval(
        r#"
struct Square {
    side: i32
}


let square: Square? = null;
square?.side ?? 5
"#,
    )
    .unwrap();
    assert_matches!(&result[..], [Value::Int32(5)]);
}

#[test]
fn null_traverse_is_not_null() {
    let result = eval(
        r#"
struct Square {
    side: i32
}


let square: Square? = Square { side: 13 };
square?.side ?? 5
"#,
    )
    .unwrap();
    assert_matches!(&result[..], [Value::Int32(13)]);
}

#[test]
fn null_traverse_union() {
    let result = eval(
        r#"
union Shape {
    bouba(Circle),
    keke(Square)
}

struct Square {
    side: i32
}

struct Circle {
    radius: i32,
}

let shape = Shape { keke: Square { side: 10 } };
(shape.bouba?.radius ?? 0) + (shape.keke?.side ?? 0)
"#,
    )
    .unwrap();
    assert_matches!(&result[..], [Value::Int32(10)]);
}
