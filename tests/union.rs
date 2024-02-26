use assert_matches::assert_matches;
use brick::{eval, Value};

#[test]
fn basic_construction() {
    let result = eval(
        r#"
union Number {
    int(i32),
    float(f32),
}

let num = Number { int: 12 };
num.int ?? 0
"#,
    )
    .unwrap();
    assert_matches!(&result[..], [Value::Int32(12)]);
}

#[test]
fn incorrect_access() {
    let result = eval(
        r#"
union Number {
    a(i32),
    b(i32),
}

let num = Number { a: 12 };
num.b ?? -50
"#,
    )
    .unwrap();
    assert_matches!(&result[..], [Value::Int32(-50)]);
}
