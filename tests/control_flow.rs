use assert_matches::assert_matches;
use brick::{eval, Value};

#[test]
fn branching() {
    let result = eval(
        r#"
let i = 0;
let n = 2;
if i < 6 {
    i += 1;
    n *= 2;
}
n
"#,
    )
    .unwrap();
    assert_matches!(&result[..], [Value::Int32(4)]);
}

#[test]
fn else_if() {
    let result = eval(
        r#"
let i = 0;
let n = 0;
if i > 3 {
    n = 30;
} else if i > 2 {
    n = 20;
} else if i > 1 {
    n = 10;
} else {
    n = 80;
}
n
"#,
    )
    .unwrap();
    assert_matches!(&result[..], [Value::Int32(80)]);
}

#[test]
fn basic_loop() {
    let result = eval(
        r#"
let i = 0;
let n = 2;
while i < 6 {
    i += 1;
    n *= 2;
}
n
"#,
    )
    .unwrap();
    assert_matches!(&result[..], [Value::Int32(128)]);
}
