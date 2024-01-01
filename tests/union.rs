use brick::typecheck_module;

#[test]
fn basic_construction() {
    typecheck_module(
        "test",
        "test.brick",
        r#"
union Number {
    int(i32),
    float(f32),
}

let num = Number { int: 12 };
num.int
"#
        .to_string(),
    )
    .unwrap();
}
