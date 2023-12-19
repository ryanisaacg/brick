use assert_matches::assert_matches;
use brick::{eval, typecheck_module, Value};

#[tokio::test]
async fn arrays() {
    let result = eval(
        r#"
let array = [1, 2, 3, 4];
let index = 0;
let total = 0;
while index < 4 {
    total += array[index];
    index += 1;
}
total
"#,
    )
    .await
    .unwrap();
    assert_matches!(&result[..], [Value::Int32(10)]);
}

#[tokio::test]
#[should_panic]
async fn bad_arrays() {
    typecheck_module(
        "test",
        "test",
        r#"
let array = [1, 2, 3, 4];
array[2] = "test";
"#
        .to_string(),
    )
    .unwrap();
}
