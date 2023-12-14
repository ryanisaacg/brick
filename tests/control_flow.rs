use assert_matches::assert_matches;
use brick::{eval, Value};

#[tokio::test]
async fn branching() {
    let result = eval(r#"
let i = 0;
let n = 2;
if i < 6 {
    i += 1;
    n *= 2;
}
n
"#).await.unwrap();
    assert_matches!(&result[..], [Value::Int32(4)]);
}

#[tokio::test]
async fn basic_loop() {
    let result = eval(r#"
let i = 0;
let n = 2;
while i < 6 {
    i += 1;
    n *= 2;
}
n
"#).await.unwrap();
    assert_matches!(&result[..], [Value::Int32(128)]);
}
