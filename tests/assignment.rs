use assert_matches::assert_matches;
use brick::{eval, Value};

#[tokio::test]
async fn assignment() {
    let result = eval(
        r#"
        let x = 1;
        x = x + 2;
        x
    "#,
    )
    .await
    .unwrap();
    assert_matches!(&result[..], [Value::Int32(3)]);
}

#[tokio::test]
#[should_panic]
async fn declaration_not_assignment() {
    eval(
        r#"
        let x += 1;
    "#,
    )
    .await
    .unwrap();
}

#[tokio::test]
async fn numeric_assignment() {
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
    .await
    .unwrap();
    assert_matches!(&result[..], [Value::Int32(4)]);
}

#[tokio::test]
async fn multiple_variables() {
    let result = eval(r#"
let a = 5;
let b = 3 + a;
let c = b / 2;
a - c
"#).await.unwrap();
    assert_matches!(&result[..], [Value::Int32(1)]);
}
