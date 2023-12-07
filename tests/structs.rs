use assert_matches::assert_matches;
use brick::{eval, Value};

#[tokio::test]
async fn basic_construction() {
    let result = eval(r#"
struct Triple {
    a: i32,
    b: i32,
    c: i32
}

let tri = Triple { a: 1, b: 2, c: 3 };

tri.a + tri.b + tri.c
"#).await.unwrap();
    assert_matches!(&result[..], [Value::Int(6)]);
}


#[tokio::test]
async fn associated_functions() {
    let result = eval(r#"
struct Point2 {
    x: i32,
    y: i32,

    fn length2(point: Point2): i32 {
        (point.x * point.x) + (point.y * point.y)
    }
}

let x = Point2 { x: 3, y: -1 };
x.length2()
"#).await.unwrap();
    assert_matches!(&result[..], [Value::Int(10)]);
}
