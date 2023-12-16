use assert_matches::assert_matches;
use brick::{eval_both, Value};

#[tokio::test]
async fn basic_construction() {
    let result = eval_both(
        r#"
struct Triple {
    a: i32,
    b: i32,
    c: i32
}

let tri = Triple { a: 1, b: 2, c: 3 };

tri.a + tri.b + tri.c
"#,
    )
    .await
    .unwrap();
    assert_matches!(&result[..], [Value::Int32(6)]);
}

#[tokio::test]
async fn associated_functions() {
    let result = eval_both(
        r#"
struct Point2 {
    x: i32,
    y: i32,

    fn length2(point: Point2): i32 {
        (point.x * point.x) + (point.y * point.y)
    }
}

let x = Point2 { x: 3, y: -1 };
x.length2()
"#,
    )
    .await
    .unwrap();
    assert_matches!(&result[..], [Value::Int32(10)]);
}


#[tokio::test]
async fn nested_structs() {
    let result = eval_both(
        r#"
struct Point2 {
    x: i32,
    y: i32,
}

struct Rectangle {
    tl: Point2,
    br: Point2,

    fn width(self: Rectangle): i32 {
        self.br.x - self.tl.x
    }

    fn height(self: Rectangle): i32 {
        self.br.y - self.tl.y
    }

    fn area(self: Rectangle): i32 {
        self.width() * self.height()
    }
}

let tl = Point2 { x: 0, y : 0 };
let br = Point2 { x: 10, y: 10 };

let rect = Rectangle { tl, br };
tl = Point2 { x: 1000, y: 1000 };

rect.area()
"#,
    )
    .await
    .unwrap();
    assert_matches!(&result[..], [Value::Int32(100)]);
}

#[tokio::test]
async fn order_matters() {
    let result = eval_both(r#"
struct Triplet {
    a: Point,
    b: Point,
    c: Point,
}
struct Point {
    x: i32,
    y: i32,
}

let a = Point { x: 1, y: 0 };
let b = Point { x: 0, y: 2 };
let c = Point { x: 3, y: 0 };
let n = Triplet { a, b, c};

n.a.x + n.b.y * n.c.x
"#).await.unwrap();
    assert_matches!(&result[..], [Value::Int32(7)]);
}

