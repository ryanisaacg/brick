use assert_matches::assert_matches;
use brick::{eval, Value};

#[tokio::test]
async fn associated_functions() {
    eval(
        r#"
interface HasArea {
    fn area(area: HasArea): f32,
}

fn area_2(shape: HasArea): f32 {
    let area = shape.area();
    area * area
}

"#,
    )
    .await
    .unwrap();
}

#[tokio::test]
async fn can_assign_structs_easy() {
    eval(
        r#"
interface HasArea {
    fn area(area: HasArea): i32,
}

struct Square {
    size: i32,

    fn area(self: ref Square): i32 {
        self.size * self.size
    }
}

fn area_2(shape: HasArea): i32 {
    shape.area()
}

let x = Square { size: 2 };
area_2(x);
"#,
    )
    .await
    .unwrap();
}

#[tokio::test]
async fn can_assign_structs() {
    let result = eval(
        r#"
interface HasArea {
    fn area(area: HasArea): i32,
}

struct Square {
    size: i32,

    fn area(self: ref Square): i32 {
        self.size * self.size
    }
}

fn area_2(shape: HasArea): i32 {
    let area = shape.area();
    area * area
}

let x = Square { size: 2 };
let a1 = area_2(x);
let x = Square { size: 1 };
let a2 = area_2(x);
a1 + a2
"#,
    )
    .await
    .unwrap();
    assert_matches!(&result[..], [Value::Int32(17)]);
}
