use brick::eval;

#[tokio::test]
async fn basic_interfaces() {
    eval(r#"
interface Vector {
    x: f32,
    y: f32,
}

fn length2(vector: Vector): f32 {
    vector.x * vector.x + vector.y * vector.y
}
"#)
    .await
    .unwrap();
}

#[tokio::test]
async fn associated_functions() {
    eval(r#"
interface HasArea {
    fn area(area: HasArea): f32,
}

fn area_2(shape: HasArea): f32 {
    let area = shape.area();
    area * area
}

"#)
    .await
    .unwrap();
}

#[tokio::test]
async fn can_assign_structs() {
    eval(r#"
interface HasArea {
    fn area(area: HasArea): f32,
}

struct Square {
    size: f32,

    fn area(self: Square): f32 {
        self.size * self.size
    }
}

fn test() {
    let x = Square { size: 10.0 };
    area_2(x);
}

fn area_2(shape: HasArea): f32 {
    let area = shape.area();
    area * area
}

"#)
    .await
    .unwrap();
}

#[tokio::test]
#[should_panic]
async fn bad_associated_functions() {
    eval(r#"
struct Square {
    size: f32,

    fn area(self: Square): string {
        self.size * self.size
    }
}
"#)
    .await
    .unwrap();
}

