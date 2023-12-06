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

