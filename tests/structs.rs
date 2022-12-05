mod common;
use common::run_test;

#[test]
#[should_panic] // TODO: backend and analyzer
fn structs() {
    let a = 5i64;
    let b = 10i64;
    assert_eq!(
        a + b,
        run_test(
            r#"
struct Point {
    x: f64,
    y: f64,
}

fn test(x: f64, y: f64): f64 {
    let new_point = add_points(Point { x, y }, Point { x: 1.0, y: 2.0 })
    new_point.y = new_point.y - 1
    new_point.x + new_point.y
}

fn add_points(a: Point, b: Point): Point {
    Point {
        x: a.x + b.x,
        y: a.y + b.y,
    }
}
"#,
            (a, b)
        )
        .unwrap()
    );
}
