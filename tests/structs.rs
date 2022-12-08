mod common;
use common::run_test;

#[test]
fn points() {
    let a = 5f64;
    let b = 10f64;
    assert_eq!(
        a + b + 2f64,
        run_test(
            r#"
struct Point {
    x: f64,
    y: f64,
}

fn add_points(a: Point, b: Point): Point {
    Point {
        x: a.x + b.x,
        y: a.y + b.y,
    }
}

fn test(x: f64, y: f64): f64 {
    let new_point = add_points(Point { x, y }, Point { x: 1.0, y: 2.0 })
    new_point.y = new_point.y - 1.0
    new_point.x + new_point.y
}
"#,
            (a, b)
        )
        .unwrap()
    );
}

fn maybe_double(double: bool, value: f64) -> f64 {
    run_test(
        r#"
struct Input {
    double: bool,
    value: f64,
}

fn logic(input: Input): f64 {
    let value = input.value
    if input.double {
        value = value + value
    }
    value
}

fn test(double: bool, value: f64): f64 {
    logic(Input { double, value })
}
"#,
        (i32::from(double), value),
    )
    .unwrap()
}

#[test]
fn doubling() {
    assert_eq!(maybe_double(true, 1.0), 2.0);
    assert_eq!(maybe_double(false, 10.0), 10.0);
}

#[test]
fn references() {
    assert_eq!(
        5.0,
        run_test(
            r#"
struct Point {
    x: f64,
    y: f64,
}

fn set_x_to_five(point: unique Point): void {
    point.x = 5.0
}

fn test(): f64 {
    let point = Point { x: 3.0, y: 1.0 }
    set_x_to_five(unique point)
    point.x
}
"#,
            ()
        )
        .unwrap()
    );
}
