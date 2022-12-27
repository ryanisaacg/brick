mod common;
use common::run_test;

// TODO: rewrite this test once we have i32 in the language
#[test]
fn basic_array() {
    assert_eq!(
        5.0,
        run_test::<i32, f32>(
            r#"
extern fn test(index: i32): f32 {
    let values = [7.0; 3];
    values[index] = values[index] - 2.0;
    values[index - index] = 4.0;
    values[index + index] = 6.0;

    values[index]
}
"#,
            1,
        )
        .unwrap()
    );
}

#[test]
fn array_of_structs() {
    assert_eq!(
        500,
        run_test(
            r#"
struct Point {
    x: i32,
    y: i32,
}

extern fn test(): i32 {
    let values = [Point { x: 500, y: 2}; 3];

    values[2].x
}
"#,
            (),
        )
        .unwrap()
    );
}

#[test]
fn multiple_arrays() {
    assert_eq!(
        120,
        run_test(
            r#"
extern fn test(): i32 {
    let initial = [120; 1];
    let last = [240; 1];
    last[0] = 6;

    initial[0]
}
"#,
            (),
        )
        .unwrap()
    );
}

#[test]
fn two_d_array() {
    assert_eq!(
        7,
        run_test(
            r#"
extern fn test(): i32 {
    let two_array = [[7; 100]; 3];

    two_array[2][30]
}
"#,
            (),
        )
        .unwrap()
    );
}

#[test]
fn three_d_array() {
    assert_eq!(
        300,
        run_test(
            r#"
extern fn test(): i32 {
    let three_array = [[[300; 100]; 100]; 500];

    three_array[10][10][10]
}
"#,
            (),
        )
        .unwrap()
    );
}
