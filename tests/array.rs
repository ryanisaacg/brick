mod common;
use common::run_test;

// TODO: rewrite this test once we have i32 in the language
#[test]
fn basic_array() {
    assert_eq!(
        5.0,
        run_test(
            r#"
fn test(index: i32): f64 {
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
        500i64,
        run_test(
            r#"
struct Point {
    x: i64,
    y: i64,
}

fn test(index: i32): i64 {
    let values = [Point { x: 500, y: 2}; 3];

    values[index].x
}
"#,
            2,
        )
        .unwrap()
    );
}

#[test]
fn multiple_arrays() {
    assert_eq!(
        120i64,
        run_test(
            r#"
fn test(index: i32): i64 {
    let initial = [120; 1];
    let last = [240; 1];
    last[index] = 6;

    initial[index]
}
"#,
            0,
        )
        .unwrap()
    );
}

#[test]
fn two_d_array() {
    assert_eq!(
        7i64,
        run_test(
            r#"
fn test(index: i32): i64 {
    let two_array = [[7; 100]; 3];

    two_array[index][index]
}
"#,
            0,
        )
        .unwrap()
    );
}

#[test]
fn three_d_array() {
    assert_eq!(
        300i64,
        run_test(
            r#"
fn test(index: i32): i64 {
    let three_array = [[[300; 100]; 100]; 500];

    three_array[index][index][index]
}
"#,
            0,
        )
        .unwrap()
    );
}
