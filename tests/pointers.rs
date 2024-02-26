use assert_matches::assert_matches;
use brick::{eval, Value};

#[test]
#[should_panic]
fn reference_immutability_violation() {
    eval(
        r#"
fn immutable_mutation(x: ref i32) {
    *x += 5;
}

let x = 2;
immutable_mutation(ref x);
"#,
    )
    .unwrap();
}

#[test]
fn immutable_reference() {
    let result = eval(
        r#"
fn plus_one(x: ref i32): i32 {
    *x + 1
}

let x = 2;
plus_one(ref x)
"#,
    )
    .unwrap();
    assert_matches!(&result[..], [Value::Int32(3)]);
}

#[test]
fn mutable_reference() {
    let result = eval(
        r#"
let x = 2;
let y = unique x;
*y += 2;
*y
"#,
    )
    .unwrap();
    assert_matches!(&result[..], [Value::Int32(4)]);
}

#[test]
fn mutable_reference_param() {
    let result = eval(
        r#"
fn unique_mutation(x: unique i32) {
    *x += 5;
}

let x = 2;
unique_mutation(unique x);
x
"#,
    )
    .unwrap();
    assert_matches!(&result[..], [Value::Int32(7)]);
}

#[test]
fn mutability_conversion_legal() {
    let result = eval(
        r#"
fn add_one(x: unique i32) {
    *x = incremented(x);
}

fn incremented(x: ref i32): i32 {
    *x + 1
}

let x = 2;
add_one(unique x);
x
"#,
    )
    .unwrap();
    assert_matches!(&result[..], [Value::Int32(3)]);
}

#[should_panic]
#[test]
fn mutability_conversion_illegal() {
    eval(
        r#"
fn add_one(x: unique i32) {
   *x += 1; 
}

fn incremented(x: ref i32): i32 {
    add_one(x);
    *x
}

let x = 2;
incremented(ref x);
"#,
    )
    .unwrap();
}

// TODO: borrow check non-main
#[test]
#[should_panic]
fn mutable_borrow_already_borrowed() {
    eval(
        r#"
fn main(): i32 {
    let x = 30;
    let immutable_reference = ref x;
    let mutable_reference = unique x;
    *mutable_reference += 1;
    *immutable_reference
}
"#,
    )
    .unwrap();
}

// TODO: borrow check non-main
#[test]
#[should_panic]
fn double_mutable_borrow() {
    eval(
        r#"
fn main(): i32 {
    let x = 30;
    let ref1 = unique x;
    let ref2 = unique x;
    *ref1 += 1;
    *ref1
}
"#,
    )
    .unwrap();
}

#[test]
fn many_immutable_borrows() {
    let result = eval(
        r#"
fn main(): i32 {
    let x = 30;
    let ref1 = ref x;
    let ref2 = ref x;
    *ref1
}

main()
"#,
    )
    .unwrap();
    assert_matches!(&result[..], [Value::Int32(30)]);
}
