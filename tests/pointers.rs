use assert_matches::assert_matches;
use brick::{eval, Value};


#[tokio::test]
#[should_panic]
async fn reference_immutability_violation() {
    eval(r#"
fn immutable_mutation(x: ref i32) {
    *x += 5;
}

let x = 2;
immutable_mutation(ref x);
"#).await.unwrap();
}

#[tokio::test]
async fn immutable_reference() {
    let result = eval(r#"
fn plus_one(x: ref i32): i32 {
    *x + 1
}

let x = 2;
plus_one(ref x)
"#).await.unwrap();
    assert_matches!(&result[..], [Value::Int32(3)]);
}

#[tokio::test]
async fn mutable_reference() {
    let result = eval(r#"
let x = 2;
let y = unique x;
*y += 2;
*y
"#).await.unwrap();
    assert_matches!(&result[..], [Value::Int32(4)]);
}

#[tokio::test]
async fn mutable_reference_param() {
    let result = eval(r#"
fn unique_mutation(x: unique i32) {
    *x += 5;
}

let x = 2;
unique_mutation(unique x);
x
"#).await.unwrap();
    assert_matches!(&result[..], [Value::Int32(7)]);
}

#[tokio::test]
async fn mutability_conversion_legal() {
    let result = eval(r#"
fn add_one(x: unique i32) {
    *x = incremented(x);
}

fn incremented(x: ref i32): i32 {
    *x + 1
}

let x = 2;
add_one(unique x);
x
"#).await.unwrap();
    assert_matches!(&result[..], [Value::Int32(3)]);
}

#[tokio::test]
#[should_panic]
async fn mutability_conversion_illegal() {
    eval(r#"
fn add_one(x: unique i32) {
   *x += 1; 
}

fn incremented(x: ref i32): i32 {
    add_one(x);
    *x
}

let x = 2;
incremented(ref x);
"#).await.unwrap();
}
