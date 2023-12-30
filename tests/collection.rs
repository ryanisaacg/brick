use assert_matches::assert_matches;
use brick::{eval, Value};

#[tokio::test]
async fn basic_index() {
    let result = eval(
        r#"
let array = [30, 31, 32, 33, 34, 35, 36];
array[5]
"#,
    )
    .await
    .unwrap();
    assert_matches!(&result[..], [Value::Int32(35)]);
}

#[tokio::test]
async fn arrays() {
    let result = eval(
        r#"
let array = [1, 2, 3, 4];
let index = 0;
let total = 0;
while index < 4 {
    total += array[index];
    index += 1;
}
total
"#,
    )
    .await
    .unwrap();
    assert_matches!(&result[..], [Value::Int32(10)]);
}

#[tokio::test]
async fn array_assignments() {
    let result = eval(
        r#"
let array = [0, 0, 0, 0];
let index = 0;
while index < 4 {
    array[index] = index;
    index += 1;
}
array[2]
"#,
    )
    .await
    .unwrap();
    assert_matches!(&result[..], [Value::Int32(2)]);
}

#[tokio::test]
async fn array_assignments_length() {
    let result = eval(
        r#"
let length = 30;
let array = [0; length];
let index = 0;
while index < length {
    array[index] = index;
    index += 1;
}
array[length - 3]
"#,
    )
    .await
    .unwrap();
    assert_matches!(&result[..], [Value::Int32(27)]);
}

#[tokio::test]
#[should_panic]
async fn bad_array_assignment() {
    eval(
        r#"
let array = [1, 2, 3, 4];
array[2] = "test";
"#,
    )
    .await
    .unwrap();
}

#[tokio::test]
#[should_panic]
async fn array_index_overflow() {
    eval(
        r#"
let array = [1, 2, 3, 4];
array[5]
"#,
    )
    .await
    .unwrap();
}

#[tokio::test]
async fn basic_dict_keys() {
    let result = eval(
        r#"
let val = dict[ [1]: 30, [2]: 8 ];
val[1] - val[2]
"#,
    )
    .await
    .unwrap();
    assert_matches!(&result[..], [Value::Int32(22)]);
}

#[tokio::test]
async fn write_to_dict() {
    let result = eval(
        r#"
let val = dict[ [1]: 30, [2]: 8 ];
val[1] = 80;
val[1]
"#,
    )
    .await
    .unwrap();
    assert_matches!(&result[..], [Value::Int32(80)]);
}
