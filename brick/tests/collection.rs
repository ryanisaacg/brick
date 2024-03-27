use assert_matches::assert_matches;
use brick::{eval, Value};

#[test]
fn basic_index() {
    let result = eval(
        r#"
let array = list[30, 31, 32, 33, 34, 35, 36];
array[5]
"#,
    )
    .unwrap();
    assert_matches!(&result[..], [Value::Int32(35)]);
}

#[test]
fn arrays() {
    let result = eval(
        r#"
let array = list[1, 2, 3, 4];
let index = 0;
let total = 0;
while index < array.len() {
    total += array[index];
    index += 1;
}
total
"#,
    )
    .unwrap();
    assert_matches!(&result[..], [Value::Int32(10)]);
}

#[test]
fn array_assignments() {
    let result = eval(
        r#"
let array = list[0, 0, 0, 0];
let index = 0;
while index < array.len() {
    array[index] = index;
    index += 1;
}
array[2]
"#,
    )
    .unwrap();
    assert_matches!(&result[..], [Value::Int32(2)]);
}

#[test]
fn array_assignments_length() {
    let result = eval(
        r#"
let length = 30;
let array = list[0; length];
let index = 0;
while index < array.len() {
    array[index] = index;
    index += 1;
}
array[length - 3]
"#,
    )
    .unwrap();
    assert_matches!(&result[..], [Value::Int32(27)]);
}

#[should_panic]
#[test]
fn bad_array_assignment() {
    eval(
        r#"
let array = list[1, 2, 3, 4];
array[2] = "test";
"#,
    )
    .unwrap();
}

#[should_panic]
#[test]
fn array_index_overflow() {
    eval(
        r#"
let array = list[1, 2, 3, 4];
array[5]
"#,
    )
    .unwrap();
}

#[test]
fn append_to_array() {
    let result = eval(
        r#"
let array = list[0];
while array.len() < 10 {
    array.push(array.len());
}
array[9]
"#,
    )
    .unwrap();
    assert_matches!(&result[..], [Value::Int32(9)]);
}

#[should_panic]
#[test]
fn append_illegal_type_to_array() {
    eval(
        r#"
let array = list[0];
while array.len() < 10 {
    array.push(array.len());
}
array[9.0]
"#,
    )
    .unwrap();
}

#[test]
fn basic_dict_keys() {
    let result = eval(
        r#"
let val = dict{ [1]: 30, [2]: 8 };
val[1] - val[2]
"#,
    )
    .unwrap();
    assert_matches!(&result[..], [Value::Int32(22)]);
}

#[test]
fn write_to_dict() {
    let result = eval(
        r#"
let val = dict{ [1]: 30, [2]: 8 };
val[1] = 80;
val[1]
"#,
    )
    .unwrap();
    assert_matches!(&result[..], [Value::Int32(80)]);
}

#[test]
fn dict_contains() {
    let result = eval(
        r#"
let value = 0;
let d = dict{ [1]: 30 };
let key = 1;
if d.contains_key(ref key) {
    value += 5;
}
key = 2;
if d.contains_key(ref key) {
    value += 50;
}
value
"#,
    )
    .unwrap();
    assert_matches!(&result[..], [Value::Int32(5)]);
}

#[test]
fn insert_existing_dict() {
    let result = eval(
        r#"
let val = dict{ [1]: 10, [2]: 15, [3]: 20 };
val.insert(3, 800);
val[1] + val[2] + val[3]
"#,
    )
    .unwrap();
    assert_matches!(&result[..], [Value::Int32(825)]);
}

#[test]
fn insert_new_in_dict() {
    let result = eval(
        r#"
let val = dict{ [1]: 30 };
let i = 1;
while i < 10 {
    val.insert(i, i * 3);
    i += 1;
}
val[7]
"#,
    )
    .unwrap();
    assert_matches!(&result[..], [Value::Int32(21)]);
}
