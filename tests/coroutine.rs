use assert_matches::assert_matches;
use brick::{eval, eval_types, Value};

#[test]
fn yield_basic() {
    let result = eval(
        r#"
gen fn basic(): generator[i32, void] {
    yield 1;
    yield 2;
}

let seq = basic();
seq() + seq()
"#,
    )
    .unwrap();
    assert_matches!(&result[..], [Value::Int32(3)]);
}

#[test]
fn yield_once() {
    let result = eval(
        r#"
gen fn once(): generator[i32, void] {
    let x = 1200 + 34;

    yield x;

    // Ensure this code doesn't execute by forcing a panic
    let arr = list[1];
    arr[1000];
}

let seq = once();
seq()
"#,
    )
    .unwrap();
    assert_matches!(&result[..], [Value::Int32(1234)]);
}

#[test]
fn yield_twice() {
    let result = eval(
        r#"
gen fn once(): generator[i32, void] {
    let x = 1200 + 34;

    yield x;

    let y = x + 1;

    yield y;
}

let seq = once();
seq();
seq()
"#,
    )
    .unwrap();
    assert_eq!(result.last().cloned(), Some(Value::Int32(1235)));
}

#[test]
#[should_panic] // TODO: support goto labels in non-top-level blocks
fn count_up() {
    let result = eval(
        r#"
gen fn infinite_seq(): generator[i32, void] {
    let current = 1;
    while true {
        yield current;
        current += 1;
    }
}

let seq = infinite_seq();
let value = seq() + seq() + seq();
value
"#,
    )
    .unwrap();
    assert_matches!(&result[..], [Value::Int32(6)]);
}

#[test]
fn mutable_ref() {
    let result = eval(
        r#"
gen fn basic(): generator[i32, void] {
    yield 1;
}

let seq = basic();
let seq_ref = unique seq;
seq_ref()
"#,
    )
    .unwrap();
    assert_matches!(&result[..], [Value::Int32(1)]);
}

#[test]
#[should_panic] // TODO: borrowck bug, mutable references are treated as afine when passed to
                // functions
fn mutable_ref_repeated() {
    let result = eval(
        r#"
gen fn basic(): generator[i32, void] {
    yield 1;
    yield 2;
}

let seq = basic();
let seq_ref = unique seq;
seq_ref() + seq_ref()
"#,
    )
    .unwrap();
    assert_matches!(&result[..], [Value::Int32(3)]);
}

#[test]
#[should_panic]
fn immutable_ref() {
    eval(
        r#"
gen fn basic(): generator[i32, void] {
    yield 1;
    yield 2;
}

let seq = basic();
let seq_ref = ref seq;
seq_ref() + seq_ref()
"#,
    )
    .unwrap();
}

#[test]
#[should_panic] // TODO: coroutines don't actually restore their state
fn other_functions() {
    let result = eval(
        r#"
gen fn basic(): generator[i32, void] {
    let x = 5;
    yield x;
    let y = 1;
    yield y;
    yield x + y;
}

fn stack_mangle() {
    let x = 1;
    let y = 2;
    let z = 3;
    let a = x + y + z;
    let b = a * 3;
}

let seq = basic();
let a = seq(); // 5
let b = seq(); // 1
stack_mangle();
let c = seq(); // 6, but comes out as 24
c
"#,
    )
    .unwrap();
    assert_matches!(&result[..], [Value::Int32(6)]);
}

#[test]
#[should_panic] // TODO: passing values back down to coroutines
fn echo() {
    eval_types(
        r#"
gen fn echo(initial: i32): generator[i32, i32] {
    let current = initial;
    while true {
        let next = yield current;
        current = next;
    }
}

let seq = echo(1);
seq(2) + seq(3) + seq(4)
"#,
    )
    .unwrap();
}

#[test]
#[should_panic] // TODO: passing values back down to coroutines
fn no_yield_value() {
    eval_types(
        r#"
gen fn consume(): generator[void, i32] {
    let acc = 0;
    while true {
        let next = yield void;
        acc += next;
    }
}

let seq = consume();
seq(1);
seq(2);
seq(3);
"#,
    )
    .unwrap();
}
