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
#[should_panic] // TODO: what's wrong with this guy
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
#[should_panic] // TODO: calling coroutines through unique references
fn other_functions() {
    let result = eval(
        r#"
gen fn basic(): generator[i32, void] {
    yield 1;
    yield 2;
}

fn double(seq: unique generator[i32, void]): i32 {
    seq() * 2
}

let seq = basic();
double(unique seq) + double(unique seq)
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
