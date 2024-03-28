use std::{
    collections::HashMap,
    sync::{Arc, Mutex},
};

use assert_matches::assert_matches;
use brick::{check_types, eval, eval_with_bindings, ExternBinding, Value};

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
fn infinite() {
    let result = eval(
        r#"
gen fn infinite_seq(): generator[i32, void] {
    while true {
        yield 5;
    }
}

let seq = infinite_seq();
seq() + seq() + seq()
"#,
    )
    .unwrap();
    assert_matches!(&result[..], [Value::Int32(15)]);
}

#[test]
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
fn multiple_generators() {
    let result = eval(
        r#"
gen fn infinite_seq(): generator[i32, void] {
    let current = 1;
    while true {
        yield current;
        current += 1;
    }
}

let seq1 = infinite_seq();
let seq2 = infinite_seq();

seq1() + seq1() + seq2()
"#,
    )
    .unwrap();
    assert_matches!(&result[..], [Value::Int32(4)]);
}

#[test]
fn regression_test_branch_in_yielding_loop() {
    eval(
        r#"
gen fn demo(): generator[i32, void] {
    while true {
        // Completely useless if statement
        if 1 == -1 {
        }
        yield 1;
    }
}

let seq = demo();

seq() + seq()
"#,
    )
    .unwrap();
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
    check_types(
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
    check_types(
        r#"
gen fn consume(): generator[void, i32] {
    let acc = 0;
    while true {
        let next = yield;
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

#[test]
#[should_panic] // TODO: probably a coroutine-save-state problem
fn nested_coroutines() {
    let result = eval(
        r#"
gen fn ones(): generator[i32, void] {
    while true {
        yield 1;
    }
}

gen fn count_up(): generator[i32, void] {
    let x = 0;
    let seq = ones();
    while true {
        yield x;
        x += seq();
    }
}

let seq = count_up();
seq() + seq() + seq()
"#,
    )
    .unwrap();
    assert_matches!(&result[..], [Value::Int32(6)]);
}

#[test]
fn externally_driven_coroutine() {
    let mut bindings: HashMap<String, ExternBinding> = HashMap::new();
    let results = Arc::new(Mutex::new(Vec::new()));

    let push_results = results.clone();
    bindings.insert(
        "push".to_string(),
        Box::new(move |_, mut args| {
            let mut results = push_results.lock().unwrap();
            results.push(args.remove(0));
            None
        }),
    );
    bindings.insert(
        "call_generator_times".to_string(),
        Box::new(|vm, mut args| {
            let Value::Int32(times) = args.pop().unwrap() else {
                unreachable!()
            };
            let generator = args.pop().unwrap();
            for _ in 0..times {
                vm.resume_generator(generator.clone()).unwrap();
            }
            None
        }),
    );

    eval_with_bindings(
        r#"
extern fn push(number: i32);
extern fn call_generator_times(coroutine: unique generator[void, void], times: i32);

gen fn push_increasing(): generator[void, void] {
    let value = 0;
    while true {
        push(value);
        value += 1;
        yield;
    }
}

let seq = push_increasing();
call_generator_times(unique seq, 5);
"#,
        bindings,
    )
    .unwrap();

    let results = results.lock().unwrap();
    assert_eq!(results.len(), 5);
}
