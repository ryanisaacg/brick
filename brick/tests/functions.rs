use std::sync::{Arc, Mutex};

use assert_matches::assert_matches;
use brick::{interpret_code, ExternBinding, IntepreterError, SourceFile, Value};

static mut INCR_VALUE: i32 = 0;

pub fn eval_with_bindings(
    source: &str,
    bindings: Vec<(&str, ExternBinding)>,
) -> Result<Vec<Value>, IntepreterError> {
    let (val, _) = interpret_code(
        &[SourceFile {
            module_name: "main",
            filename: "main.brick",
            contents: source.to_string(),
        }],
        bindings,
    )?;

    Ok(val)
}

#[test]
fn extern_binding() {
    let result = eval_with_bindings(
        r#"
extern fn next(): i32;
let x = next();
x = next();
x = next();
x
"#,
        vec![(
            "next",
            Box::new(|_, _| {
                let x = unsafe { INCR_VALUE };
                unsafe {
                    INCR_VALUE += 1;
                }
                Some(Value::Int32(x))
            }),
        )],
    )
    .unwrap();
    assert_matches!(&result[..], [Value::Int32(2)]);
}

#[test]
fn extern_pointer() {
    let result = eval_with_bindings(
        r#"
extern fn increment(a: unique i32);
let x = 10;
increment(unique x);
x
"#,
        vec![(
            "increment",
            Box::new(|vm, mut stack| {
                let Value::Size(pointer) = stack.pop().unwrap() else {
                    unreachable!()
                };
                let size = std::mem::size_of::<i32>();
                let mut value: i32 = *bytemuck::from_bytes(&vm.memory[pointer..(pointer + size)]);
                value += 1;
                vm.memory[pointer..(pointer + size)].copy_from_slice(bytemuck::bytes_of(&value));

                None
            }),
        )],
    )
    .unwrap();
    assert_matches!(&result[..], [Value::Int32(11)]);
}

#[test]
#[should_panic]
fn externally_driven_coroutine() {
    let results = Arc::new(Mutex::new(Vec::new()));

    let push_results = results.clone();

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
        vec![
            (
                "push",
                Box::new(move |_, mut args| {
                    let mut results = push_results.lock().unwrap();
                    results.push(args.remove(0));
                    None
                }),
            ),
            (
                "call_generator_times",
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
            ),
        ],
    )
    .unwrap();

    let results = results.lock().unwrap();
    assert_eq!(results.len(), 5);
}

// Requirements
// TODO: string support
// TODO: string keys for dictionaries

// Ergonomics
// TODO: null checks OR ?. for unions
// TODO: nullable index operator
// TODO: Zero-variant unions
// TODO: for loops / ranges
// TODO: empty array, dict operator
// TODO: don't close quotes with escaped quotes

#[test]
#[should_panic]
fn json_parse() {
    let result = eval_with_bindings(
        r#"
extern fn ext_parse_json(val: string);
// Null -> 0, Bool -> 1, Number -> 2, String -> 3, Array -> 4, Object -> 5
extern fn value_tag(): i32;
extern fn value_as_bool(): bool;
extern fn value_as_number(): f64;
extern fn value_as_string(): string;
extern fn value_as_array_items(): i32;
extern fn value_as_object_entries(): i32;

union JsonValue {
    Null(bool),
    Bool(bool),
    Number(f64),
    String(string),
    Array(list[JsonValue]),
    Object(dict[string, JsonValue]),
}

fn parse_json(val: string): JsonValue {
    ext_parse_json(val);
    parse_json_node()
}

fn parse_json_node(): JsonValue {
    let tag = value_tag();
    if tag == 0 {
        return JsonValue { Null: false };
    } else if tag == 1 {
        return JsonValue { Bool: value_as_bool() };
    } else if tag == 2 {
        return JsonValue { Number: value_as_number() };
    } else if tag == 3 {
        return JsonValue { String: value_as_string() };
    } else if tag == 4 {
        let length = value_as_array_items();
        let items = list[JsonValue { Null: false }; length];
        let idx = 0; 
        while idx < length {
            items[idx] = parse_json_node();
            idx += 1;
        }
        return JsonValue { Array: items };
    } else if tag == 5 {
        let entries = value_as_object_entries();
        let obj = dict{ dummy: JsonValue { Null: false } };
        let idx = 0;
        while idx < entries {
            let key = value_as_string();
            obj.insert(key, parse_json_node());
        }
        return JsonValue { Object: obj };
    }
    return JsonValue { Null: true };
}

let json = parse_json("{\'hello\': 1024 }");
let obj = json.Object ?? dict{ dummy: JsonValue { Null: false } };
obj["hello"].Number
"#,
        Vec::new(),
    )
    .unwrap();
    assert_eq!(&result[..], &[Value::Float64(1024.0)]);
}
