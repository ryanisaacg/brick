use std::collections::HashMap;

use brick::{eval_with_bindings, Value};

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
        HashMap::new(),
    )
    .unwrap();
    assert_eq!(&result[..], &[Value::Float64(1024.0)]);
}
