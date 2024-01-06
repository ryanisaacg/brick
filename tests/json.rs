use std::collections::HashMap;

use brick::{eval_with_bindings, Value};

// Requirements
// TODO: string support
// TODO: push to dictionaries
// TODO: string keys for dictionaries
// TODO: collection assignments
// TODO: null checks OR ?. for unions

// Ergonomics
// TODO: Zero-variant unions
// TODO: push to arrays
// TODO: for loops / ranges
// TODO: empty dict operator
// TODO: don't close quotes with escaped quotes
// TODO: comments

#[tokio::test]
#[should_panic] // TODO
async fn json_parse() {
    // Null -> 0, Bool -> 1, Number -> 2, String -> 3, Array -> 4, Object -> 5
    let result = eval_with_bindings(
        r#"
extern fn ext_parse_json(val: string);
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
    Array(array[JsonValue]),
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
        let items = [JsonValue { Null: false }; length];
        let idx = 0; 
        while idx < length {
            items[idx] = parse_json_node();
            idx += 1;
        }
        return JsonValue { Array: items };
    } else if tag == 5 {
        let entries = value_as_object_entries();
        let obj = dict[ dummy: JsonValue { Null: false } ];
        let idx = 0;
        while idx < entries {
            let key = value_as_string();
            obj[key] = parse_json_node();
        }
        return JsonValue { Object: obj };
    }
    return JsonValue { Null: true };
}

let json = parse_json("{\'hello\': 1024 }");
json?.Object["hello"]?.Number
"#,
        HashMap::new(),
    )
    .await
    .unwrap();
    assert_eq!(&result[..], &[Value::Float64(1024.0)]);
}
