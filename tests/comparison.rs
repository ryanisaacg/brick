use assert_matches::assert_matches;
use brick::{eval, Value};

#[test]
fn comparison() {
    for x in 0..5 {
        for y in 0..5 {
            let result = eval(format!("{} == {}", x, y).as_str()).unwrap();
            if x == y {
                assert_matches!(&result[..], [Value::Byte(1)]);
            } else {
                assert_matches!(&result[..], [Value::Byte(0)]);
            }

            let result = eval(format!("{} != {}", x, y).as_str()).unwrap();
            if x != y {
                assert_matches!(&result[..], [Value::Byte(1)]);
            } else {
                assert_matches!(&result[..], [Value::Byte(0)]);
            }

            let result = eval(format!("{} > {}", x, y).as_str()).unwrap();
            if x > y {
                assert_matches!(&result[..], [Value::Byte(1)]);
            } else {
                assert_matches!(&result[..], [Value::Byte(0)]);
            }

            let result = eval(format!("{} < {}", x, y).as_str()).unwrap();
            if x < y {
                assert_matches!(&result[..], [Value::Byte(1)]);
            } else {
                assert_matches!(&result[..], [Value::Byte(0)]);
            }

            let result = eval(format!("{} >= {}", x, y).as_str()).unwrap();
            if x >= y {
                assert_matches!(&result[..], [Value::Byte(1)]);
            } else {
                assert_matches!(&result[..], [Value::Byte(0)]);
            }

            let result = eval(format!("{} <= {}", x, y).as_str()).unwrap();
            if x <= y {
                assert_matches!(&result[..], [Value::Byte(1)]);
            } else {
                assert_matches!(&result[..], [Value::Byte(0)]);
            }
        }
    }
}
