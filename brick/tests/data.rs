use brick::{check_types, eval};
use data_test_driver::TestValue;

#[test]
fn data() {
    data_test_driver::test_folder(
        // TODO
        "/Users/ryanisaacg/git/ryanisaacg/ludus/tests",
        |contents| -> anyhow::Result<()> {
            check_types(contents)?;
            Ok(())
        },
        |contents| -> anyhow::Result<TestValue> {
            let mut results = eval(contents)?;
            if results.len() == 0 {
                Ok(TestValue::Void)
            } else if results.len() == 1 {
                let result = results.remove(0);
                match result {
                    brick::Value::FunctionID(_) => todo!(),
                    brick::Value::Size(_) => todo!(),
                    brick::Value::Byte(_) => todo!(),
                    brick::Value::Int32(val) => Ok(TestValue::Int(val as i64)),
                    brick::Value::Int64(val) => Ok(TestValue::Int(val)),
                    brick::Value::Float32(val) => Ok(TestValue::Float(val as f64)),
                    brick::Value::Float64(val) => Ok(TestValue::Float(val)),
                }
            } else {
                todo!()
            }
        },
    );
}
