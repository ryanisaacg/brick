use crate::parser::{BinOp, Expression, Statement};
use wasm_encoder::*;

pub fn compile(root: Expression) -> Vec<u8> {
    let mut module = Module::new();

    // Encode the type section.
    let mut types = TypeSection::new();
    let params = vec![];
    let results = vec![ValType::I64];
    types.function(params, results);
    module.section(&types);

    // Encode the function section.
    let mut functions = FunctionSection::new();
    let type_index = 0;
    functions.function(type_index);
    module.section(&functions);

    // Encode the export section.
    let mut exports = ExportSection::new();
    exports.export("f", ExportKind::Func, 0);
    module.section(&exports);

    // Encode the code section.
    let mut codes = CodeSection::new();
    let locals = vec![];
    let mut f = Function::new(locals);
    expression(&mut f, root);
    f.instruction(&Instruction::End);
    codes.function(&f);
    module.section(&codes);

    // Extract the encoded Wasm bytes for this module.
    module.finish()
}

fn expression(f: &mut Function, root: Expression) {
    match root {
        Expression::Int(constant) => {
            f.instruction(&Instruction::I64Const(constant));
        }
        Expression::BinOp(operator, left, right) => {
            expression(f, *left);
            expression(f, *right);
            match operator {
                BinOp::Add => {
                    f.instruction(&Instruction::I64Add);
                }
                BinOp::Subtract => {
                    f.instruction(&Instruction::I64Sub);
                }
                _ => unimplemented!(),
            }
        }
        Expression::Name(_) => unimplemented!(),
    }
}
