use std::collections::HashMap;

use crate::{
    parser::{AstExpression, AstStatement, BinOp},
    typecheck::{BinOpNumeric, IRExpression, IRExpressionValue, IRStatement, PrimitiveType, Type},
};
use wasm_encoder::*;

pub fn compile(statements: Vec<IRStatement>, arena: &Vec<IRExpression>) -> Vec<u8> {
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
    // TODO: get locals
    let locals = vec![(1, ValType::I64)];
    let mut f = Function::new(locals);
    let mut locals = HashMap::new();
    let mut current_offset = 0;
    for statement in statements {
        match statement {
            IRStatement::Expression(expr) => {
                expression(&mut f, &expr, arena, &locals);
            }
            IRStatement::Declaration(name, expr) => {
                locals.insert(name, current_offset);
                expression(&mut f, &expr, arena, &locals);
                f.instruction(&Instruction::LocalSet(current_offset));
                current_offset += 1;
            }
            IRStatement::Assignment(name, expr) => match locals.get(&name) {
                Some(offset) => {
                    expression(&mut f, &expr, arena, &locals);
                    f.instruction(&Instruction::LocalSet(*offset));
                }
                None => todo!(),
            },
        }
    }
    f.instruction(&Instruction::End);
    codes.function(&f);
    module.section(&codes);

    // Extract the encoded Wasm bytes for this module.
    module.finish()
}

fn expression(
    f: &mut Function,
    expr: &IRExpression,
    arena: &Vec<IRExpression>,
    locals: &HashMap<String, u32>,
) {
    match &expr.0 {
        IRExpressionValue::Int(constant) => {
            f.instruction(&Instruction::I64Const(*constant));
        }
        IRExpressionValue::LocalVariable(name) => match locals.get(name) {
            Some(offset) => {
                f.instruction(&Instruction::LocalGet(*offset));
            }
            None => todo!(),
        },
        IRExpressionValue::BinaryNumeric(operator, left, right) => {
            expression(f, &arena[*left], arena, locals);
            expression(f, &arena[*right], arena, locals);
            match operator {
                BinOpNumeric::Add => {
                    f.instruction(&Instruction::I64Add);
                }
                BinOpNumeric::Subtract => {
                    f.instruction(&Instruction::I64Sub);
                }
            }
        }
    }
}

impl Into<ValType> for Type {
    fn into(self) -> ValType {
        match self {
            Type::Primitive(PrimitiveType::Int64) => ValType::I64,
        }
    }
}
