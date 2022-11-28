use std::collections::HashMap;

use crate::typecheck::{
    BinOpComparison, BinOpNumeric, IRContext, IRExpression, IRExpressionValue, IRStatement,
    IRStatementValue, NumericType, Type,
};
use wasm_encoder::*;

pub fn compile(statements: Vec<IRStatement>, arena: &IRContext) -> Vec<u8> {
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
    let (locals, function_locals) = analyze_locals(&statements[..], arena);
    let mut f = Function::new(function_locals);
    for statement in statements {
        emit_statement(&mut f, &statement, arena, &locals);
    }
    f.instruction(&Instruction::End);
    codes.function(&f);
    module.section(&codes);

    // Extract the encoded Wasm bytes for this module.
    module.finish()
}

// TODO: handle different bind points with the same name

fn analyze_locals(
    statements: &[IRStatement],
    _arena: &IRContext,
) -> (HashMap<String, u32>, Vec<(u32, ValType)>) {
    let mut local_mapping = HashMap::new();
    let mut current_offset = 0;
    let mut locals = Vec::new();

    let mut statements_to_analyze = Vec::new();
    statements_to_analyze.extend(statements.iter());

    // TODO: general-purpose traversal of the expression tree?

    for val_type in [ValType::I32, ValType::I64, ValType::F32, ValType::F64] {
        let mut val_offset = 0;
        for statement in statements_to_analyze.iter() {
            if let IRStatement {
                value: IRStatementValue::Declaration(name, expr),
                ..
            } = statement
            {
                if represented_by(&expr.kind) == Some(val_type) {
                    val_offset += 1;
                    local_mapping.insert(name.to_string(), current_offset);
                    current_offset += 1;
                }
            }
        }
        locals.push((val_offset, val_type));
    }

    (local_mapping, locals)
}

fn emit_statement(
    f: &mut Function,
    statement: &IRStatement,
    arena: &IRContext,
    locals: &HashMap<String, u32>,
) {
    match &statement.value {
        IRStatementValue::Expression(expr) => {
            emit_expression(f, &expr, arena, &locals);
        }
        IRStatementValue::Declaration(name, expr) => match locals.get(name) {
            Some(offset) => {
                emit_expression(f, &expr, arena, &locals);
                f.instruction(&Instruction::LocalSet(*offset));
            }
            None => todo!(),
        },
    }
}

fn emit_expression(
    f: &mut Function,
    expr: &IRExpression,
    arena: &IRContext,
    locals: &HashMap<String, u32>,
) {
    match &expr.value {
        IRExpressionValue::Bool(val) => {
            if *val {
                f.instruction(&Instruction::I32Const(1));
            } else {
                f.instruction(&Instruction::I32Const(0));
            }
        }
        IRExpressionValue::Int(constant) => {
            f.instruction(&Instruction::I64Const(*constant));
        }
        IRExpressionValue::Float(constant) => {
            f.instruction(&Instruction::F64Const(*constant));
        }
        IRExpressionValue::Assignment(name, expr) => {
            // TODO de-dupe with declaration
            emit_expression(f, arena.expression(*expr), arena, locals);
            match locals.get(name) {
                Some(offset) => {
                    f.instruction(&Instruction::LocalSet(*offset));
                }
                None => todo!(),
            }
        }
        IRExpressionValue::LocalVariable(name) => match locals.get(name) {
            Some(offset) => {
                f.instruction(&Instruction::LocalGet(*offset));
            }
            None => todo!(),
        },
        IRExpressionValue::BinaryNumeric(operator, left, right) => {
            emit_expression(f, arena.expression(*left), arena, locals);
            emit_expression(f, arena.expression(*right), arena, locals);
            match operator {
                BinOpNumeric::Add => {
                    f.instruction(&match expr.kind {
                        Type::Number(NumericType::Int64) => Instruction::I64Add,
                        Type::Number(NumericType::Float64) => Instruction::F64Add,
                        _ => unreachable!(),
                    });
                }
                BinOpNumeric::Subtract => {
                    f.instruction(&match expr.kind {
                        Type::Number(NumericType::Int64) => Instruction::I64Sub,
                        Type::Number(NumericType::Float64) => Instruction::F64Sub,
                        _ => unreachable!(),
                    });
                }
            }
        }
        IRExpressionValue::Comparison(operator, left, right) => {
            let left = arena.expression(*left);
            emit_expression(f, left, arena, locals);
            emit_expression(f, arena.expression(*right), arena, locals);
            match operator {
                BinOpComparison::GreaterThan => {
                    f.instruction(&match left.kind {
                        Type::Number(NumericType::Int64) => Instruction::I64GtS,
                        Type::Number(NumericType::Float64) => Instruction::F64Gt,
                        _ => unreachable!(),
                    });
                }
                BinOpComparison::LessThan => {
                    f.instruction(&match left.kind {
                        Type::Number(NumericType::Int64) => Instruction::I64LtS,
                        Type::Number(NumericType::Float64) => Instruction::F64Lt,
                        _ => unreachable!(),
                    });
                }
            }
        }
        IRExpressionValue::If(predicate, block) => {
            emit_expression(f, arena.expression(*predicate), arena, locals);
            f.instruction(&Instruction::If(BlockType::Empty)); // TODO
            emit_expression(f, arena.expression(*block), arena, locals);
            f.instruction(&Instruction::End); // TODO
        }
        IRExpressionValue::While(predicate, block) => {
            f.instruction(&Instruction::Loop(BlockType::Empty));
            emit_expression(f, arena.expression(*predicate), arena, locals);
            f.instruction(&Instruction::If(BlockType::Empty)); // TODO
            emit_expression(f, arena.expression(*block), arena, locals);
            f.instruction(&Instruction::Br(1)); // TODO: does this work
            f.instruction(&Instruction::End); // TODO
            f.instruction(&Instruction::End);
        }
        IRExpressionValue::Block(statements) => {
            for statement in statements {
                emit_statement(f, arena.statement(*statement), arena, locals);
            }
        }
    }
}

fn represented_by(kind: &Type) -> Option<ValType> {
    match kind {
        Type::Bool => Some(ValType::I32),
        Type::Number(NumericType::Int64) => Some(ValType::I64),
        Type::Number(NumericType::Float64) => Some(ValType::F64),
        Type::Void => None,
    }
}
