use std::collections::HashMap;

use crate::{
    tree::{Node, NodePtr},
    typecheck::{
        BinOpComparison, BinOpNumeric, FunDecl, IRContext, IRExpression, IRExpressionValue,
        IRStatement, IRStatementValue, NumericType, Type,
    },
};
use wasm_encoder::*;

pub fn emit(statements: Vec<IRStatement>, arena: &IRContext) -> Vec<u8> {
    let mut module = Module::new();
    let mut types = TypeSection::new();
    let mut functions = FunctionSection::new();
    let mut exports = ExportSection::new();
    let mut codes = CodeSection::new();

    let mut function_index = 0;
    for statement in statements {
        if let IRStatementValue::FunctionDeclaration(decl) = statement.value {
            emit_function_types(&decl, &mut types);
            functions.function(function_index);
            // TODO: should we export function
            exports.export(decl.name.as_ref(), ExportKind::Func, function_index);
            let (locals, function_locals) = analyze_locals(&decl, arena);
            let mut f = Function::new(function_locals);
            emit_expression(&mut f, arena.expression(decl.body), arena, &locals);
            f.instruction(&Instruction::End);
            codes.function(&f);

            function_index += 1;
        }
    }

    module.section(&types);
    module.section(&functions);
    module.section(&exports);
    module.section(&codes);

    module.finish()
}

fn emit_function_types(decl: &FunDecl, types: &mut TypeSection) {
    // TODO: param types that can't be represented
    let params = decl
        .params
        .iter()
        .map(|param| represented_by(&param.kind).unwrap());
    let results = if let Some(kind) = represented_by(&decl.returns) {
        vec![kind]
    } else {
        // TODO
        vec![]
    };
    types.function(params, results);
}

// TODO: handle different bind points with the same name

fn analyze_locals(
    decl: &FunDecl,
    arena: &IRContext,
) -> (HashMap<String, u32>, Vec<(u32, ValType)>) {
    let mut local_mapping = HashMap::new();
    let mut current_offset = 0;
    let mut locals = Vec::new();

    for param in decl.params.iter() {
        let val_type = represented_by(&param.kind).unwrap(); // TODO: params unrepresentable
        local_mapping.insert(param.name.to_string(), current_offset);
        current_offset += 1;
        locals.push((1, val_type));
    }

    for val_type in [ValType::I32, ValType::I64, ValType::F32, ValType::F64] {
        let mut val_count = 0;
        for node in arena.iter_from(NodePtr::Expression(decl.body)) {
            if let Node::Statement(IRStatement {
                value: IRStatementValue::Declaration(name, expr),
                ..
            }) = node
            {
                let expr = arena.expression(*expr);
                if represented_by(&expr.kind) == Some(val_type) {
                    val_count += 1;
                    local_mapping.insert(name.to_string(), current_offset);
                    current_offset += 1;
                }
            }
        }
        locals.push((val_count, val_type));
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
        IRStatementValue::FunctionDeclaration(_) => {
            unreachable!(); // TODO
        }
        IRStatementValue::Expression(expr) => {
            emit_expression(f, arena.expression(*expr), arena, locals);
        }
        IRStatementValue::Declaration(name, expr) => match locals.get(name) {
            Some(offset) => {
                emit_expression(f, arena.expression(*expr), arena, locals);
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
