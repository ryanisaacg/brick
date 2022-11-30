use std::collections::HashMap;

use crate::{
    tree::{Node, NodePtr},
    typecheck::{
        BinOpComparison, BinOpNumeric, FunDecl, IRContext, IRExpression, IRExpressionValue,
        IRStatement, IRStatementValue, NumericType, Type,
    },
};
use wasm_encoder::*;

// TODO: initial header-file like parse

pub fn emit(statements: Vec<IRStatement>, arena: &IRContext) -> Vec<u8> {
    let mut module = Module::new();
    let mut types = TypeSection::new();
    let mut functions = FunctionSection::new();
    let mut exports = ExportSection::new();
    let mut codes = CodeSection::new();

    let mut current_function_idx = 0;
    let mut function_indices = HashMap::new();

    for statement in statements {
        if let IRStatementValue::FunctionDeclaration(decl) = statement.value {
            emit_function_types(&decl, &mut types);
            functions.function(current_function_idx);
            // TODO: should we export function
            exports.export(decl.name.as_ref(), ExportKind::Func, current_function_idx);
            let (locals, function_locals) = analyze_locals(&decl, arena);
            let mut f = Function::new(function_locals);
            emit_expression(&mut EmitContext {
                f: &mut f,
                arena,
                locals: &locals,
                functions: &function_indices,
            },
            arena.expression(decl.body));
            f.instruction(&Instruction::End);
            codes.function(&f);

            function_indices.insert(decl.name, current_function_idx);

            current_function_idx += 1;
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

struct EmitContext<'a> {
    f: &'a mut Function,
    arena: &'a IRContext,
    locals: &'a HashMap<String, u32>,
    functions: &'a HashMap<String, u32>,
}


fn emit_statement<'a>(
    ctx: &mut EmitContext<'a>,
    statement: &IRStatement,
) {
    match &statement.value {
        IRStatementValue::FunctionDeclaration(_) => {
            unreachable!(); // TODO
        }
        IRStatementValue::Expression(expr) => {
            let expr = ctx.arena.expression(*expr);
            emit_expression(ctx, expr);
        }
        IRStatementValue::Declaration(name, expr) => match ctx.locals.get(name) {
            Some(offset) => {
                emit_expression(ctx, ctx.arena.expression(*expr));
                ctx.f.instruction(&Instruction::LocalSet(*offset));
            }
            None => todo!(),
        },
    }
}

fn emit_expression<'a>(
    ctx: &mut EmitContext<'a>,
    expr: &IRExpression,
) {
    match &expr.value {
        IRExpressionValue::Bool(val) => {
            if *val {
                ctx.f.instruction(&Instruction::I32Const(1));
            } else {
                ctx.f.instruction(&Instruction::I32Const(0));
            }
        }
        IRExpressionValue::Int(constant) => {
            ctx.f.instruction(&Instruction::I64Const(*constant));
        }
        IRExpressionValue::Float(constant) => {
            ctx.f.instruction(&Instruction::F64Const(*constant));
        }
        IRExpressionValue::Assignment(name, expr) => {
            // TODO de-dupe with declaration
            emit_expression(ctx, ctx.arena.expression(*expr));
            match ctx.locals.get(name) {
                Some(offset) => {
                    ctx.f.instruction(&Instruction::LocalSet(*offset));
                }
                None => todo!(),
            }
        }
        IRExpressionValue::Call(function, arguments) => {
            for arg in arguments.iter() {
                let arg = ctx.arena.expression(*arg);
                emit_expression(ctx, arg);
            }
            let function = ctx.arena.expression(*function);
            // TODO: be able to emit other functions?
            let called_function = match &function.value {
                IRExpressionValue::LocalVariable(name) => name,
                _ => todo!(),
            };
            let function_index = ctx.functions.get(called_function).unwrap();
            ctx.f.instruction(&Instruction::Call(*function_index));
        }
        IRExpressionValue::LocalVariable(name) => match ctx.locals.get(name) {
            Some(offset) => {
                ctx.f.instruction(&Instruction::LocalGet(*offset));
            }
            None => todo!(),
        },
        IRExpressionValue::BinaryNumeric(operator, left, right) => {
            emit_expression(ctx, ctx.arena.expression(*left));
            emit_expression(ctx, ctx.arena.expression(*right));
            match operator {
                BinOpNumeric::Add => {
                    ctx.f.instruction(&match expr.kind {
                        Type::Number(NumericType::Int64) => Instruction::I64Add,
                        Type::Number(NumericType::Float64) => Instruction::F64Add,
                        _ => unreachable!(),
                    });
                }
                BinOpNumeric::Subtract => {
                    ctx.f.instruction(&match expr.kind {
                        Type::Number(NumericType::Int64) => Instruction::I64Sub,
                        Type::Number(NumericType::Float64) => Instruction::F64Sub,
                        _ => unreachable!(),
                    });
                }
            }
        }
        IRExpressionValue::Comparison(operator, left, right) => {
            let left = ctx.arena.expression(*left);
            emit_expression(ctx, left);
            emit_expression(ctx, ctx.arena.expression(*right));
            match operator {
                BinOpComparison::GreaterThan => {
                    ctx.f.instruction(&match left.kind {
                        Type::Number(NumericType::Int64) => Instruction::I64GtS,
                        Type::Number(NumericType::Float64) => Instruction::F64Gt,
                        _ => unreachable!(),
                    });
                }
                BinOpComparison::LessThan => {
                    ctx.f.instruction(&match left.kind {
                        Type::Number(NumericType::Int64) => Instruction::I64LtS,
                        Type::Number(NumericType::Float64) => Instruction::F64Lt,
                        _ => unreachable!(),
                    });
                }
            }
        }
        IRExpressionValue::If(predicate, block) => {
            emit_expression(ctx, ctx.arena.expression(*predicate));
            ctx.f.instruction(&Instruction::If(BlockType::Empty)); // TODO
            emit_expression(ctx, ctx.arena.expression(*block));
            ctx.f.instruction(&Instruction::End); // TODO
        }
        IRExpressionValue::While(predicate, block) => {
            ctx.f.instruction(&Instruction::Loop(BlockType::Empty));
            emit_expression(ctx, ctx.arena.expression(*predicate));
            ctx.f.instruction(&Instruction::If(BlockType::Empty)); // TODO
            emit_expression(ctx, ctx.arena.expression(*block));
            ctx.f.instruction(&Instruction::Br(1)); // TODO: does this work
            ctx.f.instruction(&Instruction::End); // TODO
            ctx.f.instruction(&Instruction::End);
        }
        IRExpressionValue::Block(statements) => {
            for statement in statements {
                emit_statement(ctx, ctx.arena.statement(*statement));
            }
        }
    }
}

fn represented_by(kind: &Type) -> Option<ValType> {
    match kind {
        Type::Bool => Some(ValType::I32),
        Type::Number(NumericType::Int64) => Some(ValType::I64),
        Type::Number(NumericType::Float64) => Some(ValType::F64),
        Type::Function { .. } => None,
        Type::Void => None,
    }
}
