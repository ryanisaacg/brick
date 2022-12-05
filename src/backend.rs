use std::collections::HashMap;

use crate::{
    analyzer::{
        BinOpComparison, BinOpNumeric, FunDecl, IRContext, IRExpression, IRExpressionValue,
        IRStatement, IRStatementValue, IRType, NumericType,
    },
    tree::{Node, NodePtr},
};
use wasm_encoder::*;

/**
 * Note: currently in WASM, there is only a 0-memory. However, the spec is forwards-compatible with
 * more
 */
const MAIN_MEMORY: u32 = 0;
const STACK_MINIMUM_PAGES: u64 = 16;
const HEAP_MINIMUM_PAGES: u64 = 48;
const MEMORY_MINIMUM_PAGES: u64 = STACK_MINIMUM_PAGES + HEAP_MINIMUM_PAGES;
const MAXIMUM_MEMORY: u64 = 16384;

// TODO: handle stack overflows
const STACK_PTR: u32 = 0;
const BASE_PTR: u32 = 1;

pub fn emit(statements: Vec<IRStatement>, arena: &IRContext) -> Vec<u8> {
    let mut module = Module::new();
    let mut types = TypeSection::new();
    let mut functions = FunctionSection::new();
    let mut exports = ExportSection::new();
    let mut codes = CodeSection::new();
    let mut globals = GlobalSection::new();

    let mut function_indices = HashMap::new();

    // TODO: how stable is this order

    let mut current_function_idx = 0;
    for statement in statements.iter() {
        if let IRStatementValue::FunctionDeclaration(decl) = &statement.value {
            function_indices.insert(decl.name.clone(), current_function_idx);
            current_function_idx += 1;
        }
    }

    // Set up the stack pointer
    globals.global(
        GlobalType {
            val_type: ValType::I32,
            mutable: true,
        },
        &ConstExpr::i32_const(1),
    );
    // Base pointer
    globals.global(
        GlobalType {
            val_type: ValType::I32,
            mutable: true,
        },
        &ConstExpr::i32_const(1),
    );

    current_function_idx = 0;

    for statement in statements {
        if let IRStatementValue::FunctionDeclaration(decl) = statement.value {
            emit_function_types(&decl, arena, &mut types);
            functions.function(current_function_idx);
            // TODO: should we export function
            exports.export(decl.name.as_ref(), ExportKind::Func, current_function_idx);
            let LocalsAnalysis {
                name_to_offset,
                wasm_locals,
                stack_size,
            } = analyze_locals(&decl, arena);
            let mut f = Function::new(wasm_locals);
            let mut ctx = EmitContext {
                f: &mut f,
                arena,
                locals: &name_to_offset,
                functions: &function_indices,
            };
            // Write the current base pointer to the base of the stack
            ctx.f.instruction(&Instruction::GlobalGet(STACK_PTR));
            ctx.f.instruction(&Instruction::GlobalGet(BASE_PTR));
            ctx.f.instruction(&Instruction::I32Store(MemArg {
                offset: 0,
                align: 1,
                memory_index: MAIN_MEMORY,
            }));
            // Set the base pointer to the stack pointer
            ctx.f.instruction(&Instruction::GlobalGet(STACK_PTR));
            ctx.f.instruction(&Instruction::GlobalSet(BASE_PTR));
            // Set the new stack size
            ctx.f.instruction(&Instruction::GlobalGet(STACK_PTR));
            ctx.f.instruction(&Instruction::I32Const(stack_size as i32));
            ctx.f.instruction(&Instruction::I32Add);
            // TODO: check for stack overflow
            ctx.f.instruction(&Instruction::GlobalSet(STACK_PTR));
            for (idx, param) in decl.params.iter().enumerate() {
                emit_stack_ptr_offset(&mut ctx, param.name.as_str());
                ctx.f.instruction(&Instruction::LocalGet(idx as u32));
                let kind = ctx.arena.kind(param.kind);
                let repr = represented_by(kind);
                emit_assignment(&mut ctx, repr);
            }
            println!("{}", arena.pretty_dbg(NodePtr::Expression(decl.body)));
            emit_expression(&mut ctx, arena.expression(decl.body));
            // Set the stack pointer back to the base pointer
            ctx.f.instruction(&Instruction::GlobalGet(BASE_PTR));
            ctx.f.instruction(&Instruction::GlobalSet(STACK_PTR));
            // Reset the base pointer
            ctx.f.instruction(&Instruction::GlobalGet(BASE_PTR));
            ctx.f.instruction(&Instruction::I32Load(MemArg {
                offset: 0,
                align: 1,
                memory_index: MAIN_MEMORY,
            }));
            ctx.f.instruction(&Instruction::GlobalSet(BASE_PTR));
            f.instruction(&Instruction::End);
            codes.function(&f);

            current_function_idx += 1;
        }
    }

    module.section(&types);
    module.section(&functions);
    let mut memories = MemorySection::new();
    memories.memory(MemoryType {
        minimum: MEMORY_MINIMUM_PAGES,
        maximum: Some(MAXIMUM_MEMORY),
        memory64: false,
        shared: false,
    });
    exports.export("memory", ExportKind::Memory, 0);
    module.section(&memories);
    module.section(&globals);
    module.section(&exports);
    module.section(&codes);

    module.finish()
}

fn emit_function_types(decl: &FunDecl, ir_context: &IRContext, types: &mut TypeSection) {
    // TODO: param types that can't be represented
    let params = decl
        .params
        .iter()
        .map(|param| represented_by(ir_context.kind(param.kind)).unwrap());
    let results = if let Some(kind) = represented_by(ir_context.kind(decl.returns)) {
        vec![kind]
    } else {
        // TODO
        vec![]
    };
    types.function(params, results);
}

// TODO: handle different bind points with the same name

struct LocalsAnalysis {
    name_to_offset: HashMap<String, u32>,
    wasm_locals: Vec<(u32, ValType)>,
    stack_size: u32,
}

fn analyze_locals(decl: &FunDecl, arena: &IRContext) -> LocalsAnalysis {
    let mut results = LocalsAnalysis {
        name_to_offset: HashMap::new(),
        wasm_locals: Vec::new(),
        stack_size: 4,
    };

    for param in decl.params.iter() {
        let val_type = represented_by(arena.kind(param.kind)).unwrap(); // TODO: params unrepresentable
        results
            .name_to_offset
            .insert(param.name.to_string(), results.stack_size);
        results.stack_size += size_of_val(&val_type);
        results.wasm_locals.push((1, val_type));
    }

    for node in arena.iter_from(NodePtr::Expression(decl.body)) {
        if let Node::Statement(IRStatement {
            value: IRStatementValue::Declaration(name, expr),
            ..
        }) = node
        {
            let expr = arena.expression(*expr);
            results
                .name_to_offset
                .insert(name.to_string(), results.stack_size);
            let repr = represented_by(arena.kind(expr.kind)).unwrap(); // TODO
            results.stack_size += size_of_val(&repr);
        }
    }

    results
}

struct EmitContext<'a> {
    f: &'a mut Function,
    arena: &'a IRContext,
    locals: &'a HashMap<String, u32>,
    functions: &'a HashMap<String, u32>,
}

fn emit_statement<'a>(ctx: &mut EmitContext<'a>, statement: &IRStatement) {
    match &statement.value {
        IRStatementValue::FunctionDeclaration(_) => {
            unreachable!(); // TODO
        }
        IRStatementValue::Expression(expr) => {
            let expr = ctx.arena.expression(*expr);
            emit_expression(ctx, expr);
        }
        IRStatementValue::Declaration(name, expr) => {
            emit_stack_ptr_offset(ctx, name.as_str());
            let expr = ctx.arena.expression(*expr);
            emit_expression(ctx, expr);
            let kind = ctx.arena.kind(expr.kind);
            let repr = represented_by(kind);
            emit_assignment(ctx, repr);
        }
    }
}

fn emit_expression<'a>(ctx: &mut EmitContext<'a>, expr: &IRExpression) {
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
            emit_stack_ptr_offset(ctx, name.as_str());
            let expr = ctx.arena.expression(*expr);
            emit_expression(ctx, expr);
            let kind = ctx.arena.kind(expr.kind);
            let repr = represented_by(kind);
            emit_assignment(ctx, repr);
        }
        IRExpressionValue::Dereference(child) => {
            emit_expression(ctx, ctx.arena.expression(*child));
            let intended_type = ctx.arena.kind(expr.kind);
            let representation = represented_by(intended_type);
            emit_dereference(ctx, representation);
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
        IRExpressionValue::TakeShared(child) | IRExpressionValue::TakeUnique(child) => {
            let child = ctx.arena.expression(*child);
            // TODO: support non-variable references?
            match &child.value {
                IRExpressionValue::LocalVariable(name) => {
                    emit_stack_ptr_offset(ctx, name.as_str());
                }
                _ => todo!(),
            }
        }
        IRExpressionValue::LocalVariable(name) => {
            let ir_type = ctx.arena.kind(expr.kind);
            let repr = represented_by(ir_type);
            emit_stack_ptr_offset(ctx, name.as_str());
            emit_dereference(ctx, repr);
        }
        IRExpressionValue::BinaryNumeric(operator, left, right) => {
            emit_expression(ctx, ctx.arena.expression(*left));
            emit_expression(ctx, ctx.arena.expression(*right));
            match operator {
                BinOpNumeric::Add => {
                    ctx.f.instruction(&match ctx.arena.kind(expr.kind) {
                        IRType::Number(NumericType::Int64) => Instruction::I64Add,
                        IRType::Number(NumericType::Float64) => Instruction::F64Add,
                        _ => unreachable!(),
                    });
                }
                BinOpNumeric::Subtract => {
                    ctx.f.instruction(&match ctx.arena.kind(expr.kind) {
                        IRType::Number(NumericType::Int64) => Instruction::I64Sub,
                        IRType::Number(NumericType::Float64) => Instruction::F64Sub,
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
                    ctx.f.instruction(&match ctx.arena.kind(left.kind) {
                        IRType::Number(NumericType::Int64) => Instruction::I64GtS,
                        IRType::Number(NumericType::Float64) => Instruction::F64Gt,
                        _ => unreachable!(),
                    });
                }
                BinOpComparison::LessThan => {
                    ctx.f.instruction(&match ctx.arena.kind(left.kind) {
                        IRType::Number(NumericType::Int64) => Instruction::I64LtS,
                        IRType::Number(NumericType::Float64) => Instruction::F64Lt,
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

fn emit_stack_ptr_offset<'a>(ctx: &mut EmitContext<'a>, target: &str) {
    ctx.f.instruction(&Instruction::GlobalGet(BASE_PTR));
    match ctx.locals.get(target) {
        Some(offset) => {
            ctx.f.instruction(&Instruction::I32Const(*offset as i32));
        }
        None => todo!(),
    }
    ctx.f.instruction(&Instruction::I32Add);
}

fn emit_assignment<'a>(ctx: &mut EmitContext<'a>, representation: Option<ValType>) {
    // TODO: offset & alignment
    let mem_arg = MemArg {
        offset: 0,
        align: 1,
        memory_index: MAIN_MEMORY,
    };
    match representation {
        None => todo!(),
        Some(ValType::I32) => {
            ctx.f.instruction(&Instruction::I32Store(mem_arg));
        }
        Some(ValType::I64) => {
            ctx.f.instruction(&Instruction::I64Store(mem_arg));
        }
        Some(ValType::F32) => {
            ctx.f.instruction(&Instruction::F32Store(mem_arg));
        }
        Some(ValType::F64) => {
            ctx.f.instruction(&Instruction::F64Store(mem_arg));
        }
        Some(_) => todo!(),
    }
}

fn emit_dereference(ctx: &mut EmitContext<'_>, representation: Option<ValType>) {
    // TODO: offset & alignment
    let mem_arg = MemArg {
        offset: 0,
        align: 1,
        memory_index: MAIN_MEMORY,
    };
    match representation {
        None => todo!(),
        Some(ValType::I32) => {
            ctx.f.instruction(&Instruction::I32Load(mem_arg));
        }
        Some(ValType::I64) => {
            ctx.f.instruction(&Instruction::I64Load(mem_arg));
        }
        Some(ValType::F32) => {
            ctx.f.instruction(&Instruction::F32Load(mem_arg));
        }
        Some(ValType::F64) => {
            ctx.f.instruction(&Instruction::F64Load(mem_arg));
        }
        Some(_) => todo!(),
    }
}

fn represented_by(kind: &IRType) -> Option<ValType> {
    match kind {
        IRType::Bool
        | IRType::Unique(_)
        | IRType::Shared(_)
        | IRType::Number(NumericType::Int32) => Some(ValType::I32),
        IRType::Number(NumericType::Float32) => Some(ValType::F32),
        IRType::Number(NumericType::Int64) => Some(ValType::I64),
        IRType::Number(NumericType::Float64) => Some(ValType::F64),
        IRType::Function { .. } => None,
        IRType::Void => None,
    }
}

fn size_of_val(val: &ValType) -> u32 {
    match val {
        ValType::I32 => 4,
        ValType::F32 => 4,
        ValType::I64 => 8,
        ValType::F64 => 8,
        _ => todo!(),
    }
}
