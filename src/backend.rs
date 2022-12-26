use std::collections::HashMap;

use crate::{
    analyzer::{
        BinOpComparison, BinOpNumeric, FunDecl, IRContext, IRNode, IRNodeValue, IRType,
        NumericType, F32_KIND, I32_KIND,
    },
    arena::ArenaIter,
};
use gimli::write::{EndianVec, Sections};
use wasm_encoder::*;

use self::debug::DebugInfo;

mod debug;

/**
 * Note: currently in WASM, there is only a 0-memory. However, the spec is forwards-compatible with
 * more
 */
const MAIN_MEMORY: u32 = 0;
const STACK_PAGES: u64 = 16;
const HEAP_MINIMUM_PAGES: u64 = 48;
const MEMORY_MINIMUM_PAGES: u64 = STACK_PAGES + HEAP_MINIMUM_PAGES;
const MAXIMUM_MEMORY: u64 = 16_384;
const WASM_PAGE_SIZE: u64 = 65_536;

// TODO: handle stack overflows
const STACK_PTR: u32 = 0;
const BASE_PTR: u32 = 1;
const REFERENCE_PTR: u32 = 2;
const HEAP_PTR: u32 = 3;

pub fn emit(statements: Vec<IRNode>, arena: &IRContext, with_debug: bool) -> Vec<u8> {
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
        if let IRNodeValue::FunctionDeclaration(decl) = &statement.value {
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
        &ConstExpr::i32_const(4),
    );
    // Base pointer
    globals.global(
        GlobalType {
            val_type: ValType::I32,
            mutable: true,
        },
        &ConstExpr::i32_const(4),
    );
    // Assignment pointer
    globals.global(
        GlobalType {
            val_type: ValType::I32,
            mutable: true,
        },
        &ConstExpr::i32_const(0x50),
    );
    // Heap pointer
    globals.global(
        GlobalType {
            val_type: ValType::I32,
            mutable: true,
        },
        &ConstExpr::i32_const((STACK_PAGES * WASM_PAGE_SIZE) as i32),
    );

    let mut debug_info = DebugInfo::new();
    let mut type_representations = Vec::new();
    let mut debug_representations = Vec::new();
    // TODO: deref programs fail to generate debug representations
    for (idx, kind) in arena.types.iter().enumerate() {
        let repr = represented_by(arena, idx);
        type_representations.push(repr);
        match kind {
            IRType::Void => debug_representations.push(None),
            IRType::Bool => debug_representations.push(Some(debug_info.bool_id)),
            IRType::Number(num) => match num {
                NumericType::Int32 => debug_representations.push(Some(debug_info.i32_id)),
                NumericType::Int64 => debug_representations.push(Some(debug_info.i64_id)),
                NumericType::Float32 => debug_representations.push(Some(debug_info.f32_id)),
                NumericType::Float64 => debug_representations.push(Some(debug_info.f64_id)),
            },
            IRType::Unique(child) | IRType::Shared(child) => {
                // TODO: maybe throw if type doesn't exist?
                let kind = debug_representations[*child]
                    .map(|child_type_id| debug_info.pointer_type(child_type_id));
                debug_representations.push(kind);
            }
            IRType::Array(child) => {
                // TODO: maybe throw if type doesn't exist?
                let kind = debug_representations[*child]
                    .map(|child_type_id| debug_info.array_type(child_type_id));
                debug_representations.push(kind);
            }
            IRType::Function { .. } | IRType::Struct { .. } => {
                debug_representations.push(None); // TODO
            }
            IRType::Unresolved(..) => {
                debug_representations.push(None);
            }
        }
    }

    current_function_idx = 0;

    for statement in statements {
        if let IRNodeValue::FunctionDeclaration(decl) = statement.value {
            emit_function_types(&decl, &type_representations, &mut types);
            functions.function(current_function_idx);
            // TODO: should we export function
            exports.export(decl.name.as_ref(), ExportKind::Func, current_function_idx);
            let LocalsAnalysis {
                name_to_offset,
                parameter_locals,
                stack_size,
            } = analyze_locals(&decl, &type_representations, arena);
            let param_count = parameter_locals.len();
            let mut ctx = EmitContext {
                instructions: Vec::new(),
                arena,
                stack_offsets: &name_to_offset,
                functions: &function_indices,
                representations: &type_representations,
                wasm_locals: parameter_locals,
            };
            emit_function_declaration(&mut ctx, stack_size, &decl);
            let mut wasm_locals = Vec::new();
            // TODO: optimize by combining adjacent locals
            // TODO: is there a bug in wasm-encode?
            for val_type in ctx.wasm_locals.drain(param_count..) {
                wasm_locals.push((1, val_type));
            }
            let mut f = Function::new(wasm_locals);
            for instruction in ctx.instructions.iter() {
                f.instruction(instruction);
            }

            // TODO: will I need to adjust for the header?
            let low_addr = codes.byte_len();
            codes.function(&f);
            let high_addr = codes.byte_len();

            debug_info.function_declaration(
                &decl,
                &debug_representations[..],
                Some((low_addr, high_addr)),
            );

            current_function_idx += 1;
        }
    }

    debug_info.set_unit_range(0, codes.byte_len());

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

    if with_debug {
        let mut sections = Sections::new(EndianVec::new(gimli::LittleEndian));
        debug_info.dwarf.write(&mut sections).unwrap();
        sections
            .for_each(|id, data| {
                let name = id.name();
                module.section(&CustomSection {
                    name,
                    data: data.slice(),
                });

                Ok::<(), ()>(())
            })
            .unwrap()
    }

    module.finish()
}

fn emit_function_types(
    decl: &FunDecl,
    type_representations: &[Representation],
    types: &mut TypeSection,
) {
    let mut params = Vec::new();
    for param in decl.params.iter() {
        flatten_repr(&type_representations[param.kind], &mut params);
    }
    let mut results = Vec::new();
    flatten_repr(&type_representations[decl.returns], &mut results);

    types.function(params, results);
}

// TODO: handle different bind points with the same name

struct LocalsAnalysis {
    name_to_offset: HashMap<String, u32>,
    parameter_locals: Vec<ValType>,
    stack_size: u32,
}

fn analyze_locals(
    decl: &FunDecl,
    type_representations: &[Representation],
    arena: &IRContext,
) -> LocalsAnalysis {
    let mut results = LocalsAnalysis {
        name_to_offset: HashMap::new(),
        parameter_locals: Vec::new(),
        stack_size: 4,
    };

    let mut repr_buffer = Vec::new();

    for param in decl.params.iter() {
        results
            .name_to_offset
            .insert(param.name.to_string(), results.stack_size);
        let repr = &type_representations[param.kind];
        results.stack_size += size_of_repr_in_bytes(repr);
        flatten_repr(repr, &mut repr_buffer);

        for val_type in repr_buffer.drain(..) {
            results.parameter_locals.push(val_type);
        }
    }

    for (_, node) in ArenaIter::iter_from(&arena.nodes, decl.body) {
        if let IRNode {
            value: IRNodeValue::Declaration(name, expr),
            ..
        } = node
        {
            let expr = arena.node(*expr);
            results
                .name_to_offset
                .insert(name.to_string(), results.stack_size);
            results.stack_size += size_of_repr_in_bytes(&type_representations[expr.kind]);
        }
    }

    results
}

struct EmitContext<'a> {
    instructions: Vec<Instruction<'a>>,
    arena: &'a IRContext,
    stack_offsets: &'a HashMap<String, u32>,
    functions: &'a HashMap<String, u32>,
    /**
     * A mapping from the indices of a type in the IRContext to that type's calculated
     * representation
     */
    representations: &'a Vec<Representation>,
    wasm_locals: Vec<ValType>,
}

impl<'a> EmitContext<'a> {
    fn add_instruction(&mut self, instruction: Instruction<'a>) {
        self.instructions.push(instruction);
    }
}

// TODO: consider dynamically-sized stack frames?

fn emit_function_declaration(ctx: &mut EmitContext<'_>, stack_size: u32, decl: &FunDecl) {
    // Write the current base pointer to the base of the stack
    ctx.add_instruction(Instruction::GlobalGet(STACK_PTR));
    ctx.add_instruction(Instruction::GlobalGet(BASE_PTR));
    ctx.add_instruction(Instruction::I32Store(MemArg {
        offset: 0,
        align: 1,
        memory_index: MAIN_MEMORY,
    }));
    // Set the base pointer to the stack pointer
    ctx.add_instruction(Instruction::GlobalGet(STACK_PTR));
    ctx.add_instruction(Instruction::GlobalSet(BASE_PTR));
    // Set the new stack size
    ctx.add_instruction(Instruction::GlobalGet(STACK_PTR));
    ctx.add_instruction(Instruction::I32Const(stack_size as i32));
    ctx.add_instruction(Instruction::I32Add);
    // TODO: check for stack overflow
    ctx.add_instruction(Instruction::GlobalSet(STACK_PTR));
    let mut local_index = 0;
    for param in decl.params.iter() {
        let Some(offset) = ctx.stack_offsets.get(param.name.as_str())  else {
            panic!("internal compiler error: unknown name {}", param.name);
        };
        local_index = emit_parameter(
            &mut ctx.instructions,
            local_index,
            *offset,
            &ctx.representations[param.kind],
        );
    }
    emit_node(ctx, decl.body);
    // Set the stack pointer back to the base pointer
    ctx.add_instruction(Instruction::GlobalGet(BASE_PTR));
    ctx.add_instruction(Instruction::GlobalSet(STACK_PTR));
    // Reset the base pointer
    ctx.add_instruction(Instruction::GlobalGet(BASE_PTR));
    ctx.add_instruction(Instruction::I32Load(MemArg {
        offset: 0,
        align: 1,
        memory_index: MAIN_MEMORY,
    }));
    ctx.add_instruction(Instruction::GlobalSet(BASE_PTR));
    ctx.add_instruction(Instruction::End);
}

fn emit_node(ctx: &mut EmitContext<'_>, expr_index: usize) {
    let expr = ctx.arena.node(expr_index);
    match &expr.value {
        IRNodeValue::Return(expr) => {
            emit_node(ctx, *expr);
            ctx.add_instruction(Instruction::Return);
        }
        IRNodeValue::ArrayIndex(array, index) => {
            // TODO: is this generally the correct way to handle things? is there a case where
            // there's a non-lvalue-array?
            // it almost seems like there needs to be auto-derefs at a higher IR level for stack
            // variables, then we can maybe remove emit_lvalue as a concept
            let offset = emit_lvalue(ctx, *array);
            ctx.add_instruction(Instruction::GlobalSet(REFERENCE_PTR));
            emit_dereference(
                &mut ctx.instructions,
                offset,
                &Representation::Scalar(ValType::I32),
            );
            // TODO: bound check instead of dropping length
            //ctx.add_instruction(Instruction::Drop);
            emit_node(ctx, *index);
            let repr = &ctx.representations[expr.kind];
            ctx.add_instruction(Instruction::I32Const(size_of_repr_in_bytes(repr) as i32));
            ctx.add_instruction(Instruction::I32Mul);
            ctx.add_instruction(Instruction::I32Add);
            ctx.add_instruction(Instruction::GlobalSet(REFERENCE_PTR));
            emit_dereference(&mut ctx.instructions, 0, repr);
        }
        IRNodeValue::ArrayLiteralLength(value, length) => {
            // TODO: should this logic be lifted into the IR?
            let repr = &ctx.representations[ctx.arena.node(*value).kind];
            let size_of = size_of_repr_in_bytes(repr);
            emit_node(ctx, *value);
            // Store the current heap value in the reference pointer, because that's
            // where we'll be storing the value
            ctx.add_instruction(Instruction::GlobalGet(HEAP_PTR));
            ctx.add_instruction(Instruction::GlobalSet(REFERENCE_PTR));
            for index in 0..(*length) {
                let offset = (index as u32) * size_of;
                emit_assignment(&mut ctx.instructions, &mut ctx.wasm_locals, offset, repr);
                if index < length - 1 {
                    // For all the non-last indices, put the array value that
                    // is being copied back on the stack
                    ctx.add_instruction(Instruction::GlobalGet(HEAP_PTR));
                    ctx.add_instruction(Instruction::GlobalSet(REFERENCE_PTR));
                    emit_dereference(&mut ctx.instructions, offset, repr);
                }
            }
            // TODO: general-purpose allocator
            // Put the heap pointer on the stack, because that's where the array starts
            ctx.add_instruction(Instruction::GlobalGet(HEAP_PTR));
            // Put the length of the array onto the stack
            ctx.add_instruction(Instruction::I32Const(*length as i32));
            // Move the heap pointer forward by the size of the array
            ctx.add_instruction(Instruction::GlobalGet(HEAP_PTR));
            ctx.add_instruction(Instruction::I32Const(*length as i32 * size_of as i32 * 8));
            ctx.add_instruction(Instruction::I32Add);
            ctx.add_instruction(Instruction::GlobalSet(HEAP_PTR));
        }
        IRNodeValue::LocalVariable(name) => {
            ctx.add_instruction(Instruction::GlobalGet(BASE_PTR));
            ctx.add_instruction(Instruction::GlobalSet(REFERENCE_PTR));
            emit_dereference(
                &mut ctx.instructions,
                *ctx.stack_offsets.get(name.as_str()).unwrap(),
                &ctx.representations[expr.kind],
            );
        }
        IRNodeValue::Dot(..) => {
            let offset = emit_lvalue(ctx, expr_index);
            ctx.add_instruction(Instruction::GlobalSet(REFERENCE_PTR));
            emit_dereference(
                &mut ctx.instructions,
                offset,
                &ctx.representations[expr.kind],
            );
        }
        IRNodeValue::Assignment(lvalue, expr) => {
            emit_node(ctx, *expr);
            let expr = ctx.arena.node(*expr);
            let offset = emit_lvalue(ctx, *lvalue);
            ctx.add_instruction(Instruction::GlobalSet(REFERENCE_PTR));
            emit_assignment(
                &mut ctx.instructions,
                &mut ctx.wasm_locals,
                offset,
                &ctx.representations[expr.kind],
            );
        }
        IRNodeValue::StructLiteral(fields) => {
            let Representation::Struct { field_order, .. } = &ctx.representations[expr.kind] else {
                panic!("expected representation of struct to be struct");
            };
            for field in field_order {
                emit_node(
                    ctx,
                    *fields.get(field).expect("expected struct fields to match"),
                );
            }
        }
        IRNodeValue::Bool(val) => {
            if *val {
                ctx.add_instruction(Instruction::I32Const(1));
            } else {
                ctx.add_instruction(Instruction::I32Const(0));
            }
        }
        IRNodeValue::Int(constant) => {
            if expr.kind == I32_KIND {
                ctx.add_instruction(Instruction::I32Const(*constant as i32));
            } else {
                ctx.add_instruction(Instruction::I64Const(*constant));
            }
        }
        IRNodeValue::Float(constant) => {
            if expr.kind == F32_KIND {
                ctx.add_instruction(Instruction::F32Const(*constant as f32));
            } else {
                ctx.add_instruction(Instruction::F64Const(*constant));
            }
        }
        IRNodeValue::Dereference(child) => {
            emit_node(ctx, *child);
            ctx.add_instruction(Instruction::GlobalSet(REFERENCE_PTR));
            emit_dereference(&mut ctx.instructions, 0, &ctx.representations[expr.kind]);
        }
        IRNodeValue::Call(function, arguments) => {
            for arg in arguments.iter() {
                emit_node(ctx, *arg);
            }
            let function = ctx.arena.node(*function);
            // TODO: be able to emit other functions?
            let called_function = match &function.value {
                IRNodeValue::LocalVariable(name) => name,
                _ => todo!(),
            };
            let function_index = ctx.functions.get(called_function).unwrap();
            ctx.add_instruction(Instruction::Call(*function_index));
        }
        IRNodeValue::TakeShared(child) | IRNodeValue::TakeUnique(child) => {
            let child = ctx.arena.node(*child);
            // TODO: support non-variable references?
            match &child.value {
                IRNodeValue::LocalVariable(name) => {
                    ctx.add_instruction(Instruction::GlobalGet(BASE_PTR));
                    let offset = ctx.stack_offsets.get(name.as_str()).unwrap();
                    ctx.add_instruction(Instruction::I32Const(*offset as i32));
                    ctx.add_instruction(Instruction::I32Add);
                }
                _ => todo!(),
            }
        }
        IRNodeValue::BinaryNumeric(operator, left, right) => {
            emit_node(ctx, *left);
            emit_node(ctx, *right);
            match operator {
                BinOpNumeric::Add => {
                    ctx.add_instruction(match ctx.arena.kind(expr.kind) {
                        IRType::Number(NumericType::Int64) => Instruction::I64Add,
                        IRType::Number(NumericType::Float64) => Instruction::F64Add,
                        IRType::Number(NumericType::Int32) => Instruction::I32Add,
                        IRType::Number(NumericType::Float32) => Instruction::F32Add,
                        _ => unreachable!(),
                    });
                }
                BinOpNumeric::Subtract => {
                    ctx.add_instruction(match ctx.arena.kind(expr.kind) {
                        IRType::Number(NumericType::Int64) => Instruction::I64Sub,
                        IRType::Number(NumericType::Float64) => Instruction::F64Sub,
                        IRType::Number(NumericType::Int32) => Instruction::I32Sub,
                        IRType::Number(NumericType::Float32) => Instruction::F32Sub,
                        _ => unreachable!(),
                    });
                }
                BinOpNumeric::Multiply => {
                    ctx.add_instruction(match ctx.arena.kind(expr.kind) {
                        IRType::Number(NumericType::Int64) => Instruction::I64Mul,
                        IRType::Number(NumericType::Float64) => Instruction::F64Mul,
                        IRType::Number(NumericType::Int32) => Instruction::I32Mul,
                        IRType::Number(NumericType::Float32) => Instruction::F32Mul,
                        _ => unreachable!(),
                    });
                }
                BinOpNumeric::Divide => {
                    ctx.add_instruction(match ctx.arena.kind(expr.kind) {
                        IRType::Number(NumericType::Int64) => Instruction::I64DivS,
                        IRType::Number(NumericType::Float64) => Instruction::F64Div,
                        IRType::Number(NumericType::Int32) => Instruction::I32DivS,
                        IRType::Number(NumericType::Float32) => Instruction::F32Div,
                        _ => unreachable!(),
                    });
                }
            }
        }
        IRNodeValue::Comparison(operator, left, right) => {
            emit_node(ctx, *left);
            emit_node(ctx, *right);
            let left = ctx.arena.node(*left);
            match operator {
                BinOpComparison::GreaterThan => {
                    ctx.add_instruction(match ctx.arena.kind(left.kind) {
                        IRType::Number(NumericType::Int32) => Instruction::I32GtS,
                        IRType::Number(NumericType::Float32) => Instruction::F32Gt,
                        IRType::Number(NumericType::Int64) => Instruction::I64GtS,
                        IRType::Number(NumericType::Float64) => Instruction::F64Gt,
                        _ => unreachable!(),
                    });
                }
                BinOpComparison::LessThan => {
                    ctx.add_instruction(match ctx.arena.kind(left.kind) {
                        IRType::Number(NumericType::Int32) => Instruction::I32LtS,
                        IRType::Number(NumericType::Float32) => Instruction::F32Lt,
                        IRType::Number(NumericType::Int64) => Instruction::I64LtS,
                        IRType::Number(NumericType::Float64) => Instruction::F64Lt,
                        _ => unreachable!(),
                    });
                }
            }
        }
        IRNodeValue::If(predicate, block) => {
            emit_node(ctx, *predicate);
            ctx.add_instruction(Instruction::If(BlockType::Empty)); // TODO
            emit_node(ctx, *block);
            ctx.add_instruction(Instruction::End); // TODO
        }
        IRNodeValue::While(predicate, block) => {
            ctx.add_instruction(Instruction::Loop(BlockType::Empty));
            emit_node(ctx, *predicate);
            ctx.add_instruction(Instruction::If(BlockType::Empty)); // TODO
            emit_node(ctx, *block);
            ctx.add_instruction(Instruction::Br(1)); // TODO: does this work
            ctx.add_instruction(Instruction::End); // TODO
            ctx.add_instruction(Instruction::End);
        }
        IRNodeValue::Block(statements) => {
            for statement in statements {
                emit_node(ctx, *statement);
            }
        }
        IRNodeValue::FunctionDeclaration(_) => {
            unreachable!(); // TODO
        }
        IRNodeValue::Expression(expr) => {
            emit_node(ctx, *expr);
        }
        IRNodeValue::Declaration(name, expr) => {
            // TODO: should this just be an expect
            let Some(offset) = ctx.stack_offsets.get(name.as_str())  else {
                panic!("internal compiler error: unknown name {}", name);
            };
            emit_node(ctx, *expr);
            let expr = ctx.arena.node(*expr);
            ctx.add_instruction(Instruction::GlobalGet(BASE_PTR));
            ctx.add_instruction(Instruction::GlobalSet(REFERENCE_PTR));
            emit_assignment(
                &mut ctx.instructions,
                &mut ctx.wasm_locals,
                *offset,
                &ctx.representations[expr.kind],
            );
        }
        IRNodeValue::Promote(inner) => {
            emit_node(ctx, *inner);
            let IRType::Number(promote_from) = ctx.arena.kind(ctx.arena.node(*inner).kind) else {
                unreachable!("promotion from non-numeric type");
            };
            let IRType::Number(promote_to) = ctx.arena.kind(expr.kind) else {
                unreachable!("promotion to non-numeric type");
            };
            match (promote_from, promote_to) {
                (NumericType::Int32, NumericType::Int64) => {
                    ctx.add_instruction(Instruction::I64ExtendI32S);
                }
                (NumericType::Int32, NumericType::Float32) => {
                    ctx.add_instruction(Instruction::F32ConvertI32S);
                }
                (NumericType::Int32, NumericType::Float64) => {
                    ctx.add_instruction(Instruction::F64ConvertI32S);
                }
                (NumericType::Int64, NumericType::Float64) => {
                    ctx.add_instruction(Instruction::F64ConvertI64S);
                }
                (NumericType::Float32, NumericType::Float64) => {
                    ctx.add_instruction(Instruction::F64PromoteF32);
                }
                (a, b) => unreachable!("illegal promotion from {:?} to {:?}", a, b),
            }
        }
    }
}

fn emit_parameter(
    instructions: &mut Vec<Instruction<'_>>,
    local_index: u32,
    offset: u32,
    representation: &Representation,
) -> u32 {
    use Representation::*;

    match representation {
        Void => local_index,
        Scalar(scalar) => {
            instructions.push(Instruction::GlobalGet(BASE_PTR));
            instructions.push(Instruction::LocalGet(local_index as u32));
            // TODO: alignment
            let mem_arg = MemArg {
                offset: offset.into(),
                align: 1,
                memory_index: MAIN_MEMORY,
            };
            match scalar {
                ValType::I32 => {
                    instructions.push(Instruction::I32Store(mem_arg));
                }
                ValType::I64 => {
                    instructions.push(Instruction::I64Store(mem_arg));
                }
                ValType::F32 => {
                    instructions.push(Instruction::F32Store(mem_arg));
                }
                ValType::F64 => {
                    instructions.push(Instruction::F64Store(mem_arg));
                }
                _ => todo!(),
            }

            local_index + 1
        }
        Vector(reprs) | Struct { reprs, .. } => {
            let mut local_index = local_index;
            let mut offset = offset;
            for repr in reprs.iter() {
                local_index = emit_parameter(instructions, local_index, offset, repr);
                offset += size_of_repr_in_bytes(repr);
            }

            local_index
        }
    }
}

// TODO: do not spam this many locals
fn emit_assignment(
    instructions: &mut Vec<Instruction<'_>>,
    locals: &mut Vec<ValType>,
    offset: u32,
    representation: &Representation,
) {
    use Representation::*;

    match representation {
        Void => todo!(),
        Scalar(scalar) => {
            // TODO: unnecessary overhead for scalar assignments
            // TODO: alignment
            let mem_arg = MemArg {
                offset: offset.into(),
                align: 1,
                memory_index: MAIN_MEMORY,
            };
            let local_idx = locals.len() as u32;
            instructions.push(Instruction::LocalSet(local_idx));
            instructions.push(Instruction::GlobalGet(REFERENCE_PTR));
            instructions.push(Instruction::LocalGet(local_idx));
            locals.push(*scalar);
            match scalar {
                ValType::I32 => {
                    instructions.push(Instruction::I32Store(mem_arg));
                }
                ValType::I64 => {
                    instructions.push(Instruction::I64Store(mem_arg));
                }
                ValType::F32 => {
                    instructions.push(Instruction::F32Store(mem_arg));
                }
                ValType::F64 => {
                    instructions.push(Instruction::F64Store(mem_arg));
                }
                _ => todo!("non-simple scalar assignments"),
            }
        }
        Vector(reprs) | Struct { reprs, .. } => {
            // Backwards, so the first value goes on the stack first
            let mut offset = offset;
            for repr in reprs.iter() {
                offset += size_of_repr_in_bytes(repr);
            }
            for repr in reprs.iter() {
                offset -= size_of_repr_in_bytes(repr);
                emit_assignment(instructions, locals, offset, repr);
            }
        }
    }
}

fn emit_dereference(
    instructions: &mut Vec<Instruction<'_>>,
    offset: u32,
    representation: &Representation,
) {
    use Representation::*;

    match representation {
        Void => todo!(),
        Scalar(scalar) => {
            // TODO: alignment
            let mem_arg = MemArg {
                offset: offset.into(),
                align: 1,
                memory_index: MAIN_MEMORY,
            };
            instructions.push(Instruction::GlobalGet(REFERENCE_PTR));
            match scalar {
                ValType::I32 => {
                    instructions.push(Instruction::I32Load(mem_arg));
                }
                ValType::I64 => {
                    instructions.push(Instruction::I64Load(mem_arg));
                }
                ValType::F32 => {
                    instructions.push(Instruction::F32Load(mem_arg));
                }
                ValType::F64 => {
                    instructions.push(Instruction::F64Load(mem_arg));
                }
                _ => todo!("non-simple scalar assignments"),
            }
        }
        Vector(reprs) | Struct { reprs, .. } => {
            // Backwards, because the first value should go on the stack first
            let mut offset = offset;
            for repr in reprs.iter() {
                offset += size_of_repr_in_bytes(repr);
            }
            for repr in reprs.iter() {
                offset -= size_of_repr_in_bytes(repr);
                emit_dereference(instructions, offset, repr);
            }
        }
    }
}

#[derive(Debug)]
enum Representation {
    Void,
    Scalar(ValType),
    Vector(Vec<Representation>),
    Struct {
        field_offsets: HashMap<String, u32>,
        field_order: Vec<String>,
        reprs: Vec<Representation>,
    },
}

fn represented_by(ctx: &IRContext, kind: usize) -> Representation {
    use Representation::*;

    match ctx.kind(kind) {
        IRType::Bool
        | IRType::Unique(_)
        | IRType::Shared(_)
        | IRType::Number(NumericType::Int32) => Scalar(ValType::I32),
        IRType::Number(NumericType::Float32) => Scalar(ValType::F32),
        IRType::Number(NumericType::Int64) => Scalar(ValType::I64),
        IRType::Number(NumericType::Float64) => Scalar(ValType::F64),
        // pair of (start, length)
        IRType::Array(_) => Vector(vec![Scalar(ValType::I32), Scalar(ValType::I32)]),
        IRType::Struct { fields } => {
            // TODO: don't constantly re-calculate struct reprs
            // TODO: support re-ordering of fields

            let mut total_size = 0;
            let mut field_offsets = HashMap::new();
            let mut reprs = Vec::new();
            let mut field_order = Vec::new();
            for (key, field) in fields.iter() {
                field_order.push(key.clone());
                field_offsets.insert(key.clone(), total_size);
                let repr = represented_by(ctx, *field);
                total_size += size_of_repr_in_bytes(&repr);
                reprs.push(repr);
            }

            Struct {
                field_offsets,
                reprs,
                field_order,
            }
        }
        IRType::Function { .. } => Void,
        IRType::Void => Void,
        IRType::Unresolved(..) => Void,
    }
}

fn flatten_repr(repr: &Representation, buffer: &mut Vec<ValType>) {
    use Representation::*;
    match repr {
        Void => {}
        Scalar(val) => buffer.push(*val),
        Vector(reprs) | Struct { reprs, .. } => {
            for repr in reprs.iter() {
                flatten_repr(repr, buffer);
            }
        }
    }
}

fn size_of_repr_in_bytes(repr: &Representation) -> u32 {
    use Representation::*;
    match repr {
        Void => 0,
        Scalar(val) => value_size(*val),
        Vector(reprs) | Struct { reprs, .. } => reprs.iter().map(size_of_repr_in_bytes).sum(),
    }
}

fn value_size(val: ValType) -> u32 {
    match val {
        ValType::I32 => 4,
        ValType::F32 => 4,
        ValType::I64 => 8,
        ValType::F64 => 8,
        _ => todo!(),
    }
}

fn emit_lvalue(ctx: &mut EmitContext, lvalue: usize) -> u32 {
    let expr = ctx.arena.node(lvalue);
    match &expr.value {
        IRNodeValue::LocalVariable(name) => {
            ctx.add_instruction(Instruction::GlobalGet(BASE_PTR));
            // TODO: don't unwrap?
            *ctx.stack_offsets.get(name.as_str()).unwrap()
        }
        IRNodeValue::Dereference(child) => {
            let offset = emit_lvalue(ctx, *child) as u64;
            let mem_arg = MemArg {
                offset,
                align: 1,
                memory_index: MAIN_MEMORY,
            };
            ctx.add_instruction(Instruction::I32Load(mem_arg));

            0
        }
        IRNodeValue::Dot(left, name) => {
            let expr = ctx.arena.node(*left);
            let Representation::Struct { field_offsets, .. } = &ctx.representations[expr.kind] else {
                panic!("expected left hand of dot operator to be a struct");
            };
            let field_offset = field_offsets
                .get(name)
                .expect("expected right hand of dot operator to be present in left");
            let offset = emit_lvalue(ctx, *left);

            offset + field_offset
        }
        IRNodeValue::ArrayIndex(array, index) => {
            let offset = emit_lvalue(ctx, *array); // TODO
            ctx.add_instruction(Instruction::GlobalSet(REFERENCE_PTR));
            emit_dereference(
                &mut ctx.instructions,
                offset,
                &Representation::Scalar(ValType::I32),
            );
            emit_node(ctx, *index);
            let repr = &ctx.representations[expr.kind];
            ctx.add_instruction(Instruction::I32Const(size_of_repr_in_bytes(repr) as i32));
            ctx.add_instruction(Instruction::I32Mul);
            ctx.add_instruction(Instruction::I32Add);

            0
        }
        _ => unreachable!(),
    }
}
