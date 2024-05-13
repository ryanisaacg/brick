use std::collections::HashMap;

use brick::{
    id::{FunctionID, RegisterID, TypeID, VariableID},
    ArithmeticOp, BinaryLogicalOp, ComparisonOp, DeclaredTypeLayout, LinearFunction, LinearNode,
    LinearNodeValue, LinearRuntimeFunction, PhysicalCollection, PhysicalPrimitive, PhysicalType,
    TypeLayoutValue, UnaryLogicalOp,
};
use wasm_encoder::{BlockType, Function, Instruction, MemArg, ValType};

pub fn encode(
    function_id_to_idx: &HashMap<FunctionID, u32>,
    declarations: &HashMap<TypeID, DeclaredTypeLayout>,
    stackptr_global_idx: u32,
    allocptr_global_idx: u32,
    linear_function_to_id: &HashMap<LinearRuntimeFunction, u32>,
    func: &LinearFunction,
) -> Function {
    let mut ctx = Context {
        instructions: Vec::new(),
        declarations,
        stackptr_global_idx,
        allocptr_global_idx,
        linear_function_to_id,

        function_id_to_idx,
        register_to_local: HashMap::new(),
        locals: Vec::new(),
        local_index: func.params.len() as u32,
        variable_locations: HashMap::new(),
        stack_size: 0,
        last_loop_depth: 0,
    };
    for node in func.body.iter() {
        encode_node(&mut ctx, node);
    }
    let locals = ctx.locals.iter().cloned();
    let mut f = Function::new(locals);
    // Expand stack
    f.instruction(&Instruction::GlobalGet(stackptr_global_idx));
    f.instruction(&Instruction::I32Const(ctx.stack_size));
    f.instruction(&Instruction::I32Sub);
    f.instruction(&Instruction::GlobalSet(stackptr_global_idx));
    for instr in ctx.instructions.iter() {
        if matches!(instr, Instruction::Return) {
            contract_stack(&mut f, stackptr_global_idx, ctx.stack_size);
        }
        f.instruction(instr);
    }
    // Contract stack
    contract_stack(&mut f, stackptr_global_idx, ctx.stack_size);
    f.instruction(&Instruction::End);

    f
}

struct Context<'a> {
    // Environment
    function_id_to_idx: &'a HashMap<FunctionID, u32>,
    stackptr_global_idx: u32,
    allocptr_global_idx: u32,
    declarations: &'a HashMap<TypeID, DeclaredTypeLayout>,
    linear_function_to_id: &'a HashMap<LinearRuntimeFunction, u32>,
    // Result
    instructions: Vec<Instruction<'a>>,
    // Temp
    register_to_local: HashMap<RegisterID, u32>,
    locals: Vec<(u32, ValType)>,
    local_index: u32,
    variable_locations: HashMap<VariableID, i32>,
    stack_size: i32,
    last_loop_depth: u32,
}

impl<'a> Context<'a> {
    fn alloc_local(&mut self, val: ValType) -> u32 {
        let local = self.local_index;
        self.locals.push((local, val));
        self.local_index += 1;
        local
    }
}

fn encode_node(ctx: &mut Context<'_>, node: &LinearNode) {
    match &node.value {
        LinearNodeValue::HeapAlloc(alloc_size) => {
            ctx.instructions
                .push(Instruction::GlobalGet(ctx.allocptr_global_idx));
            let original_pointer = ctx.alloc_local(ValType::I32);
            ctx.instructions
                .push(Instruction::LocalSet(original_pointer));
            encode_node(ctx, alloc_size);
            ctx.instructions
                .push(Instruction::GlobalGet(ctx.allocptr_global_idx));
            ctx.instructions.push(Instruction::I32Add);
            ctx.instructions
                .push(Instruction::GlobalSet(ctx.allocptr_global_idx));
            ctx.instructions
                .push(Instruction::LocalGet(original_pointer));
        }
        LinearNodeValue::Parameter(idx) => {
            ctx.instructions.push(Instruction::LocalGet(*idx as u32));
        }
        LinearNodeValue::VariableInit(var_id, ty) => {
            ctx.variable_locations.insert(*var_id, ctx.stack_size);
            ctx.stack_size += ty.size(&ctx.declarations) as i32;
        }
        LinearNodeValue::VariableDestroy(_) => {
            // TODO: do we care
        }
        LinearNodeValue::VariableLocation(var_id) => {
            // TODO: optimize
            ctx.instructions
                .push(Instruction::GlobalGet(ctx.stackptr_global_idx));
            ctx.instructions
                .push(Instruction::I32Const(ctx.variable_locations[var_id]));
            ctx.instructions.push(Instruction::I32Add);
        }
        LinearNodeValue::ReadMemory {
            location,
            offset,
            ty,
        } => {
            encode_node(ctx, location);
            if let PhysicalType::Primitive(prim) = ty {
                read_primitive(ctx, primitive_to_val_type(*prim), *offset as u64);
            } else {
                let location_var = ctx.alloc_local(ValType::I32);
                ctx.instructions.push(Instruction::LocalSet(location_var));
                read_memory(ctx, ty, location_var, *offset as u64);
            }
        }
        LinearNodeValue::WriteMemory {
            location,
            offset,
            ty,
            value,
        } => {
            if let PhysicalType::Primitive(prim) = ty {
                encode_node(ctx, location);
                encode_node(ctx, value);
                let mem = MemArg {
                    offset: *offset as u64,
                    align: 0,
                    memory_index: 0,
                };
                match prim {
                    PhysicalPrimitive::Byte
                    | PhysicalPrimitive::Int32
                    | PhysicalPrimitive::PointerSize => {
                        ctx.instructions.push(Instruction::I32Store(mem));
                    }
                    PhysicalPrimitive::Float32 => {
                        ctx.instructions.push(Instruction::F32Store(mem));
                    }
                    PhysicalPrimitive::Int64 => {
                        ctx.instructions.push(Instruction::I64Store(mem));
                    }
                    PhysicalPrimitive::Float64 => {
                        ctx.instructions.push(Instruction::F64Store(mem));
                    }
                }
            } else {
                encode_node(ctx, location);
                let location_var = ctx.alloc_local(ValType::I32);
                ctx.instructions.push(Instruction::LocalSet(location_var));
                encode_node(ctx, value);
                write_memory(ctx, ty, location_var, *offset as u64);
            }
        }
        LinearNodeValue::TopOfStack => {}
        LinearNodeValue::Discard => {
            ctx.instructions.push(Instruction::Drop);
        }
        LinearNodeValue::Call(lhs, args) => {
            for arg in args.iter() {
                encode_node(ctx, arg);
            }
            match &lhs.value {
                LinearNodeValue::FunctionID(fn_id) => {
                    let fn_idx = ctx.function_id_to_idx[fn_id];
                    ctx.instructions.push(Instruction::Call(fn_idx));
                }
                LinearNodeValue::VariableLocation(var) => {
                    // TODO: dynamic function call
                }
                LinearNodeValue::ReadMemory { .. } => {
                    // TODO: what's emitting this?
                }
                other => unreachable!("{:?}", other),
            }
        }
        LinearNodeValue::RuntimeCall(func, args) => {
            for arg in args.iter() {
                encode_node(ctx, arg);
            }
            let fn_id = ctx.linear_function_to_id.get(func);
            if let Some(fn_id) = fn_id {
                ctx.instructions.push(Instruction::Call(*fn_id));
            }
        }
        LinearNodeValue::Return(value) => {
            if let Some(value) = value {
                encode_node(ctx, value);
            }
            ctx.instructions.push(Instruction::Return);
        }
        LinearNodeValue::If(cond, then_branch, else_branch) => {
            encode_node(ctx, cond);
            ctx.last_loop_depth += 1;
            // TODO: BlockType for branches
            ctx.instructions.push(Instruction::If(BlockType::Empty));
            for node in then_branch.iter() {
                encode_node(ctx, node);
            }
            if let Some(else_branch) = else_branch {
                ctx.instructions.push(Instruction::Else);
                for node in else_branch.iter() {
                    encode_node(ctx, node);
                }
            }
            ctx.instructions.push(Instruction::End);
            ctx.last_loop_depth -= 1;
        }
        LinearNodeValue::Loop(inner) => {
            ctx.instructions.push(Instruction::Block(BlockType::Empty));
            ctx.instructions.push(Instruction::Loop(BlockType::Empty));
            let prev_loop_depth = ctx.last_loop_depth;
            ctx.last_loop_depth = 1;
            for node in inner.iter() {
                encode_node(ctx, node);
            }
            ctx.instructions.push(Instruction::Br(0));
            ctx.instructions.push(Instruction::End);
            ctx.instructions.push(Instruction::End);
            ctx.last_loop_depth = prev_loop_depth;
        }
        LinearNodeValue::Break => {
            ctx.instructions.push(Instruction::Br(ctx.last_loop_depth));
        }
        LinearNodeValue::Abort => {
            ctx.instructions.push(Instruction::Unreachable);
        }
        LinearNodeValue::Goto(_) => { /* TODO */ }
        LinearNodeValue::GotoLabel(_) => { /* TODO */ }
        LinearNodeValue::Sequence(nodes) => {
            for node in nodes.iter() {
                encode_node(ctx, node);
            }
        }
        LinearNodeValue::WriteRegister(reg_id, value) => {
            encode_node(ctx, value);
            if !ctx.register_to_local.contains_key(reg_id) {
                let local = ctx.alloc_local(ValType::I32);
                ctx.register_to_local.insert(*reg_id, local);
            }
            let local_idx = ctx.register_to_local[reg_id];
            ctx.instructions.push(Instruction::LocalSet(local_idx));
        }
        LinearNodeValue::ReadRegister(reg_id) => {
            let local_idx = ctx.register_to_local[reg_id];
            ctx.instructions.push(Instruction::LocalGet(local_idx));
        }
        LinearNodeValue::KillRegister(_) => {
            // TODO (later): efficient register <-> local allocation
        }
        LinearNodeValue::Arithmetic(operator, prim, lhs, rhs) => {
            encode_node(ctx, lhs);
            encode_node(ctx, rhs);
            match (operator, prim) {
                (ArithmeticOp::Add, PhysicalPrimitive::Byte)
                | (ArithmeticOp::Add, PhysicalPrimitive::PointerSize)
                | (ArithmeticOp::Add, PhysicalPrimitive::Int32) => {
                    ctx.instructions.push(Instruction::I32Add);
                }
                (ArithmeticOp::Add, PhysicalPrimitive::Float32) => {
                    ctx.instructions.push(Instruction::F32Add);
                }
                (ArithmeticOp::Add, PhysicalPrimitive::Int64) => {
                    ctx.instructions.push(Instruction::I64Add);
                }
                (ArithmeticOp::Add, PhysicalPrimitive::Float64) => {
                    ctx.instructions.push(Instruction::F64Add);
                }
                (ArithmeticOp::Subtract, PhysicalPrimitive::Byte)
                | (ArithmeticOp::Subtract, PhysicalPrimitive::PointerSize)
                | (ArithmeticOp::Subtract, PhysicalPrimitive::Int32) => {
                    ctx.instructions.push(Instruction::I32Sub);
                }
                (ArithmeticOp::Subtract, PhysicalPrimitive::Float32) => {
                    ctx.instructions.push(Instruction::F32Sub);
                }
                (ArithmeticOp::Subtract, PhysicalPrimitive::Int64) => {
                    ctx.instructions.push(Instruction::I64Sub);
                }
                (ArithmeticOp::Subtract, PhysicalPrimitive::Float64) => {
                    ctx.instructions.push(Instruction::F64Sub);
                }
                (ArithmeticOp::Multiply, PhysicalPrimitive::Byte)
                | (ArithmeticOp::Multiply, PhysicalPrimitive::PointerSize)
                | (ArithmeticOp::Multiply, PhysicalPrimitive::Int32) => {
                    ctx.instructions.push(Instruction::I32Mul);
                }
                (ArithmeticOp::Multiply, PhysicalPrimitive::Float32) => {
                    ctx.instructions.push(Instruction::F32Mul);
                }
                (ArithmeticOp::Multiply, PhysicalPrimitive::Int64) => {
                    ctx.instructions.push(Instruction::I64Mul);
                }
                (ArithmeticOp::Multiply, PhysicalPrimitive::Float64) => {
                    ctx.instructions.push(Instruction::F64Mul);
                }
                (ArithmeticOp::Divide, PhysicalPrimitive::Byte)
                | (ArithmeticOp::Divide, PhysicalPrimitive::PointerSize)
                | (ArithmeticOp::Divide, PhysicalPrimitive::Int32) => {
                    ctx.instructions.push(Instruction::I32DivS);
                }
                (ArithmeticOp::Divide, PhysicalPrimitive::Float32) => {
                    ctx.instructions.push(Instruction::F32Div);
                }
                (ArithmeticOp::Divide, PhysicalPrimitive::Int64) => {
                    ctx.instructions.push(Instruction::I64DivS);
                }
                (ArithmeticOp::Divide, PhysicalPrimitive::Float64) => {
                    ctx.instructions.push(Instruction::F64Div);
                }
            }
        }
        LinearNodeValue::Comparison(operator, prim, lhs, rhs) => {
            encode_node(ctx, lhs);
            encode_node(ctx, rhs);
            match (operator, prim) {
                (ComparisonOp::LessThan, PhysicalPrimitive::Byte) => {
                    ctx.instructions.push(Instruction::I32LtS);
                }
                (ComparisonOp::LessThan, PhysicalPrimitive::Int32) => {
                    ctx.instructions.push(Instruction::I32LtS);
                }
                (ComparisonOp::LessThan, PhysicalPrimitive::Float32) => {
                    ctx.instructions.push(Instruction::F32Lt);
                }
                (ComparisonOp::LessThan, PhysicalPrimitive::Int64) => {
                    ctx.instructions.push(Instruction::I64LtS);
                }
                (ComparisonOp::LessThan, PhysicalPrimitive::Float64) => {
                    ctx.instructions.push(Instruction::F64Lt);
                }
                (ComparisonOp::LessThan, PhysicalPrimitive::PointerSize) => {
                    ctx.instructions.push(Instruction::I32LtS);
                }
                (ComparisonOp::GreaterThan, PhysicalPrimitive::Byte) => {
                    ctx.instructions.push(Instruction::I32GtS);
                }
                (ComparisonOp::GreaterThan, PhysicalPrimitive::Int32) => {
                    ctx.instructions.push(Instruction::I32GtS);
                }
                (ComparisonOp::GreaterThan, PhysicalPrimitive::Float32) => {
                    ctx.instructions.push(Instruction::F32Gt);
                }
                (ComparisonOp::GreaterThan, PhysicalPrimitive::Int64) => {
                    ctx.instructions.push(Instruction::I64GtS);
                }
                (ComparisonOp::GreaterThan, PhysicalPrimitive::Float64) => {
                    ctx.instructions.push(Instruction::F64Gt);
                }
                (ComparisonOp::GreaterThan, PhysicalPrimitive::PointerSize) => {
                    ctx.instructions.push(Instruction::I32GtS);
                }
                (ComparisonOp::LessEqualThan, PhysicalPrimitive::Byte) => {
                    ctx.instructions.push(Instruction::I32LeS);
                }
                (ComparisonOp::LessEqualThan, PhysicalPrimitive::Int32) => {
                    ctx.instructions.push(Instruction::I32LeS);
                }
                (ComparisonOp::LessEqualThan, PhysicalPrimitive::Float32) => {
                    ctx.instructions.push(Instruction::F32Le);
                }
                (ComparisonOp::LessEqualThan, PhysicalPrimitive::Int64) => {
                    ctx.instructions.push(Instruction::I64LeS);
                }
                (ComparisonOp::LessEqualThan, PhysicalPrimitive::Float64) => {
                    ctx.instructions.push(Instruction::F64Le);
                }
                (ComparisonOp::LessEqualThan, PhysicalPrimitive::PointerSize) => {
                    ctx.instructions.push(Instruction::I32LeS);
                }
                (ComparisonOp::GreaterEqualThan, PhysicalPrimitive::Byte) => {
                    ctx.instructions.push(Instruction::I32GeS);
                }
                (ComparisonOp::GreaterEqualThan, PhysicalPrimitive::Int32) => {
                    ctx.instructions.push(Instruction::I32GeS);
                }
                (ComparisonOp::GreaterEqualThan, PhysicalPrimitive::Float32) => {
                    ctx.instructions.push(Instruction::F32Ge);
                }
                (ComparisonOp::GreaterEqualThan, PhysicalPrimitive::Int64) => {
                    ctx.instructions.push(Instruction::I64GeS);
                }
                (ComparisonOp::GreaterEqualThan, PhysicalPrimitive::Float64) => {
                    ctx.instructions.push(Instruction::F64Ge);
                }
                (ComparisonOp::GreaterEqualThan, PhysicalPrimitive::PointerSize) => {
                    ctx.instructions.push(Instruction::I32GeS);
                }
                (ComparisonOp::EqualTo, PhysicalPrimitive::Byte) => {
                    ctx.instructions.push(Instruction::I32Eq);
                }
                (ComparisonOp::EqualTo, PhysicalPrimitive::Int32) => {
                    ctx.instructions.push(Instruction::I32Eq);
                }
                (ComparisonOp::EqualTo, PhysicalPrimitive::Float32) => {
                    ctx.instructions.push(Instruction::F32Eq);
                }
                (ComparisonOp::EqualTo, PhysicalPrimitive::Int64) => {
                    ctx.instructions.push(Instruction::I64Eq);
                }
                (ComparisonOp::EqualTo, PhysicalPrimitive::Float64) => {
                    ctx.instructions.push(Instruction::F64Eq);
                }
                (ComparisonOp::EqualTo, PhysicalPrimitive::PointerSize) => {
                    ctx.instructions.push(Instruction::I32Eq);
                }
                (ComparisonOp::NotEquals, PhysicalPrimitive::Byte) => {
                    ctx.instructions.push(Instruction::I32Ne);
                }
                (ComparisonOp::NotEquals, PhysicalPrimitive::Int32) => {
                    ctx.instructions.push(Instruction::I32Ne);
                }
                (ComparisonOp::NotEquals, PhysicalPrimitive::Float32) => {
                    ctx.instructions.push(Instruction::F32Ne);
                }
                (ComparisonOp::NotEquals, PhysicalPrimitive::Int64) => {
                    ctx.instructions.push(Instruction::I64Ne);
                }
                (ComparisonOp::NotEquals, PhysicalPrimitive::Float64) => {
                    ctx.instructions.push(Instruction::F64Ne);
                }
                (ComparisonOp::NotEquals, PhysicalPrimitive::PointerSize) => {
                    ctx.instructions.push(Instruction::I32Ne);
                }
            }
        }
        LinearNodeValue::BinaryLogical(op, lhs, rhs) => {
            encode_node(ctx, lhs);
            encode_node(ctx, rhs);
            match op {
                BinaryLogicalOp::BooleanAnd => {
                    ctx.instructions.push(Instruction::I32And);
                }
                BinaryLogicalOp::BooleanOr => {
                    ctx.instructions.push(Instruction::I32Or);
                }
            }
        }
        LinearNodeValue::UnaryLogical(op, value) => {
            encode_node(ctx, value);
            match op {
                UnaryLogicalOp::BooleanNot => {
                    ctx.instructions.push(Instruction::I32Const(1));
                    ctx.instructions.push(Instruction::I32Xor);
                }
            }
        }
        LinearNodeValue::Cast { value, from, to } => {
            encode_node(ctx, value);
            match (from, to) {
                (
                    PhysicalPrimitive::Byte
                    | PhysicalPrimitive::Int32
                    | PhysicalPrimitive::PointerSize,
                    PhysicalPrimitive::Byte
                    | PhysicalPrimitive::Int32
                    | PhysicalPrimitive::PointerSize,
                )
                | (PhysicalPrimitive::Float32, PhysicalPrimitive::Float32)
                | (PhysicalPrimitive::Float64, PhysicalPrimitive::Float64)
                | (PhysicalPrimitive::Int64, PhysicalPrimitive::Int64) => {
                    // ignore, no-op cast
                }
                (
                    PhysicalPrimitive::Byte
                    | PhysicalPrimitive::Int32
                    | PhysicalPrimitive::PointerSize,
                    PhysicalPrimitive::Float32,
                ) => {
                    ctx.instructions.push(Instruction::F32ConvertI32S);
                }
                (
                    PhysicalPrimitive::Byte
                    | PhysicalPrimitive::Int32
                    | PhysicalPrimitive::PointerSize,
                    PhysicalPrimitive::Int64,
                ) => {
                    ctx.instructions.push(Instruction::I64Extend32S);
                }
                (
                    PhysicalPrimitive::Byte
                    | PhysicalPrimitive::Int32
                    | PhysicalPrimitive::PointerSize,
                    PhysicalPrimitive::Float64,
                ) => {
                    ctx.instructions.push(Instruction::F64ConvertI32S);
                }
                (
                    PhysicalPrimitive::Float32,
                    PhysicalPrimitive::Byte
                    | PhysicalPrimitive::Int32
                    | PhysicalPrimitive::PointerSize,
                ) => {
                    ctx.instructions.push(Instruction::I32TruncF32S);
                }
                (PhysicalPrimitive::Float32, PhysicalPrimitive::Int64) => {
                    ctx.instructions.push(Instruction::I64TruncF32S);
                }
                (PhysicalPrimitive::Float32, PhysicalPrimitive::Float64) => {
                    ctx.instructions.push(Instruction::F64PromoteF32);
                }
                (
                    PhysicalPrimitive::Int64,
                    PhysicalPrimitive::Byte
                    | PhysicalPrimitive::Int32
                    | PhysicalPrimitive::PointerSize,
                ) => {
                    ctx.instructions.push(Instruction::I32WrapI64);
                }
                (PhysicalPrimitive::Int64, PhysicalPrimitive::Float32) => {
                    ctx.instructions.push(Instruction::F32ConvertI64S);
                }
                (PhysicalPrimitive::Int64, PhysicalPrimitive::Float64) => {
                    ctx.instructions.push(Instruction::F64ConvertI64S);
                }
                (
                    PhysicalPrimitive::Float64,
                    PhysicalPrimitive::Byte
                    | PhysicalPrimitive::Int32
                    | PhysicalPrimitive::PointerSize,
                ) => {
                    ctx.instructions.push(Instruction::I32TruncF64S);
                }
                (PhysicalPrimitive::Float64, PhysicalPrimitive::Float32) => {
                    ctx.instructions.push(Instruction::F32DemoteF64);
                }
                (PhysicalPrimitive::Float64, PhysicalPrimitive::Int64) => {
                    ctx.instructions.push(Instruction::I64TruncF64S);
                }
            }
        }
        LinearNodeValue::Size(value) => {
            ctx.instructions.push(Instruction::I32Const(*value as i32));
        }
        LinearNodeValue::Int(value) => {
            // TODO (later): don't cast
            ctx.instructions.push(Instruction::I32Const(*value as i32));
        }
        LinearNodeValue::Float(value) => {
            // TODO (later): don't cast
            ctx.instructions.push(Instruction::F32Const(*value as f32));
        }
        LinearNodeValue::CharLiteral(value) => {
            ctx.instructions.push(Instruction::I32Const(*value as i32));
        }
        LinearNodeValue::Byte(value) => {
            ctx.instructions.push(Instruction::I32Const(*value as i32));
        }
        LinearNodeValue::FunctionID(fn_id) => {
            let fn_idx = ctx.function_id_to_idx[fn_id];
            ctx.instructions.push(Instruction::I32Const(fn_idx as i32));
        }
        LinearNodeValue::ConstantData(_) => { /* TODO */ }
        LinearNodeValue::Debug(_) => { /* TODO */ }
    }
}

fn read_memory(ctx: &mut Context<'_>, ty: &PhysicalType, location_var: u32, offset: u64) {
    walk_vals_read_order(&ctx.declarations, ty, offset, &mut |val_ty, offset| {
        ctx.instructions.push(Instruction::LocalGet(location_var));
        read_primitive(ctx, val_ty, offset);
    });
}

fn walk_vals_read_order(
    declarations: &HashMap<TypeID, DeclaredTypeLayout>,
    ty: &PhysicalType,
    offset: u64,
    callback: &mut dyn FnMut(ValType, u64),
) {
    match ty {
        PhysicalType::Primitive(prim) => callback(primitive_to_val_type(*prim), offset),
        PhysicalType::FunctionPointer => callback(ValType::I32, offset),
        PhysicalType::Referenced(ty_id) => {
            let ty = &declarations[ty_id];
            match &ty.value {
                TypeLayoutValue::Structure(fields) => {
                    for (_, field_offset, ty) in fields.iter().rev() {
                        let offset = *field_offset as u64 + offset;
                        walk_vals_read_order(declarations, ty, offset, callback);
                    }
                }
                TypeLayoutValue::Interface(fields) => {
                    let mut location = offset + (ty.size as u64);
                    for _ in fields.iter().rev() {
                        callback(ValType::I32, location);
                        location -= 4;
                    }
                    callback(ValType::I32, offset);
                }
                TypeLayoutValue::Union(_) => todo!(),
            }
        }
        PhysicalType::Nullable(inner) => {
            // TODO: uhhhh this doesn't match the linear IR
            walk_vals_read_order(
                declarations,
                &PhysicalType::Primitive(PhysicalPrimitive::Byte),
                offset,
                callback,
            );
            walk_vals_read_order(declarations, inner.as_ref(), offset + 4, callback);
        }
        PhysicalType::Collection(PhysicalCollection::Array | PhysicalCollection::Dict)
        | PhysicalType::Generator => {
            callback(ValType::I32, offset + 16);
            callback(ValType::I32, offset + 8);
            callback(ValType::I32, offset);
        }
    }
}

fn read_primitive(ctx: &mut Context<'_>, ty: ValType, offset: u64) {
    let mem = MemArg {
        offset,
        align: 0,
        memory_index: 0,
    };
    ctx.instructions.push(match ty {
        ValType::I32 => Instruction::I32Load(mem),
        ValType::F32 => Instruction::F32Load(mem),
        ValType::I64 => Instruction::I64Load(mem),
        ValType::F64 => Instruction::F64Load(mem),
        ValType::V128 | ValType::Ref(_) => unreachable!(),
    });
}

fn write_memory(ctx: &mut Context<'_>, ty: &PhysicalType, location_var: u32, offset: u64) {
    walk_vals_write_order(&ctx.declarations, ty, offset, &mut |val_ty, offset| {
        write_primitive(ctx, val_ty, location_var, offset);
    });
}

fn walk_vals_write_order(
    declarations: &HashMap<TypeID, DeclaredTypeLayout>,
    ty: &PhysicalType,
    offset: u64,
    callback: &mut dyn FnMut(ValType, u64),
) {
    match ty {
        PhysicalType::Primitive(prim) => {
            callback(primitive_to_val_type(*prim), offset);
        }
        PhysicalType::FunctionPointer => {
            callback(ValType::I32, offset);
        }
        PhysicalType::Referenced(id) => match &declarations[id].value {
            TypeLayoutValue::Structure(fields) => {
                for (_, field_offset, ty) in fields.iter() {
                    let offset = offset + *field_offset as u64;
                    walk_vals_write_order(declarations, ty, offset, callback);
                }
            }
            TypeLayoutValue::Interface(fields) => {
                callback(ValType::I32, offset);
                let mut offset = offset + 4;
                for _field in fields.iter() {
                    callback(ValType::I32, offset);
                    offset += 4;
                }
            }
            TypeLayoutValue::Union(variants) => {
                // TODO
            }
        },
        PhysicalType::Nullable(inner) => {
            callback(ValType::I32, offset);
            walk_vals_write_order(declarations, inner.as_ref(), offset + 4, callback);
        }
        PhysicalType::Collection(PhysicalCollection::Array | PhysicalCollection::Dict)
        | PhysicalType::Generator => {
            callback(ValType::I32, offset);
            callback(ValType::I32, offset + 8);
            callback(ValType::I32, offset + 16);
        }
    }
}

// TODO: don't spam locals
fn write_primitive(ctx: &mut Context<'_>, ty: ValType, location_var: u32, offset: u64) {
    let mem = MemArg {
        offset,
        align: 0,
        memory_index: 0,
    };
    let swap_local = ctx.alloc_local(ty);
    // TODO: there has to be something less goofy than this
    ctx.instructions.push(Instruction::LocalSet(swap_local));
    ctx.instructions.push(Instruction::LocalGet(location_var));
    ctx.instructions.push(Instruction::LocalGet(swap_local));
    ctx.instructions.push(match ty {
        ValType::I32 => Instruction::I32Store(mem),
        ValType::F32 => Instruction::F32Store(mem),
        ValType::I64 => Instruction::I64Store(mem),
        ValType::F64 => Instruction::F64Store(mem),
        ValType::V128 | ValType::Ref(_) => unreachable!(),
    });
}

fn contract_stack(f: &mut Function, stackptr_global_idx: u32, stack_size: i32) {
    f.instruction(&Instruction::GlobalGet(stackptr_global_idx));
    f.instruction(&Instruction::I32Const(stack_size));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::GlobalSet(stackptr_global_idx));
}

fn primitive_to_val_type(primitive: PhysicalPrimitive) -> ValType {
    match primitive {
        PhysicalPrimitive::Byte | PhysicalPrimitive::Int32 | PhysicalPrimitive::PointerSize => {
            ValType::I32
        }
        PhysicalPrimitive::Float32 => ValType::F32,
        PhysicalPrimitive::Int64 => ValType::I64,
        PhysicalPrimitive::Float64 => ValType::F64,
    }
}
