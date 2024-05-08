use std::collections::HashMap;

use brick::{
    id::{FunctionID, RegisterID, TypeID, VariableID},
    ArithmeticOp, BinaryLogicalOp, ComparisonOp, DeclaredTypeLayout, LinearFunction, LinearNode,
    LinearNodeValue, PhysicalPrimitive, PhysicalType, UnaryLogicalOp,
};
use wasm_encoder::{BlockType, Function, Instruction, MemArg, ValType};

pub fn encode(
    function_id_to_idx: &HashMap<FunctionID, u32>,
    declarations: &HashMap<TypeID, DeclaredTypeLayout>,
    stackptr_global_idx: u32,
    func: &LinearFunction,
) -> Function {
    let mut body = Context {
        instructions: Vec::new(),
        declarations,
        stackptr_global_idx,

        function_id_to_idx,
        register_to_local: HashMap::new(),
        local_index: 0,
        variable_locations: HashMap::new(),
        stack_size: 0,
    };
    for node in func.body.iter() {
        encode_node(&mut body, node);
    }
    let locals = body
        .register_to_local
        .values()
        .map(|idx| (*idx, ValType::I32));
    let mut f = Function::new(locals);
    // Expand stack
    f.instruction(&Instruction::GlobalGet(stackptr_global_idx));
    f.instruction(&Instruction::I32Const(body.stack_size));
    f.instruction(&Instruction::I32Sub);
    f.instruction(&Instruction::GlobalSet(stackptr_global_idx));
    for instr in body.instructions.iter() {
        f.instruction(instr);
    }
    // Contract stack
    f.instruction(&Instruction::GlobalGet(stackptr_global_idx));
    f.instruction(&Instruction::I32Const(body.stack_size));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::GlobalSet(stackptr_global_idx));
    f.instruction(&Instruction::End);

    f
}

struct Context<'a> {
    // Environment
    function_id_to_idx: &'a HashMap<FunctionID, u32>,
    stackptr_global_idx: u32,
    declarations: &'a HashMap<TypeID, DeclaredTypeLayout>,
    // Temp
    instructions: Vec<Instruction<'a>>,
    register_to_local: HashMap<RegisterID, u32>,
    local_index: u32,
    variable_locations: HashMap<VariableID, i32>,
    stack_size: i32,
}

fn encode_node(ctx: &mut Context<'_>, node: &LinearNode) {
    match &node.value {
        LinearNodeValue::HeapAlloc(_) => { /* TODO */ }
        LinearNodeValue::Parameter(_) => { /* TODO */ }
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
            // TODO: non-prim
            if let PhysicalType::Primitive(prim) = ty {
                encode_node(ctx, location);
                let mem = MemArg {
                    offset: *offset as u64,
                    align: 0,
                    memory_index: 0,
                };
                match prim {
                    PhysicalPrimitive::Byte
                    | PhysicalPrimitive::Int32
                    | PhysicalPrimitive::PointerSize => {
                        ctx.instructions.push(Instruction::I32Load(mem));
                    }
                    PhysicalPrimitive::Float32 => {
                        ctx.instructions.push(Instruction::F32Load(mem));
                    }
                    PhysicalPrimitive::Int64 => {
                        ctx.instructions.push(Instruction::I64Load(mem));
                    }
                    PhysicalPrimitive::Float64 => {
                        ctx.instructions.push(Instruction::F64Load(mem));
                    }
                }
            }
        }
        LinearNodeValue::WriteMemory {
            location,
            offset,
            ty,
            value,
        } => {
            // TODO: non-prim
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
            }
        }
        LinearNodeValue::MemoryCopy { source, dest, size } => { /* TODO */ }
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
        LinearNodeValue::RuntimeCall(_, _) => { /* TODO */ }
        LinearNodeValue::Return(value) => {
            if let Some(value) = value {
                encode_node(ctx, value);
            }
            ctx.instructions.push(Instruction::Return);
        }
        LinearNodeValue::If(cond, then_branch, else_branch) => {
            encode_node(ctx, cond);
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
            } else {
                ctx.instructions.push(Instruction::End);
            }
        }
        LinearNodeValue::Loop(inner) => {
            ctx.instructions.push(Instruction::Loop(BlockType::Empty));
            for node in inner.iter() {
                encode_node(ctx, node);
            }
            ctx.instructions.push(Instruction::End);
        }
        LinearNodeValue::Break => {
            // TODO: doesn't work
            ctx.instructions.push(Instruction::Br(0));
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
                ctx.register_to_local.insert(*reg_id, ctx.local_index);
                ctx.local_index += 1;
            }
            let local_idx = ctx.register_to_local[reg_id];
            ctx.instructions.push(Instruction::LocalGet(local_idx));
        }
        LinearNodeValue::ReadRegister(reg_id) => {
            let local_idx = ctx.register_to_local[reg_id];
            ctx.instructions.push(Instruction::LocalGet(local_idx));
        }
        LinearNodeValue::KillRegister(_) => {
            // TODO: efficient register <-> local allocation
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
                (PhysicalPrimitive::Byte, PhysicalPrimitive::Byte) => { /* TODO */ }
                (PhysicalPrimitive::Byte, PhysicalPrimitive::Int32) => { /* TODO */ }
                (PhysicalPrimitive::Byte, PhysicalPrimitive::Float32) => { /* TODO */ }
                (PhysicalPrimitive::Byte, PhysicalPrimitive::Int64) => { /* TODO */ }
                (PhysicalPrimitive::Byte, PhysicalPrimitive::Float64) => { /* TODO */ }
                (PhysicalPrimitive::Byte, PhysicalPrimitive::PointerSize) => { /* TODO */ }
                (PhysicalPrimitive::Int32, PhysicalPrimitive::Byte) => { /* TODO */ }
                (PhysicalPrimitive::Int32, PhysicalPrimitive::Int32) => { /* TODO */ }
                (PhysicalPrimitive::Int32, PhysicalPrimitive::Float32) => { /* TODO */ }
                (PhysicalPrimitive::Int32, PhysicalPrimitive::Int64) => { /* TODO */ }
                (PhysicalPrimitive::Int32, PhysicalPrimitive::Float64) => { /* TODO */ }
                (PhysicalPrimitive::Int32, PhysicalPrimitive::PointerSize) => { /* TODO */ }
                (PhysicalPrimitive::Float32, PhysicalPrimitive::Byte) => { /* TODO */ }
                (PhysicalPrimitive::Float32, PhysicalPrimitive::Int32) => { /* TODO */ }
                (PhysicalPrimitive::Float32, PhysicalPrimitive::Float32) => { /* TODO */ }
                (PhysicalPrimitive::Float32, PhysicalPrimitive::Int64) => { /* TODO */ }
                (PhysicalPrimitive::Float32, PhysicalPrimitive::Float64) => { /* TODO */ }
                (PhysicalPrimitive::Float32, PhysicalPrimitive::PointerSize) => { /* TODO */ }
                (PhysicalPrimitive::Int64, PhysicalPrimitive::Byte) => { /* TODO */ }
                (PhysicalPrimitive::Int64, PhysicalPrimitive::Int32) => { /* TODO */ }
                (PhysicalPrimitive::Int64, PhysicalPrimitive::Float32) => { /* TODO */ }
                (PhysicalPrimitive::Int64, PhysicalPrimitive::Int64) => { /* TODO */ }
                (PhysicalPrimitive::Int64, PhysicalPrimitive::Float64) => { /* TODO */ }
                (PhysicalPrimitive::Int64, PhysicalPrimitive::PointerSize) => { /* TODO */ }
                (PhysicalPrimitive::Float64, PhysicalPrimitive::Byte) => { /* TODO */ }
                (PhysicalPrimitive::Float64, PhysicalPrimitive::Int32) => { /* TODO */ }
                (PhysicalPrimitive::Float64, PhysicalPrimitive::Float32) => { /* TODO */ }
                (PhysicalPrimitive::Float64, PhysicalPrimitive::Int64) => { /* TODO */ }
                (PhysicalPrimitive::Float64, PhysicalPrimitive::Float64) => { /* TODO */ }
                (PhysicalPrimitive::Float64, PhysicalPrimitive::PointerSize) => { /* TODO */ }
                (PhysicalPrimitive::PointerSize, PhysicalPrimitive::Byte) => { /* TODO */ }
                (PhysicalPrimitive::PointerSize, PhysicalPrimitive::Int32) => { /* TODO */ }
                (PhysicalPrimitive::PointerSize, PhysicalPrimitive::Float32) => { /* TODO */ }
                (PhysicalPrimitive::PointerSize, PhysicalPrimitive::Int64) => { /* TODO */ }
                (PhysicalPrimitive::PointerSize, PhysicalPrimitive::Float64) => { /* TODO */ }
                (PhysicalPrimitive::PointerSize, PhysicalPrimitive::PointerSize) => { /* TODO */ }
            }
        }
        LinearNodeValue::Size(value) => {
            ctx.instructions.push(Instruction::I32Const(*value as i32));
        }
        LinearNodeValue::Int(value) => {
            // TODO: don't cast
            ctx.instructions.push(Instruction::I32Const(*value as i32));
        }
        LinearNodeValue::Float(value) => {
            // TODO: don't cast
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
