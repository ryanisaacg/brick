use std::collections::HashMap;

use brick::{
    id::FunctionID, ArithmeticOp, BinaryLogicalOp, ComparisonOp, LinearFunction, LinearNode,
    LinearNodeValue, PhysicalPrimitive, UnaryLogicalOp,
};
use wasm_encoder::{BlockType, Function, Instruction};

pub fn encode(function_id_to_idx: &HashMap<FunctionID, u32>, func: &LinearFunction) -> Function {
    let mut body = EncodedBody {
        instructions: Vec::new(),
        function_id_to_idx,
    };
    for node in func.body.iter() {
        encode_node(&mut body, node);
    }
    let locals = vec![];
    let mut f = Function::new(locals);
    for instr in body.instructions.iter() {
        f.instruction(instr);
    }
    f.instruction(&Instruction::End);

    f
}

struct EncodedBody<'a> {
    instructions: Vec<Instruction<'a>>,
    function_id_to_idx: &'a HashMap<FunctionID, u32>,
}

fn encode_node(encoded: &mut EncodedBody<'_>, node: &LinearNode) {
    match &node.value {
        LinearNodeValue::HeapAlloc(_) => { /* TODO */ }
        LinearNodeValue::Parameter(_) => { /* TODO */ }
        LinearNodeValue::VariableInit(_, _) => { /* TODO */ }
        LinearNodeValue::VariableDestroy(_) => {
            // TODO
        }
        LinearNodeValue::VariableLocation(_) => { /* TODO */ }
        LinearNodeValue::ReadMemory {
            location,
            offset,
            ty,
        } => { /* TODO */ }
        LinearNodeValue::WriteMemory {
            location,
            offset,
            ty,
            value,
        } => { /* TODO */ }
        LinearNodeValue::MemoryCopy { source, dest, size } => { /* TODO */ }
        LinearNodeValue::TopOfStack => {}
        LinearNodeValue::Discard => {
            encoded.instructions.push(Instruction::Drop);
        }
        LinearNodeValue::Call(lhs, args) => {
            for arg in args.iter() {
                encode_node(encoded, arg);
            }
            match &lhs.value {
                LinearNodeValue::FunctionID(fn_id) => {
                    let fn_idx = encoded.function_id_to_idx[fn_id];
                    encoded.instructions.push(Instruction::Call(fn_idx));
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
                encode_node(encoded, value);
            }
            encoded.instructions.push(Instruction::Return);
        }
        LinearNodeValue::If(cond, then_branch, else_branch) => {
            encode_node(encoded, cond);
            // TODO: BlockType for branches
            encoded.instructions.push(Instruction::If(BlockType::Empty));
            for node in then_branch.iter() {
                encode_node(encoded, node);
            }
            if let Some(else_branch) = else_branch {
                encoded.instructions.push(Instruction::Else);
                for node in else_branch.iter() {
                    encode_node(encoded, node);
                }
            } else {
                encoded.instructions.push(Instruction::End);
            }
        }
        LinearNodeValue::Loop(inner) => {
            encoded
                .instructions
                .push(Instruction::Loop(BlockType::Empty));
            for node in inner.iter() {
                encode_node(encoded, node);
            }
            encoded.instructions.push(Instruction::End);
        }
        LinearNodeValue::Break => {
            // TODO: break loops
        }
        LinearNodeValue::Abort => {
            encoded.instructions.push(Instruction::Unreachable);
        }
        LinearNodeValue::Goto(_) => { /* TODO */ }
        LinearNodeValue::GotoLabel(_) => { /* TODO */ }
        LinearNodeValue::Sequence(nodes) => {
            for node in nodes.iter() {
                encode_node(encoded, node);
            }
        }
        LinearNodeValue::WriteRegister(_, _) => { /* TODO */ }
        LinearNodeValue::ReadRegister(_) => { /* TODO */ }
        LinearNodeValue::KillRegister(_) => { /* TODO */ }
        LinearNodeValue::Arithmetic(operator, prim, lhs, rhs) => {
            encode_node(encoded, lhs);
            encode_node(encoded, rhs);
            match (operator, prim) {
                (ArithmeticOp::Add, PhysicalPrimitive::Byte)
                | (ArithmeticOp::Add, PhysicalPrimitive::PointerSize)
                | (ArithmeticOp::Add, PhysicalPrimitive::Int32) => {
                    encoded.instructions.push(Instruction::I32Add);
                }
                (ArithmeticOp::Add, PhysicalPrimitive::Float32) => {
                    encoded.instructions.push(Instruction::F32Add);
                }
                (ArithmeticOp::Add, PhysicalPrimitive::Int64) => {
                    encoded.instructions.push(Instruction::I64Add);
                }
                (ArithmeticOp::Add, PhysicalPrimitive::Float64) => {
                    encoded.instructions.push(Instruction::F64Add);
                }
                (ArithmeticOp::Subtract, PhysicalPrimitive::Byte)
                | (ArithmeticOp::Subtract, PhysicalPrimitive::PointerSize)
                | (ArithmeticOp::Subtract, PhysicalPrimitive::Int32) => {
                    encoded.instructions.push(Instruction::I32Sub);
                }
                (ArithmeticOp::Subtract, PhysicalPrimitive::Float32) => {
                    encoded.instructions.push(Instruction::F32Sub);
                }
                (ArithmeticOp::Subtract, PhysicalPrimitive::Int64) => {
                    encoded.instructions.push(Instruction::I64Sub);
                }
                (ArithmeticOp::Subtract, PhysicalPrimitive::Float64) => {
                    encoded.instructions.push(Instruction::F64Sub);
                }
                (ArithmeticOp::Multiply, PhysicalPrimitive::Byte)
                | (ArithmeticOp::Multiply, PhysicalPrimitive::PointerSize)
                | (ArithmeticOp::Multiply, PhysicalPrimitive::Int32) => {
                    encoded.instructions.push(Instruction::I32Mul);
                }
                (ArithmeticOp::Multiply, PhysicalPrimitive::Float32) => {
                    encoded.instructions.push(Instruction::F32Mul);
                }
                (ArithmeticOp::Multiply, PhysicalPrimitive::Int64) => {
                    encoded.instructions.push(Instruction::I64Mul);
                }
                (ArithmeticOp::Multiply, PhysicalPrimitive::Float64) => {
                    encoded.instructions.push(Instruction::F64Mul);
                }
                (ArithmeticOp::Divide, PhysicalPrimitive::Byte)
                | (ArithmeticOp::Divide, PhysicalPrimitive::PointerSize)
                | (ArithmeticOp::Divide, PhysicalPrimitive::Int32) => {
                    encoded.instructions.push(Instruction::I32DivS);
                }
                (ArithmeticOp::Divide, PhysicalPrimitive::Float32) => {
                    encoded.instructions.push(Instruction::F32Div);
                }
                (ArithmeticOp::Divide, PhysicalPrimitive::Int64) => {
                    encoded.instructions.push(Instruction::I64DivS);
                }
                (ArithmeticOp::Divide, PhysicalPrimitive::Float64) => {
                    encoded.instructions.push(Instruction::F64Div);
                }
            }
        }
        LinearNodeValue::Comparison(operator, prim, lhs, rhs) => {
            encode_node(encoded, lhs);
            encode_node(encoded, rhs);
            match (operator, prim) {
                (ComparisonOp::LessThan, PhysicalPrimitive::Byte) => {
                    encoded.instructions.push(Instruction::I32LtS);
                }
                (ComparisonOp::LessThan, PhysicalPrimitive::Int32) => {
                    encoded.instructions.push(Instruction::I32LtS);
                }
                (ComparisonOp::LessThan, PhysicalPrimitive::Float32) => {
                    encoded.instructions.push(Instruction::F32Lt);
                }
                (ComparisonOp::LessThan, PhysicalPrimitive::Int64) => {
                    encoded.instructions.push(Instruction::I64LtS);
                }
                (ComparisonOp::LessThan, PhysicalPrimitive::Float64) => {
                    encoded.instructions.push(Instruction::F64Lt);
                }
                (ComparisonOp::LessThan, PhysicalPrimitive::PointerSize) => {
                    encoded.instructions.push(Instruction::I32LtS);
                }
                (ComparisonOp::GreaterThan, PhysicalPrimitive::Byte) => {
                    encoded.instructions.push(Instruction::I32GtS);
                }
                (ComparisonOp::GreaterThan, PhysicalPrimitive::Int32) => {
                    encoded.instructions.push(Instruction::I32GtS);
                }
                (ComparisonOp::GreaterThan, PhysicalPrimitive::Float32) => {
                    encoded.instructions.push(Instruction::F32Gt);
                }
                (ComparisonOp::GreaterThan, PhysicalPrimitive::Int64) => {
                    encoded.instructions.push(Instruction::I64GtS);
                }
                (ComparisonOp::GreaterThan, PhysicalPrimitive::Float64) => {
                    encoded.instructions.push(Instruction::F64Gt);
                }
                (ComparisonOp::GreaterThan, PhysicalPrimitive::PointerSize) => {
                    encoded.instructions.push(Instruction::I32GtS);
                }
                (ComparisonOp::LessEqualThan, PhysicalPrimitive::Byte) => {
                    encoded.instructions.push(Instruction::I32LeS);
                }
                (ComparisonOp::LessEqualThan, PhysicalPrimitive::Int32) => {
                    encoded.instructions.push(Instruction::I32LeS);
                }
                (ComparisonOp::LessEqualThan, PhysicalPrimitive::Float32) => {
                    encoded.instructions.push(Instruction::F32Le);
                }
                (ComparisonOp::LessEqualThan, PhysicalPrimitive::Int64) => {
                    encoded.instructions.push(Instruction::I64LeS);
                }
                (ComparisonOp::LessEqualThan, PhysicalPrimitive::Float64) => {
                    encoded.instructions.push(Instruction::F64Le);
                }
                (ComparisonOp::LessEqualThan, PhysicalPrimitive::PointerSize) => {
                    encoded.instructions.push(Instruction::I32LeS);
                }
                (ComparisonOp::GreaterEqualThan, PhysicalPrimitive::Byte) => {
                    encoded.instructions.push(Instruction::I32GeS);
                }
                (ComparisonOp::GreaterEqualThan, PhysicalPrimitive::Int32) => {
                    encoded.instructions.push(Instruction::I32GeS);
                }
                (ComparisonOp::GreaterEqualThan, PhysicalPrimitive::Float32) => {
                    encoded.instructions.push(Instruction::F32Ge);
                }
                (ComparisonOp::GreaterEqualThan, PhysicalPrimitive::Int64) => {
                    encoded.instructions.push(Instruction::I64GeS);
                }
                (ComparisonOp::GreaterEqualThan, PhysicalPrimitive::Float64) => {
                    encoded.instructions.push(Instruction::F64Ge);
                }
                (ComparisonOp::GreaterEqualThan, PhysicalPrimitive::PointerSize) => {
                    encoded.instructions.push(Instruction::I32GeS);
                }
                (ComparisonOp::EqualTo, PhysicalPrimitive::Byte) => {
                    encoded.instructions.push(Instruction::I32Eq);
                }
                (ComparisonOp::EqualTo, PhysicalPrimitive::Int32) => {
                    encoded.instructions.push(Instruction::I32Eq);
                }
                (ComparisonOp::EqualTo, PhysicalPrimitive::Float32) => {
                    encoded.instructions.push(Instruction::F32Eq);
                }
                (ComparisonOp::EqualTo, PhysicalPrimitive::Int64) => {
                    encoded.instructions.push(Instruction::I64Eq);
                }
                (ComparisonOp::EqualTo, PhysicalPrimitive::Float64) => {
                    encoded.instructions.push(Instruction::F64Eq);
                }
                (ComparisonOp::EqualTo, PhysicalPrimitive::PointerSize) => {
                    encoded.instructions.push(Instruction::I32Eq);
                }
                (ComparisonOp::NotEquals, PhysicalPrimitive::Byte) => {
                    encoded.instructions.push(Instruction::I32Ne);
                }
                (ComparisonOp::NotEquals, PhysicalPrimitive::Int32) => {
                    encoded.instructions.push(Instruction::I32Ne);
                }
                (ComparisonOp::NotEquals, PhysicalPrimitive::Float32) => {
                    encoded.instructions.push(Instruction::F32Ne);
                }
                (ComparisonOp::NotEquals, PhysicalPrimitive::Int64) => {
                    encoded.instructions.push(Instruction::I64Ne);
                }
                (ComparisonOp::NotEquals, PhysicalPrimitive::Float64) => {
                    encoded.instructions.push(Instruction::F64Ne);
                }
                (ComparisonOp::NotEquals, PhysicalPrimitive::PointerSize) => {
                    encoded.instructions.push(Instruction::I32Ne);
                }
            }
        }
        LinearNodeValue::BinaryLogical(op, lhs, rhs) => {
            encode_node(encoded, lhs);
            encode_node(encoded, rhs);
            match op {
                BinaryLogicalOp::BooleanAnd => {
                    encoded.instructions.push(Instruction::I32And);
                }
                BinaryLogicalOp::BooleanOr => {
                    encoded.instructions.push(Instruction::I32Or);
                }
            }
        }
        LinearNodeValue::UnaryLogical(op, value) => {
            encode_node(encoded, value);
            match op {
                UnaryLogicalOp::BooleanNot => {
                    encoded.instructions.push(Instruction::I32Const(1));
                    encoded.instructions.push(Instruction::I32Xor);
                }
            }
        }
        LinearNodeValue::Cast { value, from, to } => {
            encode_node(encoded, value);
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
            encoded
                .instructions
                .push(Instruction::I32Const(*value as i32));
        }
        LinearNodeValue::Int(value) => {
            // TODO: don't cast
            encoded
                .instructions
                .push(Instruction::I32Const(*value as i32));
        }
        LinearNodeValue::Float(value) => {
            // TODO: don't cast
            encoded
                .instructions
                .push(Instruction::F32Const(*value as f32));
        }
        LinearNodeValue::CharLiteral(value) => {
            encoded
                .instructions
                .push(Instruction::I32Const(*value as i32));
        }
        LinearNodeValue::Byte(value) => {
            encoded
                .instructions
                .push(Instruction::I32Const(*value as i32));
        }
        LinearNodeValue::FunctionID(fn_id) => {
            let fn_idx = encoded.function_id_to_idx[fn_id];
            encoded
                .instructions
                .push(Instruction::I32Const(fn_idx as i32));
        }
        LinearNodeValue::ConstantData(_) => { /* TODO */ }
        LinearNodeValue::Debug(_) => { /* TODO */ }
    }
}
