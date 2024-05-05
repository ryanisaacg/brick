use brick::{ArithmeticOp, LinearFunction, LinearNode, LinearNodeValue, PhysicalPrimitive};
use wasm_encoder::{Function, Instruction};

pub fn encode(func: &LinearFunction) -> Function {
    let mut body = EncodedBody {
        instructions: Vec::new(),
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
}

fn encode_node(encoded: &mut EncodedBody<'_>, node: &LinearNode) {
    match &node.value {
        LinearNodeValue::HeapAlloc(_) => todo!(),
        LinearNodeValue::Parameter(_) => todo!(),
        LinearNodeValue::VariableInit(_, _) => todo!(),
        LinearNodeValue::VariableDestroy(_) => todo!(),
        LinearNodeValue::VariableLocation(_) => todo!(),
        LinearNodeValue::ReadMemory {
            location,
            offset,
            ty,
        } => todo!(),
        LinearNodeValue::WriteMemory {
            location,
            offset,
            ty,
            value,
        } => todo!(),
        LinearNodeValue::MemoryCopy { source, dest, size } => todo!(),
        LinearNodeValue::TopOfStack => todo!(),
        LinearNodeValue::Discard => todo!(),
        LinearNodeValue::Call(_, _) => todo!(),
        LinearNodeValue::RuntimeCall(_, _) => todo!(),
        LinearNodeValue::Return(_) => todo!(),
        LinearNodeValue::If(_, _, _) => todo!(),
        LinearNodeValue::Break => todo!(),
        LinearNodeValue::Loop(_) => todo!(),
        LinearNodeValue::Abort => todo!(),
        LinearNodeValue::Goto(_) => todo!(),
        LinearNodeValue::GotoLabel(_) => todo!(),
        LinearNodeValue::Sequence(_) => todo!(),
        LinearNodeValue::WriteRegister(_, _) => todo!(),
        LinearNodeValue::ReadRegister(_) => todo!(),
        LinearNodeValue::KillRegister(_) => todo!(),
        LinearNodeValue::Arithmetic(operator, prim, lhs, rhs) => {
            encode_node(encoded, lhs);
            encode_node(encoded, rhs);
            match (operator, prim) {
                (ArithmeticOp::Add, PhysicalPrimitive::Byte) => todo!(),
                (ArithmeticOp::Add, PhysicalPrimitive::Int32) => {
                    encoded.instructions.push(Instruction::I32Add);
                }
                (ArithmeticOp::Add, PhysicalPrimitive::Float32) => todo!(),
                (ArithmeticOp::Add, PhysicalPrimitive::Int64) => todo!(),
                (ArithmeticOp::Add, PhysicalPrimitive::Float64) => todo!(),
                (ArithmeticOp::Add, PhysicalPrimitive::PointerSize) => todo!(),
                (ArithmeticOp::Subtract, PhysicalPrimitive::Byte) => todo!(),
                (ArithmeticOp::Subtract, PhysicalPrimitive::Int32) => todo!(),
                (ArithmeticOp::Subtract, PhysicalPrimitive::Float32) => todo!(),
                (ArithmeticOp::Subtract, PhysicalPrimitive::Int64) => todo!(),
                (ArithmeticOp::Subtract, PhysicalPrimitive::Float64) => todo!(),
                (ArithmeticOp::Subtract, PhysicalPrimitive::PointerSize) => todo!(),
                (ArithmeticOp::Multiply, PhysicalPrimitive::Byte) => todo!(),
                (ArithmeticOp::Multiply, PhysicalPrimitive::Int32) => todo!(),
                (ArithmeticOp::Multiply, PhysicalPrimitive::Float32) => todo!(),
                (ArithmeticOp::Multiply, PhysicalPrimitive::Int64) => todo!(),
                (ArithmeticOp::Multiply, PhysicalPrimitive::Float64) => todo!(),
                (ArithmeticOp::Multiply, PhysicalPrimitive::PointerSize) => todo!(),
                (ArithmeticOp::Divide, PhysicalPrimitive::Byte) => todo!(),
                (ArithmeticOp::Divide, PhysicalPrimitive::Int32) => todo!(),
                (ArithmeticOp::Divide, PhysicalPrimitive::Float32) => todo!(),
                (ArithmeticOp::Divide, PhysicalPrimitive::Int64) => todo!(),
                (ArithmeticOp::Divide, PhysicalPrimitive::Float64) => todo!(),
                (ArithmeticOp::Divide, PhysicalPrimitive::PointerSize) => todo!(),
            }
        }
        LinearNodeValue::Comparison(_, _, _, _) => todo!(),
        LinearNodeValue::BinaryLogical(_, _, _) => todo!(),
        LinearNodeValue::UnaryLogical(_, _) => todo!(),
        LinearNodeValue::Cast { value, from, to } => todo!(),
        LinearNodeValue::Size(_) => todo!(),
        LinearNodeValue::Int(value) => {
            // TODO: don't cast
            encoded
                .instructions
                .push(Instruction::I32Const(*value as i32));
        }
        LinearNodeValue::Float(_) => todo!(),
        LinearNodeValue::CharLiteral(_) => todo!(),
        LinearNodeValue::Byte(_) => todo!(),
        LinearNodeValue::FunctionID(_) => todo!(),
        LinearNodeValue::ConstantData(_) => todo!(),
        LinearNodeValue::Debug(_) => todo!(),
    }
}
