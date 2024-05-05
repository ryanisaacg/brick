use brick::LinearFunction;
use wasm_encoder::{Function, Instruction};

pub fn encode(func: &LinearFunction) -> Function {
    let locals = vec![];
    let mut f = Function::new(locals);
    f.instruction(&Instruction::I32Const(2));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::End);

    f
}
