use std::collections::HashMap;

use async_recursion::async_recursion;

use crate::{
    hir::HirBinOp,
    id::ID,
    interpreter::Numeric,
    linear_ir::{
        LinearFunction, LinearNode, LinearNodeValue, TypeLayoutField, TypeLayoutValue,
        TypeMemoryLayout,
    },
    typecheck::{ExpressionType, PrimitiveType},
    Value,
};

/*#[derive(Clone, Debug)]
pub enum Value {
    Null,
    Int(i64),
    Float(f64),
    Bool(bool),
    Char(char),
    String(String),
    Array(Vec<Value>),
    Struct(HashMap<String, Value>),
}

impl Value {
    fn to_numeric(&self) -> Option<Numeric> {
        match self {
            Value::Int(x) => Some(Numeric::Int(*x)),
            Value::Float(x) => Some(Numeric::Float(*x)),
            _ => None,
        }
    }
}

pub type ExternBinding =
    dyn Fn(Vec<Value>) -> Pin<Box<dyn Future<Output = Option<Value>> + Send>> + Send + Sync;*/

pub enum Function {
    Ir(LinearFunction),
    //Extern(Arc<ExternBinding>),
}

impl std::fmt::Debug for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Function::Ir(inner) => write!(f, "Function::Ir({:?})", inner),
            //Function::Extern(_) => write!(f, "Function::Extern(opaque)"),
        }
    }
}

/*pub async fn evaluate_function(
    fns: &HashMap<ID, Function>,
    function: &Function,
    params: Vec<Value>,
) -> Option<Value> {
    match function {
        Function::Ir(func) => {
            let mut ctx = Context::new(params);
            ctx.add_fns(fns);
            match evaluate_node(fns, &mut ctx, &func.body).await {
                Ok(_) => ctx.value_stack.pop(),
                Err(Unwind::Returned(val)) => Some(val),
            }
        }
        Function::Extern(ext) => ext(params).await,
    }
}*/

pub enum Unwind {
    Return(Value),
    Break,
}

pub struct VM {
    pub memory: [u8; 1024],
    pub layouts: HashMap<ID, TypeMemoryLayout>,
    pub op_stack: Vec<Value>,
    pub base_ptr: usize,
    pub stack_ptr: usize,
}

impl VM {
    pub fn new(layouts: HashMap<ID, TypeMemoryLayout>) -> VM {
        let memory = [0; 1024];
        VM {
            memory,
            layouts,
            op_stack: Vec::new(),
            base_ptr: memory.len(),
            stack_ptr: memory.len(),
        }
    }
}

pub async fn evaluate_function(
    fns: &HashMap<ID, Function>,
    params: &mut [Value],
    vm: &mut VM,
    fn_id: ID,
) {
    let function = fns.get(&fn_id).unwrap();
    match function {
        Function::Ir(function) => {
            // Write the current base ptr at the stack ptr location
            let base_ptr = vm.base_ptr.to_le_bytes();
            (&mut vm.memory[(vm.stack_ptr - base_ptr.len())..vm.stack_ptr])
                .copy_from_slice(&base_ptr);
            vm.base_ptr = vm.stack_ptr;
            vm.stack_ptr -= base_ptr.len();

            for node in function.body.iter() {
                let result = evaluate_block(fns, params, vm, node).await;
                if let Err(Unwind::Return(val)) = result {
                    vm.op_stack.push(val);
                    break;
                }
            }

            let base_ptr = &vm.memory[(vm.base_ptr - base_ptr.len())..vm.base_ptr];
            let base_ptr = usize::from_le_bytes(base_ptr.try_into().unwrap());
            vm.stack_ptr = vm.base_ptr;
            vm.base_ptr = base_ptr;
        }
    }
}

// Kinda a hack: when we return, unwind the stack via Result
#[async_recursion]
pub async fn evaluate_block(
    fns: &HashMap<ID, Function>,
    params: &mut [Value],
    vm: &mut VM,
    node: &LinearNode,
) -> Result<(), Unwind> {
    match &node.value {
        LinearNodeValue::Sequence(seq) => {
            for node in seq.iter() {
                evaluate_block(fns, params, vm, node).await?;
            }
        }
        LinearNodeValue::StackFrame => {
            vm.op_stack.push(Value::Size(vm.base_ptr));
        }
        LinearNodeValue::StackAlloc(amount) => {
            vm.stack_ptr -= amount;
        }
        LinearNodeValue::Parameter(idx) => {
            let mut temp = Value::Null;
            std::mem::swap(&mut temp, &mut params[*idx]);
            vm.op_stack.push(temp);
        }
        LinearNodeValue::ReadMemory {
            location,
            offset,
            ty,
        } => {
            evaluate_block(fns, params, vm, location).await?;
            let Some(Value::Size(mut location)) = vm.op_stack.pop() else {
                unreachable!()
            };
            location += offset;
            read(&mut vm.op_stack, &vm.layouts, &vm.memory, location, &ty);
        }
        LinearNodeValue::WriteMemory {
            location,
            offset,
            value,
            ty,
        } => {
            evaluate_block(fns, params, vm, value).await?;
            evaluate_block(fns, params, vm, location).await?;
            let Some(Value::Size(mut location)) = vm.op_stack.pop() else {
                unreachable!()
            };
            location += offset;
            write(&mut vm.op_stack, &vm.layouts, &mut vm.memory, location, ty);
        }
        LinearNodeValue::Call(lhs, parameters) => {
            evaluate_block(fns, params, vm, lhs).await?;
            // TODO: should I figure out
            let Some(Value::ID(fn_id)) = dbg!(vm.op_stack.pop()) else { unreachable!() };
            for param in parameters.iter().rev() {
                evaluate_block(fns, params, vm, param).await?;
            }
            let mut parameters: Vec<_> = (0..parameters.len())
                .map(|_| vm.op_stack.pop().unwrap())
                .collect();

            evaluate_function(fns, &mut parameters[..], vm, fn_id).await;
        }
        LinearNodeValue::Return(expr) => {
            evaluate_block(fns, params, vm, expr).await?;
            let val = vm.op_stack.pop().unwrap();
            return Err(Unwind::Return(val));
        }
        LinearNodeValue::If(cond, if_branch, else_branch) => {
            evaluate_block(fns, params, vm, cond).await?;
            let Some(Value::Bool(cond)) = vm.op_stack.pop() else {
                unreachable!()
            };
            if cond {
                for node in if_branch.iter() {
                    evaluate_block(fns, params, vm, node).await?;
                }
            } else if let Some(else_branch) = else_branch {
                for node in else_branch.iter() {
                    evaluate_block(fns, params, vm, node).await?;
                }
            }
        }
        LinearNodeValue::Break => return Err(Unwind::Break),
        LinearNodeValue::Loop(inner) => 'outer: loop {
            for node in inner.iter() {
                match evaluate_block(fns, params, vm, node).await {
                    Err(Unwind::Break) => break 'outer,
                    other @ Err(_) => return other,
                    Ok(_) => {}
                }
            }
        },
        LinearNodeValue::BinOp(op, _ty, lhs, rhs) => {
            evaluate_block(fns, params, vm, rhs).await?;
            evaluate_block(fns, params, vm, lhs).await?;
            let left = vm.op_stack.pop().unwrap().to_numeric().unwrap();
            let right = vm.op_stack.pop().unwrap().to_numeric().unwrap();
            let val = match (left, right) {
                (Numeric::Int32(left), Numeric::Int32(right)) => match op {
                    HirBinOp::Add => Value::Int32(left + right),
                    HirBinOp::Subtract => Value::Int32(left - right),
                    HirBinOp::Multiply => Value::Int32(left * right),
                    HirBinOp::Divide => Value::Int32(left / right),
                    HirBinOp::LessThan => Value::Bool(left < right),
                    HirBinOp::GreaterThan => Value::Bool(left > right),
                    HirBinOp::LessEqualThan => Value::Bool(left <= right),
                    HirBinOp::GreaterEqualThan => Value::Bool(left >= right),
                    HirBinOp::EqualTo => Value::Bool(left == right),
                    HirBinOp::NotEquals => Value::Bool(left != right),
                },
                (Numeric::Float32(left), Numeric::Float32(right)) => match op {
                    HirBinOp::Add => Value::Float32(left + right),
                    HirBinOp::Subtract => Value::Float32(left - right),
                    HirBinOp::Multiply => Value::Float32(left * right),
                    HirBinOp::Divide => Value::Float32(left / right),
                    HirBinOp::LessThan => Value::Bool(left < right),
                    HirBinOp::GreaterThan => Value::Bool(left > right),
                    HirBinOp::LessEqualThan => Value::Bool(left <= right),
                    HirBinOp::GreaterEqualThan => Value::Bool(left >= right),
                    HirBinOp::EqualTo => Value::Bool(left == right),
                    HirBinOp::NotEquals => Value::Bool(left != right),
                },
                (Numeric::Int64(left), Numeric::Int64(right)) => match op {
                    HirBinOp::Add => Value::Int64(left + right),
                    HirBinOp::Subtract => Value::Int64(left - right),
                    HirBinOp::Multiply => Value::Int64(left * right),
                    HirBinOp::Divide => Value::Int64(left / right),
                    HirBinOp::LessThan => Value::Bool(left < right),
                    HirBinOp::GreaterThan => Value::Bool(left > right),
                    HirBinOp::LessEqualThan => Value::Bool(left <= right),
                    HirBinOp::GreaterEqualThan => Value::Bool(left >= right),
                    HirBinOp::EqualTo => Value::Bool(left == right),
                    HirBinOp::NotEquals => Value::Bool(left != right),
                },
                (Numeric::Float64(left), Numeric::Float64(right)) => match op {
                    HirBinOp::Add => Value::Float64(left + right),
                    HirBinOp::Subtract => Value::Float64(left - right),
                    HirBinOp::Multiply => Value::Float64(left * right),
                    HirBinOp::Divide => Value::Float64(left / right),
                    HirBinOp::LessThan => Value::Bool(left < right),
                    HirBinOp::GreaterThan => Value::Bool(left > right),
                    HirBinOp::LessEqualThan => Value::Bool(left <= right),
                    HirBinOp::GreaterEqualThan => Value::Bool(left >= right),
                    HirBinOp::EqualTo => Value::Bool(left == right),
                    HirBinOp::NotEquals => Value::Bool(left != right),
                },
                (Numeric::Size(left), Numeric::Size(right)) => match op {
                    HirBinOp::Add => Value::Size(left + right),
                    HirBinOp::Subtract => Value::Size(left - right),
                    HirBinOp::Multiply => Value::Size(left * right),
                    HirBinOp::Divide => Value::Size(left / right),
                    HirBinOp::LessThan => Value::Bool(left < right),
                    HirBinOp::GreaterThan => Value::Bool(left > right),
                    HirBinOp::LessEqualThan => Value::Bool(left <= right),
                    HirBinOp::GreaterEqualThan => Value::Bool(left >= right),
                    HirBinOp::EqualTo => Value::Bool(left == right),
                    HirBinOp::NotEquals => Value::Bool(left != right),
                },
                (_, _) => unreachable!(),
            };
            vm.op_stack.push(val);
        }
        LinearNodeValue::Size(size) => {
            vm.op_stack.push(Value::Size(*size));
        }
        LinearNodeValue::Int(x) => {
            vm.op_stack.push(Value::Int32(*x as i32));
        }
        LinearNodeValue::Float(x) => {
            vm.op_stack.push(Value::Float32(*x as f32));
        }
        LinearNodeValue::Bool(x) => {
            vm.op_stack.push(Value::Bool(*x));
        }
        LinearNodeValue::Null => {
            vm.op_stack.push(Value::Null);
        }
        LinearNodeValue::CharLiteral(x) => {
            vm.op_stack.push(Value::Char(*x));
        }
        LinearNodeValue::StringLiteral(x) => {
            vm.op_stack.push(Value::String(x.clone()));
        }
        LinearNodeValue::FunctionID(x) => {
            vm.op_stack.push(Value::ID(*x));
        }
    }

    Ok(())
}

fn write(
    op_stack: &mut Vec<Value>,
    layouts: &HashMap<ID, TypeMemoryLayout>,
    memory: &mut [u8],
    location: usize,
    ty: &ExpressionType,
) {
    match ty {
        ExpressionType::Void => unreachable!(),
        ExpressionType::Primitive(_) => {
            write_primitive(op_stack, memory, location);
        }
        ExpressionType::DeclaredType(id) => match &layouts.get(id).unwrap().value {
            TypeLayoutValue::Structure(fields) => {
                for (_, offset, ty) in fields.iter() {
                    let location = location + offset;
                    match ty {
                        TypeLayoutField::Primitive(_) => {
                            write_primitive(op_stack, memory, location);
                        }
                        TypeLayoutField::Referenced(id) => write(op_stack, layouts, memory, location, &ExpressionType::DeclaredType(*id)),
                        TypeLayoutField::Nullable(_) => todo!(),
                        TypeLayoutField::Pointer => todo!(),
                    }
                }
            }
            TypeLayoutValue::Interface(fields) => {
                dbg!(op_stack.last().unwrap());
                let mut offset = write_primitive(op_stack, memory, location);
                dbg!(offset);
                for _field in fields.iter() {
                    dbg!(op_stack.last().unwrap());
                    offset += write_primitive(op_stack, memory, location + offset);
                }
            }
            TypeLayoutValue::FunctionPointer => todo!(),
        }
        ExpressionType::Pointer(_, _) => todo!(),
        ExpressionType::Array(_) => todo!(),
        ExpressionType::Null => todo!(),
        ExpressionType::Nullable(_) => todo!(),
    }
}

fn write_primitive(
    op_stack: &mut Vec<Value>,
    memory: &mut [u8],
    location: usize,
) -> usize {
    let value = op_stack.pop().unwrap();
    let bytes = match &value {
        Value::Null => todo!(),
        Value::ID(id) => bytemuck::bytes_of(id),
        Value::Size(x) => bytemuck::bytes_of(x),
        Value::Int32(x) => bytemuck::bytes_of(x),
        Value::Int64(x) => bytemuck::bytes_of(x),
        Value::Float32(x) => bytemuck::bytes_of(x),
        Value::Float64(x) => bytemuck::bytes_of(x),
        Value::Bool(x) => bytemuck::bytes_of(x),
        Value::Char(x) => bytemuck::bytes_of(x),
        Value::String(_) => todo!(),
        Value::Array(_) |
        Value::Struct(_) |
        Value::Function(_) |
        Value::Interface(_, _) => unreachable!("only used in the non-linear interpreter"),
    };
    (&mut memory[location..(location + bytes.len())]).copy_from_slice(bytes);
    bytes.len()
}

fn read(
    op_stack: &mut Vec<Value>,
    layouts: &HashMap<ID, TypeMemoryLayout>,
    memory: &[u8],
    location: usize,
    ty: &ExpressionType,
) {
    match ty {
        ExpressionType::Void | ExpressionType::Null => unreachable!("{:?}", ty),
        ExpressionType::Primitive(p) => {
            read_primitive(op_stack, memory, location, *p);
        }
        ExpressionType::DeclaredType(id) => match &layouts.get(id).unwrap().value {
            TypeLayoutValue::Structure(fields) => {
                for (_, offset, ty) in fields.iter().rev() {
                    let location = location + offset;
                    match ty {
                        TypeLayoutField::Primitive(p) => {
                            read_primitive(op_stack, memory, location, *p)
                        }
                        TypeLayoutField::Referenced(id) => read(
                            op_stack,
                            layouts,
                            memory,
                            location,
                            &ExpressionType::DeclaredType(*id),
                        ),
                        TypeLayoutField::Nullable(_) => todo!(),
                        TypeLayoutField::Pointer => todo!(),
                    }
                }
            }
            TypeLayoutValue::Interface(fields) => {
                let mut offset = 0;
                for _ in fields.iter().rev() {
                    read_primitive(op_stack, memory, location + offset, PrimitiveType::FunctionID);
                    dbg!(op_stack.last().unwrap());
                    offset += 4;
                }
                read_primitive(op_stack, memory, location + offset, PrimitiveType::PointerSize);
                dbg!(op_stack.last().unwrap());
            }
            TypeLayoutValue::FunctionPointer => {
                let fn_id: ID = *bytemuck::from_bytes(&memory[location..(location + 4)]);
                op_stack.push(Value::ID(fn_id));
            }
        },
        ExpressionType::Pointer(_, _) => todo!(),
        ExpressionType::Array(_) => todo!(),
        ExpressionType::Nullable(_) => todo!(),
    }
}

fn read_primitive(
    op_stack: &mut Vec<Value>,
    memory: &[u8],
    location: usize,
    primitive: PrimitiveType,
) {
    op_stack.push(match primitive {
        PrimitiveType::Char => todo!(), //Value::Char(*bytemuck::from_bytes(memory)),
        PrimitiveType::String => todo!(),
        PrimitiveType::Int32 => {
            Value::Int32(*bytemuck::from_bytes(&memory[location..(location + 4)]))
        }
        PrimitiveType::Float32 => {
            Value::Float32(*bytemuck::from_bytes(&memory[location..(location + 4)]))
        }
        PrimitiveType::Int64 => {
            Value::Int64(*bytemuck::from_bytes(&memory[location..(location + 8)]))
        }
        PrimitiveType::Float64 => {
            Value::Float64(*bytemuck::from_bytes(&memory[location..(location + 8)]))
        }
        PrimitiveType::Bool => todo!(), //Value::Bool(*bytemuck::from_bytes(memory)),
        PrimitiveType::PointerSize => {
            Value::Size(*bytemuck::from_bytes(dbg!(&memory[dbg!(location)..(location + 8)])))
        }
        PrimitiveType::FunctionID => Value::ID(*bytemuck::from_bytes(&memory[location..(location + 4)]))
    });
}
