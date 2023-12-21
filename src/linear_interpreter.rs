use std::{collections::HashMap, fmt::Debug, future::Future, pin::Pin, sync::Arc};

use async_recursion::async_recursion;

use crate::{
    hir::HirBinOp,
    id::ID,
    linear_ir::{
        LinearFunction, LinearNode, LinearNodeValue, TypeLayoutField, TypeLayoutValue,
        TypeMemoryLayout,
    },
    typecheck::{ExpressionType, PrimitiveType},
};

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Null,
    ID(ID),
    Size(usize),
    Int32(i32),
    Int64(i64),
    Float32(f32),
    Float64(f64),
    Bool(bool),
    Char(char),
    String(String),
}

impl Value {
    pub fn to_numeric(&self) -> Option<Numeric> {
        match self {
            Value::Int32(x) => Some(Numeric::Int32(*x)),
            Value::Float32(x) => Some(Numeric::Float32(*x)),
            Value::Int64(x) => Some(Numeric::Int64(*x)),
            Value::Float64(x) => Some(Numeric::Float64(*x)),
            Value::Size(x) => Some(Numeric::Size(*x)),
            _ => None,
        }
    }
}

#[derive(Debug)]
pub enum Numeric {
    Int32(i32),
    Float32(f32),
    Int64(i64),
    Float64(f64),
    Size(usize),
}

pub type ExternBinding =
    dyn Fn(Vec<Value>) -> Pin<Box<dyn Future<Output = Option<Value>> + Send>> + Send + Sync;

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
}*/

pub enum Function {
    Ir(LinearFunction),
    Extern(Arc<ExternBinding>),
}

impl Debug for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Function::Ir(inner) => write!(f, "Function::Ir({:?})", inner),
            Function::Extern(_) => write!(f, "Function::Extern(opaque)"),
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

#[derive(Debug)]
pub enum Unwind {
    Return(Value),
    Break,
    Aborted,
}

pub struct VM {
    pub memory: [u8; 1024],
    pub temporaries: [usize; 16],
    pub layouts: HashMap<ID, TypeMemoryLayout>,
    pub op_stack: Vec<Value>,
    pub base_ptr: usize,
    pub stack_ptr: usize,
    pub heap_ptr: usize,
}

impl VM {
    pub fn new(layouts: HashMap<ID, TypeMemoryLayout>) -> VM {
        let memory = [0; 1024];
        VM {
            memory,
            temporaries: [0; 16],
            layouts,
            op_stack: Vec::new(),
            base_ptr: memory.len(),
            stack_ptr: memory.len(),
            heap_ptr: std::mem::size_of::<usize>(),
        }
    }
}

pub async fn evaluate_function(
    fns: &HashMap<ID, Function>,
    params: &mut [Value],
    vm: &mut VM,
    fn_id: ID,
) -> Result<(), Unwind> {
    let function = fns.get(&fn_id).unwrap();
    match function {
        Function::Ir(function) => {
            // Write the current base ptr at the stack ptr location
            let base_ptr = vm.base_ptr.to_le_bytes();
            vm.memory[(vm.stack_ptr - base_ptr.len())..vm.stack_ptr].copy_from_slice(&base_ptr);
            vm.base_ptr = vm.stack_ptr;
            vm.stack_ptr -= base_ptr.len();

            for node in function.body.iter() {
                let result = evaluate_block(fns, params, vm, node).await;
                if let Err(Unwind::Return(val)) = result {
                    vm.op_stack.push(val);
                    break;
                } else {
                    result?;
                }
            }

            let base_ptr = &vm.memory[(vm.base_ptr - base_ptr.len())..vm.base_ptr];
            let base_ptr = usize::from_le_bytes(base_ptr.try_into().unwrap());
            vm.stack_ptr = vm.base_ptr;
            vm.base_ptr = base_ptr;

            Ok(())
        }
        Function::Extern(ext) => {
            if let Some(returned) = ext(params.to_vec()).await {
                vm.op_stack.push(returned);
            }
            Ok(())
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
            let mut temp = [0; 16];
            std::mem::swap(&mut vm.temporaries, &mut temp);
            for node in seq.iter() {
                evaluate_block(fns, params, vm, node).await?;
            }
            std::mem::swap(&mut vm.temporaries, &mut temp);
        }
        LinearNodeValue::StackFrame => {
            vm.op_stack.push(Value::Size(vm.base_ptr));
        }
        LinearNodeValue::StackAlloc(amount) => {
            vm.stack_ptr -= amount;
        }
        LinearNodeValue::HeapAlloc(amount) => {
            evaluate_block(fns, params, vm, amount).await?;
            let Some(Value::Size(amount)) = vm.op_stack.pop() else {
                unreachable!()
            };
            vm.op_stack.push(Value::Size(vm.heap_ptr));
            vm.heap_ptr += amount;
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
            read(&mut vm.op_stack, &vm.layouts, &vm.memory, location, ty);
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
            let Some(Value::ID(fn_id)) = vm.op_stack.pop() else {
                unreachable!()
            };
            for param in parameters.iter().rev() {
                evaluate_block(fns, params, vm, param).await?;
            }
            let mut parameters: Vec<_> = (0..parameters.len())
                .map(|_| vm.op_stack.pop().unwrap())
                .collect();

            evaluate_function(fns, &mut parameters[..], vm, fn_id).await?;
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
        LinearNodeValue::WriteTemporary(tmp, val) => {
            evaluate_block(fns, params, vm, val).await?;
            let Value::Size(val) = vm.op_stack.pop().unwrap() else {
                unreachable!()
            };
            vm.temporaries[*tmp as usize] = val;
        }
        LinearNodeValue::ReadTemporary(tmp) => {
            vm.op_stack.push(Value::Size(vm.temporaries[*tmp as usize]));
        }
        LinearNodeValue::Discard => {
            vm.op_stack.pop().unwrap();
        }
        LinearNodeValue::TopOfStack => {}
        LinearNodeValue::Abort => {
            return Err(Unwind::Aborted);
        }
        LinearNodeValue::Cast { value, from: _, to } => {
            evaluate_block(fns, params, vm, value).await?;
            let val = vm.op_stack.pop().unwrap();
            vm.op_stack.push(match val {
                Value::Null | Value::String(_) | Value::ID(_) | Value::Bool(_) | Value::Char(_) => {
                    unreachable!()
                }
                Value::Size(_) => todo!(),
                Value::Int32(val) => match to {
                    PrimitiveType::Int32 => Value::Int32(val),
                    PrimitiveType::Int64 => Value::Int64(val as i64),
                    PrimitiveType::Float32 => Value::Float32(val as f32),
                    PrimitiveType::Float64 => Value::Float64(val as f64),
                    PrimitiveType::PointerSize => Value::Size(val as usize),
                    PrimitiveType::String
                    | PrimitiveType::Bool
                    | PrimitiveType::FunctionID
                    | PrimitiveType::Char => {
                        unreachable!()
                    }
                },
                Value::Int64(val) => match to {
                    PrimitiveType::Int32 => Value::Int32(val as i32),
                    PrimitiveType::Int64 => Value::Int64(val),
                    PrimitiveType::Float32 => Value::Float32(val as f32),
                    PrimitiveType::Float64 => Value::Float64(val as f64),
                    PrimitiveType::PointerSize => Value::Size(val as usize),
                    PrimitiveType::String
                    | PrimitiveType::Bool
                    | PrimitiveType::FunctionID
                    | PrimitiveType::Char => {
                        unreachable!()
                    }
                },
                Value::Float32(val) => match to {
                    PrimitiveType::Int32 => Value::Int32(val as i32),
                    PrimitiveType::Int64 => Value::Int64(val as i64),
                    PrimitiveType::Float32 => Value::Float32(val),
                    PrimitiveType::Float64 => Value::Float64(val as f64),
                    PrimitiveType::PointerSize => Value::Size(val as usize),
                    PrimitiveType::String
                    | PrimitiveType::Bool
                    | PrimitiveType::FunctionID
                    | PrimitiveType::Char => {
                        unreachable!()
                    }
                },
                Value::Float64(val) => match to {
                    PrimitiveType::Int32 => Value::Int32(val as i32),
                    PrimitiveType::Int64 => Value::Int64(val as i64),
                    PrimitiveType::Float32 => Value::Float32(val as f32),
                    PrimitiveType::Float64 => Value::Float64(val),
                    PrimitiveType::PointerSize => Value::Size(val as usize),
                    PrimitiveType::String
                    | PrimitiveType::Bool
                    | PrimitiveType::FunctionID
                    | PrimitiveType::Char => {
                        unreachable!()
                    }
                },
            });
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
                        TypeLayoutField::Referenced(id) => write(
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
                println!("I'm an interface");
                let mut offset = write_primitive(op_stack, memory, location);
                for _field in fields.iter() {
                    offset += write_primitive(op_stack, memory, location + offset);
                }
            }
            TypeLayoutValue::FunctionPointer => todo!(),
        },
        ExpressionType::Array(_) => {
            write_primitive(op_stack, memory, location);
            write_primitive(op_stack, memory, location + 8);
            write_primitive(op_stack, memory, location + 16);
        }
        ExpressionType::Pointer(_, _) => {
            write_primitive(op_stack, memory, location);
        }
        ExpressionType::Null => todo!(),
        ExpressionType::Nullable(_) => todo!(),
    }
}

fn write_primitive(op_stack: &mut Vec<Value>, memory: &mut [u8], location: usize) -> usize {
    let value = op_stack.pop().unwrap();
    let bytes = match &value {
        Value::Null => todo!(),
        Value::ID(id) => bytemuck::bytes_of(id),
        Value::Size(x) => {
            // lifetime issues
            let bytes = x.to_le_bytes();
            memory[location..(location + bytes.len())].copy_from_slice(&bytes);
            return bytes.len();
        }
        Value::Int32(x) => bytemuck::bytes_of(x),
        Value::Int64(x) => bytemuck::bytes_of(x),
        Value::Float32(x) => bytemuck::bytes_of(x),
        Value::Float64(x) => bytemuck::bytes_of(x),
        Value::Bool(x) => bytemuck::bytes_of(x),
        Value::Char(x) => bytemuck::bytes_of(x),
        Value::String(_) => todo!(),
    };
    memory[location..(location + bytes.len())].copy_from_slice(bytes);
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
        ExpressionType::DeclaredType(id) => {
            let layout = layouts.get(id).unwrap();
            match &layout.value {
                TypeLayoutValue::Structure(fields) => {
                    println!("I'm a struct");
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
                    println!("I'm an interface");
                    let mut location = location + layout.size;
                    for _ in fields.iter().rev() {
                        read_primitive(op_stack, memory, location, PrimitiveType::FunctionID);
                        location -= 4;
                    }
                    read_primitive(op_stack, memory, location, PrimitiveType::PointerSize);
                }
                TypeLayoutValue::FunctionPointer => {
                    let fn_id: ID = *bytemuck::from_bytes(&memory[location..(location + 4)]);
                    op_stack.push(Value::ID(fn_id));
                }
            }
        }
        ExpressionType::Array(_) => {
            read_primitive(op_stack, memory, location + 16, PrimitiveType::PointerSize);
            read_primitive(op_stack, memory, location + 8, PrimitiveType::PointerSize);
            read_primitive(op_stack, memory, location, PrimitiveType::PointerSize);
        }
        ExpressionType::Pointer(_, _) => {
            read_primitive(op_stack, memory, location, PrimitiveType::PointerSize);
        }
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
            let base_ptr = &memory[location..(location + 8)];
            let base_ptr = usize::from_le_bytes(base_ptr.try_into().unwrap());
            Value::Size(base_ptr)
        }
        PrimitiveType::FunctionID => {
            Value::ID(*bytemuck::from_bytes(&memory[location..(location + 4)]))
        }
    });
}
