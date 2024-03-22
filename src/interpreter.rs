use std::{collections::HashMap, fmt::Debug, sync::Arc};

use crate::{
    hir::{ArithmeticOp, BinaryLogicalOp, ComparisonOp, UnaryLogicalOp},
    id::{FunctionID, RegisterID, TypeID, VariableID},
    linear_ir::{
        DeclaredTypeLayout, LinearFunction, LinearNode, LinearNodeValue, PhysicalCollection,
        PhysicalPrimitive, PhysicalType, TypeLayoutValue, NULL_TAG_SIZE,
    },
};

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    FunctionID(FunctionID),
    Size(usize),
    Byte(u8),
    Int32(i32),
    Int64(i64),
    Float32(f32),
    Float64(f64),
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

pub type ExternBinding = dyn Fn(&mut [u8], Vec<Value>) -> Option<Value>;

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

#[derive(Debug)]
pub enum Unwind {
    Return(Option<Value>),
    Break,
    Aborted,
}

pub struct VM {
    pub memory: [u8; 1024],
    pub temporaries: HashMap<RegisterID, usize>,
    pub layouts: HashMap<TypeID, DeclaredTypeLayout>,
    pub op_stack: Vec<Value>,
    pub base_ptr: usize,
    pub stack_ptr: usize,
    pub heap_ptr: usize,
    pub in_progress_goto: Option<usize>,
    pub variable_locations: Vec<HashMap<VariableID, (usize, PhysicalType)>>,
}

impl VM {
    pub fn new(layouts: HashMap<TypeID, DeclaredTypeLayout>) -> VM {
        let memory = [0; 1024];
        VM {
            memory,
            temporaries: HashMap::new(),
            layouts,
            op_stack: Vec::new(),
            base_ptr: memory.len(),
            stack_ptr: memory.len(),
            heap_ptr: std::mem::size_of::<usize>(),
            in_progress_goto: None,
            variable_locations: Vec::new(),
        }
    }
}

pub fn evaluate_function(
    fns: &HashMap<FunctionID, Function>,
    params: &mut [Value],
    vm: &mut VM,
    fn_id: FunctionID,
) -> Result<(), Unwind> {
    let function = &fns[&fn_id];
    match function {
        Function::Ir(function) => {
            // Write the current base ptr at the stack ptr location
            let base_ptr = vm.base_ptr.to_le_bytes();
            vm.memory[(vm.stack_ptr - base_ptr.len())..vm.stack_ptr].copy_from_slice(&base_ptr);
            vm.base_ptr = vm.stack_ptr;
            vm.stack_ptr -= base_ptr.len();

            vm.variable_locations.push(HashMap::new());

            for node in function.body.iter() {
                let result = evaluate_node(fns, params, vm, node);
                if let Err(Unwind::Return(val)) = result {
                    if let Some(val) = val {
                        vm.op_stack.push(val);
                    }
                    break;
                } else {
                    result?;
                }
            }

            vm.variable_locations.pop();

            let base_ptr = &vm.memory[(vm.base_ptr - base_ptr.len())..vm.base_ptr];
            let base_ptr = usize::from_le_bytes(base_ptr.try_into().unwrap());
            vm.stack_ptr = vm.base_ptr;
            vm.base_ptr = base_ptr;

            Ok(())
        }
        Function::Extern(ext) => {
            if let Some(returned) = ext(&mut vm.memory[..], params.to_vec()) {
                vm.op_stack.push(returned);
            }
            Ok(())
        }
    }
}

// Kinda a hack: when we return, unwind the stack via Result
pub fn evaluate_node(
    fns: &HashMap<FunctionID, Function>,
    params: &mut [Value],
    vm: &mut VM,
    node: &LinearNode,
) -> Result<(), Unwind> {
    if let Some(target_label) = vm.in_progress_goto {
        match &node.value {
            LinearNodeValue::GotoLabel(current_label) if *current_label == target_label => {
                vm.in_progress_goto = None;
            }
            LinearNodeValue::Sequence(children) | LinearNodeValue::Loop(children) => {
                for node in children.iter() {
                    evaluate_node(fns, params, vm, node)?;
                }
            }
            LinearNodeValue::If(_, if_branch, else_branch) => {
                let mut found_goto = false;
                for node in if_branch.iter() {
                    evaluate_node(fns, params, vm, node)?;
                    if vm.in_progress_goto.is_none() {
                        found_goto = true;
                    }
                }
                if !found_goto {
                    if let Some(else_branch) = else_branch {
                        for node in else_branch.iter() {
                            evaluate_node(fns, params, vm, node)?;
                        }
                    }
                }
            }
            _ => return Ok(()),
        }
    }
    match &node.value {
        LinearNodeValue::Sequence(seq) => {
            for node in seq.iter() {
                evaluate_node(fns, params, vm, node)?;
            }
        }
        LinearNodeValue::VariableInit(var_id, ty) => {
            vm.stack_ptr -= ty.size(&vm.layouts);
            vm.variable_locations
                .last_mut()
                .unwrap()
                .insert(*var_id, (vm.stack_ptr, ty.clone()));
        }
        LinearNodeValue::VariableDestroy(var_id) => {
            let (_location, ty) = vm
                .variable_locations
                .last_mut()
                .unwrap()
                .remove(var_id)
                .unwrap();
            vm.stack_ptr += ty.size(&vm.layouts);
        }
        LinearNodeValue::VariableLocation(var_id) => {
            vm.op_stack.push(Value::Size(
                vm.variable_locations.last().unwrap().get(var_id).unwrap().0,
            ));
        }
        LinearNodeValue::HeapAlloc(amount) => {
            evaluate_node(fns, params, vm, amount)?;
            let Some(Value::Size(amount)) = vm.op_stack.pop() else {
                unreachable!()
            };
            vm.op_stack.push(Value::Size(vm.heap_ptr));
            vm.heap_ptr += amount;
        }
        LinearNodeValue::Parameter(idx) => {
            let mut temp = Value::Byte(0);
            std::mem::swap(&mut temp, &mut params[*idx]);
            vm.op_stack.push(temp);
        }
        LinearNodeValue::ReadMemory {
            location,
            offset,
            ty,
        } => {
            evaluate_node(fns, params, vm, location)?;
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
            evaluate_node(fns, params, vm, value)?;
            evaluate_node(fns, params, vm, location)?;
            let Some(Value::Size(mut location)) = vm.op_stack.pop() else {
                unreachable!()
            };
            location += offset;
            write(&mut vm.op_stack, &vm.layouts, &mut vm.memory, location, ty);
        }
        LinearNodeValue::Call(lhs, parameters) => {
            evaluate_node(fns, params, vm, lhs)?;
            // TODO: should I figure out
            let Some(Value::FunctionID(fn_id)) = vm.op_stack.pop() else {
                unreachable!()
            };
            for param in parameters.iter().rev() {
                evaluate_node(fns, params, vm, param)?;
            }
            let mut parameters: Vec<_> = (0..parameters.len())
                .map(|_| vm.op_stack.pop().unwrap())
                .collect();

            evaluate_function(fns, &mut parameters[..], vm, fn_id)?;
        }
        LinearNodeValue::Return(expr) => {
            if let Some(expr) = expr {
                evaluate_node(fns, params, vm, expr)?;
                // TODO: support wide returns
                let val = vm.op_stack.pop().unwrap();
                return Err(Unwind::Return(Some(val)));
            } else {
                return Err(Unwind::Return(None));
            }
        }
        LinearNodeValue::If(cond, if_branch, else_branch) => {
            evaluate_node(fns, params, vm, cond)?;
            let Some(Value::Byte(cond)) = vm.op_stack.pop() else {
                unreachable!()
            };
            if cond != 0 {
                for node in if_branch.iter() {
                    evaluate_node(fns, params, vm, node)?;
                }
            } else if let Some(else_branch) = else_branch {
                for node in else_branch.iter() {
                    evaluate_node(fns, params, vm, node)?;
                }
            }
        }
        LinearNodeValue::Break => return Err(Unwind::Break),
        LinearNodeValue::Loop(inner) => 'outer: loop {
            for node in inner.iter() {
                match evaluate_node(fns, params, vm, node) {
                    Err(Unwind::Break) => break 'outer,
                    other @ Err(_) => return other,
                    Ok(_) => {}
                }
            }
        },
        LinearNodeValue::UnaryLogical(UnaryLogicalOp::BooleanNot, child) => {
            evaluate_node(fns, params, vm, child)?;
            let Value::Byte(val) = vm.op_stack.pop().unwrap() else {
                unreachable!()
            };
            vm.op_stack.push(bool_value(val == 0));
        }
        LinearNodeValue::Arithmetic(op, _ty, lhs, rhs) => {
            evaluate_node(fns, params, vm, rhs)?;
            evaluate_node(fns, params, vm, lhs)?;
            let left = vm.op_stack.pop().unwrap().to_numeric().unwrap();
            let right = vm.op_stack.pop().unwrap().to_numeric().unwrap();
            let val = match (left, right) {
                (Numeric::Int32(left), Numeric::Int32(right)) => match op {
                    ArithmeticOp::Add => Value::Int32(left + right),
                    ArithmeticOp::Subtract => Value::Int32(left - right),
                    ArithmeticOp::Multiply => Value::Int32(left * right),
                    ArithmeticOp::Divide => Value::Int32(left / right),
                },
                (Numeric::Float32(left), Numeric::Float32(right)) => match op {
                    ArithmeticOp::Add => Value::Float32(left + right),
                    ArithmeticOp::Subtract => Value::Float32(left - right),
                    ArithmeticOp::Multiply => Value::Float32(left * right),
                    ArithmeticOp::Divide => Value::Float32(left / right),
                },
                (Numeric::Int64(left), Numeric::Int64(right)) => match op {
                    ArithmeticOp::Add => Value::Int64(left + right),
                    ArithmeticOp::Subtract => Value::Int64(left - right),
                    ArithmeticOp::Multiply => Value::Int64(left * right),
                    ArithmeticOp::Divide => Value::Int64(left / right),
                },
                (Numeric::Float64(left), Numeric::Float64(right)) => match op {
                    ArithmeticOp::Add => Value::Float64(left + right),
                    ArithmeticOp::Subtract => Value::Float64(left - right),
                    ArithmeticOp::Multiply => Value::Float64(left * right),
                    ArithmeticOp::Divide => Value::Float64(left / right),
                },
                (Numeric::Size(left), Numeric::Size(right)) => match op {
                    ArithmeticOp::Add => Value::Size(left + right),
                    ArithmeticOp::Subtract => Value::Size(left - right),
                    ArithmeticOp::Multiply => Value::Size(left * right),
                    ArithmeticOp::Divide => Value::Size(left / right),
                },
                (_, _) => unreachable!(),
            };
            vm.op_stack.push(val);
        }
        LinearNodeValue::Comparison(
            op @ (ComparisonOp::EqualTo | ComparisonOp::NotEquals),
            _ty,
            lhs,
            rhs,
        ) => {
            evaluate_node(fns, params, vm, rhs)?;
            evaluate_node(fns, params, vm, lhs)?;
            let left = vm.op_stack.pop().unwrap();
            let right = vm.op_stack.pop().unwrap();
            vm.op_stack.push(bool_value(
                (*op == ComparisonOp::EqualTo) == (left == right),
            ));
        }
        LinearNodeValue::Comparison(op, _ty, lhs, rhs) => {
            evaluate_node(fns, params, vm, rhs)?;
            evaluate_node(fns, params, vm, lhs)?;
            let left = vm.op_stack.pop().unwrap().to_numeric().unwrap();
            let right = vm.op_stack.pop().unwrap().to_numeric().unwrap();
            let val = match (left, right) {
                (Numeric::Int32(left), Numeric::Int32(right)) => match op {
                    ComparisonOp::LessThan => bool_value(left < right),
                    ComparisonOp::GreaterThan => bool_value(left > right),
                    ComparisonOp::LessEqualThan => bool_value(left <= right),
                    ComparisonOp::GreaterEqualThan => bool_value(left >= right),
                    ComparisonOp::EqualTo | ComparisonOp::NotEquals => unreachable!(),
                },
                (Numeric::Float32(left), Numeric::Float32(right)) => match op {
                    ComparisonOp::LessThan => bool_value(left < right),
                    ComparisonOp::GreaterThan => bool_value(left > right),
                    ComparisonOp::LessEqualThan => bool_value(left <= right),
                    ComparisonOp::GreaterEqualThan => bool_value(left >= right),
                    ComparisonOp::EqualTo | ComparisonOp::NotEquals => unreachable!(),
                },
                (Numeric::Int64(left), Numeric::Int64(right)) => match op {
                    ComparisonOp::LessThan => bool_value(left < right),
                    ComparisonOp::GreaterThan => bool_value(left > right),
                    ComparisonOp::LessEqualThan => bool_value(left <= right),
                    ComparisonOp::GreaterEqualThan => bool_value(left >= right),
                    ComparisonOp::EqualTo | ComparisonOp::NotEquals => unreachable!(),
                },
                (Numeric::Float64(left), Numeric::Float64(right)) => match op {
                    ComparisonOp::LessThan => bool_value(left < right),
                    ComparisonOp::GreaterThan => bool_value(left > right),
                    ComparisonOp::LessEqualThan => bool_value(left <= right),
                    ComparisonOp::GreaterEqualThan => bool_value(left >= right),
                    ComparisonOp::EqualTo | ComparisonOp::NotEquals => unreachable!(),
                },
                (Numeric::Size(left), Numeric::Size(right)) => match op {
                    ComparisonOp::LessThan => bool_value(left < right),
                    ComparisonOp::GreaterThan => bool_value(left > right),
                    ComparisonOp::LessEqualThan => bool_value(left <= right),
                    ComparisonOp::GreaterEqualThan => bool_value(left >= right),
                    ComparisonOp::EqualTo | ComparisonOp::NotEquals => unreachable!(),
                },
                (_, _) => unreachable!(),
            };
            vm.op_stack.push(val);
        }
        LinearNodeValue::BinaryLogical(op, lhs, rhs) => {
            evaluate_node(fns, params, vm, lhs)?;
            let Value::Byte(left) = vm.op_stack.pop().unwrap() else {
                unreachable!();
            };
            let left = left != 0;
            let result = match op {
                BinaryLogicalOp::BooleanAnd => {
                    left && {
                        evaluate_node(fns, params, vm, rhs)?;
                        let Value::Byte(right) = vm.op_stack.pop().unwrap() else {
                            unreachable!();
                        };
                        right != 0
                    }
                }
                BinaryLogicalOp::BooleanOr => {
                    left || {
                        evaluate_node(fns, params, vm, rhs)?;
                        let Value::Byte(right) = vm.op_stack.pop().unwrap() else {
                            unreachable!();
                        };
                        right != 0
                    }
                }
            };

            vm.op_stack.push(bool_value(result));
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
        LinearNodeValue::Byte(x) => {
            vm.op_stack.push(Value::Byte(*x));
        }
        LinearNodeValue::CharLiteral(x) => {
            // TODO: lossy conversion
            vm.op_stack.push(Value::Byte(*x as u8));
        }
        LinearNodeValue::FunctionID(x) => {
            vm.op_stack.push(Value::FunctionID(*x));
        }
        LinearNodeValue::WriteRegister(tmp, val) => {
            evaluate_node(fns, params, vm, val)?;
            let Value::Size(val) = vm.op_stack.pop().unwrap() else {
                unreachable!()
            };
            vm.temporaries.insert(*tmp, val);
        }
        LinearNodeValue::ReadRegister(tmp) => {
            vm.op_stack.push(Value::Size(
                *vm.temporaries
                    .get(tmp)
                    .expect("temp to be defined before use"),
            ));
        }
        LinearNodeValue::KillRegister(tmp) => {
            vm.temporaries.remove(tmp);
        }
        LinearNodeValue::Discard => {
            vm.op_stack.pop().unwrap();
        }
        LinearNodeValue::TopOfStack => {}
        LinearNodeValue::Abort => {
            return Err(Unwind::Aborted);
        }
        LinearNodeValue::Cast { value, from: _, to } => {
            evaluate_node(fns, params, vm, value)?;
            let val = vm.op_stack.pop().unwrap();
            vm.op_stack.push(match val {
                Value::FunctionID(_) => {
                    unreachable!()
                }
                Value::Size(_) => todo!(),
                Value::Byte(val) => match to {
                    PhysicalPrimitive::Byte => Value::Byte(val),
                    PhysicalPrimitive::Int32 => Value::Int32(val as i32),
                    PhysicalPrimitive::Int64 => Value::Int64(val as i64),
                    PhysicalPrimitive::Float32 => Value::Float32(val as f32),
                    PhysicalPrimitive::Float64 => Value::Float64(val as f64),
                    PhysicalPrimitive::PointerSize => Value::Size(val as usize),
                },
                Value::Int32(val) => match to {
                    PhysicalPrimitive::Byte => Value::Byte(val as u8),
                    PhysicalPrimitive::Int32 => Value::Int32(val),
                    PhysicalPrimitive::Int64 => Value::Int64(val as i64),
                    PhysicalPrimitive::Float32 => Value::Float32(val as f32),
                    PhysicalPrimitive::Float64 => Value::Float64(val as f64),
                    PhysicalPrimitive::PointerSize => Value::Size(val as usize),
                },
                Value::Int64(val) => match to {
                    PhysicalPrimitive::Byte => Value::Byte(val as u8),
                    PhysicalPrimitive::Int32 => Value::Int32(val as i32),
                    PhysicalPrimitive::Int64 => Value::Int64(val),
                    PhysicalPrimitive::Float32 => Value::Float32(val as f32),
                    PhysicalPrimitive::Float64 => Value::Float64(val as f64),
                    PhysicalPrimitive::PointerSize => Value::Size(val as usize),
                },
                Value::Float32(val) => match to {
                    PhysicalPrimitive::Byte => Value::Byte(val as u8),
                    PhysicalPrimitive::Int32 => Value::Int32(val as i32),
                    PhysicalPrimitive::Int64 => Value::Int64(val as i64),
                    PhysicalPrimitive::Float32 => Value::Float32(val),
                    PhysicalPrimitive::Float64 => Value::Float64(val as f64),
                    PhysicalPrimitive::PointerSize => Value::Size(val as usize),
                },
                Value::Float64(val) => match to {
                    PhysicalPrimitive::Byte => Value::Byte(val as u8),
                    PhysicalPrimitive::Int32 => Value::Int32(val as i32),
                    PhysicalPrimitive::Int64 => Value::Int64(val as i64),
                    PhysicalPrimitive::Float32 => Value::Float32(val as f32),
                    PhysicalPrimitive::Float64 => Value::Float64(val),
                    PhysicalPrimitive::PointerSize => Value::Size(val as usize),
                },
            });
        }
        LinearNodeValue::Debug(inner) => {
            evaluate_node(fns, params, vm, inner)?;
            println!("{:?}", vm.op_stack.last().unwrap());
        }
        LinearNodeValue::MemoryCopy { source, dest, size } => {
            evaluate_node(fns, params, vm, source)?;
            let Value::Size(source) = vm.op_stack.pop().unwrap() else {
                unreachable!()
            };
            evaluate_node(fns, params, vm, dest)?;
            let Value::Size(dest) = vm.op_stack.pop().unwrap() else {
                unreachable!()
            };
            evaluate_node(fns, params, vm, size)?;
            let Value::Size(size) = vm.op_stack.pop().unwrap() else {
                unreachable!()
            };
            vm.memory.copy_within(source..source + size, dest);
        }
        LinearNodeValue::GotoLabel(_) => {}
        LinearNodeValue::Goto(label) => {
            evaluate_node(fns, params, vm, label)?;
            let Value::Size(label) = vm.op_stack.pop().unwrap() else {
                unreachable!()
            };
            vm.in_progress_goto = Some(label);
        }
    }

    Ok(())
}

fn bool_value(val: bool) -> Value {
    Value::Byte(if val { 1 } else { 0 })
}

fn write(
    op_stack: &mut Vec<Value>,
    layouts: &HashMap<TypeID, DeclaredTypeLayout>,
    memory: &mut [u8],
    location: usize,
    ty: &PhysicalType,
) {
    match ty {
        PhysicalType::Primitive(_) => {
            write_primitive(op_stack, memory, location);
        }
        PhysicalType::Referenced(id) => match &layouts[id].value {
            TypeLayoutValue::Structure(fields) => {
                for (_, offset, ty) in fields.iter() {
                    let location = location + offset;
                    write(op_stack, layouts, memory, location, ty);
                }
            }
            TypeLayoutValue::Interface(fields) => {
                let mut offset = write_primitive(op_stack, memory, location);
                for _field in fields.iter() {
                    offset += write_primitive(op_stack, memory, location + offset);
                }
            }
            TypeLayoutValue::Union(variants) => {
                let Value::Size(variant) = op_stack.pop().unwrap() else {
                    unreachable!()
                };
                op_stack.push(Value::Size(variant));
                let offset = write_primitive(op_stack, memory, location);
                let variant = variants
                    .values()
                    .find_map(|(id, ty)| if *id == variant { Some(ty) } else { None })
                    .unwrap();
                write(op_stack, layouts, memory, location + offset, variant);
            }
        },
        PhysicalType::Collection(PhysicalCollection::Array | PhysicalCollection::Dict)
        | PhysicalType::Generator => {
            write_primitive(op_stack, memory, location);
            write_primitive(op_stack, memory, location + 8);
            write_primitive(op_stack, memory, location + 16);
        }
        PhysicalType::Pointer | PhysicalType::FunctionPointer => {
            write_primitive(op_stack, memory, location);
        }
        PhysicalType::Nullable(ty) => match op_stack.pop().unwrap() {
            Value::Byte(0) => {
                memory[location] = 0;
            }
            Value::Byte(_) => {
                memory[location] = 1;
                write(op_stack, layouts, memory, location + NULL_TAG_SIZE, ty);
            }
            _ => unreachable!(),
        },
    }
}

fn write_primitive(op_stack: &mut Vec<Value>, memory: &mut [u8], location: usize) -> usize {
    let value = op_stack.pop().unwrap();
    let bytes = match &value {
        Value::FunctionID(id) => bytemuck::bytes_of(id),
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
        Value::Byte(x) => bytemuck::bytes_of(x),
    };
    memory[location..(location + bytes.len())].copy_from_slice(bytes);
    bytes.len()
}

fn read(
    op_stack: &mut Vec<Value>,
    layouts: &HashMap<TypeID, DeclaredTypeLayout>,
    memory: &[u8],
    location: usize,
    ty: &PhysicalType,
) {
    match ty {
        PhysicalType::Primitive(p) => {
            read_primitive(op_stack, memory, location, *p);
        }
        PhysicalType::Referenced(id) => {
            let layout = &layouts[id];
            match &layout.value {
                TypeLayoutValue::Structure(fields) => {
                    for (_, offset, ty) in fields.iter().rev() {
                        let location = location + offset;
                        read(op_stack, layouts, memory, location, ty);
                    }
                }
                TypeLayoutValue::Interface(fields) => {
                    let mut location = location + layout.size;
                    for _ in fields.iter().rev() {
                        read(
                            op_stack,
                            layouts,
                            memory,
                            location,
                            &PhysicalType::FunctionPointer,
                        );
                        location -= 4;
                    }
                    read_primitive(op_stack, memory, location, PhysicalPrimitive::PointerSize);
                }
                TypeLayoutValue::Union(_) => todo!(),
            }
        }
        PhysicalType::Collection(PhysicalCollection::Array | PhysicalCollection::Dict)
        | PhysicalType::Generator => {
            read_primitive(
                op_stack,
                memory,
                location + 16,
                PhysicalPrimitive::PointerSize,
            );
            read_primitive(
                op_stack,
                memory,
                location + 8,
                PhysicalPrimitive::PointerSize,
            );
            read_primitive(op_stack, memory, location, PhysicalPrimitive::PointerSize);
        }
        PhysicalType::Pointer => {
            read_primitive(op_stack, memory, location, PhysicalPrimitive::PointerSize);
        }
        PhysicalType::Nullable(ty) => {
            let null_flag = memory[location];
            if null_flag == 0 {
                op_stack.push(bool_value(false));
            } else {
                read(op_stack, layouts, memory, location + NULL_TAG_SIZE, ty);
                op_stack.push(bool_value(true));
            }
        }
        PhysicalType::FunctionPointer => {
            let fn_id: FunctionID = *bytemuck::from_bytes(&memory[location..(location + 4)]);
            op_stack.push(Value::FunctionID(fn_id));
        }
    }
}

fn read_primitive(
    op_stack: &mut Vec<Value>,
    memory: &[u8],
    location: usize,
    primitive: PhysicalPrimitive,
) {
    op_stack.push(match primitive {
        PhysicalPrimitive::Byte => Value::Byte(memory[location]),
        PhysicalPrimitive::Int32 => {
            Value::Int32(*bytemuck::from_bytes(&memory[location..(location + 4)]))
        }
        PhysicalPrimitive::Float32 => {
            Value::Float32(*bytemuck::from_bytes(&memory[location..(location + 4)]))
        }
        PhysicalPrimitive::Int64 => {
            Value::Int64(*bytemuck::from_bytes(&memory[location..(location + 8)]))
        }
        PhysicalPrimitive::Float64 => {
            Value::Float64(*bytemuck::from_bytes(&memory[location..(location + 8)]))
        }
        PhysicalPrimitive::PointerSize => {
            let base_ptr = &memory[location..(location + 8)];
            let base_ptr = usize::from_le_bytes(base_ptr.try_into().unwrap());
            Value::Size(base_ptr)
        }
    });
}
