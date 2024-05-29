use std::{collections::HashMap, fmt::Debug};

use brick_runtime::{
    brick_memcpy, brick_runtime_alloc, brick_runtime_dealloc, brick_runtime_realloc,
    brick_string_concat,
};

use crate::{
    declaration_context::TypeID,
    hir::{ArithmeticOp, BinaryLogicalOp, ComparisonOp, UnaryLogicalOp},
    id::{FunctionID, RegisterID, VariableID},
    linear_ir::{
        DeclaredTypeLayout, LinearFunction, LinearNode, LinearNodeValue, PhysicalCollection,
        PhysicalPrimitive, PhysicalType, RuntimeFunction, TypeLayoutValue, NULL_TAG_SIZE,
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

pub type ExternBinding = Box<dyn Fn(&mut VM, Vec<Value>) -> Option<Value>>;

pub enum Function {
    Ir(LinearFunction),
    Extern(ExternBinding),
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

const CONSTANT_DATA_START: usize = 1024 * 1024 * 2;
const HEAP_SEGMENT_LENGTH: usize = 1024 * 1024;

pub struct VM<'a> {
    pub memory: Vec<u8>,
    pub op_stack: Vec<Value>,
    temporaries: HashMap<RegisterID, usize>,
    layouts: HashMap<TypeID, DeclaredTypeLayout>,
    base_ptr: usize,
    stack_ptr: usize,
    heap_ptr: usize,
    in_progress_goto: Option<usize>,
    variable_locations: Vec<HashMap<VariableID, (usize, PhysicalType)>>,
    fns: &'a HashMap<FunctionID, Function>,
}

const USIZE: usize = std::mem::size_of::<usize>();

impl<'a> VM<'a> {
    pub fn new(
        layouts: HashMap<TypeID, DeclaredTypeLayout>,
        functions: &'a HashMap<FunctionID, Function>,
        constant_data_region: Vec<u8>,
    ) -> VM {
        let mut memory = vec![0; CONSTANT_DATA_START];
        memory.extend(constant_data_region);
        unsafe {
            brick_runtime::brick_runtime_init(memory.as_mut_ptr(), HEAP_SEGMENT_LENGTH);
        }
        VM {
            memory,
            temporaries: HashMap::new(),
            layouts,
            op_stack: Vec::new(),
            base_ptr: CONSTANT_DATA_START,
            stack_ptr: CONSTANT_DATA_START,
            heap_ptr: 0,
            in_progress_goto: None,
            variable_locations: vec![HashMap::new()],
            fns: functions,
        }
    }

    pub fn evaluate_function(
        &mut self,
        params: &mut [Value],
        fn_id: FunctionID,
    ) -> Result<(), Unwind> {
        let function = &self.fns[&fn_id];
        match function {
            Function::Ir(function) => {
                // Write the current base ptr at the stack ptr location
                let base_ptr = self.base_ptr.to_le_bytes();
                self.memory[(self.stack_ptr - base_ptr.len())..self.stack_ptr]
                    .copy_from_slice(&base_ptr);
                self.base_ptr = self.stack_ptr;
                self.stack_ptr -= base_ptr.len();

                self.variable_locations.push(HashMap::new());

                for node in function.body.iter() {
                    let result = self.evaluate_node(params, node);
                    if let Err(Unwind::Return(val)) = result {
                        if let Some(val) = val {
                            self.op_stack.push(val);
                        }
                        break;
                    } else {
                        result?;
                    }
                }

                self.variable_locations.pop();

                let base_ptr = &self.memory[(self.base_ptr - base_ptr.len())..self.base_ptr];
                let base_ptr = usize::from_le_bytes(base_ptr.try_into().unwrap());
                self.stack_ptr = self.base_ptr;
                self.base_ptr = base_ptr;

                Ok(())
            }
            Function::Extern(ext) => {
                if let Some(returned) = ext(self, params.to_vec()) {
                    self.op_stack.push(returned);
                }
                Ok(())
            }
        }
    }

    pub fn resume_generator(&mut self, generator_ptr: Value) -> Result<(), Unwind> {
        let Value::Size(location) = generator_ptr else {
            panic!("must provide a valid generator pointer to resume_generator");
        };
        let fn_id: FunctionID = *bytemuck::from_bytes(
            &self.memory[location..(location + std::mem::size_of::<FunctionID>())],
        );
        self.evaluate_function(&mut [generator_ptr], fn_id)?;

        Ok(())
    }

    pub(crate) fn evaluate_top_level_statements(
        mut self,
        statements: &[LinearNode],
    ) -> Result<(Vec<Value>, Vec<u8>), Unwind> {
        for statement in statements.iter() {
            self.evaluate_node(&mut [], statement)?;
        }
        debug_assert_eq!(self.temporaries.len(), 0);

        Ok((self.op_stack, self.memory))
    }

    // Kinda a hack: when we return, unwind the stack via Result
    fn evaluate_node(&mut self, params: &mut [Value], node: &LinearNode) -> Result<(), Unwind> {
        if let Some(target_label) = self.in_progress_goto {
            match &node.value {
                LinearNodeValue::GotoLabel(current_label) if *current_label == target_label => {
                    self.in_progress_goto = None;
                }
                LinearNodeValue::Sequence(children) => {
                    // We can safely eval all of sequence's children - they'll
                    // ignore the GOTO if they need to. once we're done with that
                    // eval, return Ok to avoid running the whole thing again
                    for node in children.iter() {
                        self.evaluate_node(params, node)?;
                    }
                }
                LinearNodeValue::Loop(children) => 'outer: loop {
                    for node in children.iter() {
                        match self.evaluate_node(params, node) {
                            Err(Unwind::Break) => break 'outer,
                            other @ Err(_) => return other,
                            Ok(_) => {}
                        }
                    }
                },
                LinearNodeValue::If(_, if_branch, else_branch) => {
                    let mut found_goto = false;
                    for node in if_branch.iter() {
                        self.evaluate_node(params, node)?;
                        if self.in_progress_goto.is_none() {
                            found_goto = true;
                        }
                    }
                    if !found_goto {
                        if let Some(else_branch) = else_branch {
                            for node in else_branch.iter() {
                                self.evaluate_node(params, node)?;
                            }
                        }
                    }
                }
                _ => {}
            }
            return Ok(());
        }

        match &node.value {
            LinearNodeValue::Sequence(seq) => {
                for node in seq.iter() {
                    self.evaluate_node(params, node)?;
                }
            }
            LinearNodeValue::VariableInit(var_id, ty) => {
                self.stack_ptr -= ty.size_from_decls(&self.layouts, 1, USIZE);
                self.variable_locations
                    .last_mut()
                    .unwrap()
                    .insert(*var_id, (self.stack_ptr, ty.clone()));
            }
            LinearNodeValue::VariableDestroy(var_id) => {
                let (_location, ty) = self
                    .variable_locations
                    .last_mut()
                    .unwrap()
                    .remove(var_id)
                    .unwrap();
                self.stack_ptr += ty.size_from_decls(&self.layouts, 1, USIZE);
            }
            LinearNodeValue::VariableLocation(var_id) => {
                self.op_stack.push(Value::Size(
                    self.variable_locations
                        .last()
                        .unwrap()
                        .get(var_id)
                        .unwrap()
                        .0,
                ));
            }
            LinearNodeValue::Parameter(_, idx) => {
                let mut temp = Value::Byte(0);
                std::mem::swap(&mut temp, &mut params[*idx]);
                self.op_stack.push(temp);
            }
            LinearNodeValue::ReadMemory {
                location,
                offset,
                ty,
            } => {
                self.evaluate_node(params, location)?;
                let Some(Value::Size(mut location)) = self.op_stack.pop() else {
                    unreachable!()
                };
                location += offset;
                read(
                    &mut self.op_stack,
                    &self.layouts,
                    &self.memory,
                    location,
                    ty,
                );
            }
            LinearNodeValue::WriteMemory {
                location,
                offset,
                value,
                ty,
            } => {
                self.evaluate_node(params, value)?;
                self.evaluate_node(params, location)?;
                let Some(Value::Size(mut location)) = self.op_stack.pop() else {
                    unreachable!()
                };
                location += offset;
                write(
                    &mut self.op_stack,
                    &self.layouts,
                    &mut self.memory,
                    location,
                    ty,
                );
            }
            LinearNodeValue::IndirectCall(lhs, parameters) => {
                self.evaluate_node(params, lhs)?;
                let Some(Value::FunctionID(fn_id)) = self.op_stack.pop() else {
                    unreachable!()
                };
                for param in parameters.iter().rev() {
                    self.evaluate_node(params, param)?;
                }
                let mut parameters: Vec<_> = (0..parameters.len())
                    .map(|_| self.op_stack.pop().unwrap())
                    .collect();

                self.evaluate_function(&mut parameters[..], fn_id)?;
            }
            LinearNodeValue::Call(fn_id, parameters) => {
                for param in parameters.iter().rev() {
                    self.evaluate_node(params, param)?;
                }
                let mut parameters: Vec<_> = (0..parameters.len())
                    .map(|_| self.op_stack.pop().unwrap())
                    .collect();

                self.evaluate_function(&mut parameters[..], *fn_id)?;
            }
            LinearNodeValue::Return(expr) => {
                if let Some(expr) = expr {
                    self.evaluate_node(params, expr)?;
                    // TODO: support wide returns
                    let val = self.op_stack.pop().unwrap();
                    return Err(Unwind::Return(Some(val)));
                } else {
                    return Err(Unwind::Return(None));
                }
            }
            LinearNodeValue::If(cond, if_branch, else_branch) => {
                self.evaluate_node(params, cond)?;
                let Some(Value::Byte(cond)) = self.op_stack.pop() else {
                    unreachable!()
                };
                if cond != 0 {
                    for node in if_branch.iter() {
                        self.evaluate_node(params, node)?;
                    }
                } else if let Some(else_branch) = else_branch {
                    for node in else_branch.iter() {
                        self.evaluate_node(params, node)?;
                    }
                }
            }
            LinearNodeValue::Break => return Err(Unwind::Break),
            // easy-to-miss loop after 'outer
            LinearNodeValue::Loop(inner) => 'outer: loop {
                for node in inner.iter() {
                    match self.evaluate_node(params, node) {
                        Err(Unwind::Break) => break 'outer,
                        other @ Err(_) => return other,
                        Ok(_) => {}
                    }
                }
            },
            LinearNodeValue::UnaryLogical(UnaryLogicalOp::BooleanNot, child) => {
                self.evaluate_node(params, child)?;
                let Value::Byte(val) = self.op_stack.pop().unwrap() else {
                    unreachable!()
                };
                self.op_stack.push(bool_value(val == 0));
            }
            LinearNodeValue::Arithmetic(op, _ty, lhs, rhs) => {
                self.evaluate_node(params, rhs)?;
                self.evaluate_node(params, lhs)?;
                let left = self.op_stack.pop().unwrap().to_numeric().unwrap();
                let right = self.op_stack.pop().unwrap().to_numeric().unwrap();
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
                self.op_stack.push(val);
            }
            LinearNodeValue::Comparison(
                op @ (ComparisonOp::EqualTo | ComparisonOp::NotEquals),
                _ty,
                lhs,
                rhs,
            ) => {
                self.evaluate_node(params, rhs)?;
                self.evaluate_node(params, lhs)?;
                let left = self.op_stack.pop().unwrap();
                let right = self.op_stack.pop().unwrap();
                self.op_stack.push(bool_value(
                    (*op == ComparisonOp::EqualTo) == (left == right),
                ));
            }
            LinearNodeValue::Comparison(op, _ty, lhs, rhs) => {
                self.evaluate_node(params, rhs)?;
                self.evaluate_node(params, lhs)?;
                let left = self.op_stack.pop().unwrap().to_numeric().unwrap();
                let right = self.op_stack.pop().unwrap().to_numeric().unwrap();
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
                self.op_stack.push(val);
            }
            LinearNodeValue::BinaryLogical(op, lhs, rhs) => {
                self.evaluate_node(params, lhs)?;
                let Value::Byte(left) = self.op_stack.pop().unwrap() else {
                    unreachable!();
                };
                let left = left != 0;
                let result = match op {
                    BinaryLogicalOp::BooleanAnd => {
                        left && {
                            self.evaluate_node(params, rhs)?;
                            let Value::Byte(right) = self.op_stack.pop().unwrap() else {
                                unreachable!();
                            };
                            right != 0
                        }
                    }
                    BinaryLogicalOp::BooleanOr => {
                        left || {
                            self.evaluate_node(params, rhs)?;
                            let Value::Byte(right) = self.op_stack.pop().unwrap() else {
                                unreachable!();
                            };
                            right != 0
                        }
                    }
                };

                self.op_stack.push(bool_value(result));
            }
            LinearNodeValue::Size(size) => {
                self.op_stack.push(Value::Size(*size));
            }
            LinearNodeValue::Int(x) => {
                self.op_stack.push(Value::Int32(*x as i32));
            }
            LinearNodeValue::Float(x) => {
                self.op_stack.push(Value::Float32(*x as f32));
            }
            LinearNodeValue::Byte(x) => {
                self.op_stack.push(Value::Byte(*x));
            }
            LinearNodeValue::CharLiteral(x) => {
                // TODO: lossy conversion
                self.op_stack.push(Value::Byte(*x as u8));
            }
            LinearNodeValue::FunctionID(x) => {
                self.op_stack.push(Value::FunctionID(*x));
            }
            LinearNodeValue::WriteRegister(tmp, val) => {
                self.evaluate_node(params, val)?;
                let Value::Size(val) = self.op_stack.pop().unwrap() else {
                    unreachable!()
                };
                self.temporaries.insert(*tmp, val);
            }
            LinearNodeValue::ReadRegister(tmp) => {
                self.op_stack.push(Value::Size(
                    *self
                        .temporaries
                        .get(tmp)
                        .expect("tmp to be defined before use"),
                ));
            }
            LinearNodeValue::KillRegister(tmp) => {
                self.temporaries.remove(tmp);
            }
            LinearNodeValue::Abort => {
                return Err(Unwind::Aborted);
            }
            LinearNodeValue::Cast { value, from: _, to } => {
                self.evaluate_node(params, value)?;
                let val = self.op_stack.pop().unwrap();
                self.op_stack.push(match val {
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
                self.evaluate_node(params, inner)?;
                println!("{:?}", self.op_stack.last().unwrap());
            }
            LinearNodeValue::GotoLabel(_) => {}
            LinearNodeValue::Goto(label) => {
                self.evaluate_node(params, label)?;
                let Value::Size(label) = self.op_stack.pop().unwrap() else {
                    unreachable!()
                };
                self.in_progress_goto = Some(label);
            }
            LinearNodeValue::ConstantDataAddress(offset) => {
                self.op_stack
                    .push(Value::Size(CONSTANT_DATA_START + offset));
            }
            LinearNodeValue::RuntimeCall(RuntimeFunction::StringConcat, args) => {
                self.evaluate_node(params, &args[0])?;
                let Value::Size(a_len) = self.op_stack.pop().unwrap() else {
                    unreachable!()
                };
                let Value::Size(a_ptr) = self.op_stack.pop().unwrap() else {
                    unreachable!()
                };
                self.evaluate_node(params, &args[1])?;
                let Value::Size(b_len) = self.op_stack.pop().unwrap() else {
                    unreachable!()
                };
                let Value::Size(b_ptr) = self.op_stack.pop().unwrap() else {
                    unreachable!()
                };
                let location = unsafe {
                    let ptr = brick_string_concat(
                        self.allocator(),
                        self.memory[a_ptr..(a_ptr + a_len)].as_ptr(),
                        a_len,
                        self.memory[b_ptr..(b_ptr + b_len)].as_ptr(),
                        b_len,
                    );
                    ptr.offset_from(self.memory.as_ptr()) as usize
                };
                self.op_stack.push(Value::Size(location));
                self.op_stack.push(Value::Size(a_len + b_len));
            }
            LinearNodeValue::RuntimeCall(RuntimeFunction::Memcpy, args) => {
                self.evaluate_node(params, &args[0])?;
                let Value::Size(dest) = self.op_stack.pop().unwrap() else {
                    unreachable!()
                };
                self.evaluate_node(params, &args[1])?;
                let Value::Size(src) = self.op_stack.pop().unwrap() else {
                    unreachable!()
                };
                self.evaluate_node(params, &args[2])?;
                let Value::Size(len) = self.op_stack.pop().unwrap() else {
                    unreachable!()
                };
                unsafe {
                    brick_memcpy(
                        self.memory[dest..].as_mut_ptr(),
                        self.memory[src..].as_ptr(),
                        len,
                    );
                }
            }
            LinearNodeValue::RuntimeCall(RuntimeFunction::Alloc, args) => {
                self.evaluate_node(params, &args[0])?;
                let Some(Value::Size(amount)) = self.op_stack.pop() else {
                    unreachable!()
                };
                let allocation = unsafe {
                    let new_ptr = brick_runtime_alloc(self.allocator(), amount);

                    new_ptr.offset_from(self.memory.as_mut_ptr()) as usize
                };
                self.op_stack.push(Value::Size(allocation));
            }
            LinearNodeValue::RuntimeCall(RuntimeFunction::Realloc, args) => {
                self.evaluate_node(params, &args[0])?;
                let Some(Value::Size(ptr)) = self.op_stack.pop() else {
                    unreachable!()
                };
                self.evaluate_node(params, &args[1])?;
                let Some(Value::Size(new_size)) = self.op_stack.pop() else {
                    unreachable!()
                };
                let allocation = unsafe {
                    let new_ptr = brick_runtime_realloc(
                        self.allocator(),
                        self.memory.as_mut_ptr().add(ptr),
                        new_size,
                    );

                    new_ptr.offset_from(self.memory.as_mut_ptr()) as usize
                };
                self.op_stack.push(Value::Size(allocation));
            }
            LinearNodeValue::RuntimeCall(RuntimeFunction::Dealloc, args) => {
                self.evaluate_node(params, &args[0])?;
                let Some(Value::Size(pointer)) = self.op_stack.pop() else {
                    unreachable!()
                };
                unsafe {
                    brick_runtime_dealloc(self.allocator(), self.memory.as_mut_ptr().add(pointer));
                };
            }
            LinearNodeValue::Switch { value, cases } => {
                self.evaluate_node(params, value)?;
                let Value::Size(idx) = self.op_stack.pop().unwrap() else {
                    unreachable!()
                };
                self.evaluate_node(params, &cases[idx])?;
            }
            LinearNodeValue::WriteRegistersSplitting(value, registers) => {
                self.evaluate_node(params, value)?;
                for register in registers {
                    if let Some(register) = register {
                        let Value::Size(val) = self.op_stack.pop().unwrap() else {
                            unreachable!()
                        };
                        self.temporaries.insert(*register, val);
                    } else {
                        self.op_stack.pop().unwrap();
                    }
                }
            }
        }

        Ok(())
    }

    unsafe fn allocator(&mut self) -> *mut u8 {
        self.memory.as_mut_ptr().add(self.heap_ptr)
    }
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
        PhysicalType::Referenced(id) => {
            let ty = &layouts[id];
            match &ty.value {
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
                    let variant =
                        variants
                            .values()
                            .find_map(|(id, ty)| if *id == variant { ty.as_ref() } else { None });
                    if let Some(variant) = variant {
                        write(op_stack, layouts, memory, location + offset, variant);
                    }
                    let variant_size = variant
                        .map(|ty| ty.size_from_decls(layouts, 1, USIZE))
                        .unwrap_or(0);
                    let mut padding_to_drop = ty.size - USIZE - variant_size;
                    while padding_to_drop > 0 {
                        let padding = op_stack.pop().unwrap();
                        padding_to_drop -= match padding {
                            Value::FunctionID(_) | Value::Int32(_) | Value::Float32(_) => 4,
                            Value::Int64(_) | Value::Float64(_) => 4,
                            Value::Size(_) => USIZE,
                            Value::Byte(_) => 1,
                        };
                    }
                }
            }
        }
        PhysicalType::Collection(PhysicalCollection::Array | PhysicalCollection::Dict)
        | PhysicalType::Generator => {
            write_primitive(op_stack, memory, location);
            write_primitive(op_stack, memory, location + 8);
            write_primitive(op_stack, memory, location + 16);
        }
        PhysicalType::Collection(PhysicalCollection::String) => {
            write_primitive(op_stack, memory, location);
            write_primitive(op_stack, memory, location + 8);
        }
        PhysicalType::FunctionPointer => {
            write_primitive(op_stack, memory, location);
        }
        PhysicalType::Nullable(ty) => match op_stack.pop().unwrap() {
            Value::Byte(0) => {
                memory[location] = 0;
            }
            Value::Byte(_) => {
                memory[location] = 1;
                write(
                    op_stack,
                    layouts,
                    memory,
                    location + NULL_TAG_SIZE.size(USIZE),
                    ty,
                );
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
                    let mut location = location + layout.size - std::mem::size_of::<FunctionID>();
                    for _ in fields.iter().rev() {
                        read(
                            op_stack,
                            layouts,
                            memory,
                            location,
                            &PhysicalType::FunctionPointer,
                        );
                        location -= std::mem::size_of::<FunctionID>();
                    }
                    read_primitive(op_stack, memory, location, PhysicalPrimitive::PointerSize);
                }
                TypeLayoutValue::Union(union) => {
                    let largest_variant = union
                        .values()
                        .filter_map(|(_, ty)| ty.as_ref())
                        .max_by(|a, b| {
                            a.size_from_decls(layouts, 1, USIZE)
                                .cmp(&b.size_from_decls(layouts, 1, USIZE))
                        })
                        .unwrap();
                    read(op_stack, layouts, memory, location + USIZE, largest_variant);
                    read_primitive(op_stack, memory, location, PhysicalPrimitive::PointerSize);
                }
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
        PhysicalType::Collection(PhysicalCollection::String) => {
            read_primitive(
                op_stack,
                memory,
                location + 8,
                PhysicalPrimitive::PointerSize,
            );
            read_primitive(op_stack, memory, location, PhysicalPrimitive::PointerSize);
        }
        PhysicalType::Nullable(ty) => {
            let null_flag = memory[location];
            if null_flag == 0 {
                op_stack.push(bool_value(false));
            } else {
                read(
                    op_stack,
                    layouts,
                    memory,
                    location + NULL_TAG_SIZE.size(USIZE),
                    ty,
                );
                op_stack.push(bool_value(true));
            }
        }
        PhysicalType::FunctionPointer => {
            let fn_id: FunctionID = *bytemuck::from_bytes(
                &memory[location..(location + std::mem::size_of::<FunctionID>())],
            );
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
