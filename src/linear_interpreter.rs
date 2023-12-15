use std::collections::HashMap;

use async_recursion::async_recursion;

use crate::{
    hir::HirBinOp,
    id::ID,
    interpreter::Numeric,
    linear_ir::{LinearFunction, LinearNode, LinearNodeValue},
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
    pub op_stack: Vec<Value>,
    pub base_ptr: usize,
    pub stack_ptr: usize,
}

impl VM {
    pub fn new() -> VM {
        let memory = [0; 1024];
        VM {
            memory,
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
            vm.op_stack
                .push(Value::Size(vm.base_ptr));
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
            size,
            ty,
        } => {
            evaluate_block(fns, params, vm, location).await?;
            let Some(Value::Size(mut location)) = vm.op_stack.pop() else {
                unreachable!()
            };
            location += offset;
            let memory = &vm.memory[location..(location + size)];
            // TODO: complex types
            let ExpressionType::Primitive(ty) = ty else {
                todo!("{:?}", node.provenance);
            };
            vm.op_stack.push(match ty {
                PrimitiveType::Char => todo!(), //Value::Char(*bytemuck::from_bytes(memory)),
                PrimitiveType::String => todo!(),
                PrimitiveType::Int32 => Value::Int32(*bytemuck::from_bytes(memory)),
                PrimitiveType::Float32 => Value::Float32(*bytemuck::from_bytes(memory)),
                PrimitiveType::Int64 => Value::Int64(*bytemuck::from_bytes(memory)),
                PrimitiveType::Float64 => Value::Float64(*bytemuck::from_bytes(memory)),
                PrimitiveType::Bool => todo!(), //Value::Bool(*bytemuck::from_bytes(memory)),
                PrimitiveType::PointerSize => todo!(),
            });
        }
        LinearNodeValue::WriteMemory {
            location,
            offset,
            size,
            value,
        } => {
            evaluate_block(fns, params, vm, value).await?;
            // TODO: complex types
            let value = vm.op_stack.pop().unwrap().to_numeric().unwrap();
            evaluate_block(fns, params, vm, location).await?;
            let Some(Value::Size(mut location)) = vm.op_stack.pop() else {
                unreachable!()
            };
            location += offset;
            let bytes = match &value {
                Numeric::Float32(value) => bytemuck::bytes_of(value),
                Numeric::Int32(value) => bytemuck::bytes_of(value),
                Numeric::Float64(value) => bytemuck::bytes_of(value),
                Numeric::Int64(value) => bytemuck::bytes_of(value),
                Numeric::Size(value) => bytemuck::bytes_of(value),
            };
            (&mut vm.memory[location..(location + size)]).copy_from_slice(bytes);
        }
        LinearNodeValue::Call(lhs, parameters) => {
            evaluate_block(fns, params, vm, lhs).await?;
            let Some(Value::ID(fn_id)) = vm.op_stack.pop() else {
                unreachable!()
            };
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
        LinearNodeValue::Loop(inner) => {
            'outer: loop {
                for node in inner.iter() {
                    match evaluate_block(fns, params, vm, node).await {
                        Err(Unwind::Break) => break 'outer,
                        other @ Err(_)  => return other,
                        Ok(_) => {}
                    }
                }
            }
        }
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

/*
        HirNodeValue::Parameter(idx, id) => {
            ctx.variables.insert(*id, ctx.params[*idx].clone());
        }
        HirNodeValue::VariableReference(id) => {
            // TODO: don't clone?
            ctx.value_stack
                .push(ctx.variables.get(id).expect("var to be assigned").clone());
        }
        // No-op in the interpeter
        HirNodeValue::Declaration(_) => {}
        HirNodeValue::Call(fn_id, args) => {
            evaluate_node(fns, ctx, fn_id).await?;
            let Some(Value::Function(func)) = ctx.value_stack.pop() else {
                panic!("expected functions");
            };
            // TODO: reverse order?
            for param in args.iter().rev() {
                evaluate_node(fns, ctx, param).await?;
            }
            let params: Vec<_> = (0..args.len())
                .map(|_| ctx.value_stack.pop().expect("param on stack"))
                .collect();
            let results = evaluate_function(fns, &func, params);
            ctx.value_stack.extend(results.await.into_iter());
        }
        HirNodeValue::VtableCall(var, virtual_fn_id, args) => {
            evaluate_node(fns, ctx, var).await?;
            let Some(Value::Interface(value, vtable)) = ctx.value_stack.pop() else {
                panic!("expected interface");
            };
            let function = vtable.get(virtual_fn_id).unwrap();
            // TODO: reverse order?
            for param in args.iter().rev() {
                evaluate_node(fns, ctx, param).await?;
            }
            ctx.value_stack.push(*value);
            let params: Vec<_> = (0..args.len())
                .map(|_| ctx.value_stack.pop().expect("param on stack"))
                .collect();
            let results = evaluate_function(fns, function, params);
            ctx.value_stack.extend(results.await.into_iter());
        }
        HirNodeValue::Access(val, name) => {
            evaluate_node(fns, ctx, val).await?;
            let Some(Value::Struct(val)) = ctx.value_stack.pop() else {
                panic!("ICE: left side of '.' must be struct");
            };
            // TODO: clone
            ctx.value_stack
                .push(val.get(name).expect("field must be present").clone());
        }
        HirNodeValue::Assignment(lvalue, rvalue) => {
            evaluate_node(fns, ctx, rvalue).await?;
            // TODO: more lvalues
            let HirNodeValue::VariableReference(id) = &lvalue.value else {
                todo!("complex lvalues");
            };
            ctx.variables.insert(
                *id,
                ctx.value_stack
                    .pop()
                    .expect("right hand side should leave assignable value on stack"),
            );
        }
        // TODO: floating arithmetic
        HirNodeValue::BinOp(op, left, right) => {
            evaluate_node(fns, ctx, right).await?;
            evaluate_node(fns, ctx, left).await?;
            let left = ctx
                .value_stack
                .pop()
                .expect("value present")
                .to_numeric()
                .expect("value numeric");

            let right = ctx
                .value_stack
                .pop()
                .expect("value present")
                .to_numeric()
                .expect("value numeric");

            let val = match (left, right) {
                (Numeric::Int(left), Numeric::Int(right)) => match op {
                    HirBinOp::Add => Value::Int(left + right),
                    HirBinOp::Subtract => Value::Int(left - right),
                    HirBinOp::Multiply => Value::Int(left * right),
                    HirBinOp::Divide => Value::Int(left / right),
                    HirBinOp::LessThan => Value::Bool(left < right),
                    HirBinOp::GreaterThan => Value::Bool(left > right),
                    HirBinOp::LessEqualThan => Value::Bool(left <= right),
                    HirBinOp::GreaterEqualThan => Value::Bool(left >= right),
                    HirBinOp::EqualTo => Value::Bool(left == right),
                    HirBinOp::NotEquals => Value::Bool(left != right),
                },
                (Numeric::Float(left), Numeric::Float(right)) => match op {
                    HirBinOp::Add => Value::Float(left + right),
                    HirBinOp::Subtract => Value::Float(left - right),
                    HirBinOp::Multiply => Value::Float(left * right),
                    HirBinOp::Divide => Value::Float(left / right),
                    HirBinOp::LessThan => Value::Bool(left < right),
                    HirBinOp::GreaterThan => Value::Bool(left > right),
                    HirBinOp::LessEqualThan => Value::Bool(left <= right),
                    HirBinOp::GreaterEqualThan => Value::Bool(left >= right),
                    HirBinOp::EqualTo => Value::Bool(left == right),
                    HirBinOp::NotEquals => Value::Bool(left != right),
                },
                (_, _) => unreachable!(),
            };

            ctx.value_stack.push(val);
        }
        HirNodeValue::Return(val) => {
            evaluate_node(fns, ctx, val).await?;
            return Err(Unwind::Returned(
                ctx.value_stack.pop().expect("value on stack"),
            ));
        }
        HirNodeValue::Int(val) => ctx.value_stack.push(Value::Int(*val)),
        HirNodeValue::Float(val) => ctx.value_stack.push(Value::Float(*val)),
        HirNodeValue::Bool(val) => ctx.value_stack.push(Value::Bool(*val)),
        HirNodeValue::Null => ctx.value_stack.push(Value::Null),
        HirNodeValue::StringLiteral(val) => ctx.value_stack.push(Value::String(val.clone())),
        HirNodeValue::CharLiteral(val) => ctx.value_stack.push(Value::Char(*val)),
        HirNodeValue::Sequence(nodes) => {
            for node in nodes.iter() {
                evaluate_node(fns, ctx, node).await?;
            }
        }
        HirNodeValue::If(cond, if_branch, else_branch) => {
            evaluate_node(fns, ctx, cond).await?;
            let Some(Value::Bool(cond)) = ctx.value_stack.pop() else {
                panic!("ICE: expected boolean");
            };
            if cond {
                evaluate_node(fns, ctx, if_branch).await?;
            } else if let Some(else_branch) = else_branch {
                evaluate_node(fns, ctx, else_branch).await?;
            }
        }
        HirNodeValue::While(cond, block) => loop {
            evaluate_node(fns, ctx, cond).await?;
            let Some(Value::Bool(cond)) = ctx.value_stack.pop() else {
                panic!("ICE: expected boolean");
            };
            if !cond {
                break;
            }
            evaluate_node(fns, ctx, block).await?;
        },
        HirNodeValue::StructLiteral(_, fields) => {
            let mut values = HashMap::new();
            for (field, value) in fields.iter() {
                evaluate_node(fns, ctx, value).await?;
                values.insert(field.clone(), ctx.value_stack.pop().unwrap());
            }
            ctx.value_stack.push(Value::Struct(values));
        }

        HirNodeValue::TakeUnique(_) => todo!(),
        HirNodeValue::TakeShared(_) => todo!(),
        HirNodeValue::Dereference(_) => todo!(),

        HirNodeValue::Index(_, _) => todo!(),
        HirNodeValue::ArrayLiteral(_) => todo!(),
        HirNodeValue::ArrayLiteralLength(_, _) => todo!(),

        HirNodeValue::StructToInterface { value, vtable } => {
            evaluate_node(fns, ctx, value).await?;
            let value = ctx.value_stack.pop().unwrap();
            let vtable = vtable
                .iter()
                .map(|(virtual_fn_id, concrete_fn_id)| {
                    (*virtual_fn_id, fns.get(concrete_fn_id).unwrap().clone())
                })
                .collect();
            ctx.value_stack
                .push(Value::Interface(Box::new(value), vtable));
        }
    }

    Ok(())
}
        */
