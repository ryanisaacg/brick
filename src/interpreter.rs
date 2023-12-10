use std::{collections::HashMap, future::Future, pin::Pin, sync::Arc};

use async_recursion::async_recursion;

use crate::{
    hir::{HirBinOp, HirFunction, HirNode, HirNodeValue},
    id::ID,
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
    Array(Vec<Value>),
    Struct(HashMap<String, Value>),
    Function(Function),
    Interface(Box<Value>, HashMap<ID, Function>),
}

impl Value {
    pub fn to_numeric(&self) -> Option<Numeric> {
        match self {
            Value::Int32(x) => Some(Numeric::Int32(*x)),
            Value::Float32(x) => Some(Numeric::Float32(*x)),
            Value::Int64(x) => Some(Numeric::Int64(*x)),
            Value::Float64(x) => Some(Numeric::Float64(*x)),
            _ => None,
        }
    }
}

pub type ExternBinding =
    dyn Fn(Vec<Value>) -> Pin<Box<dyn Future<Output = Option<Value>> + Send>> + Send + Sync;

#[derive(Clone)]
pub enum Function {
    Ir(HirFunction),
    Extern(Arc<ExternBinding>),
}

impl PartialEq for Function {
    fn eq(&self, other: &Self) -> bool {
        use Function::*;
        match (self, other) {
            (Ir(a), Ir(b)) => a.id == b.id,
            _ => false,
        }
    }
}

impl std::fmt::Debug for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Function::Ir(inner) => write!(f, "Function::Ir({:?})", inner),
            Function::Extern(_) => write!(f, "Function::Extern(opaque)"),
        }
    }
}

pub async fn evaluate_function(
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
                Err(EvaluationStop::Returned(val)) => Some(val),
            }
        }
        Function::Extern(ext) => ext(params).await,
    }
}

pub enum Numeric {
    Int32(i32),
    Float32(f32),
    Int64(i64),
    Float64(f64),
}

pub struct Context {
    params: Vec<Value>,
    variables: HashMap<ID, Value>,
    value_stack: Vec<Value>,
}

impl Context {
    pub fn new(params: Vec<Value>) -> Context {
        Context {
            params,
            variables: HashMap::new(),
            value_stack: Vec::new(),
        }
    }

    pub fn add_fns(&mut self, fns: &HashMap<ID, Function>) {
        for (id, fn_val) in fns.iter() {
            self.variables.insert(*id, Value::Function(fn_val.clone()));
        }
    }

    pub fn values(self) -> Vec<Value> {
        self.value_stack
    }
}

pub enum EvaluationStop {
    Returned(Value),
}

// Kinda a hack: when we return, unwind the stack via Result
#[async_recursion]
pub async fn evaluate_node(
    fns: &HashMap<ID, Function>,
    ctx: &mut Context,
    node: &HirNode,
) -> Result<(), EvaluationStop> {
    match &node.value {
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
                (_, _) => unreachable!(),
            };

            ctx.value_stack.push(val);
        }
        HirNodeValue::Return(val) => {
            evaluate_node(fns, ctx, val).await?;
            return Err(EvaluationStop::Returned(
                ctx.value_stack.pop().expect("value on stack"),
            ));
        }
        HirNodeValue::Int(val) => ctx.value_stack.push(Value::Int32(*val as i32)),
        HirNodeValue::Float(val) => ctx.value_stack.push(Value::Float32(*val as f32)),
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
