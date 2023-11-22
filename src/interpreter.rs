use std::collections::HashMap;

use crate::{
    id::ID,
    ir::{IrBinOp, IrFunction, IrNode, IrNodeValue},
};

#[derive(Clone, Debug)]
pub enum Value {
    Null,
    Int(i64),
    Float(f64),
    Bool(bool),
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

pub fn evaluate_function<'a>(
    fns: &HashMap<ID, IrFunction<'a>>,
    function_to_run: ID,
    params: &[Value],
) -> Vec<Value> {
    let function = fns.get(&function_to_run).expect("function to exist in map");
    let mut ctx = Context {
        fns,
        params,
        variables: HashMap::new(),
        value_stack: Vec::new(),
    };
    let _ = evaluate_node(&mut ctx, &function.body);

    ctx.value_stack
}

enum Numeric {
    Int(i64),
    Float(f64),
}

pub struct Context<'a> {
    fns: &'a HashMap<ID, IrFunction<'a>>,
    params: &'a [Value],
    variables: HashMap<ID, Value>,
    value_stack: Vec<Value>,
}

impl<'a> Context<'a> {
    pub fn new(fns: &'a HashMap<ID, IrFunction<'a>>) -> Context {
        Context {
            fns,
            params: &[],
            variables: HashMap::new(),
            value_stack: Vec::new(),
        }
    }

    pub fn values(self) -> Vec<Value> {
        self.value_stack
    }
}

pub enum EvaluationStop {
    Returned,
}

// Kinda a hack: when we return, unwind the stack via Result
pub fn evaluate_node<'a>(ctx: &mut Context<'a>, node: &IrNode<'a>) -> Result<(), EvaluationStop> {
    match &node.value {
        IrNodeValue::Parameter(idx, id) => {
            ctx.variables.insert(*id, ctx.params[*idx].clone());
        }
        IrNodeValue::VariableReference(id) => {
            // TODO: don't clone?
            ctx.value_stack
                .push(ctx.variables.get(id).expect("var to be assigned").clone());
        }
        // No-op in the interpeter
        IrNodeValue::Declaration(_) => {}
        IrNodeValue::Destructor(_) => todo!(),
        IrNodeValue::Call(fn_id, params) => {
            // TODO: function call lvalues?
            let IrNodeValue::VariableReference(fn_id) = &fn_id.value else {
                panic!("expected ID");
            };
            // TODO: reverse order?
            for param in params.iter().rev() {
                evaluate_node(ctx, param)?;
            }
            let params: Vec<_> = (0..params.len())
                .map(|_| ctx.value_stack.pop().expect("param on stack"))
                .collect();
            let results = evaluate_function(ctx.fns, *fn_id, &params[..]);
            ctx.value_stack.extend(results.into_iter());
        }
        IrNodeValue::Dot(val, name) => {
            evaluate_node(ctx, val)?;
            let Some(Value::Struct(val)) = ctx.value_stack.pop() else {
                panic!("ICE: left side of '.' must be struct");
            };
            // TODO: clone
            ctx.value_stack
                .push(val.get(name).expect("field must be present").clone());
        }
        IrNodeValue::Assignment(lvalue, rvalue) => {
            evaluate_node(ctx, rvalue)?;
            // TODO: more lvalues
            let IrNodeValue::VariableReference(id) = &lvalue.value else {
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
        IrNodeValue::BinOp(op, left, right) => {
            evaluate_node(ctx, right)?;
            evaluate_node(ctx, left)?;
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
                    IrBinOp::Add => Value::Int(left + right),
                    IrBinOp::Subtract => Value::Int(left - right),
                    IrBinOp::Multiply => Value::Int(left * right),
                    IrBinOp::Divide => Value::Int(left / right),
                    IrBinOp::LessThan => Value::Bool(left < right),
                    IrBinOp::GreaterThan => Value::Bool(left > right),
                    IrBinOp::LessEqualThan => Value::Bool(left <= right),
                    IrBinOp::GreaterEqualThan => Value::Bool(left >= right),
                    IrBinOp::EqualTo => Value::Bool(left == right),
                    IrBinOp::NotEquals => Value::Bool(left != right),
                },
                (Numeric::Float(left), Numeric::Float(right)) => match op {
                    IrBinOp::Add => Value::Float(left + right),
                    IrBinOp::Subtract => Value::Float(left - right),
                    IrBinOp::Multiply => Value::Float(left * right),
                    IrBinOp::Divide => Value::Float(left / right),
                    IrBinOp::LessThan => Value::Bool(left < right),
                    IrBinOp::GreaterThan => Value::Bool(left > right),
                    IrBinOp::LessEqualThan => Value::Bool(left <= right),
                    IrBinOp::GreaterEqualThan => Value::Bool(left >= right),
                    IrBinOp::EqualTo => Value::Bool(left == right),
                    IrBinOp::NotEquals => Value::Bool(left != right),
                },
                (_, _) => unreachable!(),
            };

            ctx.value_stack.push(val);
        }
        IrNodeValue::Return(val) => {
            evaluate_node(ctx, val)?;
            return Err(EvaluationStop::Returned);
        }
        IrNodeValue::Int(val) => ctx.value_stack.push(Value::Int(*val)),
        IrNodeValue::Float(val) => ctx.value_stack.push(Value::Float(*val)),
        IrNodeValue::Bool(val) => ctx.value_stack.push(Value::Bool(*val)),
        IrNodeValue::Null => ctx.value_stack.push(Value::Null),
        IrNodeValue::Sequence(nodes) => {
            for node in nodes.iter() {
                evaluate_node(ctx, node)?;
            }
        }
        IrNodeValue::If(cond, if_branch, else_branch) => {
            evaluate_node(ctx, cond)?;
            let Some(Value::Bool(cond)) = ctx.value_stack.pop() else {
                panic!("ICE: expected boolean");
            };
            if cond {
                evaluate_node(ctx, if_branch)?;
            } else if let Some(else_branch) = else_branch {
                evaluate_node(ctx, else_branch)?;
            }
        }
        IrNodeValue::While(cond, block) => loop {
            evaluate_node(ctx, cond)?;
            let Some(Value::Bool(cond)) = ctx.value_stack.pop() else {
                panic!("ICE: expected boolean");
            };
            if !cond {
                break;
            }
            evaluate_node(ctx, block)?;
        },
        IrNodeValue::StructLiteral(_, _) => todo!(),

        IrNodeValue::TakeUnique(_) => todo!(),
        IrNodeValue::TakeShared(_) => todo!(),
        IrNodeValue::Dereference(_) => todo!(),

        IrNodeValue::Index(_, _) => todo!(),
        IrNodeValue::ArrayLiteral(_) => todo!(),
        IrNodeValue::ArrayLiteralLength(_, _) => todo!(),
    }

    Ok(())
}
