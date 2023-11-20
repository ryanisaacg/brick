use std::collections::HashMap;

use crate::{
    id::ID,
    ir::{IrFunction, IrNode, IrNodeValue},
};

pub struct Program<'a> {
    fns: HashMap<ID, IrFunction<'a>>,
}

#[derive(Clone)]
pub enum Value {
    Void,
    Int(i64),
    Float(f64),
    Bool(bool),
    Array(Vec<Value>),
    Struct(HashMap<String, Value>),
}

pub fn evaluate<'a>(
    fns: &HashMap<ID, IrFunction<'a>>,
    function_to_run: ID,
    params: &[Value],
) -> Vec<Value> {
    let function = fns.get(&function_to_run).expect("function to exist in map");
    let mut variables = HashMap::new();
    let mut value_stack = Vec::new();
    let mut ctx = Context {
        fns,
        params,
        variables: &mut variables,
        value_stack: &mut value_stack,
    };
    evaluate_node(&mut ctx, &function.body);

    value_stack
}

struct Context<'a> {
    fns: &'a HashMap<ID, IrFunction<'a>>,
    params: &'a [Value],
    variables: &'a mut HashMap<ID, Value>,
    value_stack: &'a mut Vec<Value>,
}

fn evaluate_node<'a>(ctx: &mut Context<'a>, node: &IrNode<'a>) {
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
            // TODO: reverse order?
            for param in params {
                evaluate_node(ctx, param);
            }
            // TODO: pop and call function
        }
        IrNodeValue::Dot(val, name) => {
            evaluate_node(ctx, val);
            let Some(Value::Struct(val)) = ctx.value_stack.pop() else {
                panic!("ICE: left side of '.' must be struct");
            };
            // TODO: clone
            ctx.value_stack
                .push(val.get(name).expect("field must be present").clone());
        }
        IrNodeValue::Assignment(_, _) => todo!(),
        IrNodeValue::Index(_, _) => todo!(),
        IrNodeValue::BinOp(_, _, _) => todo!(),
        IrNodeValue::Return(_) => todo!(),
        IrNodeValue::Int(val) => ctx.value_stack.push(Value::Int(*val)),
        IrNodeValue::Float(val) => ctx.value_stack.push(Value::Float(*val)),
        IrNodeValue::Bool(val) => ctx.value_stack.push(Value::Bool(*val)),
        IrNodeValue::TakeUnique(_) => todo!(),
        IrNodeValue::TakeShared(_) => todo!(),
        IrNodeValue::Dereference(_) => todo!(),
        IrNodeValue::Sequence(_) => todo!(),
        IrNodeValue::If(_, _, _) => todo!(),
        IrNodeValue::While(_, _) => todo!(),
        IrNodeValue::StructLiteral(_, _) => todo!(),
        IrNodeValue::ArrayLiteral(_) => todo!(),
        IrNodeValue::ArrayLiteralLength(_, _) => todo!(),
    }
}
