use std::collections::HashMap;

use super::{IrBinOp, IrFunction, IrModule, IrNode, IrNodeValue};

use crate::{
    id::ID,
    parser::{AstNode, AstNodeValue, BinOp, IfDeclaration},
    typecheck::{ExpressionType, StaticDeclaration, TypecheckedFile, TypecheckedFunction},
};

pub fn lower_module<'ast>(
    module: TypecheckedFile<'ast>,
    declarations: &HashMap<ID, &'ast StaticDeclaration>,
) -> IrModule {
    let TypecheckedFile {
        associated_functions: _, // TODO
        functions,
        top_level_statements,
    } = module;
    IrModule {
        functions: functions
            .into_iter()
            .map(|func| lower_function(declarations, func))
            .collect(),
        top_level_statements: top_level_statements
            .into_iter()
            .map(|stmt| lower_node(declarations, &stmt))
            .collect(),
    }
}

fn lower_function<'ast>(
    decls: &HashMap<ID, &'ast StaticDeclaration>,
    func: TypecheckedFunction<'ast>,
) -> IrFunction {
    let Some(StaticDeclaration::Func(func_ty)) = decls.get(&func.id) else {
        panic!();
    };
    let mut instructions: Vec<_> = func
        .func
        .params
        .iter()
        .enumerate()
        .map(|(i, param)| {
            IrNode::generated_with_id(
                param.id,
                IrNodeValue::Parameter(i, param.id),
                func_ty.params[i].clone(),
            )
        })
        .collect();
    let body = lower_node(decls, func.func.body);
    let ty = body.ty.clone();
    instructions.push(body);
    IrFunction {
        id: func.id,
        name: func.name,
        body: IrNode::generated_with_id(func.id, IrNodeValue::Sequence(instructions), ty),
    }
}

fn lower_node<'ast>(
    decls: &HashMap<ID, &'ast StaticDeclaration>,
    node: &'ast AstNode<'ast>,
) -> IrNode {
    let value = match &node.value {
        AstNodeValue::Int(x) => IrNodeValue::Int(*x),
        AstNodeValue::Float(x) => IrNodeValue::Float(*x),
        AstNodeValue::Bool(x) => IrNodeValue::Bool(*x),
        AstNodeValue::Null => IrNodeValue::Null,
        AstNodeValue::CharLiteral(x) => IrNodeValue::CharLiteral(*x),
        AstNodeValue::StringLiteral(x) => IrNodeValue::StringLiteral(x.clone()),

        AstNodeValue::While(cond, body) => {
            // TODO: can you assign out of a while?
            let cond = lower_node_alloc(decls, cond);
            let body = lower_node_alloc(decls, body);
            IrNodeValue::While(cond, body)
        }
        AstNodeValue::Block(contents) => {
            let contents = contents
                .iter()
                .map(|node| lower_node(decls, node))
                .collect();

            IrNodeValue::Sequence(contents)
        }
        // TODO
        AstNodeValue::If(IfDeclaration {
            condition,
            if_branch,
            else_branch,
        }) => {
            let condition = lower_node_alloc(decls, condition);
            let if_branch = lower_node_alloc(decls, if_branch);
            let else_branch = else_branch
                .as_ref()
                .map(|else_branch| lower_node_alloc(decls, else_branch));

            IrNodeValue::If(condition, if_branch, else_branch)
        }

        AstNodeValue::TakeUnique(inner) => IrNodeValue::TakeUnique(lower_node_alloc(decls, inner)),
        AstNodeValue::TakeShared(inner) => IrNodeValue::Return(lower_node_alloc(decls, inner)),

        // Statement doesn't actually add a node - the inner expression
        // has what really counts
        AstNodeValue::Statement(inner) => return lower_node(decls, inner),

        AstNodeValue::Declaration(_lvalue, rvalue) => {
            let lvalue = Box::new(IrNode::from_ast(
                node,
                IrNodeValue::VariableReference(node.id),
                rvalue.ty.get().expect("type filled in").clone(),
            ));
            let rvalue = lower_node_alloc(decls, rvalue);
            let statements = vec![
                IrNode::from_ast_void(node, IrNodeValue::Declaration(node.id)),
                IrNode::from_ast_void(node, IrNodeValue::Assignment(lvalue, rvalue)),
            ];
            IrNodeValue::Sequence(statements)
        }
        AstNodeValue::Name { referenced_id, .. } => IrNodeValue::VariableReference(
            *referenced_id.get().expect("referenced ID to be filled in"),
        ),

        AstNodeValue::Return(inner) => IrNodeValue::Return(lower_node_alloc(decls, inner)),
        AstNodeValue::BinExpr(BinOp::Dot, left, right) => {
            let Some(ExpressionType::DeclaredType(expr_ty)) = left.ty.get() else {
                panic!("expected left side of dot to be declared type");
            };
            let AstNodeValue::Name { value: name, .. } = &right.value else {
                unreachable!()
            };
            if let Some(StaticDeclaration::Module(module)) = decls.get(expr_ty) {
                IrNodeValue::VariableReference(
                    module
                        .exports
                        .get(name)
                        .expect("module export to exist")
                        .id(),
                )
            } else {
                let left = lower_node_alloc(decls, left);
                IrNodeValue::Access(left, name.clone())
            }
        }
        AstNodeValue::BinExpr(op, left, right) => {
            let left = lower_node_alloc(decls, left);
            let right = lower_node_alloc(decls, right);

            match op {
                BinOp::AddAssign => IrNodeValue::Assignment(
                    left.clone(),
                    Box::new(IrNode::from_ast_void(
                        &node,
                        IrNodeValue::BinOp(IrBinOp::Add, left, right),
                    )),
                ),
                BinOp::SubtractAssign => IrNodeValue::Assignment(
                    left.clone(),
                    Box::new(IrNode::from_ast_void(
                        &node,
                        IrNodeValue::BinOp(IrBinOp::Subtract, left, right),
                    )),
                ),
                BinOp::MultiplyAssign => IrNodeValue::Assignment(
                    left.clone(),
                    Box::new(IrNode::from_ast_void(
                        &node,
                        IrNodeValue::BinOp(IrBinOp::Multiply, left, right),
                    )),
                ),
                BinOp::DivideAssign => IrNodeValue::Assignment(
                    left.clone(),
                    Box::new(IrNode::from_ast_void(
                        &node,
                        IrNodeValue::BinOp(IrBinOp::Divide, left, right),
                    )),
                ),
                BinOp::Assignment => IrNodeValue::Assignment(left, right),

                BinOp::Add => IrNodeValue::BinOp(IrBinOp::Add, left, right),
                BinOp::Subtract => IrNodeValue::BinOp(IrBinOp::Subtract, left, right),
                BinOp::Multiply => IrNodeValue::BinOp(IrBinOp::Multiply, left, right),
                BinOp::Divide => IrNodeValue::BinOp(IrBinOp::Divide, left, right),
                BinOp::LessThan => IrNodeValue::BinOp(IrBinOp::LessThan, left, right),
                BinOp::GreaterThan => IrNodeValue::BinOp(IrBinOp::GreaterThan, left, right),
                BinOp::LessEqualThan => IrNodeValue::BinOp(IrBinOp::LessEqualThan, left, right),
                BinOp::GreaterEqualThan => {
                    IrNodeValue::BinOp(IrBinOp::GreaterEqualThan, left, right)
                }
                BinOp::EqualTo => IrNodeValue::BinOp(IrBinOp::EqualTo, left, right),
                BinOp::NotEquals => IrNodeValue::BinOp(IrBinOp::NotEquals, left, right),
                BinOp::Index => IrNodeValue::Index(left, right),
                BinOp::Dot => unreachable!(),
            }
        }
        AstNodeValue::Call(func, params) => {
            let func = lower_node_alloc(decls, func);
            let params = params
                .iter()
                .map(|param| lower_node(decls, param))
                .collect();
            IrNodeValue::Call(func, params)
        }
        AstNodeValue::StructLiteral { name, fields } => {
            let AstNodeValue::Name { referenced_id, .. } = &name.value else {
                panic!("Struct literal must have Name");
            };
            let id = referenced_id
                .get()
                .expect("referenced fields to be filled in");
            let fields = fields
                .iter()
                .map(|(name, field)| (name.clone(), lower_node(decls, field)))
                .collect();
            IrNodeValue::StructLiteral(*id, fields)
        }
        AstNodeValue::DictLiteral(_) => todo!(),
        AstNodeValue::ArrayLiteral(_) => todo!(),
        AstNodeValue::ArrayLiteralLength(_, _) => todo!(),

        AstNodeValue::FunctionDeclaration(_)
        | AstNodeValue::ExternFunctionBinding(_)
        | AstNodeValue::StructDeclaration(_)
        | AstNodeValue::UnionDeclaration(_)
        | AstNodeValue::InterfaceDeclaration(_)
        | AstNodeValue::Import(_)
        | AstNodeValue::UniqueType(_)
        | AstNodeValue::SharedType(_)
        | AstNodeValue::NullableType(_)
        | AstNodeValue::RequiredFunction(_)
        | AstNodeValue::ArrayType(_) => unreachable!("Can't have these in a function body"),
    };

    IrNode::from_ast(node, value, node.ty.get().expect("type filled").clone())
}

fn lower_node_alloc<'ast, 'ir>(
    decls: &HashMap<ID, &'ast StaticDeclaration>,
    node: &'ast AstNode<'ast>,
) -> Box<IrNode> {
    Box::new(lower_node(decls, node))
}
