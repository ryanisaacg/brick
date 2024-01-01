use std::collections::HashMap;

use super::{HirBinOp, HirFunction, HirModule, HirNode, HirNodeValue};

use crate::{
    id::{NodeID, TypeID},
    parser::{AstNode, AstNodeValue, BinOp, IfDeclaration},
    typecheck::{
        find_func, fully_dereference, CollectionType, ExpressionType, StaticDeclaration,
        TypecheckedFile, TypecheckedFunction,
    },
};

pub fn lower_module<'ast>(
    module: TypecheckedFile<'ast>,
    declarations: &HashMap<TypeID, &'ast StaticDeclaration>,
) -> HirModule {
    let TypecheckedFile {
        associated_functions: _,
        functions,
        top_level_statements,
    } = module;
    HirModule {
        functions: functions
            .into_iter()
            .map(|func| lower_function(declarations, func))
            .collect(),
        top_level_statements: HirNode::autogenerated(
            HirNodeValue::Sequence(
                top_level_statements
                    .into_iter()
                    .map(|stmt| lower_node(declarations, stmt))
                    .collect(),
            ),
            ExpressionType::Void,
        ),
    }
}

fn lower_function<'ast>(
    decls: &HashMap<TypeID, &'ast StaticDeclaration>,
    func: TypecheckedFunction<'ast>,
) -> HirFunction {
    let func_ty = find_func(decls, func.id).unwrap();
    let mut instructions: Vec<_> = func
        .func
        .params
        .iter()
        .enumerate()
        .map(|(i, (id, param))| {
            HirNode::generated_with_id(
                param.id,
                HirNodeValue::Parameter(i, *id),
                func_ty.params[i].clone(),
            )
        })
        .collect();
    let mut body = lower_node(decls, func.func.body);
    let HirNodeValue::Sequence(instrs) = &mut body.value else {
        unreachable!()
    };
    instructions.append(instrs);
    std::mem::swap(&mut instructions, instrs);
    HirFunction {
        id: func.id,
        name: func.name,
        body,
    }
}

fn lower_node<'ast>(
    decls: &HashMap<TypeID, &'ast StaticDeclaration>,
    node: &'ast AstNode<'ast>,
) -> HirNode {
    let value = match &node.value {
        AstNodeValue::Int(x) => HirNodeValue::Int(*x),
        AstNodeValue::Float(x) => HirNodeValue::Float(*x),
        AstNodeValue::Bool(x) => HirNodeValue::Bool(*x),
        AstNodeValue::Null => HirNodeValue::Null,
        AstNodeValue::CharLiteral(x) => HirNodeValue::CharLiteral(*x),
        AstNodeValue::StringLiteral(x) => HirNodeValue::StringLiteral(x.clone()),

        AstNodeValue::While(cond, body) => {
            // TODO: can you assign out of a while?
            let cond = lower_node_alloc(decls, cond);
            let body = lower_node_alloc(decls, body);
            HirNodeValue::While(cond, body)
        }
        AstNodeValue::Block(contents) => {
            let contents = contents
                .iter()
                .map(|node| lower_node(decls, node))
                .collect();

            HirNodeValue::Sequence(contents)
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

            HirNodeValue::If(condition, if_branch, else_branch)
        }

        AstNodeValue::TakeUnique(inner) => HirNodeValue::TakeUnique(lower_node_alloc(decls, inner)),
        AstNodeValue::TakeRef(inner) => HirNodeValue::TakeShared(lower_node_alloc(decls, inner)),
        AstNodeValue::Deref(inner) => HirNodeValue::Dereference(lower_node_alloc(decls, inner)),

        // Statement doesn't actually add a node - the inner expression
        // has what really counts
        AstNodeValue::Statement(inner) => return lower_node(decls, inner),

        AstNodeValue::Declaration(_lvalue, _type_hint, rvalue) => {
            let lvalue = Box::new(HirNode {
                id: NodeID::new(),
                value: HirNodeValue::VariableReference(node.id.into()),
                ty: rvalue.ty.get().expect("type filled in").clone(),
                provenance: Some(node.provenance.clone()),
            });
            let rvalue = lower_node_alloc(decls, rvalue);
            let statements = vec![
                HirNode::from_ast(
                    node,
                    HirNodeValue::Declaration(node.id.as_variable()),
                    rvalue.ty.clone(),
                ),
                HirNode {
                    id: NodeID::new(),
                    value: HirNodeValue::Assignment(lvalue, rvalue),
                    ty: ExpressionType::Void,
                    provenance: Some(node.provenance.clone()),
                },
            ];
            HirNodeValue::Sequence(statements)
        }
        AstNodeValue::Name { referenced_id, .. } => HirNodeValue::VariableReference(
            *referenced_id.get().expect("referenced ID to be filled in"),
        ),

        AstNodeValue::Return(inner) => HirNodeValue::Return(lower_node_alloc(decls, inner)),
        AstNodeValue::BinExpr(BinOp::Dot, left, right) => {
            let ExpressionType::InstanceOf(expr_ty) = fully_dereference(left.ty.get().unwrap())
            else {
                panic!("expected left side of dot to be declared type");
            };
            let AstNodeValue::Name { value: name, .. } = &right.value else {
                unreachable!()
            };
            if let Some(StaticDeclaration::Module(module)) = decls.get(expr_ty) {
                HirNodeValue::VariableReference(
                    module
                        .exports
                        .get(name)
                        .expect("module export to exist")
                        .id()
                        .into(),
                )
            } else {
                let left = lower_node_alloc(decls, left);
                HirNodeValue::Access(left, name.clone())
            }
        }
        AstNodeValue::BinExpr(BinOp::Index, left, right) => {
            let ty = left.ty.get().unwrap();
            let left = lower_node_alloc(decls, left);
            let right = lower_node_alloc(decls, right);
            match ty {
                ExpressionType::Collection(collection) => match collection {
                    CollectionType::Dict(_, _) => HirNodeValue::DictIndex(left, right),
                    CollectionType::Array(_) => HirNodeValue::ArrayIndex(left, right),
                },
                _ => unreachable!(),
            }
        }
        AstNodeValue::BinExpr(op, left, right) => {
            let left = lower_node_alloc(decls, left);
            let right = lower_node_alloc(decls, right);

            match op {
                BinOp::AddAssign => HirNodeValue::Assignment(
                    left.clone(),
                    Box::new(HirNode::from_ast_void(
                        node,
                        HirNodeValue::BinOp(HirBinOp::Add, left, right),
                    )),
                ),
                BinOp::SubtractAssign => HirNodeValue::Assignment(
                    left.clone(),
                    Box::new(HirNode::from_ast_void(
                        node,
                        HirNodeValue::BinOp(HirBinOp::Subtract, left, right),
                    )),
                ),
                BinOp::MultiplyAssign => HirNodeValue::Assignment(
                    left.clone(),
                    Box::new(HirNode::from_ast_void(
                        node,
                        HirNodeValue::BinOp(HirBinOp::Multiply, left, right),
                    )),
                ),
                BinOp::DivideAssign => HirNodeValue::Assignment(
                    left.clone(),
                    Box::new(HirNode::from_ast_void(
                        node,
                        HirNodeValue::BinOp(HirBinOp::Divide, left, right),
                    )),
                ),
                BinOp::Assignment => HirNodeValue::Assignment(left, right),

                BinOp::Add => HirNodeValue::BinOp(HirBinOp::Add, left, right),
                BinOp::Subtract => HirNodeValue::BinOp(HirBinOp::Subtract, left, right),
                BinOp::Multiply => HirNodeValue::BinOp(HirBinOp::Multiply, left, right),
                BinOp::Divide => HirNodeValue::BinOp(HirBinOp::Divide, left, right),
                BinOp::LessThan => HirNodeValue::BinOp(HirBinOp::LessThan, left, right),
                BinOp::GreaterThan => HirNodeValue::BinOp(HirBinOp::GreaterThan, left, right),
                BinOp::LessEqualThan => HirNodeValue::BinOp(HirBinOp::LessEqualThan, left, right),
                BinOp::GreaterEqualThan => {
                    HirNodeValue::BinOp(HirBinOp::GreaterEqualThan, left, right)
                }
                BinOp::EqualTo => HirNodeValue::BinOp(HirBinOp::EqualTo, left, right),
                BinOp::NotEquals => HirNodeValue::BinOp(HirBinOp::NotEquals, left, right),
                BinOp::Index | BinOp::Dot => unreachable!(),
            }
        }
        AstNodeValue::Call(func, params) => {
            let func = lower_node_alloc(decls, func);
            let params = params
                .iter()
                .map(|param| lower_node(decls, param))
                .collect();
            HirNodeValue::Call(func, params)
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
            HirNodeValue::StructLiteral(id.as_type(), fields)
        }
        AstNodeValue::DictLiteral(elements) => HirNodeValue::DictLiteral(
            elements
                .iter()
                .map(|(key, value)| (lower_node(decls, key), lower_node(decls, value)))
                .collect(),
        ),
        AstNodeValue::ArrayLiteral(arr) => {
            let arr = arr.iter().map(|elem| lower_node(decls, elem)).collect();
            HirNodeValue::ArrayLiteral(arr)
        }
        AstNodeValue::ArrayLiteralLength(elem, count) => {
            let elem = lower_node_alloc(decls, elem);
            let count = lower_node_alloc(decls, count);
            HirNodeValue::ArrayLiteralLength(elem, count)
        }

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

    HirNode::from_ast(node, value, node.ty.get().expect("type filled").clone())
}

fn lower_node_alloc<'ast>(
    decls: &HashMap<TypeID, &'ast StaticDeclaration>,
    node: &'ast AstNode<'ast>,
) -> Box<HirNode> {
    Box::new(lower_node(decls, node))
}
