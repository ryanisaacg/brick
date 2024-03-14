use std::collections::HashMap;

use crate::{
    id::{AnyID, FunctionID, NodeID, TypeID, VariableID},
    parser::AstNode,
    provenance::SourceRange,
    runtime::{info_for_function, RuntimeFunction},
    typecheck::{
        find_func, is_assignable_to, ExpressionType, PrimitiveType, StaticDeclaration,
        TypecheckedFile,
    },
};

mod auto_deref_dot;
mod auto_numeric_cast;
mod coroutines;
mod interface_conversion_pass;
mod lower;
mod rewrite_associated_functions;
mod simplify_sequence_expressions;
mod widen_null;

pub fn lower_module<'ast>(
    module: TypecheckedFile<'ast>,
    declarations: &HashMap<TypeID, &'ast StaticDeclaration>,
) -> HirModule {
    let mut module = lower::lower_module(module, declarations);

    // Important that this comes before ANY pass that uses the declarations
    coroutines::rewrite_generator_calls(&mut module);

    module.visit_mut(|expr: &mut _| rewrite_associated_functions::rewrite(declarations, expr));

    coroutines::rewrite_yields(&mut module);
    interface_conversion_pass::rewrite(&mut module, declarations);
    auto_deref_dot::auto_deref_dot(&mut module);
    auto_numeric_cast::auto_numeric_cast(&mut module, declarations);
    widen_null::widen_null(&mut module, declarations);

    simplify_sequence_expressions::simplify_sequence_assignments(&mut module);
    simplify_sequence_expressions::simplify_sequence_uses(&mut module, declarations);

    module
}

pub struct HirModule {
    pub top_level_statements: HirNode,
    // TODO: include imports, structs, and extern function declaration
    pub functions: Vec<HirFunction>,
}

impl HirModule {
    pub fn visit_mut(&mut self, mut callback: impl FnMut(&mut HirNode)) {
        self.top_level_statements.visit_mut_recursive(&mut callback);
        for func in self.functions.iter_mut() {
            func.body.visit_mut_recursive(&mut callback);
        }
    }

    pub fn visit(&self, mut callback: impl FnMut(Option<&HirNode>, &HirNode)) {
        self.top_level_statements
            .visit_recursive(None, &mut callback);
        for func in self.functions.iter() {
            func.body.visit_recursive(None, &mut callback);
        }
    }
}

#[derive(Clone, Debug)]
pub struct HirFunction {
    pub id: FunctionID,
    pub name: Option<String>,
    pub body: HirNode,
    generator: Option<(VariableID, ExpressionType)>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct HirNode {
    pub id: NodeID,
    pub value: HirNodeValue,
    pub ty: ExpressionType,
    pub provenance: Option<SourceRange>,
}

impl HirNode {
    pub fn dummy() -> HirNode {
        HirNode {
            id: NodeID::dummy(),
            value: HirNodeValue::Null,
            ty: ExpressionType::Null,
            provenance: None,
        }
    }

    pub fn autogenerated(value: HirNodeValue, ty: ExpressionType) -> HirNode {
        HirNode {
            id: NodeID::new(),
            value,
            ty,
            provenance: None,
        }
    }

    pub fn generated_with_id(id: NodeID, value: HirNodeValue, ty: ExpressionType) -> HirNode {
        HirNode {
            id,
            value,
            ty,
            provenance: None,
        }
    }

    pub fn from_ast(ast: &AstNode<'_>, value: HirNodeValue, ty: ExpressionType) -> HirNode {
        HirNode {
            id: ast.id,
            value,
            ty,
            provenance: Some(ast.provenance.clone()),
        }
    }

    pub fn from_ast_void(ast: &AstNode<'_>, value: HirNodeValue) -> HirNode {
        Self::from_ast(ast, value, ExpressionType::Void)
    }

    pub fn visit_mut(&mut self, mut callback: impl FnMut(&mut HirNode)) {
        self.visit_mut_recursive(&mut callback);
    }

    fn visit_mut_recursive(&mut self, callback: &mut impl FnMut(&mut HirNode)) {
        self.children_mut(|child| {
            child.visit_mut_recursive(callback);
        });
        callback(self);
    }

    pub fn visit(&self, mut callback: impl FnMut(Option<&HirNode>, &HirNode)) {
        self.visit_recursive(None, &mut callback);
    }

    fn visit_recursive(
        &self,
        parent: Option<&HirNode>,
        callback: &mut impl FnMut(Option<&HirNode>, &HirNode),
    ) {
        callback(parent, self);
        self.children(|child| {
            child.visit_recursive(Some(self), callback);
        });
    }

    pub fn children<'a>(&'a self, mut callback: impl FnMut(&'a HirNode)) {
        self.children_impl(None, |_, node| callback(node));
    }

    pub fn children_mut<'a>(&'a mut self, mut callback: impl FnMut(&'a mut HirNode)) {
        self.children_mut_impl(None, |_, node| callback(node));
    }

    // TODO: could use a better name
    pub fn walk_expected_types_for_children(
        &self,
        declarations: &HashMap<TypeID, &StaticDeclaration>,
        mut callback: impl FnMut(&ExpressionType, &HirNode),
    ) {
        self.children_impl(Some(declarations), |ty, node| {
            if let Some(ty) = ty {
                callback(ty, node);
            }
        });
    }

    pub fn walk_expected_types_for_children_mut(
        &mut self,
        declarations: &HashMap<TypeID, &StaticDeclaration>,
        mut callback: impl FnMut(&ExpressionType, &mut HirNode),
    ) {
        self.children_mut_impl(Some(declarations), |ty, node| {
            if let Some(ty) = ty {
                callback(ty, node);
            }
        });
    }

    fn children_impl<'a>(
        &'a self,
        declarations: Option<&HashMap<TypeID, &StaticDeclaration>>,
        mut callback: impl FnMut(Option<&ExpressionType>, &'a HirNode),
    ) {
        let callback = &mut callback;
        match &self.value {
            HirNodeValue::DictIndex(lhs, rhs) => {
                callback(None, lhs);
                // TODO: expect dictionary keys
                callback(None, rhs);
            }
            HirNodeValue::ArrayIndex(_, idx) => {
                callback(
                    Some(&ExpressionType::Primitive(PrimitiveType::PointerSize)),
                    idx,
                );
            }
            HirNodeValue::UnionLiteral(ty, variant, child) => {
                let variant_ty = declarations.map(|declarations| {
                    let StaticDeclaration::Union(ty) = declarations[ty as &TypeID] else {
                        unreachable!()
                    };
                    &ty.variants[variant as &String]
                });
                callback(variant_ty, child);
            }
            HirNodeValue::Arithmetic(_, lhs, rhs) | HirNodeValue::Comparison(_, lhs, rhs) => {
                if let Some(declarations) = declarations {
                    if is_assignable_to(declarations, None, &lhs.ty, &rhs.ty) {
                        callback(Some(&lhs.ty), rhs);
                        callback(None, lhs);
                    } else {
                        callback(Some(&rhs.ty), lhs);
                        callback(None, rhs);
                    }
                } else {
                    callback(None, lhs);
                    callback(None, rhs);
                }
            }
            HirNodeValue::NullCoalesce(lhs, rhs) => {
                callback(Some(&self.ty), rhs);
                callback(
                    Some(&ExpressionType::Nullable(Box::new(self.ty.clone()))),
                    lhs,
                );
            }
            HirNodeValue::BinaryLogical(_, lhs, rhs) => {
                callback(Some(&ExpressionType::Primitive(PrimitiveType::Bool)), lhs);
                callback(Some(&ExpressionType::Primitive(PrimitiveType::Bool)), rhs);
            }
            HirNodeValue::UnaryLogical(_, child) => {
                callback(Some(&ExpressionType::Primitive(PrimitiveType::Bool)), child)
            }
            HirNodeValue::VtableCall(vtable, fn_id, args) => {
                callback(None, vtable);
                let params = declarations.map(|declarations| {
                    let func = find_func(declarations, *fn_id).unwrap();
                    &func.params
                });
                for (i, arg) in args.iter().enumerate() {
                    callback(params.map(|params| &params[i]), arg);
                }
            }
            HirNodeValue::Call(lhs, args) => {
                let params = declarations.map(|declarations| {
                    let (ExpressionType::InstanceOf(id) | ExpressionType::ReferenceTo(id)) =
                        &lhs.ty
                    else {
                        unreachable!()
                    };
                    let Some(StaticDeclaration::Func(func)) = declarations.get(id) else {
                        unreachable!()
                    };
                    &func.params
                });
                for (i, arg) in args.iter().enumerate() {
                    callback(params.map(|params| &params[i]), arg);
                }
                callback(None, lhs);
            }
            HirNodeValue::RuntimeCall(runtime_fn, args) => {
                let StaticDeclaration::Func(func) = &info_for_function(runtime_fn).decl else {
                    unreachable!()
                };
                for (i, arg) in args.iter().enumerate() {
                    callback(Some(&func.params[i]), arg);
                }
            }
            HirNodeValue::Assignment(lhs, rhs) => {
                callback(Some(&lhs.ty), rhs);
                callback(None, lhs);
            }
            HirNodeValue::Yield(child) | HirNodeValue::Return(child) => {
                if let Some(child) = child {
                    // TODO: check return types
                    callback(None, child);
                }
            }
            // TODO: check return types of blocks
            HirNodeValue::If(cond, if_branch, else_branch) => {
                callback(Some(&ExpressionType::Primitive(PrimitiveType::Bool)), cond);
                callback(None, if_branch);
                if let Some(else_branch) = else_branch {
                    callback(None, else_branch);
                }
            }
            // TODO: check return types of blocks
            HirNodeValue::While(cond, body) => {
                callback(Some(&ExpressionType::Primitive(PrimitiveType::Bool)), cond);
                callback(None, body);
            }
            // TODO: check return types of blocks
            HirNodeValue::Sequence(children) => {
                for child in children.iter() {
                    callback(None, child);
                }
            }
            HirNodeValue::StructLiteral(ty_id, fields) => {
                let ty = declarations.map(|declarations| {
                    let Some(StaticDeclaration::Struct(ty)) = declarations.get(ty_id) else {
                        unreachable!();
                    };
                    ty
                });
                for (name, field) in fields.iter() {
                    callback(ty.map(|ty| &ty.fields[name]), field);
                }
            }
            HirNodeValue::DictLiteral(children) => {
                // TODO: check types for dicts
                for (key, value) in children.iter() {
                    callback(None, key);
                    callback(None, value);
                }
            }
            // TODO: heterogenous collections
            HirNodeValue::ArrayLiteral(children) => {
                for child in children.iter() {
                    callback(None, child);
                }
            }
            HirNodeValue::ArrayLiteralLength(_, len) => {
                callback(
                    Some(&ExpressionType::Primitive(PrimitiveType::PointerSize)),
                    len,
                );
            }
            HirNodeValue::Parameter(_, _)
            | HirNodeValue::VariableReference(_)
            | HirNodeValue::Declaration(_)
            | HirNodeValue::Int(_)
            | HirNodeValue::PointerSize(_)
            | HirNodeValue::Float(_)
            | HirNodeValue::Bool(_)
            | HirNodeValue::CharLiteral(_)
            | HirNodeValue::StringLiteral(_)
            | HirNodeValue::Null
            | HirNodeValue::GotoLabel(_) => {}
            HirNodeValue::Access(child, _)
            | HirNodeValue::NullableTraverse(child, _)
            | HirNodeValue::InterfaceAddress(child)
            | HirNodeValue::TakeUnique(child)
            | HirNodeValue::TakeShared(child)
            | HirNodeValue::Dereference(child)
            | HirNodeValue::NumericCast { value: child, .. }
            | HirNodeValue::MakeNullable(child)
            | HirNodeValue::StructToInterface { value: child, .. } => {
                callback(None, child);
            }
            HirNodeValue::GeneratorSuspend(yielded, _) => {
                // TODO: check types
                callback(None, yielded);
            }
            HirNodeValue::GeneratorResume(child) => {
                callback(None, child);
            }
            HirNodeValue::GeneratorCreate { args, .. } => {
                for arg in args.iter() {
                    callback(None, arg);
                }
            }
        }
    }

    fn children_mut_impl<'a>(
        &'a mut self,
        declarations: Option<&HashMap<TypeID, &StaticDeclaration>>,
        mut callback: impl FnMut(Option<&ExpressionType>, &'a mut HirNode),
    ) {
        let callback = &mut callback;
        match &mut self.value {
            HirNodeValue::DictIndex(lhs, rhs) => {
                callback(None, lhs);
                // TODO: expect dictionary keys
                callback(None, rhs);
            }
            HirNodeValue::ArrayIndex(_, idx) => {
                callback(
                    Some(&ExpressionType::Primitive(PrimitiveType::PointerSize)),
                    idx,
                );
            }
            HirNodeValue::UnionLiteral(ty, variant, child) => {
                let variant_ty = declarations.map(|declarations| {
                    let StaticDeclaration::Union(ty) = declarations[ty as &TypeID] else {
                        unreachable!()
                    };
                    &ty.variants[variant as &String]
                });
                callback(variant_ty, child);
            }
            HirNodeValue::Arithmetic(_, lhs, rhs) | HirNodeValue::Comparison(_, lhs, rhs) => {
                if let Some(declarations) = declarations {
                    if is_assignable_to(declarations, None, &lhs.ty, &rhs.ty) {
                        callback(Some(&lhs.ty), rhs);
                        callback(None, lhs);
                    } else {
                        callback(Some(&rhs.ty), lhs);
                        callback(None, rhs);
                    }
                } else {
                    callback(None, lhs);
                    callback(None, rhs);
                }
            }
            HirNodeValue::NullCoalesce(lhs, rhs) => {
                callback(Some(&self.ty), rhs);
                callback(
                    Some(&ExpressionType::Nullable(Box::new(self.ty.clone()))),
                    lhs,
                );
            }
            HirNodeValue::BinaryLogical(_, lhs, rhs) => {
                callback(Some(&ExpressionType::Primitive(PrimitiveType::Bool)), lhs);
                callback(Some(&ExpressionType::Primitive(PrimitiveType::Bool)), rhs);
            }
            HirNodeValue::UnaryLogical(_, child) => {
                callback(Some(&ExpressionType::Primitive(PrimitiveType::Bool)), child)
            }
            HirNodeValue::VtableCall(vtable, fn_id, args) => {
                callback(None, vtable);
                let params = declarations.map(|declarations| {
                    let func = find_func(declarations, *fn_id).unwrap();
                    &func.params
                });
                for (i, arg) in args.iter_mut().enumerate() {
                    callback(params.map(|params| &params[i]), arg);
                }
            }
            HirNodeValue::Call(lhs, args) => {
                let params = declarations.map(|declarations| match &lhs.ty {
                    ExpressionType::InstanceOf(id) | ExpressionType::ReferenceTo(id) => {
                        let Some(StaticDeclaration::Func(func)) = declarations.get(id) else {
                            unreachable!()
                        };
                        &func.params
                    }
                    ExpressionType::FunctionReference { parameters, .. } => parameters,
                    ty => unreachable!("illegal type: {:?}", ty),
                });
                for (i, arg) in args.iter_mut().enumerate() {
                    callback(params.map(|params| &params[i]), arg);
                }
                callback(None, lhs);
            }
            HirNodeValue::RuntimeCall(runtime_fn, args) => {
                let StaticDeclaration::Func(func) = &info_for_function(runtime_fn).decl else {
                    unreachable!()
                };
                for (i, arg) in args.iter_mut().enumerate() {
                    callback(Some(&func.params[i]), arg);
                }
            }
            HirNodeValue::Assignment(lhs, rhs) => {
                callback(Some(&lhs.ty), rhs);
                callback(None, lhs);
            }
            HirNodeValue::Yield(child) | HirNodeValue::Return(child) => {
                if let Some(child) = child {
                    // TODO: check return types
                    callback(None, child);
                }
            }
            // TODO: check return types of blocks
            HirNodeValue::If(cond, if_branch, else_branch) => {
                callback(Some(&ExpressionType::Primitive(PrimitiveType::Bool)), cond);
                callback(None, if_branch);
                if let Some(else_branch) = else_branch {
                    callback(None, else_branch);
                }
            }
            // TODO: check return types of blocks
            HirNodeValue::While(cond, body) => {
                callback(Some(&ExpressionType::Primitive(PrimitiveType::Bool)), cond);
                callback(None, body);
            }
            // TODO: check return types of blocks
            HirNodeValue::Sequence(children) => {
                for child in children.iter_mut() {
                    callback(None, child);
                }
            }
            HirNodeValue::StructLiteral(ty_id, fields) => {
                let ty = declarations.map(|declarations| {
                    let Some(StaticDeclaration::Struct(ty)) = declarations.get(ty_id) else {
                        unreachable!();
                    };
                    ty
                });
                for (name, field) in fields.iter_mut() {
                    callback(ty.map(|ty| &ty.fields[name]), field);
                }
            }
            HirNodeValue::DictLiteral(children) => {
                // TODO: check types for dicts
                for (key, value) in children.iter_mut() {
                    callback(None, key);
                    callback(None, value);
                }
            }
            // TODO: heterogenous collections
            HirNodeValue::ArrayLiteral(children) => {
                for child in children.iter_mut() {
                    callback(None, child);
                }
            }
            HirNodeValue::ArrayLiteralLength(_, len) => {
                callback(
                    Some(&ExpressionType::Primitive(PrimitiveType::PointerSize)),
                    len,
                );
            }
            HirNodeValue::Parameter(_, _)
            | HirNodeValue::VariableReference(_)
            | HirNodeValue::Declaration(_)
            | HirNodeValue::Int(_)
            | HirNodeValue::PointerSize(_)
            | HirNodeValue::Float(_)
            | HirNodeValue::Bool(_)
            | HirNodeValue::CharLiteral(_)
            | HirNodeValue::StringLiteral(_)
            | HirNodeValue::Null
            | HirNodeValue::GotoLabel(_) => {}
            HirNodeValue::Access(child, _)
            | HirNodeValue::NullableTraverse(child, _)
            | HirNodeValue::InterfaceAddress(child)
            | HirNodeValue::TakeUnique(child)
            | HirNodeValue::TakeShared(child)
            | HirNodeValue::Dereference(child)
            | HirNodeValue::NumericCast { value: child, .. }
            | HirNodeValue::MakeNullable(child)
            | HirNodeValue::StructToInterface { value: child, .. } => {
                callback(None, child);
            }
            HirNodeValue::GeneratorSuspend(yielded, _) => {
                // TODO: check types
                callback(None, yielded);
            }
            HirNodeValue::GeneratorResume(child) => {
                callback(None, child);
            }
            HirNodeValue::GeneratorCreate { args, .. } => {
                for arg in args.iter_mut() {
                    callback(None, arg);
                }
            }
        }
    }
}

// TODO: should struct fields also be referred to via opaque IDs?

#[derive(Clone, Debug, PartialEq)]
pub enum HirNodeValue {
    /// Give the Nth parameter the given ID
    Parameter(usize, VariableID),
    VariableReference(AnyID),
    Declaration(VariableID),

    Call(Box<HirNode>, Vec<HirNode>),
    // TODO: break this up into Union Access and Struct Access?
    Access(Box<HirNode>, String),
    NullableTraverse(Box<HirNode>, Vec<String>),
    Assignment(Box<HirNode>, Box<HirNode>),
    ArrayIndex(Box<HirNode>, Box<HirNode>),
    DictIndex(Box<HirNode>, Box<HirNode>),
    Arithmetic(ArithmeticOp, Box<HirNode>, Box<HirNode>),
    Comparison(ComparisonOp, Box<HirNode>, Box<HirNode>),
    BinaryLogical(BinaryLogicalOp, Box<HirNode>, Box<HirNode>),
    NullCoalesce(Box<HirNode>, Box<HirNode>),
    UnaryLogical(UnaryLogicalOp, Box<HirNode>),

    Return(Option<Box<HirNode>>),
    /// Desugared out of existence, but hard to do before lowering to HIR
    Yield(Option<Box<HirNode>>),

    Int(i64),
    Float(f64),
    Bool(bool),
    PointerSize(usize),
    Null,
    CharLiteral(char),
    StringLiteral(String),
    NumericCast {
        value: Box<HirNode>,
        from: PrimitiveType,
        to: PrimitiveType,
    },

    TakeUnique(Box<HirNode>),
    TakeShared(Box<HirNode>),
    Dereference(Box<HirNode>),

    /// Like a Block in that it's a collection of nodes, but the IR
    /// doesn't care about scoping or expressions
    Sequence(Vec<HirNode>),

    // Expressions
    If(Box<HirNode>, Box<HirNode>, Option<Box<HirNode>>),
    While(Box<HirNode>, Box<HirNode>),
    StructLiteral(TypeID, HashMap<String, HirNode>),
    UnionLiteral(TypeID, String, Box<HirNode>),
    ArrayLiteral(Vec<HirNode>),
    ArrayLiteralLength(Box<HirNode>, Box<HirNode>),
    DictLiteral(Vec<(HirNode, HirNode)>),

    // Instructions only generated by IR passes
    /// Look up the given virtual function ID in the LHS vtable
    VtableCall(Box<HirNode>, FunctionID, Vec<HirNode>),
    RuntimeCall(RuntimeFunction, Vec<HirNode>),
    InterfaceAddress(Box<HirNode>),
    StructToInterface {
        value: Box<HirNode>,
        vtable: HashMap<FunctionID, FunctionID>,
    },
    MakeNullable(Box<HirNode>),
    // Generator instructions, all created during HIR
    GeneratorSuspend(Box<HirNode>, usize),
    GotoLabel(usize),
    GeneratorResume(Box<HirNode>),
    GeneratorCreate {
        generator_function: FunctionID,
        args: Vec<HirNode>,
    },
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum ArithmeticOp {
    Add,
    Subtract,
    Multiply,
    Divide,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum ComparisonOp {
    LessThan,
    GreaterThan,
    LessEqualThan,
    GreaterEqualThan,
    EqualTo,
    NotEquals,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum BinaryLogicalOp {
    BooleanAnd,
    BooleanOr,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum UnaryLogicalOp {
    BooleanNot,
}
