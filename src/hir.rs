use std::collections::HashMap;

use crate::{
    id::{AnyID, FunctionID, NodeID, TypeID, VariableID},
    parser::AstNode,
    provenance::SourceRange,
    runtime::{info_for_function, RuntimeFunction},
    typecheck::{
        find_func, fully_dereference, is_assignable_to, CollectionType, ExpressionType,
        PrimitiveType, StaticDeclaration, StructType, TypecheckedFile, UnionType,
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

    module.visit_mut(|expr: &mut _| rewrite_associated_functions::rewrite(declarations, expr));
    coroutines::take_coroutine_references(&mut module);

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
    pub name: String,
    pub body: HirNode,
    pub is_generator: bool,
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

    pub fn children_mut<'a>(&'a mut self, mut callback: impl FnMut(&'a mut HirNode)) {
        self.children_recursive_mut(&mut callback);
    }

    fn children_recursive_mut<'a>(&'a mut self, callback: &mut impl FnMut(&'a mut HirNode)) {
        match &mut self.value {
            HirNodeValue::Parameter(_, _)
            | HirNodeValue::VariableReference(_)
            | HirNodeValue::Declaration(_)
            | HirNodeValue::Int(_)
            | HirNodeValue::Float(_)
            | HirNodeValue::Bool(_)
            | HirNodeValue::CharLiteral(_)
            | HirNodeValue::StringLiteral(_)
            | HirNodeValue::Null
            | HirNodeValue::Yield(None)
            | HirNodeValue::Return(None) => {}
            HirNodeValue::Call(lhs, params)
            | HirNodeValue::VtableCall(lhs, _, params)
            | HirNodeValue::GeneratorResume(lhs, params) => {
                callback(lhs);
                for param in params.iter_mut() {
                    callback(param);
                }
            }
            HirNodeValue::Access(child, _)
            | HirNodeValue::NullableTraverse(child, _)
            | HirNodeValue::UnaryLogical(_, child)
            | HirNodeValue::UnionLiteral(_, _, child)
            | HirNodeValue::InterfaceAddress(child)
            | HirNodeValue::TakeUnique(child)
            | HirNodeValue::TakeShared(child)
            | HirNodeValue::Dereference(child)
            | HirNodeValue::ArrayLiteralLength(child, _)
            | HirNodeValue::Return(Some(child))
            | HirNodeValue::Yield(Some(child))
            | HirNodeValue::NumericCast { value: child, .. }
            | HirNodeValue::MakeNullable(child)
            | HirNodeValue::StructToInterface { value: child, .. } => {
                callback(child);
            }
            HirNodeValue::DictLiteral(children) => {
                for (key, value) in children.iter_mut() {
                    callback(key);
                    callback(value);
                }
            }
            HirNodeValue::Assignment(lhs, rhs)
            | HirNodeValue::ArrayIndex(lhs, rhs)
            | HirNodeValue::DictIndex(lhs, rhs)
            | HirNodeValue::While(lhs, rhs)
            | HirNodeValue::Arithmetic(_, lhs, rhs)
            | HirNodeValue::Comparison(_, lhs, rhs)
            | HirNodeValue::BinaryLogical(_, lhs, rhs)
            | HirNodeValue::NullCoalesce(lhs, rhs) => {
                callback(lhs);
                callback(rhs);
            }
            HirNodeValue::Sequence(children)
            | HirNodeValue::ArrayLiteral(children)
            | HirNodeValue::RuntimeCall(_, children) => {
                for child in children.iter_mut() {
                    callback(child);
                }
            }
            HirNodeValue::If(cond, if_branch, else_branch) => {
                callback(cond);
                callback(if_branch);
                if let Some(else_branch) = else_branch {
                    callback(else_branch);
                }
            }
            HirNodeValue::StructLiteral(_, fields) => {
                for field in fields.values_mut() {
                    callback(field);
                }
            }
        }
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
        self.children_recursive(&mut callback);
    }

    fn children_recursive<'a>(&'a self, callback: &mut impl FnMut(&'a HirNode)) {
        match &self.value {
            HirNodeValue::Parameter(_, _)
            | HirNodeValue::VariableReference(_)
            | HirNodeValue::Declaration(_)
            | HirNodeValue::Int(_)
            | HirNodeValue::Float(_)
            | HirNodeValue::Bool(_)
            | HirNodeValue::CharLiteral(_)
            | HirNodeValue::StringLiteral(_)
            | HirNodeValue::Null
            | HirNodeValue::Yield(None)
            | HirNodeValue::Return(None) => {}
            HirNodeValue::Call(lhs, params)
            | HirNodeValue::VtableCall(lhs, _, params)
            | HirNodeValue::GeneratorResume(lhs, params) => {
                callback(lhs);
                for param in params.iter() {
                    callback(param);
                }
            }
            HirNodeValue::Access(child, _)
            | HirNodeValue::NullableTraverse(child, _)
            | HirNodeValue::UnionLiteral(_, _, child)
            | HirNodeValue::InterfaceAddress(child)
            | HirNodeValue::TakeUnique(child)
            | HirNodeValue::TakeShared(child)
            | HirNodeValue::Dereference(child)
            | HirNodeValue::UnaryLogical(_, child)
            | HirNodeValue::ArrayLiteralLength(child, _)
            | HirNodeValue::Yield(Some(child))
            | HirNodeValue::Return(Some(child))
            | HirNodeValue::NumericCast { value: child, .. }
            | HirNodeValue::MakeNullable(child)
            | HirNodeValue::StructToInterface { value: child, .. } => {
                callback(child);
            }
            HirNodeValue::Assignment(lhs, rhs)
            | HirNodeValue::ArrayIndex(lhs, rhs)
            | HirNodeValue::DictIndex(lhs, rhs)
            | HirNodeValue::While(lhs, rhs)
            | HirNodeValue::Arithmetic(_, lhs, rhs)
            | HirNodeValue::Comparison(_, lhs, rhs)
            | HirNodeValue::BinaryLogical(_, lhs, rhs)
            | HirNodeValue::NullCoalesce(lhs, rhs) => {
                callback(lhs);
                callback(rhs);
            }
            HirNodeValue::Sequence(children)
            | HirNodeValue::ArrayLiteral(children)
            | HirNodeValue::RuntimeCall(_, children) => {
                for child in children.iter() {
                    callback(child);
                }
            }
            HirNodeValue::If(cond, if_branch, else_branch) => {
                callback(cond);
                callback(if_branch);
                if let Some(else_branch) = else_branch {
                    callback(else_branch);
                }
            }
            HirNodeValue::StructLiteral(_, fields) => {
                for field in fields.values() {
                    callback(field);
                }
            }
            HirNodeValue::DictLiteral(children) => {
                for (key, value) in children.iter() {
                    callback(key);
                    callback(value);
                }
            }
        }
    }

    // TODO: could use a better name
    pub fn walk_expected_types_for_children(
        &self,
        declarations: &HashMap<TypeID, &StaticDeclaration>,
        mut callback: impl FnMut(&ExpressionType, &HirNode),
    ) {
        let callback = &mut callback;
        match &self.value {
            HirNodeValue::Int(_)
            | HirNodeValue::Float(_)
            | HirNodeValue::Bool(_)
            | HirNodeValue::Null
            | HirNodeValue::Parameter(_, _)
            | HirNodeValue::VariableReference(_)
            | HirNodeValue::Access(_, _)
            | HirNodeValue::NullableTraverse(_, _)
            | HirNodeValue::CharLiteral(_)
            | HirNodeValue::StringLiteral(_)
            | HirNodeValue::TakeUnique(_)
            | HirNodeValue::TakeShared(_)
            | HirNodeValue::Dereference(_)
            | HirNodeValue::InterfaceAddress(_)
            | HirNodeValue::NumericCast { .. }
            | HirNodeValue::MakeNullable(_)
            | HirNodeValue::StructToInterface { .. }
            | HirNodeValue::Declaration(_) => {}
            HirNodeValue::ArrayIndex(_, idx) => {
                callback(&ExpressionType::Primitive(PrimitiveType::PointerSize), idx);
            }
            HirNodeValue::UnionLiteral(ty, variant, child) => {
                let StaticDeclaration::Union(ty) = declarations[ty] else {
                    unreachable!()
                };
                let variant_ty = &ty.variants[variant];
                callback(variant_ty, child);
            }
            HirNodeValue::Arithmetic(_, lhs, rhs) | HirNodeValue::Comparison(_, lhs, rhs) => {
                if is_assignable_to(declarations, None, &lhs.ty, &rhs.ty) {
                    callback(&lhs.ty, rhs);
                } else {
                    callback(&rhs.ty, lhs);
                }
            }
            HirNodeValue::NullCoalesce(lhs, rhs) => {
                callback(&self.ty, rhs);
                callback(&ExpressionType::Nullable(Box::new(self.ty.clone())), lhs);
            }
            HirNodeValue::BinaryLogical(_, lhs, rhs) => {
                callback(&ExpressionType::Primitive(PrimitiveType::Bool), lhs);
                callback(&ExpressionType::Primitive(PrimitiveType::Bool), rhs);
            }
            HirNodeValue::UnaryLogical(_, child) => {
                callback(&ExpressionType::Primitive(PrimitiveType::Bool), child)
            }
            HirNodeValue::VtableCall(_, fn_id, params) => {
                let func = find_func(declarations, *fn_id).unwrap();
                for (i, ty) in func.params.iter().enumerate() {
                    callback(ty, &params[i]);
                }
            }
            HirNodeValue::Call(lhs, params) => {
                let ExpressionType::InstanceOf(id) = &lhs.ty else {
                    unreachable!()
                };
                let Some(StaticDeclaration::Func(func)) = declarations.get(id) else {
                    unreachable!()
                };
                for (i, ty) in func.params.iter().enumerate() {
                    callback(ty, &params[i]);
                }
            }
            HirNodeValue::GeneratorResume(lhs, params) => {
                let ExpressionType::Generator { param_ty, .. } = &lhs.ty else {
                    unreachable!()
                };
                if param_ty.as_ref() != &ExpressionType::Void {
                    callback(param_ty.as_ref(), params.last().unwrap());
                }
            }
            HirNodeValue::RuntimeCall(runtime_fn, params) => {
                let StaticDeclaration::Func(func) = &info_for_function(runtime_fn).decl else {
                    unreachable!()
                };
                for (i, ty) in func.params.iter().enumerate() {
                    callback(ty, &params[i]);
                }
            }
            HirNodeValue::Assignment(lhs, rhs) => {
                callback(&lhs.ty, rhs);
            }
            HirNodeValue::Yield(_) | HirNodeValue::Return(_) => {
                // TODO: check return types
            }
            HirNodeValue::If(_, _, _) | HirNodeValue::While(_, _) | HirNodeValue::Sequence(_) => {
                // TODO: check return types
            }
            HirNodeValue::StructLiteral(ty_id, fields) => {
                let Some(StaticDeclaration::Struct(ty)) = declarations.get(ty_id) else {
                    unreachable!();
                };
                for (name, field) in fields.iter() {
                    callback(&ty.fields[name], field);
                }
            }
            // TODO: heterogenous collections
            HirNodeValue::ArrayLiteral(_) => {}
            HirNodeValue::ArrayLiteralLength(_, len) => {
                callback(&ExpressionType::Primitive(PrimitiveType::PointerSize), len);
            }
            HirNodeValue::DictIndex(_, _) => todo!(),
            HirNodeValue::DictLiteral(_) => todo!(),
        }
    }

    pub fn walk_expected_types_for_children_mut(
        &mut self,
        declarations: &HashMap<TypeID, &StaticDeclaration>,
        mut callback: impl FnMut(&ExpressionType, &mut HirNode),
    ) {
        let callback = &mut callback;
        match &mut self.value {
            HirNodeValue::Int(_)
            | HirNodeValue::Float(_)
            | HirNodeValue::Bool(_)
            | HirNodeValue::Null
            | HirNodeValue::Parameter(_, _)
            | HirNodeValue::VariableReference(_)
            | HirNodeValue::Access(_, _)
            | HirNodeValue::NullableTraverse(_, _)
            | HirNodeValue::CharLiteral(_)
            | HirNodeValue::StringLiteral(_)
            | HirNodeValue::TakeUnique(_)
            | HirNodeValue::TakeShared(_)
            | HirNodeValue::Dereference(_)
            | HirNodeValue::InterfaceAddress(_)
            | HirNodeValue::NumericCast { .. }
            | HirNodeValue::MakeNullable(_)
            | HirNodeValue::StructToInterface { .. }
            | HirNodeValue::Declaration(_) => {}
            HirNodeValue::ArrayIndex(_, idx) => {
                callback(&ExpressionType::Primitive(PrimitiveType::PointerSize), idx);
            }
            HirNodeValue::DictIndex(dict, idx) => {
                let ExpressionType::Collection(CollectionType::Dict(key_ty, _)) = &dict.ty else {
                    unreachable!()
                };
                callback(key_ty, idx);
            }
            HirNodeValue::Arithmetic(_, lhs, rhs) | HirNodeValue::Comparison(_, lhs, rhs) => {
                if is_assignable_to(declarations, None, &lhs.ty, &rhs.ty) {
                    callback(&lhs.ty, rhs);
                } else {
                    callback(&rhs.ty, lhs);
                }
            }
            HirNodeValue::NullCoalesce(lhs, rhs) => {
                callback(&self.ty, rhs);
                callback(&ExpressionType::Nullable(Box::new(self.ty.clone())), lhs);
            }
            HirNodeValue::BinaryLogical(_, lhs, rhs) => {
                callback(&ExpressionType::Primitive(PrimitiveType::Bool), lhs);
                callback(&ExpressionType::Primitive(PrimitiveType::Bool), rhs);
            }
            HirNodeValue::UnaryLogical(_, child) => {
                callback(&ExpressionType::Primitive(PrimitiveType::Bool), child)
            }
            HirNodeValue::UnionLiteral(ty, variant, child) => {
                let StaticDeclaration::Union(ty) = declarations[ty] else {
                    unreachable!()
                };
                let variant_ty = &ty.variants[variant];
                callback(variant_ty, child);
            }
            HirNodeValue::VtableCall(_, fn_id, params) => {
                let func = find_func(declarations, *fn_id).unwrap();
                for (i, ty) in func.params.iter().enumerate() {
                    callback(ty, &mut params[i]);
                }
            }
            HirNodeValue::Call(lhs, params) => {
                let (ExpressionType::InstanceOf(id) | ExpressionType::ReferenceTo(id)) = &lhs.ty
                else {
                    unreachable!()
                };
                let Some(StaticDeclaration::Func(func)) = declarations.get(id) else {
                    unreachable!()
                };
                for (i, ty) in func.params.iter().enumerate() {
                    callback(ty, &mut params[i]);
                }
            }
            HirNodeValue::GeneratorResume(lhs, params) => {
                let ExpressionType::Generator { param_ty, .. } = fully_dereference(&lhs.ty) else {
                    unreachable!()
                };
                if param_ty.as_ref() != &ExpressionType::Void {
                    callback(param_ty.as_ref(), params.last_mut().unwrap());
                }
            }
            HirNodeValue::RuntimeCall(runtime_fn, params) => {
                let StaticDeclaration::Func(func) = &info_for_function(runtime_fn).decl else {
                    unreachable!()
                };
                for (i, ty) in func.params.iter().enumerate() {
                    callback(ty, &mut params[i]);
                }
            }
            HirNodeValue::Assignment(lhs, rhs) => {
                callback(&lhs.ty, rhs);
            }
            HirNodeValue::Yield(_) | HirNodeValue::Return(_) => {
                // TODO: check return types
            }
            HirNodeValue::If(_, _, _) | HirNodeValue::While(_, _) | HirNodeValue::Sequence(_) => {
                // TODO: check return types
            }
            HirNodeValue::StructLiteral(ty_id, fields) => {
                let (StaticDeclaration::Struct(StructType {
                    fields: ty_fields, ..
                })
                | StaticDeclaration::Union(UnionType {
                    variants: ty_fields,
                    ..
                })) = declarations.get(ty_id).unwrap()
                else {
                    unreachable!();
                };
                for (name, field) in fields.iter_mut() {
                    callback(&ty_fields[name], field);
                }
            }
            // TODO: heterogenous collections
            HirNodeValue::ArrayLiteral(_) | HirNodeValue::DictLiteral(_) => {}
            HirNodeValue::ArrayLiteralLength(_, len) => {
                callback(&ExpressionType::Primitive(PrimitiveType::PointerSize), len);
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
    GeneratorResume(Box<HirNode>, Vec<HirNode>),
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
    Yield(Option<Box<HirNode>>),

    Int(i64),
    Float(f64),
    Bool(bool),
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
    VtableCall(Box<HirNode>, FunctionID, Vec<HirNode>),
    RuntimeCall(RuntimeFunction, Vec<HirNode>),
    InterfaceAddress(Box<HirNode>),
    StructToInterface {
        value: Box<HirNode>,
        vtable: HashMap<FunctionID, FunctionID>,
    },
    MakeNullable(Box<HirNode>),
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
