use std::collections::HashMap;

use bytemuck::Zeroable;

use crate::{
    declaration_context::{IntrinsicFunction, Module, TypeID},
    hir::{
        ArithmeticOp, BinaryLogicalOp, ComparisonOp, GeneratorProperties, HirFunction, HirNode,
        HirNodeValue, UnaryLogicalOp,
    },
    id::{AnyID, FunctionID, RegisterID, VariableID},
    provenance::SourceRange,
    typecheck::{
        shallow_dereference, CollectionType, ExpressionType, PrimitiveType, TypeDeclaration,
    },
};

mod generator_local_storage;

#[derive(Debug)]
pub struct LinearFunction {
    pub id: FunctionID,
    pub params: Vec<PhysicalType>,
    pub returns: Option<PhysicalType>,
    pub body: Vec<LinearNode>,
}

#[derive(Clone, Debug)]
pub struct LinearNode {
    pub value: LinearNodeValue,
    pub provenance: Option<SourceRange>,
}

impl LinearNode {
    pub fn new(value: LinearNodeValue) -> LinearNode {
        LinearNode {
            value,
            provenance: None,
        }
    }

    fn if_node(
        cond: LinearNode,
        if_block: Vec<LinearNode>,
        else_block: Option<Vec<LinearNode>>,
        provenance: Option<SourceRange>,
    ) -> LinearNode {
        LinearNode {
            value: LinearNodeValue::If(Box::new(cond), if_block, else_block, None),
            provenance,
        }
    }

    fn if_node_value(
        cond: LinearNode,
        if_block: Vec<LinearNode>,
        else_block: Option<Vec<LinearNode>>,
        provenance: Option<SourceRange>,
        ty: PhysicalType,
    ) -> LinearNode {
        LinearNode {
            value: LinearNodeValue::If(Box::new(cond), if_block, else_block, Some(ty)),
            provenance,
        }
    }

    fn ptr_arithmetic(op: ArithmeticOp, lhs: LinearNode, rhs: LinearNode) -> LinearNode {
        LinearNode {
            value: LinearNodeValue::Arithmetic(
                op,
                PhysicalPrimitive::PointerSize,
                Box::new(lhs),
                Box::new(rhs),
            ),
            provenance: None,
        }
    }

    fn ptr_comparison(op: ComparisonOp, lhs: LinearNode, rhs: LinearNode) -> LinearNode {
        LinearNode {
            value: LinearNodeValue::Comparison(
                op,
                PhysicalPrimitive::PointerSize,
                Box::new(lhs),
                Box::new(rhs),
            ),
            provenance: None,
        }
    }

    fn size(size: usize) -> LinearNode {
        LinearNode {
            value: LinearNodeValue::Size(size),
            provenance: None,
        }
    }

    fn heap_alloc_const(size: usize) -> LinearNode {
        LinearNode {
            value: LinearNodeValue::RuntimeCall(
                RuntimeFunction::Alloc,
                vec![LinearNode::size(size)],
            ),
            provenance: None,
        }
    }

    fn heap_alloc_var(size: LinearNode) -> LinearNode {
        LinearNode {
            value: LinearNodeValue::RuntimeCall(RuntimeFunction::Alloc, vec![size]),
            provenance: None,
        }
    }

    fn call_runtime(func: RuntimeFunction, args: Vec<LinearNode>) -> LinearNode {
        LinearNode {
            value: LinearNodeValue::RuntimeCall(func, args),
            provenance: None,
        }
    }

    fn write_register(id: RegisterID, value: LinearNode) -> LinearNode {
        LinearNode {
            value: LinearNodeValue::WriteRegister(id, Box::new(value)),
            provenance: None,
        }
    }

    fn write_multi_register(value: LinearNode, ids: Vec<Option<RegisterID>>) -> LinearNode {
        LinearNode {
            value: LinearNodeValue::WriteRegistersSplitting(Box::new(value), ids),
            provenance: None,
        }
    }

    fn read_register(id: RegisterID) -> LinearNode {
        LinearNode {
            value: LinearNodeValue::ReadRegister(id),
            provenance: None,
        }
    }

    fn kill_register(id: RegisterID) -> LinearNode {
        LinearNode {
            value: LinearNodeValue::KillRegister(id),
            provenance: None,
        }
    }

    fn read_memory(location: LinearNode, offset: usize, ty: PhysicalType) -> LinearNode {
        LinearNode {
            value: LinearNodeValue::ReadMemory {
                location: Box::new(location),
                offset,
                ty,
            },
            provenance: None,
        }
    }

    fn write_memory(
        location: LinearNode,
        offset: usize,
        ty: PhysicalType,
        value: LinearNode,
    ) -> LinearNode {
        LinearNode {
            value: LinearNodeValue::WriteMemory {
                location: Box::new(location),
                offset,
                ty,
                value: Box::new(value),
            },
            provenance: None,
        }
    }

    fn abort() -> LinearNode {
        LinearNode {
            value: LinearNodeValue::Abort,
            provenance: None,
        }
    }

    #[allow(dead_code)]
    fn debug(inner: LinearNode) -> LinearNode {
        LinearNode {
            value: LinearNodeValue::Debug(Box::new(inner)),
            provenance: None,
        }
    }

    fn bool_value(val: bool) -> LinearNode {
        LinearNode {
            value: LinearNodeValue::Byte(if val { 1 } else { 0 }),
            provenance: None,
        }
    }

    pub fn ty(
        &self,
        function_returns: &HashMap<FunctionID, Option<PhysicalType>>,
    ) -> Option<PhysicalType> {
        match &self.value {
            LinearNodeValue::ConstantDataAddress(_)
            | LinearNodeValue::VariableLocation(_)
            | LinearNodeValue::ReadRegister(_)
            | LinearNodeValue::Size(_) => {
                Some(PhysicalType::Primitive(PhysicalPrimitive::PointerSize))
            }
            LinearNodeValue::WriteMemory { .. }
            | LinearNodeValue::VariableInit(_, _)
            | LinearNodeValue::VariableDestroy(_)
            | LinearNodeValue::Break
            | LinearNodeValue::Loop(_)
            | LinearNodeValue::Abort
            | LinearNodeValue::Goto(_)
            | LinearNodeValue::GotoLabel(_)
            | LinearNodeValue::Switch { .. }
            | LinearNodeValue::WriteRegister(_, _)
            | LinearNodeValue::WriteRegistersSplitting(_, _)
            | LinearNodeValue::Sequence(_)
            | LinearNodeValue::Discard(_, _)
            | LinearNodeValue::KillRegister(_) => None,
            LinearNodeValue::If(_, _, _, ty) => ty.clone(),
            LinearNodeValue::Parameter(ty, _) | LinearNodeValue::ReadMemory { ty, .. } => {
                Some(ty.clone())
            }
            LinearNodeValue::Call(func, _) => function_returns.get(func).and_then(|x| x.clone()),
            LinearNodeValue::IndirectCall(fn_id, _, _) => {
                function_returns.get(fn_id).and_then(|x| x.clone())
            }
            LinearNodeValue::RuntimeCall(func, _) => match func {
                RuntimeFunction::StringConcat => {
                    Some(PhysicalType::Collection(PhysicalCollection::String))
                }
                RuntimeFunction::Memcpy | RuntimeFunction::Dealloc => None,
                RuntimeFunction::Realloc | RuntimeFunction::Alloc => {
                    Some(PhysicalType::Primitive(PhysicalPrimitive::PointerSize))
                }
            },
            LinearNodeValue::Return(child) => {
                child.as_ref().and_then(|child| child.ty(function_returns))
            }
            LinearNodeValue::Cast { to: prim, .. } | LinearNodeValue::Arithmetic(_, prim, _, _) => {
                Some(PhysicalType::Primitive(*prim))
            }
            LinearNodeValue::Comparison(_, _, _, _)
            | LinearNodeValue::BinaryLogical(_, _, _)
            | LinearNodeValue::Byte(_)
            | LinearNodeValue::CharLiteral(_)
            | LinearNodeValue::UnaryLogical(_, _) => {
                Some(PhysicalType::Primitive(PhysicalPrimitive::Byte))
            }
            LinearNodeValue::Int(_) => Some(PhysicalType::Primitive(PhysicalPrimitive::Int32)),
            LinearNodeValue::Float(_) => Some(PhysicalType::Primitive(PhysicalPrimitive::Float32)),
            LinearNodeValue::FunctionID(_) => {
                Some(PhysicalType::Primitive(PhysicalPrimitive::FunctionPointer))
            }
            LinearNodeValue::Debug(child) => child.ty(function_returns),
        }
    }
}

// TODO: split up between 'statement' and 'expression' to reduce need for boxing?
#[derive(Clone, Debug)]
pub enum LinearNodeValue {
    /// Each parameter may only appear once in a given method body
    Parameter(PhysicalType, usize),
    // TODO: do the variables obsolete the registers?
    VariableInit(VariableID, PhysicalType),
    VariableDestroy(VariableID),
    VariableLocation(VariableID),
    ReadMemory {
        location: Box<LinearNode>,
        offset: usize,
        ty: PhysicalType,
    },
    WriteMemory {
        location: Box<LinearNode>,
        offset: usize,
        ty: PhysicalType,
        value: Box<LinearNode>,
    },
    ConstantDataAddress(usize),
    Discard(Box<LinearNode>, PhysicalType),

    // Control flow
    Call(FunctionID, Vec<LinearNode>),
    IndirectCall(FunctionID, Box<LinearNode>, Vec<LinearNode>),
    RuntimeCall(RuntimeFunction, Vec<LinearNode>),
    Return(Option<Box<LinearNode>>),
    If(
        Box<LinearNode>,
        Vec<LinearNode>,
        Option<Vec<LinearNode>>,
        Option<PhysicalType>,
    ),
    // TODO: labelled breaks?
    Break,
    Loop(Vec<LinearNode>),
    // TODO: stack unwind?
    Abort,
    Goto(Box<LinearNode>),
    GotoLabel(usize),
    Switch {
        value: Box<LinearNode>,
        cases: Vec<LinearNode>,
    },

    Sequence(Vec<LinearNode>),
    WriteRegister(RegisterID, Box<LinearNode>),
    WriteRegistersSplitting(Box<LinearNode>, Vec<Option<RegisterID>>),
    ReadRegister(RegisterID),
    // TODO: automatically?
    KillRegister(RegisterID),

    Arithmetic(
        ArithmeticOp,
        PhysicalPrimitive,
        Box<LinearNode>,
        Box<LinearNode>,
    ),
    Comparison(
        ComparisonOp,
        PhysicalPrimitive,
        Box<LinearNode>,
        Box<LinearNode>,
    ),
    BinaryLogical(BinaryLogicalOp, Box<LinearNode>, Box<LinearNode>),
    UnaryLogical(UnaryLogicalOp, Box<LinearNode>),
    Cast {
        value: Box<LinearNode>,
        from: PhysicalPrimitive,
        to: PhysicalPrimitive,
    },
    Size(usize),
    Int(i64),
    Float(f64),
    CharLiteral(char),
    Byte(u8),
    FunctionID(FunctionID),

    // Probably not keeping this around forever
    #[allow(dead_code)]
    Debug(Box<LinearNode>),
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum RuntimeFunction {
    // (alloc_size) -> ptr
    Alloc,
    // (ptr, alloc_size) -> ptr
    Realloc,
    // (ptr) -> void
    Dealloc,
    // (str, str) -> str
    StringConcat,
    // (dest, src, size) -> void
    Memcpy,
}

impl LinearNode {
    pub fn visit(&self, mut callback: impl FnMut(&LinearNode)) {
        self.visit_recursive(&mut callback);
    }

    pub fn visit_recursive(&self, callback: &mut impl FnMut(&LinearNode)) {
        callback(self);
        self.children(|child| child.visit_recursive(callback));
    }

    pub fn children(&self, mut callback: impl FnMut(&LinearNode)) {
        match &self.value {
            LinearNodeValue::WriteRegistersSplitting(child, _)
            | LinearNodeValue::Debug(child)
            | LinearNodeValue::Discard(child, _)
            | LinearNodeValue::ReadMemory {
                location: child, ..
            }
            | LinearNodeValue::WriteRegister(_, child)
            | LinearNodeValue::Goto(child)
            | LinearNodeValue::UnaryLogical(_, child)
            | LinearNodeValue::Cast { value: child, .. }
            | LinearNodeValue::Return(Some(child)) => callback(child),
            LinearNodeValue::WriteMemory {
                location: a,
                value: b,
                ..
            }
            | LinearNodeValue::Arithmetic(_, _, a, b)
            | LinearNodeValue::Comparison(_, _, a, b)
            | LinearNodeValue::BinaryLogical(_, a, b) => {
                callback(a);
                callback(b);
            }
            LinearNodeValue::IndirectCall(_, func, args) => {
                callback(func);
                args.iter().for_each(callback);
            }
            LinearNodeValue::If(cond, if_block, else_block, _) => {
                callback(cond);
                for node in if_block.iter() {
                    callback(node);
                }
                if let Some(else_block) = else_block {
                    else_block.iter().for_each(callback);
                }
            }
            LinearNodeValue::Call(_, children)
            | LinearNodeValue::RuntimeCall(_, children)
            | LinearNodeValue::Loop(children)
            | LinearNodeValue::Sequence(children) => {
                children.iter().for_each(callback);
            }
            LinearNodeValue::Switch { value, cases } => {
                callback(value);
                for case in cases.iter() {
                    callback(case);
                }
            }
            LinearNodeValue::Size(_)
            | LinearNodeValue::Int(_)
            | LinearNodeValue::Float(_)
            | LinearNodeValue::CharLiteral(_)
            | LinearNodeValue::Byte(_)
            | LinearNodeValue::FunctionID(_)
            | LinearNodeValue::Parameter(_, _)
            | LinearNodeValue::VariableInit(_, _)
            | LinearNodeValue::VariableDestroy(_)
            | LinearNodeValue::VariableLocation(_)
            | LinearNodeValue::Break
            | LinearNodeValue::GotoLabel(_)
            | LinearNodeValue::Abort
            | LinearNodeValue::ReadRegister(_)
            | LinearNodeValue::KillRegister(_)
            | LinearNodeValue::Return(None)
            | LinearNodeValue::ConstantDataAddress(_) => {}
        }
    }

    fn visit_mut(&mut self, mut callback: impl FnMut(&mut LinearNode)) {
        self.visit_mut_recursive(&mut callback);
    }

    fn visit_mut_recursive(&mut self, callback: &mut impl FnMut(&mut LinearNode)) {
        callback(self);
        self.children_mut(|child| child.visit_mut_recursive(callback));
    }

    fn children_mut(&mut self, mut callback: impl FnMut(&mut LinearNode)) {
        match &mut self.value {
            LinearNodeValue::WriteRegistersSplitting(child, _)
            | LinearNodeValue::Debug(child)
            | LinearNodeValue::Discard(child, _)
            | LinearNodeValue::ReadMemory {
                location: child, ..
            }
            | LinearNodeValue::WriteRegister(_, child)
            | LinearNodeValue::Goto(child)
            | LinearNodeValue::UnaryLogical(_, child)
            | LinearNodeValue::Cast { value: child, .. }
            | LinearNodeValue::Return(Some(child)) => callback(child),
            LinearNodeValue::WriteMemory {
                location: a,
                value: b,
                ..
            }
            | LinearNodeValue::Arithmetic(_, _, a, b)
            | LinearNodeValue::Comparison(_, _, a, b)
            | LinearNodeValue::BinaryLogical(_, a, b) => {
                callback(a);
                callback(b);
            }
            LinearNodeValue::IndirectCall(_, func, args) => {
                callback(func);
                args.iter_mut().for_each(callback);
            }
            LinearNodeValue::If(cond, if_block, else_block, _) => {
                callback(cond);
                for node in if_block.iter_mut() {
                    callback(node);
                }
                if let Some(else_block) = else_block {
                    else_block.iter_mut().for_each(callback);
                }
            }
            LinearNodeValue::Call(_, children)
            | LinearNodeValue::RuntimeCall(_, children)
            | LinearNodeValue::Loop(children)
            | LinearNodeValue::Sequence(children) => {
                children.iter_mut().for_each(callback);
            }
            LinearNodeValue::Switch { value, cases } => {
                callback(value);
                for case in cases.iter_mut() {
                    callback(case);
                }
            }
            LinearNodeValue::Size(_)
            | LinearNodeValue::Int(_)
            | LinearNodeValue::Float(_)
            | LinearNodeValue::CharLiteral(_)
            | LinearNodeValue::Byte(_)
            | LinearNodeValue::FunctionID(_)
            | LinearNodeValue::Parameter(_, _)
            | LinearNodeValue::VariableInit(_, _)
            | LinearNodeValue::VariableDestroy(_)
            | LinearNodeValue::VariableLocation(_)
            | LinearNodeValue::Break
            | LinearNodeValue::GotoLabel(_)
            | LinearNodeValue::Abort
            | LinearNodeValue::ReadRegister(_)
            | LinearNodeValue::KillRegister(_)
            | LinearNodeValue::Return(None)
            | LinearNodeValue::ConstantDataAddress(_) => {}
        }
    }
}

pub struct LinearContext<'a> {
    pub layouts: &'a HashMap<TypeID, DeclaredTypeLayout>,
    pub constant_data_region: &'a mut Vec<u8>,
    pub indirect_function_types: &'a mut HashMap<ExpressionType, FunctionID>,
    pub byte_size: usize,
    pub pointer_size: usize,
    pub module: Module,
}

impl<'a> LinearContext<'a> {
    pub fn linearize_function(&mut self, function: HirFunction) -> LinearFunction {
        let HirNodeValue::Sequence(block) = function.body.value else {
            unreachable!()
        };
        let mut body = self.linearize_nodes(block);
        if let Some(GeneratorProperties {
            generator_var_id,
            param_var_id,
            ..
        }) = function.generator
        {
            generator_local_storage::generator_local_storage(
                self,
                generator_var_id,
                param_var_id,
                &mut body[..],
            );
        }

        LinearFunction {
            id: function.id,
            body,
            params: function.params.iter().map(expr_ty_to_physical).collect(),
            returns: match &function.body.ty {
                ExpressionType::Void | ExpressionType::Unreachable => None,
                return_ty => Some(expr_ty_to_physical(return_ty)),
            },
        }
    }

    pub fn linearize_nodes(&mut self, nodes: Vec<HirNode>) -> Vec<LinearNode> {
        nodes
            .into_iter()
            .map(|node| lower_expression(self, node))
            .collect()
    }
}

fn lower_expression(ctx: &mut LinearContext<'_>, expression: HirNode) -> LinearNode {
    let HirNode {
        id: _,
        value,
        ty,
        provenance,
    } = expression;
    let value = match value {
        HirNodeValue::Int(x) => LinearNodeValue::Int(x),
        HirNodeValue::PointerSize(x) => LinearNodeValue::Size(x),
        HirNodeValue::Float(x) => LinearNodeValue::Float(x),
        HirNodeValue::Bool(x) => LinearNodeValue::Byte(if x { 1 } else { 0 }),
        HirNodeValue::Null => {
            let ty = expr_ty_to_physical(&ty);
            let mut results = Vec::new();
            ty.zeroed(ctx, &mut results);

            LinearNodeValue::Sequence(results)
        }
        HirNodeValue::CharLiteral(x) => LinearNodeValue::CharLiteral(x),
        HirNodeValue::StringLiteral(string) => {
            let bytes = string.as_bytes();
            let offset = ctx.constant_data_region.len();
            ctx.constant_data_region.extend(bytes);
            LinearNodeValue::Sequence(vec![
                LinearNode::new(LinearNodeValue::ConstantDataAddress(offset)),
                LinearNode::size(bytes.len()),
            ])
        }

        HirNodeValue::Arithmetic(op, lhs, rhs) => {
            let ExpressionType::Primitive(ty) = rhs.ty else {
                unreachable!("binoperands must be primitive not {:?}", ty)
            };
            let ty = primitive_to_physical(ty);
            LinearNodeValue::Arithmetic(
                op,
                ty,
                Box::new(lower_expression(ctx, *lhs)),
                Box::new(lower_expression(ctx, *rhs)),
            )
        }
        HirNodeValue::Comparison(op, lhs, rhs) => {
            let ExpressionType::Primitive(ty) = rhs.ty else {
                unreachable!("binoperands must be primitive not {:?}", ty)
            };
            let ty = primitive_to_physical(ty);
            LinearNodeValue::Comparison(
                op,
                ty,
                Box::new(lower_expression(ctx, *lhs)),
                Box::new(lower_expression(ctx, *rhs)),
            )
        }
        HirNodeValue::BinaryLogical(op, lhs, rhs) => LinearNodeValue::BinaryLogical(
            op,
            Box::new(lower_expression(ctx, *lhs)),
            Box::new(lower_expression(ctx, *rhs)),
        ),
        HirNodeValue::UnaryLogical(op, child) => {
            LinearNodeValue::UnaryLogical(op, Box::new(lower_expression(ctx, *child)))
        }
        HirNodeValue::VariableReference(id) => {
            let ty = expr_ty_to_physical(&ty);
            LinearNodeValue::ReadMemory {
                location: Box::new(LinearNode::new(LinearNodeValue::VariableLocation(
                    id.as_var(),
                ))),
                offset: 0,
                ty,
            }
        }
        HirNodeValue::Call(lhs, params) => {
            let params = params
                .into_iter()
                .map(|param| lower_expression(ctx, param))
                .collect();
            match &lhs.value {
                HirNodeValue::VariableReference(AnyID::Function(fn_id)) => {
                    LinearNodeValue::Call(*fn_id, params)
                }
                _ => {
                    let indirect_fn = if let Some(fn_id) = ctx.indirect_function_types.get(&lhs.ty)
                    {
                        *fn_id
                    } else {
                        let fn_id = ctx.module.new_func_id();
                        ctx.indirect_function_types.insert(lhs.ty.clone(), fn_id);
                        fn_id
                    };
                    let lhs = lower_expression(ctx, *lhs);
                    LinearNodeValue::IndirectCall(indirect_fn, Box::new(lhs), params)
                }
            }
        }
        HirNodeValue::Access(lhs, rhs) => {
            if let Some(variants) = lhs
                .ty
                .type_id()
                .and_then(|id| match &ctx.layouts[id].value {
                    TypeLayoutValue::Union(variants) => Some(variants),
                    _ => None,
                })
            {
                let (variant_idx, variant_ty) = &variants[&rhs];
                let (location, offset) = lower_lvalue(ctx, *lhs);
                LinearNodeValue::Sequence(vec![
                    LinearNode::read_memory(
                        location.clone(),
                        offset + UNION_TAG_SIZE.size(ctx.pointer_size),
                        variant_ty.clone().unwrap(),
                    ),
                    LinearNode::ptr_comparison(
                        ComparisonOp::EqualTo,
                        LinearNode::size(*variant_idx),
                        LinearNode::read_memory(
                            location,
                            offset,
                            PhysicalType::Primitive(PhysicalPrimitive::PointerSize),
                        ),
                    ),
                ])
            } else if matches!(&lhs.ty, ExpressionType::Generator { .. }) {
                let (location, mut offset) = lower_lvalue(ctx, *lhs);
                offset += match rhs.as_str() {
                    "function" => 0,
                    "resume_point" => ctx.pointer_size,
                    "stack_ptr" => ctx.pointer_size * 2,
                    rhs => unreachable!("illegal rhs: {}", rhs),
                };
                LinearNodeValue::ReadMemory {
                    location: Box::new(location),
                    offset,
                    ty: expr_ty_to_physical(&ty),
                }
            } else {
                let (location, offset) = access_location(ctx, *lhs, rhs);
                LinearNodeValue::ReadMemory {
                    location: Box::new(location),
                    offset,
                    ty: expr_ty_to_physical(&ty),
                }
            }
        }
        HirNodeValue::NullableTraverse(lhs, rhs) => {
            let temp_id = VariableID::new();

            let PhysicalType::Nullable(ty) = expr_ty_to_physical(&lhs.ty) else {
                unreachable!();
            };
            let mut ty = *ty;

            let mut read_offset = NULL_TAG_SIZE.size(ctx.pointer_size);
            for name in rhs {
                let PhysicalType::Referenced(id) = ty else {
                    unreachable!()
                };
                let decl = &ctx.layouts[&id];
                let TypeLayoutValue::Structure(fields) = &decl.value else {
                    todo!()
                };
                let (field_offset, field_ty) = fields
                    .iter()
                    .find_map(|(field_name, offset, ty)| {
                        if field_name == &name {
                            Some((offset, ty))
                        } else {
                            None
                        }
                    })
                    .unwrap();
                read_offset += field_offset;
                ty = field_ty.clone();
            }

            // TODO: union RHS

            let lhs_ty = expr_ty_to_physical(&lhs.ty);

            LinearNodeValue::Sequence(vec![
                LinearNode::new(LinearNodeValue::VariableInit(temp_id, lhs_ty.clone())),
                LinearNode::write_memory(
                    LinearNode::new(LinearNodeValue::VariableLocation(temp_id)),
                    0,
                    lhs_ty,
                    lower_expression(ctx, *lhs),
                ),
                LinearNode::read_memory(
                    LinearNode::new(LinearNodeValue::VariableLocation(temp_id)),
                    read_offset,
                    ty,
                ),
                LinearNode::read_memory(
                    LinearNode::new(LinearNodeValue::VariableLocation(temp_id)),
                    0,
                    PhysicalType::Primitive(PhysicalPrimitive::Byte),
                ),
                LinearNode::new(LinearNodeValue::VariableDestroy(temp_id)),
            ])
        }
        HirNodeValue::ArrayIndex(arr, idx) => {
            let (location, offset) = array_index_location(ctx, *arr, *idx, &ty);
            LinearNodeValue::ReadMemory {
                location: Box::new(location),
                offset,
                ty: expr_ty_to_physical(&ty),
            }
        }
        HirNodeValue::If(cond, if_block, else_block) => {
            let cond = lower_expression(ctx, *cond);
            let HirNodeValue::Sequence(if_block) = if_block.value else {
                unreachable!()
            };
            let if_block = ctx.linearize_nodes(if_block);
            let else_block = else_block.map(|else_block| {
                let HirNodeValue::Sequence(else_block) = else_block.value else {
                    unreachable!()
                };
                ctx.linearize_nodes(else_block)
            });
            LinearNodeValue::If(Box::new(cond), if_block, else_block, None)
        }
        HirNodeValue::Return(expr) => {
            let expr = expr.map(|expr| Box::new(lower_expression(ctx, *expr)));
            LinearNodeValue::Return(expr)
        }
        HirNodeValue::While(cond, block) => {
            let cond = lower_expression(ctx, *cond);
            let block = lower_expression(ctx, *block);
            LinearNodeValue::Loop(vec![LinearNode::if_node(
                cond,
                vec![block],
                Some(vec![LinearNode::new(LinearNodeValue::Break)]),
                None,
            )])
        }
        HirNodeValue::Loop(body) => {
            let body = lower_expression(ctx, *body);
            LinearNodeValue::Loop(vec![body])
        }
        HirNodeValue::Parameter(idx, id) => {
            let ty = expr_ty_to_physical(&ty);
            LinearNodeValue::Sequence(vec![
                LinearNode::new(LinearNodeValue::VariableInit(id, ty.clone())),
                LinearNode::write_memory(
                    LinearNode::new(LinearNodeValue::VariableLocation(id)),
                    0,
                    ty.clone(),
                    LinearNode::new(LinearNodeValue::Parameter(ty, idx)),
                ),
            ])
        }
        HirNodeValue::Declaration(id) => {
            let ty = expr_ty_to_physical(&ty);
            LinearNodeValue::VariableInit(id, ty)
        }
        HirNodeValue::Assignment(lhs, rhs) => {
            let ty = expr_ty_to_physical(&lhs.ty);
            let (location, offset) = lower_lvalue(ctx, *lhs);
            let rhs = lower_expression(ctx, *rhs);

            LinearNodeValue::WriteMemory {
                location: Box::new(location),
                offset,
                value: Box::new(rhs),
                ty,
            }
        }
        HirNodeValue::TakeUnique(inner) | HirNodeValue::TakeShared(inner) => {
            let (ptr, offset) = lower_lvalue(ctx, *inner);
            LinearNodeValue::Arithmetic(
                ArithmeticOp::Add,
                PhysicalPrimitive::PointerSize,
                Box::new(ptr),
                Box::new(LinearNode::size(offset)),
            )
        }
        HirNodeValue::Dereference(inner) => LinearNodeValue::ReadMemory {
            location: Box::new(lower_expression(ctx, *inner)),
            offset: 0,
            ty: expr_ty_to_physical(&ty),
        },
        HirNodeValue::Sequence(nodes) => LinearNodeValue::Sequence(
            nodes
                .into_iter()
                .map(|node| lower_expression(ctx, node))
                .collect(),
        ),
        HirNodeValue::StructLiteral(struct_id, mut values) => {
            let Some(DeclaredTypeLayout {
                value: TypeLayoutValue::Structure(layouts),
                ..
            }) = &ctx.layouts.get(&struct_id)
            else {
                unreachable!()
            };
            // TODO: push this down a layer of abstraction
            LinearNodeValue::Sequence(
                layouts
                    .iter()
                    .rev()
                    .map(|(key, _, _)| values.remove(key).unwrap())
                    .map(|value| lower_expression(ctx, value))
                    .collect(),
            )
        }
        HirNodeValue::ArrayLiteral(values) => {
            let ExpressionType::Collection(CollectionType::Array(inner_ty)) = ty else {
                unreachable!()
            };
            let inner_ty = expr_ty_to_physical(inner_ty.as_ref());
            let size = inner_ty.size(ctx);

            let length = values.len();

            let buffer_ptr = RegisterID::new();

            let mut instrs = vec![LinearNode::write_register(
                buffer_ptr,
                LinearNode::heap_alloc_const(size * values.len()),
            )];
            instrs.extend(values.into_iter().enumerate().map(|(idx, value)| {
                LinearNode::write_memory(
                    LinearNode::read_register(buffer_ptr),
                    size * idx,
                    inner_ty.clone(),
                    lower_expression(ctx, value),
                )
            }));
            // capacity
            instrs.push(LinearNode::size(length));
            // length
            instrs.push(LinearNode::size(length));
            instrs.push(LinearNode::read_register(buffer_ptr));
            instrs.push(LinearNode::kill_register(buffer_ptr));

            LinearNodeValue::Sequence(instrs)
        }
        HirNodeValue::ArrayLiteralLength(value, length) => {
            let ExpressionType::Collection(CollectionType::Array(inner_ty)) = ty else {
                unreachable!()
            };
            let inner_ty = expr_ty_to_physical(inner_ty.as_ref());
            let size = inner_ty.size(ctx);
            let length = lower_expression(ctx, *length);

            let length_register = RegisterID::new();
            let buffer_register = RegisterID::new();
            let index_register = RegisterID::new();

            LinearNodeValue::Sequence(vec![
                LinearNode::write_register(length_register, length),
                LinearNode::write_register(
                    buffer_register,
                    LinearNode::heap_alloc_var(LinearNode::ptr_arithmetic(
                        ArithmeticOp::Multiply,
                        LinearNode::size(size),
                        LinearNode::read_register(length_register),
                    )),
                ),
                LinearNode::write_register(index_register, LinearNode::size(0)),
                LinearNode::new(LinearNodeValue::Loop(vec![LinearNode::if_node(
                    LinearNode::ptr_comparison(
                        ComparisonOp::EqualTo,
                        LinearNode::read_register(index_register),
                        LinearNode::read_register(length_register),
                    ),
                    vec![LinearNode::new(LinearNodeValue::Break)],
                    Some(vec![
                        // *(ptr + idx * size) = value
                        LinearNode::write_memory(
                            LinearNode::ptr_arithmetic(
                                ArithmeticOp::Add,
                                LinearNode::read_register(buffer_register),
                                LinearNode::ptr_arithmetic(
                                    ArithmeticOp::Add,
                                    LinearNode::size(size),
                                    LinearNode::read_register(index_register),
                                ),
                            ),
                            0,
                            inner_ty,
                            lower_expression(ctx, *value),
                        ),
                        LinearNode::write_register(
                            index_register,
                            LinearNode::ptr_arithmetic(
                                ArithmeticOp::Add,
                                LinearNode::read_register(index_register),
                                LinearNode::size(1),
                            ),
                        ),
                    ]),
                    None,
                )])),
                // Return values
                LinearNode::read_register(length_register),
                LinearNode::read_register(length_register),
                LinearNode::read_register(buffer_register),
                // Cleanup
                LinearNode::kill_register(length_register),
                LinearNode::kill_register(index_register),
                LinearNode::kill_register(buffer_register),
            ])
        }
        HirNodeValue::InterfaceAddress(table) => {
            let (table, offset) = lower_lvalue(ctx, *table);
            LinearNodeValue::ReadMemory {
                location: Box::new(table),
                offset,
                ty: PhysicalType::Primitive(PhysicalPrimitive::PointerSize),
            }
        }
        HirNodeValue::VtableCall(table, fn_id, params) => {
            let ExpressionType::InstanceOf(ty_id) = &table.ty else {
                unreachable!()
            };
            let Some(DeclaredTypeLayout {
                value: TypeLayoutValue::Interface(fields),
                ..
            }) = &ctx.layouts.get(ty_id)
            else {
                unreachable!()
            };
            let (table, mut offset) = lower_lvalue(ctx, *table);
            offset += ctx.pointer_size;
            offset += fields
                .iter()
                .enumerate()
                .find_map(|(idx, id)| {
                    if *id == fn_id {
                        Some(idx * ctx.pointer_size)
                    } else {
                        None
                    }
                })
                .unwrap();
            let params = params
                .into_iter()
                .map(|param| lower_expression(ctx, param))
                .collect();

            LinearNodeValue::IndirectCall(
                fn_id,
                Box::new(LinearNode::new(LinearNodeValue::ReadMemory {
                    location: Box::new(table),
                    offset,
                    ty: PhysicalType::Primitive(PhysicalPrimitive::FunctionPointer),
                })),
                params,
            )
        }
        HirNodeValue::StructToInterface { value, vtable } => {
            let mut values = Vec::new();

            let ExpressionType::InstanceOf(ty_id) = &ty else {
                unreachable!()
            };
            let Some(DeclaredTypeLayout {
                value: TypeLayoutValue::Interface(fields),
                ..
            }) = ctx.layouts.get(ty_id)
            else {
                unreachable!()
            };
            for field in fields.iter().rev() {
                values.push(LinearNode::new(LinearNodeValue::FunctionID(vtable[field])));
            }

            let (pointer, offset) = lower_lvalue(ctx, *value);
            let pointer =
                LinearNode::ptr_arithmetic(ArithmeticOp::Add, pointer, LinearNode::size(offset));
            values.push(pointer);

            LinearNodeValue::Sequence(values)
        }
        HirNodeValue::NumericCast { value, from, to } => LinearNodeValue::Cast {
            value: Box::new(lower_expression(ctx, *value)),
            from: primitive_to_physical(from),
            to: primitive_to_physical(to),
        },
        HirNodeValue::DictIndex(dict, idx) => {
            let (location, offset) = dict_index_location_or_abort(ctx, *dict, *idx);
            LinearNodeValue::ReadMemory {
                location: Box::new(location),
                offset,
                ty: expr_ty_to_physical(&ty),
            }
        }
        HirNodeValue::DictLiteral(entries) => {
            let ExpressionType::Collection(CollectionType::Dict(key_ty, value_ty)) = ty else {
                unreachable!()
            };
            let key_ty = expr_ty_to_physical(&key_ty);
            let value_ty = expr_ty_to_physical(&value_ty);

            let key_size = key_ty.size(ctx);
            let value_size = value_ty.size(ctx);
            let entry_size = key_size + value_size;

            let length = entries.len();

            let buffer = RegisterID::new();

            let mut instrs = vec![LinearNode::write_register(
                buffer,
                LinearNode::heap_alloc_const(entry_size * length),
            )];
            let (keys, values): (Vec<_>, Vec<_>) = entries.into_iter().unzip();
            instrs.extend(keys.into_iter().enumerate().map(|(idx, value)| {
                LinearNode::write_memory(
                    LinearNode::read_register(buffer),
                    entry_size * idx,
                    key_ty.clone(),
                    lower_expression(ctx, value),
                )
            }));
            instrs.extend(values.into_iter().enumerate().map(|(idx, value)| {
                LinearNode::write_memory(
                    LinearNode::read_register(buffer),
                    entry_size * idx + key_size,
                    value_ty.clone(),
                    lower_expression(ctx, value),
                )
            }));
            instrs.extend([
                LinearNode::size(length),
                LinearNode::size(length),
                LinearNode::read_register(buffer),
                LinearNode::kill_register(buffer),
            ]);

            LinearNodeValue::Sequence(instrs)
        }
        HirNodeValue::ReferenceCountLiteral(inner) => {
            let inner_ty = expr_ty_to_physical(&inner.ty);
            let inner_size = inner_ty.size(ctx);
            let alloc_size = inner_size + 4;
            let alloc_register = RegisterID::new();

            LinearNodeValue::Sequence(vec![
                LinearNode::write_register(
                    alloc_register,
                    LinearNode::heap_alloc_const(alloc_size),
                ),
                LinearNode::write_memory(
                    LinearNode::read_register(alloc_register),
                    0,
                    PhysicalType::Primitive(PhysicalPrimitive::Int32),
                    LinearNode::new(LinearNodeValue::Int(1)),
                ),
                LinearNode::write_memory(
                    LinearNode::read_register(alloc_register),
                    4,
                    inner_ty,
                    lower_expression(ctx, *inner),
                ),
                // Point the returned pointer at the actual value, rather than
                // the reference count
                LinearNode::ptr_arithmetic(
                    ArithmeticOp::Add,
                    LinearNode::read_register(alloc_register),
                    LinearNode::size(4),
                ),
                LinearNode::kill_register(alloc_register),
            ])
        }
        HirNodeValue::UnionLiteral(ty, variant, value) => {
            let union_ty = &ctx.layouts[&ty];
            let TypeLayoutValue::Union(ty) = &union_ty.value else {
                unreachable!()
            };
            let (variant_idx, variant_ty) = &ty[&variant];
            let padding = union_ty.size()
                - UNION_TAG_SIZE.size(ctx.pointer_size)
                - variant_ty.as_ref().map(|ty| ty.size(ctx)).unwrap_or(0);
            let mut values = Vec::new();
            for _ in 0..(padding / 4) {
                values.push(LinearNode::new(LinearNodeValue::Int(0)));
            }
            if let Some(value) = value {
                values.push(lower_expression(ctx, *value));
            }
            values.push(LinearNode::new(LinearNodeValue::Size(*variant_idx)));
            LinearNodeValue::Sequence(values)
        }
        HirNodeValue::NullCoalesce(lhs, rhs) => {
            let result_ty = expr_ty_to_physical(&ty);
            let lhs_ty = expr_ty_to_physical(&lhs.ty);
            let lhs = lower_expression(ctx, *lhs);
            let rhs = lower_expression(ctx, *rhs);
            let temp_var_id = VariableID::new();

            LinearNodeValue::Sequence(vec![
                LinearNode::new(LinearNodeValue::VariableInit(temp_var_id, lhs_ty.clone())),
                LinearNode::write_memory(
                    LinearNode::new(LinearNodeValue::VariableLocation(temp_var_id)),
                    0,
                    lhs_ty,
                    lhs,
                ),
                LinearNode::if_node_value(
                    LinearNode::read_memory(
                        LinearNode::new(LinearNodeValue::VariableLocation(temp_var_id)),
                        0,
                        PhysicalType::Primitive(PhysicalPrimitive::Byte),
                    ),
                    vec![LinearNode::read_memory(
                        LinearNode::new(LinearNodeValue::VariableLocation(temp_var_id)),
                        NULL_TAG_SIZE.size(ctx.pointer_size),
                        result_ty.clone(),
                    )],
                    Some(vec![rhs]),
                    provenance.clone(),
                    result_ty,
                ),
            ])
        }
        HirNodeValue::MakeNullable(value) => LinearNodeValue::Sequence(vec![
            lower_expression(ctx, *value),
            LinearNode::bool_value(true),
        ]),
        HirNodeValue::IntrinsicCall(IntrinsicFunction::ArrayLength, mut args) => {
            let location = lower_expression(ctx, args.remove(0));
            LinearNodeValue::ReadMemory {
                location: Box::new(location),
                offset: ctx.pointer_size,
                ty: PhysicalType::Primitive(PhysicalPrimitive::PointerSize),
            }
        }
        HirNodeValue::IntrinsicCall(IntrinsicFunction::ArrayPush, mut args) => {
            let inserted = args.pop().unwrap();
            let inner_ty = expr_ty_to_physical(&inserted.ty);

            let arr = lower_expression(ctx, args.pop().unwrap());
            let inserted = lower_expression(ctx, inserted);

            LinearNodeValue::WriteMemory {
                location: Box::new(LinearNode::new(array_alloc_space_to_push(
                    arr,
                    0,
                    inner_ty.size(ctx),
                    provenance.clone(),
                    ctx.pointer_size,
                ))),
                offset: 0,
                ty: inner_ty,
                value: Box::new(inserted),
            }
        }
        HirNodeValue::IntrinsicCall(IntrinsicFunction::ArrayFree, mut args) => {
            let arr = args.pop().unwrap();
            let array = lower_expression(ctx, arr);

            LinearNodeValue::Sequence(vec![LinearNode::call_runtime(
                RuntimeFunction::Dealloc,
                vec![LinearNode::read_memory(
                    array,
                    0,
                    PhysicalType::Primitive(PhysicalPrimitive::PointerSize),
                )],
            )])
        }
        HirNodeValue::IntrinsicCall(IntrinsicFunction::DictionaryInsert, mut args) => {
            let value = args.pop().unwrap();
            let key = args.pop().unwrap();
            let dict = args.pop().unwrap();

            let ExpressionType::Pointer(_, dict_ty) = &dict.ty else {
                unreachable!()
            };
            let ExpressionType::Collection(CollectionType::Dict(key_ty, value_ty)) =
                dict_ty.as_ref()
            else {
                unreachable!()
            };

            let key_ty = expr_ty_to_physical(key_ty);
            let value_ty = expr_ty_to_physical(value_ty);
            let key_size = key_ty.size(ctx);
            let entry_size = key_size + value_ty.size(ctx);
            let PhysicalType::Primitive(key_ty) = key_ty else {
                unreachable!()
            };

            let dict_location = lower_expression(ctx, dict);

            let key = lower_expression(ctx, key);
            let value = lower_expression(ctx, value);

            let temp_key_id = VariableID::new();

            let ptr = RegisterID::new();
            let entry_register = RegisterID::new();

            LinearNodeValue::Sequence(vec![
                LinearNode::new(LinearNodeValue::VariableInit(
                    temp_key_id,
                    PhysicalType::Primitive(key_ty),
                )),
                // pointer to dict
                LinearNode::write_register(ptr, dict_location),
                LinearNode::write_memory(
                    LinearNode::new(LinearNodeValue::VariableLocation(temp_key_id)),
                    0,
                    PhysicalType::Primitive(key_ty),
                    key.clone(),
                ),
                LinearNode::if_node(
                    dict_get_entry_for_key(
                        LinearNode::read_memory(
                            LinearNode::read_register(ptr),
                            0,
                            PhysicalType::Collection(PhysicalCollection::Dict),
                        ),
                        LinearNode::new(LinearNodeValue::VariableLocation(temp_key_id)),
                        0,
                        key_ty,
                        entry_size,
                        entry_register,
                    ),
                    vec![
                        LinearNode::write_memory(
                            LinearNode::read_register(entry_register),
                            key_size,
                            value_ty.clone(),
                            value.clone(),
                        ),
                        LinearNode::kill_register(entry_register),
                    ],
                    Some(vec![
                        // Pointer to newly allocated entry
                        LinearNode::write_register(
                            entry_register,
                            LinearNode::new(array_alloc_space_to_push(
                                LinearNode::read_register(ptr),
                                0,
                                entry_size,
                                None,
                                ctx.pointer_size,
                            )),
                        ),
                        LinearNode::write_memory(
                            LinearNode::read_register(entry_register),
                            0,
                            PhysicalType::Primitive(key_ty),
                            key,
                        ),
                        LinearNode::write_memory(
                            LinearNode::read_register(entry_register),
                            key_size,
                            value_ty,
                            value,
                        ),
                    ]),
                    provenance.clone(),
                ),
                LinearNode::new(LinearNodeValue::VariableDestroy(temp_key_id)),
                LinearNode::kill_register(entry_register),
                LinearNode::kill_register(ptr),
            ])
        }
        HirNodeValue::IntrinsicCall(IntrinsicFunction::DictionaryContains, mut args) => {
            let key = args.pop().unwrap();
            let dict = args.pop().unwrap();
            let ExpressionType::Pointer(_, dict_ty) = &dict.ty else {
                unreachable!()
            };
            let ExpressionType::Collection(CollectionType::Dict(key_ty, value_ty)) =
                dict_ty.as_ref()
            else {
                unreachable!()
            };

            let key_ty = expr_ty_to_physical(key_ty);
            let value_ty = expr_ty_to_physical(value_ty);
            let entry_size = key_ty.size(ctx) + value_ty.size(ctx);
            let PhysicalType::Primitive(key_ty) = key_ty else {
                unreachable!()
            };

            let dict = lower_expression(ctx, dict);
            let key = lower_expression(ctx, key);

            let entry_pointer_output = RegisterID::new();

            LinearNodeValue::Sequence(vec![
                dict_get_entry_for_key(
                    LinearNode::read_memory(
                        dict,
                        0,
                        PhysicalType::Collection(PhysicalCollection::Dict),
                    ),
                    key,
                    0,
                    key_ty,
                    entry_size,
                    entry_pointer_output,
                ),
                LinearNode::kill_register(entry_pointer_output),
            ])
        }
        HirNodeValue::IntrinsicCall(IntrinsicFunction::RcClone, mut args) => {
            let rc = args.remove(0);
            let ptr_register = RegisterID::new();
            let reference_count_location = RegisterID::new();
            LinearNodeValue::Sequence(vec![
                LinearNode::write_register(
                    ptr_register,
                    LinearNode::read_memory(
                        lower_expression(ctx, rc),
                        0,
                        PhysicalType::Primitive(PhysicalPrimitive::PointerSize),
                    ),
                ),
                LinearNode::write_register(
                    reference_count_location,
                    LinearNode::ptr_arithmetic(
                        ArithmeticOp::Subtract,
                        LinearNode::read_register(ptr_register),
                        LinearNode::size(4),
                    ),
                ),
                // Increment the rc count
                LinearNode::write_memory(
                    LinearNode::read_register(reference_count_location),
                    0,
                    PhysicalType::Primitive(PhysicalPrimitive::Int32),
                    LinearNode::new(LinearNodeValue::Arithmetic(
                        ArithmeticOp::Add,
                        PhysicalPrimitive::Int32,
                        Box::new(LinearNode::read_memory(
                            LinearNode::read_register(reference_count_location),
                            0,
                            PhysicalType::Primitive(PhysicalPrimitive::Int32),
                        )),
                        Box::new(LinearNode::new(LinearNodeValue::Int(1))),
                    )),
                ),
                LinearNode::read_register(ptr_register),
                LinearNode::kill_register(ptr_register),
                LinearNode::kill_register(reference_count_location),
            ])
        }
        HirNodeValue::IntrinsicCall(IntrinsicFunction::RcDecrement, mut args) => {
            // Decrement the rc count and return if it's = 0
            let rc = args.remove(0);
            let ptr_register = RegisterID::new();
            let reference_count = RegisterID::new();
            LinearNodeValue::Sequence(vec![
                LinearNode::write_register(
                    ptr_register,
                    LinearNode::ptr_arithmetic(
                        ArithmeticOp::Subtract,
                        lower_expression(ctx, rc),
                        LinearNode::size(4),
                    ),
                ),
                LinearNode::write_register(
                    reference_count,
                    LinearNode::new(LinearNodeValue::Cast {
                        to: PhysicalPrimitive::PointerSize,
                        from: PhysicalPrimitive::Int32,
                        value: Box::new(LinearNode::new(LinearNodeValue::Arithmetic(
                            ArithmeticOp::Subtract,
                            PhysicalPrimitive::Int32,
                            Box::new(LinearNode::read_memory(
                                LinearNode::read_register(ptr_register),
                                0,
                                PhysicalType::Primitive(PhysicalPrimitive::Int32),
                            )),
                            Box::new(LinearNode::new(LinearNodeValue::Int(1))),
                        ))),
                    }),
                ),
                LinearNode::write_memory(
                    LinearNode::read_register(ptr_register),
                    0,
                    PhysicalType::Primitive(PhysicalPrimitive::Int32),
                    LinearNode::new(LinearNodeValue::Cast {
                        to: PhysicalPrimitive::Int32,
                        from: PhysicalPrimitive::PointerSize,
                        value: Box::new(LinearNode::read_register(reference_count)),
                    }),
                ),
                LinearNode::ptr_comparison(
                    ComparisonOp::EqualTo,
                    LinearNode::read_register(reference_count),
                    LinearNode::new(LinearNodeValue::Size(0)),
                ),
                LinearNode::kill_register(ptr_register),
                LinearNode::kill_register(reference_count),
            ])
        }
        HirNodeValue::IntrinsicCall(IntrinsicFunction::RcFree, mut args) => {
            let rc = args.remove(0);
            // delete the RC memory
            LinearNodeValue::RuntimeCall(
                RuntimeFunction::Dealloc,
                vec![LinearNode::ptr_arithmetic(
                    ArithmeticOp::Subtract,
                    lower_expression(ctx, rc),
                    LinearNode::size(4),
                )],
            )
        }
        HirNodeValue::IntrinsicCall(IntrinsicFunction::CellGet, mut args) => {
            let rc = args.remove(0);
            let ExpressionType::Pointer(_, cell_ty) = &rc.ty else {
                unreachable!()
            };
            let ExpressionType::Collection(CollectionType::Cell(inner_ty)) = cell_ty.as_ref()
            else {
                unreachable!()
            };
            let target = args.remove(0);

            let ty = expr_ty_to_physical(inner_ty);
            let rc = lower_expression(ctx, rc);
            let target = lower_expression(ctx, target);

            LinearNodeValue::WriteMemory {
                location: Box::new(target),
                offset: 0,
                ty: ty.clone(),
                value: Box::new(LinearNode::read_memory(rc, 0, ty)),
            }
        }
        HirNodeValue::IntrinsicCall(IntrinsicFunction::CellSet, mut args) => {
            let rc = args.remove(0);
            let ExpressionType::Pointer(_, cell_ty) = &rc.ty else {
                unreachable!()
            };
            let ExpressionType::Collection(CollectionType::Cell(inner_ty)) = cell_ty.as_ref()
            else {
                unreachable!()
            };
            let ty = expr_ty_to_physical(inner_ty);
            let rc = lower_expression(ctx, rc);
            let argument = lower_expression(ctx, args.remove(0));
            LinearNodeValue::WriteMemory {
                location: Box::new(rc),
                offset: 0,
                ty,
                value: Box::new(argument),
            }
        }
        HirNodeValue::GeneratorSuspend(generator, label) => {
            let location = lower_expression(ctx, *generator);
            LinearNodeValue::WriteMemory {
                location: Box::new(location),
                offset: ctx.pointer_size,
                ty: PhysicalType::Primitive(PhysicalPrimitive::PointerSize),
                value: Box::new(LinearNode::size(label)),
            }
        }
        HirNodeValue::GotoLabel(label) => LinearNodeValue::GotoLabel(label),
        HirNodeValue::GeneratorResume(generator) => {
            let resume_point = lower_expression(ctx, *generator);
            LinearNodeValue::Goto(Box::new(resume_point))
        }
        HirNodeValue::GeneratorCreate {
            generator_function, ..
        } => {
            // TODO: care about args
            LinearNodeValue::Sequence(vec![
                LinearNode::heap_alloc_const(64), // TODO: allocate a statically sized stack frame
                LinearNode::size(0),
                LinearNode::new(LinearNodeValue::FunctionID(generator_function)),
            ])
        }
        HirNodeValue::Yield(_) => unreachable!("yields should be rewritten in HIR"),
        HirNodeValue::StringConcat(left, right) => {
            let left = lower_expression(ctx, *left);
            let right = lower_expression(ctx, *right);
            LinearNodeValue::RuntimeCall(RuntimeFunction::StringConcat, vec![left, right])
        }
        HirNodeValue::Switch { value, cases } => LinearNodeValue::Switch {
            value: Box::new(lower_expression(ctx, *value)),
            cases: cases
                .into_iter()
                .map(|case| lower_expression(ctx, case))
                .collect(),
        },
        HirNodeValue::UnionTag(union) => {
            let (location, offset) = if matches!(union.ty, ExpressionType::Pointer(_, _)) {
                (lower_expression(ctx, *union), 0)
            } else {
                lower_lvalue(ctx, *union)
            };
            LinearNodeValue::ReadMemory {
                location: Box::new(location),
                offset,
                ty: PhysicalType::Primitive(PhysicalPrimitive::PointerSize),
            }
        }
        HirNodeValue::UnionVariant(union, variant) => {
            let ty = shallow_dereference(&union.ty)
                .type_id()
                .and_then(|id| match &ctx.layouts[id].value {
                    TypeLayoutValue::Union(ty) => Some(ty),
                    _ => None,
                })
                .unwrap();
            let (_variant_idx, variant_ty) = &ty[&variant];
            let (location, offset) = if matches!(union.ty, ExpressionType::Pointer(_, _)) {
                (lower_expression(ctx, *union), 0)
            } else {
                lower_lvalue(ctx, *union)
            };
            LinearNodeValue::ReadMemory {
                location: Box::new(location),
                offset: offset + UNION_TAG_SIZE.size(ctx.pointer_size),
                ty: variant_ty.clone().unwrap(),
            }
        }
        HirNodeValue::Discard(val) => {
            let ty = expr_ty_to_physical(&val.ty);
            LinearNodeValue::Discard(Box::new(lower_expression(ctx, *val)), ty)
        }
        HirNodeValue::CellLiteral(inner) => {
            return lower_expression(ctx, *inner);
        }
    };

    LinearNode { value, provenance }
}

fn lower_lvalue(ctx: &mut LinearContext<'_>, lvalue: HirNode) -> (LinearNode, usize) {
    match lvalue.value {
        HirNodeValue::VariableReference(id) => (
            LinearNode::new(LinearNodeValue::VariableLocation(id.as_var())),
            0,
        ),
        HirNodeValue::Access(lhs, rhs) => access_location(ctx, *lhs, rhs),
        HirNodeValue::Dereference(inner) => (lower_expression(ctx, *inner), 0),
        HirNodeValue::ArrayIndex(arr, idx) => array_index_location(ctx, *arr, *idx, &lvalue.ty),
        HirNodeValue::DictIndex(dict, idx) => dict_index_location_or_abort(ctx, *dict, *idx),
        HirNodeValue::UnionVariant(union, variant) => access_location(ctx, *union, variant),

        HirNodeValue::Parameter(_, _) => todo!(),
        HirNodeValue::Declaration(_) => todo!(),
        HirNodeValue::Call(_, _) => todo!(),
        HirNodeValue::Assignment(_, _) => todo!(),
        HirNodeValue::UnaryLogical(_, _) => todo!(),
        HirNodeValue::Arithmetic(_, _, _) => todo!(),
        HirNodeValue::Comparison(_, _, _) => todo!(),
        HirNodeValue::BinaryLogical(_, _, _) => todo!(),
        HirNodeValue::Return(_) => todo!(),
        HirNodeValue::Int(_) => todo!(),
        HirNodeValue::PointerSize(_) => todo!(),
        HirNodeValue::Float(_) => todo!(),
        HirNodeValue::Bool(_) => todo!(),
        HirNodeValue::Null => todo!(),
        HirNodeValue::CharLiteral(_) => todo!(),
        HirNodeValue::StringLiteral(_) => todo!(),
        HirNodeValue::TakeUnique(_) => todo!(),
        HirNodeValue::TakeShared(_) => todo!(),
        HirNodeValue::Sequence(_) => todo!(),
        HirNodeValue::If(_, _, _) => todo!(),
        HirNodeValue::While(_, _) => todo!(),
        HirNodeValue::Loop(_) => todo!(),
        HirNodeValue::StructLiteral(_, _) => todo!(),
        HirNodeValue::VtableCall(_, _, _) => todo!(),
        HirNodeValue::StructToInterface { .. } => todo!(),
        HirNodeValue::InterfaceAddress(_) => todo!(),

        HirNodeValue::ArrayLiteral(_) | HirNodeValue::ArrayLiteralLength(_, _) => unreachable!(),
        HirNodeValue::NumericCast { .. } => todo!(),
        HirNodeValue::DictLiteral(_) => todo!(),
        HirNodeValue::UnionLiteral(_, _, _) => todo!(),
        HirNodeValue::NullCoalesce(_, _) => todo!(),
        HirNodeValue::MakeNullable(_) => todo!(),
        HirNodeValue::NullableTraverse(_, _) => todo!(),
        HirNodeValue::Yield(_) => todo!(),
        HirNodeValue::IntrinsicCall(_, _) => todo!(),
        HirNodeValue::GeneratorSuspend(_, _) => todo!(),
        HirNodeValue::GotoLabel(_) => todo!(),
        HirNodeValue::GeneratorResume(_) => todo!(),
        HirNodeValue::GeneratorCreate { .. } => todo!(),
        HirNodeValue::StringConcat(_, _) => todo!(),
        HirNodeValue::Switch { value: _, cases: _ } => todo!(),
        HirNodeValue::UnionTag(_value) => todo!(),
        HirNodeValue::ReferenceCountLiteral(_) => todo!(),
        HirNodeValue::Discard(_) => todo!(),
        HirNodeValue::CellLiteral(_) => todo!(),
    }
}

fn access_location(ctx: &mut LinearContext<'_>, lhs: HirNode, rhs: String) -> (LinearNode, usize) {
    let ty_id = match &lhs.ty {
        ExpressionType::InstanceOf(ty) => ty,
        ExpressionType::Nullable(ty) => match ty as &ExpressionType {
            ExpressionType::InstanceOf(ty) => ty,
            _ => unreachable!(),
        },
        _ => unreachable!("{:?}", &lhs),
    };
    let DeclaredTypeLayout { value, .. } = &ctx.layouts[ty_id];
    let (lhs, mut offset) = lower_lvalue(ctx, lhs);
    offset += match value {
        TypeLayoutValue::Structure(fields) => {
            *(fields
                .iter()
                .find_map(|(name, offset, _)| if name == &rhs { Some(offset) } else { None })
                .unwrap())
        }
        TypeLayoutValue::Union(_) => UNION_TAG_SIZE.size(ctx.pointer_size),
        TypeLayoutValue::Interface(_fields) => todo!(), //*fields.get(&rhs).unwrap(),
    };

    (lhs, offset)
}

fn array_index_location(
    ctx: &mut LinearContext<'_>,
    arr: HirNode,
    idx: HirNode,
    ty: &ExpressionType,
) -> (LinearNode, usize) {
    let size = expr_ty_to_physical(ty).size(ctx);
    let idx = lower_expression(ctx, idx);
    let arr = lower_expression(ctx, arr);

    let idx_register = RegisterID::new();
    let arr_ptr_register = RegisterID::new();
    let length_register = RegisterID::new();

    (
        LinearNode::new(LinearNodeValue::Sequence(vec![
            LinearNode::write_register(idx_register, idx),
            LinearNode::write_multi_register(
                arr,
                vec![Some(arr_ptr_register), Some(length_register), None],
            ),
            LinearNode::if_node(
                LinearNode::ptr_comparison(
                    ComparisonOp::GreaterEqualThan,
                    LinearNode::read_register(idx_register),
                    LinearNode::read_register(length_register),
                ),
                vec![LinearNode::abort()],
                None,
                None,
            ),
            LinearNode::kill_register(length_register),
            LinearNode::ptr_arithmetic(
                ArithmeticOp::Add,
                LinearNode::ptr_arithmetic(
                    ArithmeticOp::Multiply,
                    LinearNode::size(size),
                    LinearNode::read_register(idx_register),
                ),
                LinearNode::read_register(arr_ptr_register),
            ),
            LinearNode::kill_register(idx_register),
            LinearNode::kill_register(arr_ptr_register),
        ])),
        0,
    )
}

fn dict_index_location_or_abort(
    ctx: &mut LinearContext<'_>,
    dict: HirNode,
    idx: HirNode,
) -> (LinearNode, usize) {
    let ExpressionType::Collection(CollectionType::Dict(key_ty, value_ty)) = &dict.ty else {
        unreachable!()
    };
    let ExpressionType::Primitive(idx_ty) = &idx.ty else {
        todo!("non-primitive keys for dictionaries")
    };
    let idx_ty = primitive_to_physical(*idx_ty);

    let key_ty = expr_ty_to_physical(key_ty);
    let key_size = key_ty.size(ctx);
    let value_size = expr_ty_to_physical(value_ty).size(ctx);
    let entry_size = key_size + value_size;

    let dict = lower_expression(ctx, dict);
    let idx = lower_expression(ctx, idx);

    let temp_key_id = VariableID::new();
    let entry_pointer_output = RegisterID::new();

    (
        LinearNode::new(LinearNodeValue::Sequence(vec![
            LinearNode::new(LinearNodeValue::VariableInit(temp_key_id, key_ty)),
            LinearNode::write_memory(
                LinearNode::new(LinearNodeValue::VariableLocation(temp_key_id)),
                0,
                PhysicalType::Primitive(idx_ty),
                idx,
            ),
            LinearNode::if_node_value(
                dict_get_entry_for_key(
                    dict,
                    LinearNode::new(LinearNodeValue::VariableLocation(temp_key_id)),
                    0,
                    idx_ty,
                    entry_size,
                    entry_pointer_output,
                ),
                vec![
                    LinearNode::new(LinearNodeValue::VariableDestroy(temp_key_id)),
                    LinearNode::read_register(entry_pointer_output),
                    LinearNode::kill_register(entry_pointer_output),
                ],
                Some(vec![LinearNode::new(LinearNodeValue::Abort)]),
                None,
                PhysicalType::Primitive(PhysicalPrimitive::PointerSize),
            ),
        ])),
        key_size,
    )
}

fn array_alloc_space_to_push(
    array_location: LinearNode,
    array_offset: usize,
    elem_size: usize,
    provenance: Option<SourceRange>,
    pointer_size: usize,
) -> LinearNodeValue {
    let length_offset = array_offset + pointer_size;
    let capacity_offset = length_offset + pointer_size;

    let arr_ptr = RegisterID::new();
    let length_register = RegisterID::new();
    let new_capacity_register = RegisterID::new();
    let buffer_register = RegisterID::new();

    LinearNodeValue::Sequence(vec![
        LinearNode::write_register(arr_ptr, array_location),
        LinearNode::write_register(
            buffer_register,
            LinearNode::read_memory(
                LinearNode::read_register(arr_ptr),
                0,
                PhysicalType::Primitive(PhysicalPrimitive::PointerSize),
            ),
        ),
        LinearNode::write_register(
            length_register,
            LinearNode::read_memory(
                LinearNode::read_register(arr_ptr),
                length_offset,
                PhysicalType::Primitive(PhysicalPrimitive::PointerSize),
            ),
        ),
        // if (length + 1) * 2 > capacity, realloc
        LinearNode::if_node(
            LinearNode::ptr_comparison(
                ComparisonOp::GreaterThan,
                LinearNode::ptr_arithmetic(
                    ArithmeticOp::Multiply,
                    LinearNode::ptr_arithmetic(
                        ArithmeticOp::Add,
                        LinearNode::read_register(length_register),
                        LinearNode::size(1),
                    ),
                    LinearNode::size(2),
                ),
                LinearNode::read_memory(
                    LinearNode::read_register(arr_ptr),
                    capacity_offset,
                    PhysicalType::Primitive(PhysicalPrimitive::PointerSize),
                ),
            ),
            // Increase capacity
            vec![
                // new capacity = (length + 1) * 2
                LinearNode::write_register(
                    new_capacity_register,
                    LinearNode::ptr_arithmetic(
                        ArithmeticOp::Multiply,
                        LinearNode::ptr_arithmetic(
                            ArithmeticOp::Add,
                            LinearNode::read_register(length_register),
                            LinearNode::size(1),
                        ),
                        LinearNode::size(2),
                    ),
                ),
                // allocate new buffer
                LinearNode::write_register(
                    buffer_register,
                    LinearNode::call_runtime(
                        RuntimeFunction::Realloc,
                        vec![
                            LinearNode::read_register(buffer_register),
                            LinearNode::ptr_arithmetic(
                                ArithmeticOp::Multiply,
                                LinearNode::read_register(new_capacity_register),
                                LinearNode::size(elem_size),
                            ),
                        ],
                    ),
                ),
                LinearNode::write_memory(
                    LinearNode::read_register(arr_ptr),
                    0,
                    PhysicalType::Primitive(PhysicalPrimitive::PointerSize),
                    LinearNode::read_register(buffer_register),
                ),
                // write new capacity
                LinearNode::write_memory(
                    LinearNode::read_register(arr_ptr),
                    capacity_offset,
                    PhysicalType::Primitive(PhysicalPrimitive::PointerSize),
                    LinearNode::read_register(new_capacity_register),
                ),
            ],
            None,
            provenance,
        ),
        // increment length
        LinearNode::write_memory(
            LinearNode::read_register(arr_ptr),
            length_offset,
            PhysicalType::Primitive(PhysicalPrimitive::PointerSize),
            LinearNode::ptr_arithmetic(
                ArithmeticOp::Add,
                LinearNode::read_register(length_register),
                LinearNode::size(1),
            ),
        ),
        // Return pointer to now-writable location
        LinearNode::ptr_arithmetic(
            ArithmeticOp::Add,
            LinearNode::read_register(buffer_register),
            LinearNode::ptr_arithmetic(
                ArithmeticOp::Multiply,
                LinearNode::read_register(length_register),
                LinearNode::size(elem_size),
            ),
        ),
        LinearNode::kill_register(arr_ptr),
        LinearNode::kill_register(length_register),
        LinearNode::kill_register(new_capacity_register),
        LinearNode::kill_register(buffer_register),
    ])
}

// TODO: make this work as a function, rather than inlined?
/**
 * Returns a nullable pointer to the given dictionary entry
 */
fn dict_get_entry_for_key(
    dict_pointer: LinearNode,
    key_location: LinearNode,
    key_offset: usize,
    key_ty: PhysicalPrimitive,
    entry_size: usize,
    entry_pointer_output: RegisterID,
) -> LinearNode {
    let key_ptr = RegisterID::new();
    let dict_length = RegisterID::new();
    let index = RegisterID::new();

    LinearNode::new(LinearNodeValue::Sequence(vec![
        LinearNode::write_register(key_ptr, key_location),
        LinearNode::write_multi_register(
            dict_pointer,
            vec![Some(entry_pointer_output), Some(dict_length), None],
        ),
        LinearNode::write_register(index, LinearNode::size(0)),
        LinearNode::new(LinearNodeValue::Loop(vec![
            // Check if we've found the key
            LinearNode::if_node(
                LinearNode::new(LinearNodeValue::Comparison(
                    ComparisonOp::EqualTo,
                    key_ty,
                    Box::new(LinearNode::read_memory(
                        LinearNode::read_register(entry_pointer_output),
                        0,
                        PhysicalType::Primitive(key_ty),
                    )),
                    Box::new(LinearNode::read_memory(
                        LinearNode::read_register(key_ptr),
                        key_offset,
                        PhysicalType::Primitive(key_ty),
                    )),
                )),
                vec![
                    LinearNode::new(LinearNodeValue::Byte(1)),
                    LinearNode::new(LinearNodeValue::Break),
                ],
                None,
                None,
            ),
            // Increment the length counter and check if we've overflowed the bounds
            LinearNode::write_register(
                index,
                LinearNode::ptr_arithmetic(
                    ArithmeticOp::Add,
                    LinearNode::read_register(index),
                    LinearNode::size(1),
                ),
            ),
            LinearNode::write_register(
                entry_pointer_output,
                LinearNode::ptr_arithmetic(
                    ArithmeticOp::Add,
                    LinearNode::read_register(entry_pointer_output),
                    LinearNode::size(entry_size),
                ),
            ),
            LinearNode::if_node(
                LinearNode::ptr_comparison(
                    ComparisonOp::EqualTo,
                    LinearNode::read_register(dict_length),
                    LinearNode::read_register(index),
                ),
                vec![
                    LinearNode::new(LinearNodeValue::Byte(0)),
                    LinearNode::new(LinearNodeValue::Break),
                ],
                None,
                None,
            ),
        ])),
        LinearNode::kill_register(key_ptr),
        LinearNode::kill_register(dict_length),
        LinearNode::kill_register(index),
    ]))
}

const UNION_TAG_SIZE: SizeInPointers = SizeInPointers(1);
// TODO: wait shouldn't this always be a single byte
pub const NULL_TAG_SIZE: SizeInPointers = SizeInPointers(1);

fn primitive_to_physical(p: PrimitiveType) -> PhysicalPrimitive {
    match p {
        // TODO: should chars be 4 byte or 1 byte
        PrimitiveType::Char => PhysicalPrimitive::Byte,
        PrimitiveType::Int32 => PhysicalPrimitive::Int32,
        PrimitiveType::Float32 => PhysicalPrimitive::Float32,
        PrimitiveType::Int64 => PhysicalPrimitive::Int64,
        PrimitiveType::Float64 => PhysicalPrimitive::Float64,
        PrimitiveType::Bool => PhysicalPrimitive::Byte,
        PrimitiveType::PointerSize => PhysicalPrimitive::PointerSize,
    }
}

fn primitive_type_size(prim: PhysicalPrimitive, byte_size: usize, pointer_size: usize) -> usize {
    match prim {
        PhysicalPrimitive::Int32 => 4,
        PhysicalPrimitive::Float32 => 4,
        PhysicalPrimitive::Int64 => 8,
        PhysicalPrimitive::Float64 => 8,
        PhysicalPrimitive::Byte => byte_size,
        PhysicalPrimitive::FunctionPointer | PhysicalPrimitive::PointerSize => pointer_size,
    }
}

// TODO: move to its own module?
#[derive(Debug)]
pub struct DeclaredTypeLayout {
    pub value: TypeLayoutValue,
    // TODO: remove field?
    pub size: usize,
}

impl DeclaredTypeLayout {
    fn size(&self) -> usize {
        self.size
    }
}

#[derive(Debug)]
pub enum TypeLayoutValue {
    Structure(Vec<(String, usize, PhysicalType)>),
    Interface(Vec<FunctionID>),
    Union(HashMap<String, (usize, Option<PhysicalType>)>),
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum PhysicalPrimitive {
    Byte,
    Int32,
    Float32,
    Int64,
    Float64,
    PointerSize,
    FunctionPointer,
}

#[derive(Clone, Debug)]
pub enum PhysicalType {
    Primitive(PhysicalPrimitive),
    Referenced(TypeID),
    Nullable(Box<PhysicalType>),
    Collection(PhysicalCollection),
    /// [function ID, resume point, stack ptr]
    Generator,
}

#[derive(Clone, Debug)]
pub enum PhysicalCollection {
    Array,
    Dict,
    String,
}

impl PhysicalType {
    pub fn size(&self, ctx: &LinearContext<'_>) -> usize {
        self.size_from_decls(ctx.layouts, ctx.byte_size, ctx.pointer_size)
    }

    pub fn size_from_decls(
        &self,
        declarations: &HashMap<TypeID, DeclaredTypeLayout>,
        byte_size: usize,
        pointer_size: usize,
    ) -> usize {
        match self {
            PhysicalType::Primitive(p) => primitive_type_size(*p, byte_size, pointer_size),
            PhysicalType::Referenced(id) => declarations[id].size(),
            PhysicalType::Nullable(ty) => {
                NULL_TAG_SIZE.size(pointer_size)
                    + ty.size_from_decls(declarations, byte_size, pointer_size)
            }
            PhysicalType::Collection(ty) => match ty {
                PhysicalCollection::Array => pointer_size * 3,
                PhysicalCollection::Dict => pointer_size * 3,
                PhysicalCollection::String => pointer_size * 2,
            },
            PhysicalType::Generator => pointer_size * 3,
        }
    }

    fn zeroed(&self, ctx: &LinearContext, nodes: &mut Vec<LinearNode>) {
        match self {
            PhysicalType::Primitive(p) => nodes.push(LinearNode::new(match p {
                PhysicalPrimitive::Byte => LinearNodeValue::Byte(0),
                PhysicalPrimitive::Int32 | PhysicalPrimitive::Int64 => LinearNodeValue::Int(0),
                PhysicalPrimitive::Float32 | PhysicalPrimitive::Float64 => {
                    LinearNodeValue::Float(0.0)
                }
                PhysicalPrimitive::PointerSize => LinearNodeValue::Size(0),
                PhysicalPrimitive::FunctionPointer => {
                    LinearNodeValue::FunctionID(FunctionID::zeroed())
                }
            })),
            PhysicalType::Referenced(ty_id) => {
                let ty = &ctx.layouts[ty_id];
                match &ty.value {
                    TypeLayoutValue::Structure(fields) => {
                        for (_, _, field) in fields.iter() {
                            field.zeroed(ctx, nodes);
                        }
                    }
                    TypeLayoutValue::Interface(_) => todo!(),
                    TypeLayoutValue::Union(_) => todo!(),
                }
            }
            PhysicalType::Nullable(inner) => {
                nodes.push(LinearNode::bool_value(false));
                inner.zeroed(ctx, nodes);
            }
            PhysicalType::Collection(PhysicalCollection::String) => {
                for _ in 0..3 {
                    nodes.push(LinearNode::new(LinearNodeValue::Size(0)));
                }
            }
            PhysicalType::Collection(PhysicalCollection::Dict | PhysicalCollection::Array)
            | PhysicalType::Generator => {
                for _ in 0..3 {
                    nodes.push(LinearNode::new(LinearNodeValue::Size(0)));
                }
            }
        }
    }
}

pub fn layout_types(
    declarations: &HashMap<TypeID, TypeDeclaration>,
    layouts: &mut HashMap<TypeID, DeclaredTypeLayout>,
    byte_size: usize,
    pointer_size: usize,
) {
    for decl in declarations.values() {
        layout_static_decl(declarations, layouts, decl, byte_size, pointer_size);
    }
}

fn layout_static_decl(
    declarations: &HashMap<TypeID, TypeDeclaration>,
    layouts: &mut HashMap<TypeID, DeclaredTypeLayout>,
    decl: &TypeDeclaration,
    byte_size: usize,
    pointer_size: usize,
) -> usize {
    if let Some(layout) = layouts.get(&decl.id()) {
        return layout.size;
    }

    let layout = match decl {
        TypeDeclaration::Struct(struct_ty) => {
            let mut size = 0;
            let fields = struct_ty
                .fields
                .iter()
                .map(|(name, field)| {
                    let field = layout_type(declarations, layouts, field, byte_size, pointer_size);
                    let field_size = field.size_from_decls(layouts, byte_size, pointer_size);
                    let offset = size;
                    size += field_size;
                    (name.clone(), offset, field)
                })
                .collect();
            DeclaredTypeLayout {
                value: TypeLayoutValue::Structure(fields),
                size,
            }
        }
        TypeDeclaration::Interface(interface_ty) => {
            let mut size = pointer_size;
            let fields = interface_ty
                .associated_functions
                .values()
                .map(|decl| {
                    size += pointer_size;
                    *decl
                })
                .collect();
            DeclaredTypeLayout {
                value: TypeLayoutValue::Interface(fields),
                size,
            }
        }
        TypeDeclaration::Union(union_ty) => {
            let mut largest_variant = 0;
            let variants = union_ty
                .variant_order
                .iter()
                .enumerate()
                .map(|(idx, name)| {
                    let ty = &union_ty.variants[name];
                    let variant = ty
                        .as_ref()
                        .map(|ty| layout_type(declarations, layouts, ty, byte_size, pointer_size));
                    let variant_size = variant
                        .as_ref()
                        .map(|variant| variant.size_from_decls(layouts, byte_size, pointer_size))
                        .unwrap_or(0);
                    if variant_size > largest_variant {
                        largest_variant = variant_size;
                    }

                    (name.clone(), (idx, variant))
                })
                .collect();

            DeclaredTypeLayout {
                value: TypeLayoutValue::Union(variants),
                size: UNION_TAG_SIZE.size(pointer_size) + largest_variant,
            }
        }
        TypeDeclaration::Module(_module) => {
            // Modules are completely compiled out
            return 0;
        }
    };
    let size = layout.size;
    layouts.insert(decl.id(), layout);

    size
}

fn layout_type(
    declarations: &HashMap<TypeID, TypeDeclaration>,
    layouts: &mut HashMap<TypeID, DeclaredTypeLayout>,
    ty: &ExpressionType,
    byte_size: usize,
    pointer_size: usize,
) -> PhysicalType {
    match ty {
        ExpressionType::Void | ExpressionType::Unreachable | ExpressionType::Null => unreachable!(),
        ExpressionType::InstanceOf(id) => {
            layout_static_decl(
                declarations,
                layouts,
                &declarations[id],
                byte_size,
                pointer_size,
            );
            PhysicalType::Referenced(*id)
        }
        _ => expr_ty_to_physical(ty),
    }
}

pub fn expr_ty_to_physical(ty: &ExpressionType) -> PhysicalType {
    match ty {
        ExpressionType::Void | ExpressionType::Unreachable | ExpressionType::Null => {
            unreachable!("{ty:?}")
        }
        ExpressionType::Primitive(p) => PhysicalType::Primitive(primitive_to_physical(*p)),
        ExpressionType::InstanceOf(id) => PhysicalType::Referenced(*id),
        ExpressionType::Collection(c) => PhysicalType::Collection(match c {
            CollectionType::Array(_) => PhysicalCollection::Array,
            CollectionType::Dict(_, _) => PhysicalCollection::Dict,
            CollectionType::String => PhysicalCollection::String,
            CollectionType::ReferenceCounter(_) => {
                return PhysicalType::Primitive(PhysicalPrimitive::PointerSize);
            }
            CollectionType::Cell(inner) => return expr_ty_to_physical(inner.as_ref()),
        }),
        ExpressionType::Pointer(_, _) => PhysicalType::Primitive(PhysicalPrimitive::PointerSize),
        ExpressionType::Nullable(inner) => {
            let ty = expr_ty_to_physical(inner);
            PhysicalType::Nullable(Box::new(ty))
        }
        ExpressionType::ReferenceToType(_) => todo!(),
        ExpressionType::ReferenceToFunction(_) => todo!(),
        ExpressionType::TypeParameterReference(_) => todo!(),
        ExpressionType::Generator { .. } => PhysicalType::Generator,
        ExpressionType::FunctionReference { .. } => {
            PhysicalType::Primitive(PhysicalPrimitive::FunctionPointer)
        }
    }
}

pub struct SizeInPointers(pub usize);

impl SizeInPointers {
    pub fn size(&self, pointer_size: usize) -> usize {
        self.0 * pointer_size
    }
}
