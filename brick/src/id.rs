use std::sync::atomic::{AtomicUsize, Ordering};

use bytemuck::{Pod, Zeroable};

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum AnyID {
    Type(TypeID),
    Variable(VariableID),
    Function(FunctionID),
    Node(NodeID),
}

impl AnyID {
    pub fn as_type(&self) -> TypeID {
        match self {
            AnyID::Type(id) => *id,
            other => panic!("illegal access: converting {other:?} to TypeID"),
        }
    }

    pub fn as_var(&self) -> VariableID {
        match self {
            AnyID::Variable(id) => *id,
            // TODO: remove this path - I feel worried it will cause collisions
            AnyID::Node(node_id) => node_id.as_variable(),
            other => panic!("illegal access: converting {other:?} to VariableID"),
        }
    }

    pub fn as_fn(&self) -> FunctionID {
        match self {
            AnyID::Function(id) => *id,
            other => panic!("illegal access: converting {other:?} to FunctionID"),
        }
    }
}

impl From<TypeID> for AnyID {
    fn from(value: TypeID) -> Self {
        AnyID::Type(value)
    }
}

impl From<NodeID> for AnyID {
    fn from(value: NodeID) -> Self {
        AnyID::Node(value)
    }
}

impl From<VariableID> for AnyID {
    fn from(value: VariableID) -> Self {
        AnyID::Variable(value)
    }
}

impl From<FunctionID> for AnyID {
    fn from(value: FunctionID) -> Self {
        AnyID::Function(value)
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct NodeID(u32);

impl NodeID {
    pub fn dummy() -> NodeID {
        Self(0)
    }

    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        static NODE_COUNTER: AtomicUsize = AtomicUsize::new(1);
        Self(NODE_COUNTER.fetch_add(1, Ordering::Relaxed) as u32)
    }

    pub fn as_variable(&self) -> VariableID {
        VariableID(self.0)
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct FunctionID(u32);

impl FunctionID {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        static FUNCTION_COUNTER: AtomicUsize = AtomicUsize::new(1);
        Self(FUNCTION_COUNTER.fetch_add(1, Ordering::Relaxed) as u32)
    }
}

unsafe impl Zeroable for FunctionID {
    fn zeroed() -> Self {
        FunctionID(0)
    }
}
unsafe impl Pod for FunctionID {}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct TypeID(u32);

impl TypeID {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        static TYPE_COUNTER: AtomicUsize = AtomicUsize::new(1);
        Self(TYPE_COUNTER.fetch_add(1, Ordering::Relaxed) as u32)
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct VariableID(u32);

impl VariableID {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        static VARIABLE_COUNTER: AtomicUsize = AtomicUsize::new(1);
        Self(VARIABLE_COUNTER.fetch_add(1, Ordering::Relaxed) as u32)
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct RegisterID(u32);

impl RegisterID {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        static REGISTER_COUNTER: AtomicUsize = AtomicUsize::new(1);
        Self(REGISTER_COUNTER.fetch_add(1, Ordering::Relaxed) as u32)
    }
}