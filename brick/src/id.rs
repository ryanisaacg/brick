use std::sync::atomic::{AtomicUsize, Ordering};

//use bytemuck::{Pod, Zeroable};

pub use crate::declaration_context::FunctionID;
use crate::declaration_context::TypeID;

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum AnyID {
    Type(TypeID),
    Variable(VariableID),
    Function(FunctionID),
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
