use std::sync::atomic::{AtomicUsize, Ordering};

use bytemuck::{Pod, Zeroable};

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct AnyID(u32);

impl AnyID {
    pub fn as_type(&self) -> TypeID {
        TypeID(self.0)
    }

    pub fn as_var(&self) -> VariableID {
        VariableID(self.0)
    }

    pub fn as_fn(&self) -> FunctionID {
        FunctionID(self.0)
    }
}

impl From<TypeID> for AnyID {
    fn from(value: TypeID) -> Self {
        AnyID(value.0)
    }
}

impl From<NodeID> for AnyID {
    fn from(value: NodeID) -> Self {
        AnyID(value.0)
    }
}

impl From<VariableID> for AnyID {
    fn from(value: VariableID) -> Self {
        AnyID(value.0)
    }
}

impl From<FunctionID> for AnyID {
    fn from(value: FunctionID) -> Self {
        AnyID(value.0)
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
