use std::sync::atomic::{AtomicUsize, Ordering};

use bytemuck::{Pod, Zeroable};

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct ID(u32);

impl ID {
    pub fn dummy() -> ID {
        Self(0)
    }

    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        Self(get_id() as u32)
    }
}

static COUNTER: AtomicUsize = AtomicUsize::new(1);
fn get_id() -> usize {
    COUNTER.fetch_add(1, Ordering::Relaxed)
}

unsafe impl Zeroable for ID {
    fn zeroed() -> Self {
        ID(0)
    }
}
unsafe impl Pod for ID {}
