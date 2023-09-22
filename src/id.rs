use std::{
    collections::HashMap,
    sync::atomic::{AtomicUsize, Ordering},
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ID(u32);

pub type IDMap<T> = HashMap<ID, T>;

impl ID {
    pub fn new() -> Self {
        Self(get_id() as u32)
    }
}

static COUNTER: AtomicUsize = AtomicUsize::new(1);
fn get_id() -> usize {
    COUNTER.fetch_add(1, Ordering::Relaxed)
}
