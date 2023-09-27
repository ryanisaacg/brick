use crate::id::{IDMap, ID};

pub struct ArenaIter<'a, T: ArenaNode> {
    arena: &'a IDMap<T>,
    stack: Vec<ID>,
}

impl<'a, T: ArenaNode> ArenaIter<'a, T> {
    pub fn iter_from(arena: &'a IDMap<T>, root: ID) -> ArenaIter<'a, T> {
        Self {
            arena,
            stack: vec![root],
        }
    }
}

impl<'a, T: ArenaNode> Iterator for ArenaIter<'a, T> {
    type Item = (ID, &'a T);

    fn next(&mut self) -> Option<Self::Item> {
        self.stack.pop().map(|index| {
            let node = &self.arena[&index];
            node.write_children(&mut self.stack);

            (index, node)
        })
    }
}

pub trait ArenaNode {
    fn write_children(&self, children: &mut Vec<ID>);
}
