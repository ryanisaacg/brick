pub struct ArenaIter<'a, T: ArenaNode> {
    arena: &'a [T],
    stack: Vec<usize>,
}

impl<'a, T: ArenaNode> ArenaIter<'a, T> {
    pub fn iter_from(arena: &'a [T], root: usize) -> ArenaIter<'a, T> {
        Self {
            arena,
            stack: vec![root],
        }
    }
}

impl<'a, T: ArenaNode> Iterator for ArenaIter<'a, T> {
    type Item = (usize, &'a T);

    fn next(&mut self) -> Option<Self::Item> {
        self.stack.pop().map(|index| {
            let node = &self.arena[index];
            node.write_children(&mut self.stack);

            (index, node)
        })
    }
}

pub trait ArenaNode {
    fn write_children(&self, children: &mut Vec<usize>);
}
