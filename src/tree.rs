use std::fmt;

type FindChildren<S, E, K> = Box<dyn Fn(Node<&S, &E, &K>, &mut Vec<NodePtr>)>;

// TODO: maybe unify into one vec?

pub struct SourceTree<Statement, Expression, Kind> {
    expressions: Vec<Expression>,
    statements: Vec<Statement>,
    kinds: Vec<Kind>,
    children_of: FindChildren<Statement, Expression, Kind>,
}

impl<S: fmt::Debug, E: fmt::Debug, K: fmt::Debug> fmt::Debug for SourceTree<S, E, K> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "expressions: {:?}\nstatements: {:?}\ntypes: {:?}",
            self.expressions, self.statements, self.kinds
        )
    }
}

#[derive(Copy, Clone, Debug)]
pub enum Node<Statement, Expression, Kind> {
    Statement(Statement),
    Expression(Expression),
    Kind(Kind),
}

#[derive(Clone, Copy, Debug)]
pub enum NodePtr {
    Statement(usize),
    Expression(usize),
    Kind(usize),
}

impl<S, E, K> SourceTree<S, E, K> {
    pub fn new(children_of: FindChildren<S, E, K>) -> SourceTree<S, E, K> {
        SourceTree {
            expressions: Vec::new(),
            statements: Vec::new(),
            kinds: Vec::new(),
            children_of,
        }
    }

    pub fn node(&self, node: NodePtr) -> Node<&S, &E, &K> {
        match node {
            NodePtr::Statement(idx) => Node::Statement(self.statement(idx)),
            NodePtr::Expression(idx) => Node::Expression(self.expression(idx)),
            NodePtr::Kind(idx) => Node::Kind(self.kind(idx)),
        }
    }

    pub fn expression(&self, index: usize) -> &E {
        &self.expressions[index]
    }

    pub fn add_expression(&mut self, expr: E) -> usize {
        let index = self.expressions.len();
        self.expressions.push(expr);
        index
    }

    pub fn statement(&self, index: usize) -> &S {
        &self.statements[index]
    }

    pub fn add_statement(&mut self, statement: S) -> usize {
        let index = self.statements.len();
        self.statements.push(statement);
        index
    }

    pub fn kind(&self, index: usize) -> &K {
        &self.kinds[index]
    }

    pub fn add_kind(&mut self, kind: K) -> usize {
        let index = self.kinds.len();
        self.kinds.push(kind);
        index
    }

    pub fn iter_from(&self, root: NodePtr) -> NodeIterator<'_, S, E, K> {
        NodeIterator {
            source_tree: self,
            call_stack: vec![root],
        }
    }
}

pub struct NodeIterator<'a, Statement, Expression, Kind> {
    source_tree: &'a SourceTree<Statement, Expression, Kind>,
    call_stack: Vec<NodePtr>,
}

impl<'a, Statement, Expression, Kind> Iterator for NodeIterator<'a, Statement, Expression, Kind> {
    type Item = Node<&'a Statement, &'a Expression, &'a Kind>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.call_stack.pop() {
            Some(node) => {
                let node = self.source_tree.node(node);
                (self.source_tree.children_of)(node, &mut self.call_stack);

                Some(node)
            }
            None => None,
        }
    }
}
