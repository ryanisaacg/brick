pub struct SourceTree<Statement, Expression> {
    expressions: Vec<Expression>,
    statements: Vec<Statement>,
    children_of: Box<dyn Fn(Node<&Statement, &Expression>, &mut Vec<NodePtr>)>,
}

#[derive(Copy, Clone, Debug)]
pub enum Node<Statement, Expression> {
    Statement(Statement),
    Expression(Expression),
}

#[derive(Clone, Copy, Debug)]
pub enum NodePtr {
    Statement(usize),
    Expression(usize),
}

impl<Statement, Expression> SourceTree<Statement, Expression> {
    pub fn new(
        children_of: Box<dyn Fn(Node<&Statement, &Expression>, &mut Vec<NodePtr>)>,
    ) -> SourceTree<Statement, Expression> {
        SourceTree {
            expressions: Vec::new(),
            statements: Vec::new(),
            children_of,
        }
    }

    pub fn node(&self, node: NodePtr) -> Node<&Statement, &Expression> {
        match node {
            NodePtr::Statement(idx) => Node::Statement(self.statement(idx)),
            NodePtr::Expression(idx) => Node::Expression(self.expression(idx)),
        }
    }

    pub fn expression(&self, index: usize) -> &Expression {
        &self.expressions[index]
    }

    pub fn add_expression(&mut self, expr: Expression) -> usize {
        let index = self.expressions.len();
        self.expressions.push(expr);
        index
    }

    pub fn statement(&self, index: usize) -> &Statement {
        &self.statements[index]
    }

    pub fn add_statement(&mut self, statement: Statement) -> usize {
        let index = self.statements.len();
        self.statements.push(statement);
        index
    }

    pub fn iter_from<'a>(&'a self, root: NodePtr) -> NodeIterator<'a, Statement, Expression> {
        NodeIterator {
            source_tree: self,
            call_stack: vec![root],
        }
    }
}

pub struct NodeIterator<'a, Statement, Expression> {
    source_tree: &'a SourceTree<Statement, Expression>,
    call_stack: Vec<NodePtr>,
}

impl<'a, Statement, Expression> Iterator for NodeIterator<'a, Statement, Expression> {
    type Item = Node<&'a Statement, &'a Expression>;

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
