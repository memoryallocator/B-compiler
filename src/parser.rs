use crate::{CompilerOptions, Token, SymbolTable, TypeOfLineNo};

pub struct AbstractSyntaxNode<'a> {
    parent: Option<&'a AbstractSyntaxNode<'a>>,
    left: Option<&'a AbstractSyntaxNode<'a>>,
    right: Option<&'a AbstractSyntaxNode<'a>>,
}

impl<'a> AbstractSyntaxNode<'a> {
    fn new() -> AbstractSyntaxNode<'a> {
        AbstractSyntaxNode {
            parent: None,
            left: None,
            right: None,
        }
    }
}

pub struct AbstractSyntaxTree<'a> {
    root: Option<AbstractSyntaxNode<'a>>
}

impl<'a> AbstractSyntaxTree<'a> {
    pub fn new() -> AbstractSyntaxTree<'a> {
        AbstractSyntaxTree {
            root: Some(AbstractSyntaxNode::new())
        }
    }
}

pub struct Parser<'a> {
    pub compiler_options: &'a CompilerOptions,
    pub syntax_tree: AbstractSyntaxTree<'a>,
}

impl<'a> Parser<'a> {
    fn generate_ast(&self, tokens: &Vec<(Token, TypeOfLineNo)>) -> AbstractSyntaxTree {
        AbstractSyntaxTree::new()
    }

    pub fn run(&mut self, tokens: &Vec<(Token, TypeOfLineNo)>) -> (AbstractSyntaxTree, SymbolTable) {
        (self.generate_ast(tokens), Default::default())
    }
}