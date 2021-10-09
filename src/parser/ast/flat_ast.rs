use std::*;

use ast::*;
use token::TokenPos;

use crate::tokenizer::token;
use crate::parser::ast;

pub(crate) trait FlattenNode {
    fn flatten_node(self) -> Vec<FlatNodeAndPos>;
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub(crate) enum NameOrConstant {
    Name(String),
    Constant(Constant),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub(crate) struct Ival {
    pub(crate) pos: TokenPos,
    pub(crate) value: NameOrConstant,
}

impl From<ast::Ival> for Ival {
    fn from(x: ast::Ival) -> Self {
        match x {
            ast::Ival::Constant(ConstantNode { position, constant }) => Ival {
                value: NameOrConstant::Constant(constant),
                pos: position,
            },
            ast::Ival::Name(name, pos) => Ival {
                value: NameOrConstant::Name(name),
                pos,
            }
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub(crate) enum FlatDefinitionInfo {
    Variable { ival: Option<Ival> },
    Vector { specified_size: Option<ConstantNode>, ivals: Vec<Ival> },
    Function { params: Vec<(String, TokenPos)> },
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub(crate) struct FlatDefinition {
    pub(crate) name: String,
    pub(crate) info: FlatDefinitionInfo,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub(crate) enum FlatDeclarationNameInfo {
    Auto { specified_size_if_vec: Option<ConstantNode> },
    Extern,
    Label,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub(crate) struct FlatDeclaration {
    pub(crate) name: String,
    pub(crate) info: FlatDeclarationNameInfo,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub(crate) enum FlatNode {
    EndOfStmt { positions_away: usize },
    Def(FlatDefinition),
    Decl(FlatDeclaration),
    Compound,
    Rvalue(Rvalue),
    If(Rvalue),
    Else,
    While(Rvalue),
    Switch(Rvalue),
    Break,
    Continue,
    Goto(Rvalue),
    Case(Option<ConstantNode>),
    Return(Option<Rvalue>),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub(crate) struct FlatNodeAndPos {
    pub(crate) node: FlatNode,
    pub(crate) pos: TokenPos,
}

impl FlattenNode for ProgramNode {
    fn flatten_node(self) -> Vec<FlatNodeAndPos> {
        self.definitions.into_iter().map(|def| def.flatten_node()).flatten().collect()
    }
}

impl FlattenNode for DefinitionNode {
    fn flatten_node(self) -> Vec<FlatNodeAndPos> {
        match self {
            DefinitionNode::Variable(var_def) => var_def.flatten_node(),
            DefinitionNode::Vector(vec_def) => vec_def.flatten_node(),
            DefinitionNode::Function(fn_def) => fn_def.flatten_node(),
        }
    }
}

impl FlattenNode for VariableDefinitionNode {
    fn flatten_node(self) -> Vec<FlatNodeAndPos> {
        vec![FlatNodeAndPos {
            node: FlatNode::Def(FlatDefinition {
                name: self.name,
                info: FlatDefinitionInfo::Variable {
                    ival: self.initial_value.map(Ival::from)
                },
            }),
            pos: self.position,
        }]
    }
}

impl FlattenNode for VectorDefinitionNode {
    fn flatten_node(self) -> Vec<FlatNodeAndPos> {
        vec![FlatNodeAndPos {
            node: FlatNode::Def(FlatDefinition {
                name: self.name,
                info: FlatDefinitionInfo::Vector {
                    specified_size: self.specified_size,
                    ivals: self.initial_values
                        .into_iter()
                        .map(|ival| Ival::from(ival))
                        .collect(),
                },
            }),
            pos: self.position,
        }]
    }
}

impl FlattenNode for FunctionDefinitionNode {
    fn flatten_node(self) -> Vec<FlatNodeAndPos> {
        let mut res = vec![FlatNodeAndPos {
            node: FlatNode::Def(FlatDefinition {
                name: self.name,
                info: FlatDefinitionInfo::Function { params: self.parameters },
            }),
            pos: self.position,
        }];
        res.extend(self.body.flatten_node());
        res
    }
}

impl FlattenNode for StatementNode {
    fn flatten_node(self) -> Vec<FlatNodeAndPos> {
        let append_end_marker = {
            use Statement::*;
            if let Compound(_) | Switch(_) | If(_) | While(_) = self.statement.as_ref() {
                true
            } else {
                false
            }
        };
        let mut res = match *self.statement {
            Statement::NullStatement => vec![],
            Statement::Compound(comp_stmt) => comp_stmt.flatten_node(),
            Statement::Declaration(decl) => decl.flatten_node(),

            Statement::Return(ret) => {
                vec![FlatNodeAndPos {
                    pos: self.position,
                    node: FlatNode::Return(
                        if let Some(rv) = ret.rvalue {
                            Some(*rv.rvalue)
                        } else {
                            None
                        }),
                }]
            }

            Statement::Switch(sw) => {
                let mut res = vec![
                    FlatNodeAndPos {
                        node: FlatNode::Switch(*sw.rvalue.rvalue),
                        pos: self.position,
                    }];
                res.extend(sw.body.flatten_node());
                res
            }

            Statement::Case(cs) => {
                let mut res = vec![FlatNodeAndPos {
                    pos: self.position,
                    node: FlatNode::Case(cs.constant_if_not_default),
                }];
                res.extend(cs.next_statement.flatten_node());
                res
            }

            Statement::If(r#if) => {
                let mut res = vec![
                    FlatNodeAndPos {
                        node: FlatNode::If(*r#if.condition.rvalue),
                        pos: self.position,
                    }];
                res.extend(r#if.body.flatten_node());

                if let Some(else_node) = r#if.r#else {
                    res.push(FlatNodeAndPos {
                        node: FlatNode::Else,
                        pos: else_node.position,
                    });
                    res.extend(else_node.else_body.flatten_node());
                }
                res
            }

            Statement::While(wh) => {
                let mut res = vec![
                    FlatNodeAndPos {
                        node: FlatNode::While(*wh.condition.rvalue),
                        pos: self.position,
                    }];
                res.extend(wh.body.flatten_node());
                res
            }

            Statement::Goto(goto) => {
                vec![FlatNodeAndPos {
                    node: FlatNode::Goto(*goto.label.rvalue),
                    pos: self.position,
                }]
            }
            Statement::RvalueAndSemicolon(rv) => rv.flatten_node(),

            Statement::Break(_br) => {
                vec![FlatNodeAndPos { node: FlatNode::Break, pos: self.position }]
            }

            Statement::Continue(_cn) => {
                vec![FlatNodeAndPos { node: FlatNode::Continue, pos: self.position }]
            }
        };

        if append_end_marker {
            res.push(FlatNodeAndPos {
                node: FlatNode::EndOfStmt { positions_away: res.len() },
                pos: res.last().unwrap().pos,
            })
        }
        res
    }
}

impl FlattenNode for CompoundStatementNode {
    fn flatten_node(self) -> Vec<FlatNodeAndPos> {
        let mut res = vec![FlatNodeAndPos { node: FlatNode::Compound, pos: self.position }];
        let body: Vec<FlatNodeAndPos> = self.statement_list
            .into_iter()
            .map(|stmt| stmt.flatten_node())
            .flatten()
            .collect();

        res.extend(body);
        res
    }
}

impl FlattenNode for DeclarationNode {
    fn flatten_node(self) -> Vec<FlatNodeAndPos> {
        match self {
            DeclarationNode::AutoDeclaration(auto_decl) => auto_decl.flatten_node(),
            DeclarationNode::ExternDeclaration(extern_decl) => extern_decl.flatten_node(),
            DeclarationNode::LabelDeclaration(label_decl) => label_decl.flatten_node(),
        }
    }
}

impl FlattenNode for AutoDeclarationNode {
    fn flatten_node(self) -> Vec<FlatNodeAndPos> {
        let mut res = self.declarations
            .into_iter()
            .map(|auto_decl| auto_decl.flatten_node())
            .flatten()
            .collect::<Vec<FlatNodeAndPos>>();
        res.extend(self.next_statement.flatten_node());
        res
    }
}

impl FlattenNode for AutoDeclaration {
    fn flatten_node(self) -> Vec<FlatNodeAndPos> {
        vec![FlatNodeAndPos {
            node: FlatNode::Decl(
                FlatDeclaration {
                    info: FlatDeclarationNameInfo::Auto { specified_size_if_vec: self.size_if_vector },
                    name: self.name,
                }),
            pos: self.position,
        }]
    }
}

impl FlattenNode for ExternDeclarationNode {
    fn flatten_node(self) -> Vec<FlatNodeAndPos> {
        let mut res = self.names
            .into_iter()
            .map(|(name, pos)| FlatNodeAndPos {
                node: FlatNode::Decl(
                    FlatDeclaration {
                        name,
                        info: FlatDeclarationNameInfo::Extern,
                    }),
                pos,
            })
            .collect::<Vec<FlatNodeAndPos>>();
        res.extend(self.next_statement.flatten_node());
        res
    }
}

impl FlattenNode for LabelDeclarationNode {
    fn flatten_node(self) -> Vec<FlatNodeAndPos> {
        let mut res = vec![FlatNodeAndPos {
            node: FlatNode::Decl(FlatDeclaration {
                name: self.label_name,
                info: FlatDeclarationNameInfo::Label,
            }),
            pos: self.position,
        }];
        res.extend(self.next_statement.flatten_node());
        res
    }
}

impl FlattenNode for RvalueAndSemicolonNode {
    fn flatten_node(self) -> Vec<FlatNodeAndPos> {
        vec![FlatNodeAndPos {
            pos: self.rvalue.position,
            node: FlatNode::Rvalue(*self.rvalue.rvalue),
        }]
    }
}
