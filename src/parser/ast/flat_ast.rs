use crate::parser;
use crate::tokenizer::token::{Constant, TokenPos};

use ast::Ival as AstIval;
use ast::*;
use parser::ast;
use parser::Rvalue;

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

impl From<AstIval> for Ival {
    fn from(x: AstIval) -> Self {
        match x {
            AstIval::Constant(ConstantNode { position, constant }) => Ival {
                value: NameOrConstant::Constant(constant),
                pos: position,
            },
            AstIval::Name(name, pos) => Ival {
                value: NameOrConstant::Name(name),
                pos,
            },
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub(crate) enum FlatDefinitionInfo {
    Variable {
        ival: Option<Ival>,
    },
    Vector {
        specified_size: Option<ConstantNode>,
        ivals: Vec<Ival>,
    },
    Function {
        params: Vec<(String, TokenPos)>,
    },
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub(crate) struct FlatDefinition {
    pub(crate) name: String,
    pub(crate) info: FlatDefinitionInfo,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub(crate) enum FlatDeclarationNameInfo {
    Auto {
        specified_size_if_vec: Option<ConstantNode>,
    },
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
        self.definitions
            .into_iter()
            .flat_map(FlattenNode::flatten_node)
            .collect()
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
                    ival: self.initial_value.map(Ival::from),
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
                    ivals: self.initial_values.into_iter().map(Ival::from).collect(),
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
                info: FlatDefinitionInfo::Function {
                    params: self.parameters,
                },
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
            matches!(
                self.statement.as_ref(),
                Statement::Compound(_)
                    | Statement::Switch(_)
                    | Statement::If(_)
                    | Statement::While(_)
            )
        };
        let mut res = match *self.statement {
            Statement::Null => vec![],
            Statement::Compound(comp_stmt) => comp_stmt.flatten_node(),
            Statement::Declaration(decl) => decl.flatten_node(),

            Statement::Return(ret) => {
                vec![FlatNodeAndPos {
                    pos: self.position,
                    node: FlatNode::Return(if let Some(rv) = ret.rvalue {
                        Some(*rv.rvalue)
                    } else {
                        None
                    }),
                }]
            }

            Statement::Switch(sw) => {
                let mut res = vec![FlatNodeAndPos {
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
                let mut res = vec![FlatNodeAndPos {
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
                let mut res = vec![FlatNodeAndPos {
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
                vec![FlatNodeAndPos {
                    node: FlatNode::Break,
                    pos: self.position,
                }]
            }

            Statement::Continue(_cn) => {
                vec![FlatNodeAndPos {
                    node: FlatNode::Continue,
                    pos: self.position,
                }]
            }
        };

        if append_end_marker {
            res.push(FlatNodeAndPos {
                node: FlatNode::EndOfStmt {
                    positions_away: res.len(),
                },
                pos: res.last().unwrap().pos,
            })
        }
        res
    }
}

impl FlattenNode for CompoundStatementNode {
    fn flatten_node(self) -> Vec<FlatNodeAndPos> {
        let mut res = vec![FlatNodeAndPos {
            node: FlatNode::Compound,
            pos: self.position,
        }];
        let body: Vec<FlatNodeAndPos> = self
            .statement_list
            .into_iter()
            .flat_map(FlattenNode::flatten_node)
            .collect();

        res.extend(body);
        res
    }
}

impl FlattenNode for DeclarationNode {
    fn flatten_node(self) -> Vec<FlatNodeAndPos> {
        match self {
            DeclarationNode::Auto(auto_decl) => auto_decl.flatten_node(),
            DeclarationNode::Extern(extern_decl) => extern_decl.flatten_node(),
            DeclarationNode::Label(label_decl) => label_decl.flatten_node(),
        }
    }
}

impl FlattenNode for AutoDeclarationNode {
    fn flatten_node(self) -> Vec<FlatNodeAndPos> {
        let mut res = self
            .declarations
            .into_iter()
            .flat_map(FlattenNode::flatten_node)
            .collect::<Vec<FlatNodeAndPos>>();
        res.extend(self.next_statement.flatten_node());
        res
    }
}

impl FlattenNode for AutoDeclaration {
    fn flatten_node(self) -> Vec<FlatNodeAndPos> {
        vec![FlatNodeAndPos {
            node: FlatNode::Decl(FlatDeclaration {
                info: FlatDeclarationNameInfo::Auto {
                    specified_size_if_vec: self.size_if_vector,
                },
                name: self.name,
            }),
            pos: self.position,
        }]
    }
}

impl FlattenNode for ExternDeclarationNode {
    fn flatten_node(self) -> Vec<FlatNodeAndPos> {
        let mut res = self
            .names
            .into_iter()
            .map(|(name, pos)| FlatNodeAndPos {
                node: FlatNode::Decl(FlatDeclaration {
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
