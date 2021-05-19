use std::*;

use crate::parser::ast;
use ast::*;
use crate::lexical_analyzer::token;
use token::TokenPos;

pub(crate) trait FlattenNode {
    fn flatten_node(self) -> Vec<FlatNodeAndPos>;
}

enum NameOrConstant {
    Name(String),
    Constant(token::Constant),
}

struct Ival {
    value: NameOrConstant,
    pos: TokenPos,
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

enum FlatDefinition {
    VarDef { name: String, initial_value: Option<Ival> },
    VecDef {
        name: String,
        specified_size: Option<ConstantNode>,
        initial_values: Option<Vec<Ival>>,
    },
    FnDef {
        name: String,
        parameters: Vec<(String, TokenPos)>,
    },
}

enum FlatDeclarationType {
    Auto(Option<ConstantNode>),
    Extern,
    Label,
}

enum FlatNode {
    Def(FlatDefinition),
    Decl(FlatDeclarationType, String),
    Compound,
    EndOfCompound,
    Rvalue(Rvalue),
    If,
    Else,
    While,
    Switch,
    EndOfControlStatement,
    Break,
    Goto,
    Case(Option<ConstantNode>),
    Return { return_last_rvalue: bool },
}

pub struct FlatNodeAndPos {
    node: FlatNode,
    pos: TokenPos,
}

type FlatAst = Vec<FlatNodeAndPos>;

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
            node: FlatNode::Def(FlatDefinition::VarDef {
                name: self.name,
                initial_value: if let Some(ival) = self.initial_value {
                    Some(Ival::from(ival))
                } else {
                    None
                },
            }),
            pos: self.position,
        }]
    }
}

impl FlattenNode for VectorDefinitionNode {
    fn flatten_node(self) -> Vec<FlatNodeAndPos> {
        vec![FlatNodeAndPos {
            node: FlatNode::Def(FlatDefinition::VecDef {
                name: self.name,
                specified_size: self.specified_size,
                initial_values: if let Some(ivals) = self.initial_values {
                    Some(ivals.into_iter().map(|ival| Ival::from(ival)).collect())
                } else {
                    None
                },
            }),
            pos: self.position,
        }]
    }
}

impl FlattenNode for FunctionDefinitionNode {
    fn flatten_node(self) -> Vec<FlatNodeAndPos> {
        let mut res = vec![FlatNodeAndPos {
            node: FlatNode::Def(FlatDefinition::FnDef {
                name: self.name,
                parameters: self.parameters,
            }),
            pos: self.position,
        }];
        res.append(&mut self.body.flatten_node());
        res
    }
}

impl FlattenNode for StatementNode {
    fn flatten_node(self) -> Vec<FlatNodeAndPos> {
        match *self.statement {
            Statement::NullStatement => vec![],
            Statement::Compound(comp_stmt) => comp_stmt.flatten_node(),
            Statement::Declaration(decl) => decl.flatten_node(),

            Statement::Return(ret) => {
                let (return_last_rvalue, mut res) =
                    if let Some(rv) = ret.rvalue {
                        (true, vec![FlatNodeAndPos {
                            pos: rv.position,
                            node: FlatNode::Rvalue(*rv.rvalue),
                        }])
                    } else {
                        (false, vec![])
                    };
                res.push(FlatNodeAndPos {
                    node: FlatNode::Return { return_last_rvalue },
                    pos: self.position,
                });
                res
            }

            Statement::Switch(sw) => {
                let mut res = vec![
                    FlatNodeAndPos {
                        pos: sw.rvalue.position,
                        node: FlatNode::Rvalue(*sw.rvalue.rvalue),
                    },
                    FlatNodeAndPos {
                        node: FlatNode::Switch,
                        pos: self.position,
                    },
                ];
                res.append(&mut sw.body.flatten_node());
                res
            }

            Statement::Case(cs) => {
                let mut res = vec![FlatNodeAndPos {
                    pos: self.position,
                    node: FlatNode::Case(cs.constant),
                }];
                res.append(&mut cs.next_statement.flatten_node());
                res
            }

            Statement::If(r#if) => {
                let mut res = vec![
                    FlatNodeAndPos {
                        pos: r#if.condition.position,
                        node: FlatNode::Rvalue(*r#if.condition.rvalue),
                    },
                    FlatNodeAndPos {
                        node: FlatNode::If,
                        pos: self.position,
                    }];
                res.append(&mut r#if.body.flatten_node());

                if let Some(else_node) = r#if.r#else {
                    res.push(FlatNodeAndPos {
                        node: FlatNode::Else,
                        pos: else_node.position,
                    });
                    res.append(&mut else_node.else_body.flatten_node());
                }

                res.push(FlatNodeAndPos {
                    node: FlatNode::EndOfControlStatement,
                    pos: res.last().unwrap().pos,
                });
                res
            }

            Statement::While(wh) => {
                let mut res = vec![
                    FlatNodeAndPos {
                        pos: wh.condition.position,
                        node: FlatNode::Rvalue(*wh.condition.rvalue),
                    },
                    FlatNodeAndPos {
                        node: FlatNode::While,
                        pos: self.position,
                    }];

                res.append(&mut wh.body.flatten_node());
                res.push(
                    FlatNodeAndPos {
                        node: FlatNode::EndOfControlStatement,
                        pos: res.last().unwrap().pos,
                    });
                res
            }

            Statement::Goto(goto) => {
                vec![
                    FlatNodeAndPos {
                        pos: goto.label.position,
                        node: FlatNode::Rvalue(*goto.label.rvalue),
                    },
                    FlatNodeAndPos {
                        node: FlatNode::Goto,
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
        }
    }
}

impl FlattenNode for CompoundStatementNode {
    fn flatten_node(self) -> Vec<FlatNodeAndPos> {
        let mut res = vec![FlatNodeAndPos { node: FlatNode::Compound, pos: self.position }];
        let mut body: Vec<FlatNodeAndPos> = self.statement_list
            .into_iter()
            .map(|stmt| stmt.flatten_node())
            .flatten()
            .collect();
        let last_stmt_pos = if let Some(last_stmt) = body.last() {
            last_stmt.pos
        } else {
            self.position
        };

        res.append(&mut body);
        res.push(FlatNodeAndPos {
            node: FlatNode::EndOfCompound,
            pos: last_stmt_pos,
        });
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
        res.append(&mut self.next_statement.flatten_node());
        res
    }
}

impl FlattenNode for AutoDeclaration {
    fn flatten_node(self) -> Vec<FlatNodeAndPos> {
        vec![FlatNodeAndPos {
            node: FlatNode::Decl(FlatDeclarationType::Auto(self.vector_size), self.name),
            pos: self.position,
        }]
    }
}

impl FlattenNode for ExternDeclarationNode {
    fn flatten_node(self) -> Vec<FlatNodeAndPos> {
        let mut res = self.names
            .into_iter()
            .map(|(name, pos)| FlatNodeAndPos {
                node: FlatNode::Decl(FlatDeclarationType::Extern, name),
                pos,
            })
            .collect::<Vec<FlatNodeAndPos>>();
        res.append(&mut self.next_statement.flatten_node());
        res
    }
}

impl FlattenNode for LabelDeclarationNode {
    fn flatten_node(self) -> Vec<FlatNodeAndPos> {
        let mut res = vec![FlatNodeAndPos {
            node: FlatNode::Decl(FlatDeclarationType::Label, self.label_name),
            pos: self.position,
        }];
        res.append(&mut self.next_statement.flatten_node());
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