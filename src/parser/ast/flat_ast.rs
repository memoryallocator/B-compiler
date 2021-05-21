use std::*;

use crate::parser::ast;
use ast::*;
use crate::lexical_analyzer::token;
use token::TokenPos;

pub(crate) trait FlattenNode {
    fn flatten_node(self) -> Vec<FlatNodeAndPos>;
}

#[derive(Clone)]
pub(crate) enum NameOrConstant {
    Name(String),
    Constant(token::Constant),
}

#[derive(Clone)]
pub(crate) struct Ival {
    pub(crate) value: NameOrConstant,
    pub(crate) pos: TokenPos,
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

#[derive(Clone)]
pub(crate) enum FlatDefinition {
    VarDef { name: String, initial_value: Option<Ival> },
    VecDef {
        name: String,
        specified_size: Option<ConstantNode>,
        initial_values: Vec<Ival>,
    },
    FnDef {
        name: String,
        parameters: Vec<(String, TokenPos)>,
    },
}

#[derive(Clone)]
pub(crate) enum FlatDeclarationType {
    Auto(Option<ConstantNode>),
    Extern,
    Label,
}

#[derive(Clone)]
pub(crate) enum FlatNode {
    RestoreScope,
    Def(FlatDefinition),
    Decl(FlatDeclarationType, String),
    Compound,
    Rvalue(Rvalue),
    If(Rvalue),
    Else,
    While(Rvalue),
    Switch(Rvalue),
    Break,
    Goto(Rvalue),
    Case(Option<ConstantNode>),
    Return(Option<Rvalue>),
}

#[derive(Clone)]
pub(crate) struct FlatNodeAndPos {
    pub(crate) node: FlatNode,
    pub(crate) pos: TokenPos,
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
                initial_values: self.initial_values
                    .into_iter()
                    .map(|ival| Ival::from(ival))
                    .collect(),
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

fn control_statement_require_end_marker(node: &Statement) -> bool {
    use Statement::*;
    match node {
        Compound(_) | Switch(_) | If(_) | While(_) => true,
        _ => false,
    }
}

impl FlattenNode for StatementNode {
    fn flatten_node(self) -> Vec<FlatNodeAndPos> {
        let append_end_marker = control_statement_require_end_marker(&self.statement);
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
                        node: FlatNode::If(*r#if.condition.rvalue),
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
                res
            }

            Statement::While(wh) => {
                let mut res = vec![
                    FlatNodeAndPos {
                        node: FlatNode::While(*wh.condition.rvalue),
                        pos: self.position,
                    }];
                res.append(&mut wh.body.flatten_node());
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
        };

        if append_end_marker {
            res.push(FlatNodeAndPos {
                node: FlatNode::RestoreScope,
                pos: res.last().unwrap().pos,
            })
        }
        res
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

        res.append(&mut body);
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