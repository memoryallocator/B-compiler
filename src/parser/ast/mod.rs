pub(crate) mod flat_ast;

use std::*;
use convert::TryFrom;

use crate::lexical_analyzer::token;
use token::*;

#[derive(Debug)]
pub(crate) enum Ival {
    Constant(ConstantNode),
    Name(String, TokenPos),
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub(crate) struct ConstantNode {
    pub(crate) position: TokenPos,
    pub(crate) constant: Constant,
}

impl TryFrom<&Token> for ConstantNode {
    type Error = ();

    fn try_from(t: &Token) -> Result<Self, Self::Error> {
        if let WrappedToken::Constant(constant) = &t.token {
            Ok(ConstantNode {
                position: t.pos,
                constant: constant.clone(),
            })
        } else {
            Err(())
        }
    }
}

#[derive(Debug)]
pub(crate) struct VariableDefinitionNode {
    pub(crate) position: TokenPos,
    pub(crate) name: String,
    pub(crate) initial_value: Option<Ival>,
}

#[derive(Debug)]
pub(crate) struct VectorDefinitionNode {
    pub(crate) position: TokenPos,
    pub(crate) name: String,
    pub(crate) specified_size: Option<ConstantNode>,
    pub(crate) initial_values: Vec<Ival>,
}

#[derive(Debug)]
pub(crate) struct FunctionDefinitionNode {
    pub(crate) position: TokenPos,
    pub(crate) name: String,
    pub(crate) parameters: Vec<(String, TokenPos)>,
    pub(crate) body: StatementNode,
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub(crate) struct AutoDeclaration {
    pub(crate) position: TokenPos,
    pub(crate) name: String,
    pub(crate) size_if_vector: Option<ConstantNode>,
}

#[derive(Debug)]
pub(crate) struct AutoDeclarationNode {
    pub(crate) position: TokenPos,
    pub(crate) declarations: Vec<AutoDeclaration>,
    pub(crate) next_statement: StatementNode,
}

#[derive(Debug)]
pub(crate) struct ExternDeclarationNode {
    pub(crate) position: TokenPos,
    pub(crate) names: Vec<(String, TokenPos)>,
    pub(crate) next_statement: StatementNode,
}

#[derive(Debug)]
pub(crate) struct LabelDeclarationNode {
    pub(crate) position: TokenPos,
    pub(crate) label_name: String,
    pub(crate) next_statement: StatementNode,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub(crate) enum Lvalue {
    Name(String),
    DerefRvalue(RvalueNode),
    Indexing { vector: RvalueNode, index: RvalueNode },
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub(crate) struct LvalueNode {
    pub(crate) position: TokenPos,
    pub(crate) lvalue: Lvalue,
}

impl TryFrom<&RvalueNode> for LvalueNode {
    type Error = ();

    fn try_from(rvalue_node: &RvalueNode) -> Result<Self, Self::Error> {
        if let Rvalue::Lvalue(lvalue) = &*rvalue_node.rvalue {
            Ok(lvalue.clone())
        } else {
            Err(())
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub(crate) enum IncDecType {
    Prefix,
    Postfix,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub(crate) struct IncDecNode {
    pub(crate) inc_or_dec: IncDec,
    pub(crate) inc_dec_type: IncDecType,
    pub(crate) lvalue: LvalueNode,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub(crate) enum Unary {
    Plus,
    Minus,
    LogicalNot,
    Complement,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub(crate) struct FunctionCallNode {
    pub(crate) fn_name: RvalueNode,
    pub(crate) arguments: Vec<RvalueNode>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub(crate) enum Rvalue {
    Constant(ConstantNode),
    Lvalue(LvalueNode),
    Assign { lhs: LvalueNode, assign: token::Assign, rhs: RvalueNode },
    IncDec(IncDecNode),
    Unary(Unary, RvalueNode),
    TakeAddress(LvalueNode),
    Binary { lhs: RvalueNode, bin_op: token::RichBinaryOperation, rhs: RvalueNode },
    ConditionalExpression {
        condition: RvalueNode,
        on_true: RvalueNode,
        on_false: RvalueNode,
        colon_pos: TokenPos,
    },
    BracketedExpression(RvalueNode),
    FunctionCall(FunctionCallNode),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub(crate) struct RvalueNode {
    pub(crate) position: TokenPos,
    pub(crate) rvalue: Box<Rvalue>,
}

impl RvalueNode {
    pub(crate) fn try_to_truth_value(&self) -> Option<Self> {
        return Some(match &*self.rvalue {
            Rvalue::Constant(_) => self.clone(),
            Rvalue::Lvalue(lvalue_node) =>
                if let Lvalue::Name(_) = lvalue_node.lvalue {
                    self.clone()
                } else {
                    return None;
                },
            Rvalue::Binary {
                lhs,
                bin_op,
                rhs
            } => {
                let lhs = lhs.try_to_truth_value()?;
                let rhs = rhs.try_to_truth_value()?;

                match bin_op {
                    RichBinaryOperation::LogicalAnd => self.clone(),
                    RichBinaryOperation::RegularBinary(BinaryOperation::Or) => {
                        RvalueNode {
                            position: self.position,
                            rvalue: Box::new(Rvalue::Binary {
                                lhs,
                                bin_op: *bin_op,
                                rhs,
                            }),
                        }
                    }

                    RichBinaryOperation::BitwiseAnd => {
                        RvalueNode {
                            position: self.position,
                            rvalue: Box::new(Rvalue::Binary {
                                lhs,
                                bin_op: RichBinaryOperation::LogicalAnd,
                                rhs,
                            }),
                        }
                    }

                    _ => return None,
                }
            }

            Rvalue::ConditionalExpression {
                condition,
                on_true,
                on_false,
                colon_pos,
            } => {
                let condition = condition.try_to_truth_value()?;
                let on_true = on_true.clone();
                let on_false = on_false.clone();

                RvalueNode {
                    position: self.position,
                    rvalue: Box::new(Rvalue::ConditionalExpression {
                        condition,
                        on_true,
                        on_false,
                        colon_pos: *colon_pos,
                    }),
                }
            }

            Rvalue::BracketedExpression(br_expr) =>
                RvalueNode {
                    position: self.position,
                    rvalue: Box::new(Rvalue::BracketedExpression(
                        br_expr.try_to_truth_value()?
                    )),
                },

            Rvalue::Unary(Unary::LogicalNot, rvalue_node) => {
                RvalueNode {
                    position: self.position,
                    rvalue: Box::new(Rvalue::Unary(Unary::LogicalNot,
                                                   rvalue_node.try_to_truth_value()?)),
                }
            }
            _ => return None,
        });
    }
}

#[derive(Debug)]
pub(crate) struct SwitchNode {
    pub(crate) rvalue: RvalueNode,
    pub(crate) body: StatementNode,
}

#[derive(Debug)]
pub(crate) struct CaseNode {
    pub(crate) constant_if_not_default: Option<ConstantNode>,
    pub(crate) next_statement: StatementNode,
}

#[derive(Debug)]
pub(crate) struct ElseNode {
    pub(crate) position: TokenPos,
    pub(crate) else_body: StatementNode,
}

#[derive(Debug)]
pub(crate) struct IfNode {
    pub(crate) condition: RvalueNode,
    pub(crate) body: StatementNode,
    pub(crate) r#else: Option<ElseNode>,
}

#[derive(Debug)]
pub(crate) struct WhileNode {
    pub(crate) condition: RvalueNode,
    pub(crate) body: StatementNode,
}

#[derive(Debug)]
pub(crate) struct CompoundStatementNode {
    pub(crate) position: TokenPos,
    pub(crate) statement_list: Vec<StatementNode>,
}

#[derive(Debug)]
pub(crate) struct GotoNode {
    pub(crate) label: RvalueNode,
}

#[derive(Debug)]
pub(crate) struct ReturnNode {
    pub(crate) rvalue: Option<RvalueNode>,
}

#[derive(Debug)]
pub(crate) struct RvalueAndSemicolonNode {
    pub(crate) rvalue: RvalueNode,
}

#[derive(Debug)]
pub(crate) struct BreakNode {}

#[derive(Debug)]
pub(crate) enum DeclarationNode {
    AutoDeclaration(AutoDeclarationNode),
    ExternDeclaration(ExternDeclarationNode),
    LabelDeclaration(LabelDeclarationNode),
}

#[derive(Debug)]
pub(crate) enum Statement {
    NullStatement,
    Compound(CompoundStatementNode),
    Declaration(DeclarationNode),
    Return(ReturnNode),
    Switch(SwitchNode),
    Case(CaseNode),
    If(IfNode),
    While(WhileNode),
    Goto(GotoNode),
    RvalueAndSemicolon(RvalueAndSemicolonNode),
    Break(BreakNode),
}

#[derive(Debug)]
pub(crate) struct StatementNode {
    pub(crate) position: TokenPos,
    pub(crate) statement: Box<Statement>,
}

#[derive(Debug)]
pub(crate) enum DefinitionNode {
    Variable(VariableDefinitionNode),
    Vector(VectorDefinitionNode),
    Function(FunctionDefinitionNode),
}

#[derive(Debug)]
pub(crate) struct ProgramNode {
    pub(crate) definitions: Vec<DefinitionNode>,
}

impl From<Vec<DefinitionNode>> for ProgramNode {
    fn from(definitions: Vec<DefinitionNode>) -> Self {
        ProgramNode { definitions }
    }
}