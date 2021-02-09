use std::rc::Weak;

use crate::config::{CompilerOptions, SymbolTable};
use crate::grammar;
use crate::grammar::{GrammarSymbol, NonTerminal};
use crate::lexical_analyzer::TokenPos;
use crate::token;
use crate::token::{Bracket, LeftOrRight, Token, TokenType};

trait SetParent {
    fn set_parent(&self, p: Option<&AbstractSyntaxNode>) -> Self;
}

trait Translate {
    fn translate(&self) -> Vec<u8>;
}

#[derive(Debug)]
struct ConstantNode {
    parent: Option<Weak<AbstractSyntaxNode>>,
    constant_type: token::Constant,
    value: Vec<u8>,
}

#[derive(Debug)]
struct LvalueNode {
    parent: Option<Weak<AbstractSyntaxNode>>,
    value: Box<AbstractSyntaxNode>,
}

#[derive(Debug)]
struct RvalueNode {
    parent: Option<Weak<AbstractSyntaxNode>>,
    value: Box<AbstractSyntaxNode>,
}

#[derive(Debug)]
enum ValueNode {
    Constant(ConstantNode),
    Lvalue(LvalueNode),
    Rvalue(RvalueNode),
}

#[derive(Debug)]
struct AddNode {
    parent: Option<Weak<AbstractSyntaxNode>>,
    left: Box<AbstractSyntaxNode>,
    right: Box<AbstractSyntaxNode>,
}

#[derive(Debug)]
struct SubtractNode {
    parent: Option<Weak<AbstractSyntaxNode>>,
    left: Box<AbstractSyntaxNode>,
    right: Box<AbstractSyntaxNode>,
}

#[derive(Debug)]
struct MultiplyNode {
    parent: Option<Weak<AbstractSyntaxNode>>,
    left: Box<AbstractSyntaxNode>,
    right: Box<AbstractSyntaxNode>,
}

#[derive(Debug)]
struct DivideNode {
    parent: Option<Weak<AbstractSyntaxNode>>,
    left: Box<AbstractSyntaxNode>,
    right: Box<AbstractSyntaxNode>,
}

#[derive(Debug)]
struct ShiftNode {
    parent: Option<Weak<AbstractSyntaxNode>>,
    shift_type: LeftOrRight,
    left: Box<AbstractSyntaxNode>,
    right: Box<AbstractSyntaxNode>,
}

#[derive(Debug)]
enum AndInterpretation {
    Logical,
    Bitwise,
}

#[derive(Debug)]
struct AndNode {
    parent: Option<Weak<AbstractSyntaxNode>>,
    and_interpretation: AndInterpretation,
    left: Box<AbstractSyntaxNode>,
    right: Box<AbstractSyntaxNode>,
}

#[derive(Debug)]
struct OrNode {
    parent: Option<Weak<AbstractSyntaxNode>>,
    left: Box<AbstractSyntaxNode>,
    right: Box<AbstractSyntaxNode>,
}

#[derive(Debug)]
struct ExclusiveOrNode {
    parent: Option<Weak<AbstractSyntaxNode>>,
    left: Box<AbstractSyntaxNode>,
    right: Box<AbstractSyntaxNode>,
}

#[derive(Debug)]
enum BinaryOperator {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Eq,
    Ne,
    Le,
    Ge,
    Less,
    Greater,
    Or,
    And,
    Xor,
    Shift(LeftOrRight),
}

#[derive(Debug)]
struct AssignNode {
    parent: Option<Weak<AbstractSyntaxNode>>,
    bin_op_assign: Option<BinaryOperator>,
    left: Box<AbstractSyntaxNode>,
    right: Box<AbstractSyntaxNode>,
}

#[derive(Debug)]
enum BinaryOperationNode {
    Add(AddNode),
    Subtract(SubtractNode),
    Multiply(MultiplyNode),
    Divide(DivideNode),
    Shift(ShiftNode),
    And(AndNode),
    Or(OrNode),
    ExclusiveOr(ExclusiveOrNode),
    Assign(AssignNode),
}

#[derive(Debug)]
struct ExpressionNode {
    parent: Option<Weak<AbstractSyntaxNode>>,
    children: Vec<Box<AbstractSyntaxNode>>,
}

#[derive(Debug)]
struct StatementNode {
    parent: Option<Weak<AbstractSyntaxNode>>,
    children: Option<Vec<Box<AbstractSyntaxNode>>>,
}

#[derive(Debug)]
struct UnaryMinusNode {
    parent: Option<Weak<AbstractSyntaxNode>>,
    expr: Box<AbstractSyntaxNode>,
}

#[derive(Debug)]
struct ComplementNode {
    parent: Option<Weak<AbstractSyntaxNode>>,
    expr: Box<AbstractSyntaxNode>,
}

#[derive(Debug)]
struct LogicalNotNode {
    parent: Option<Weak<AbstractSyntaxNode>>,
    expr: Box<AbstractSyntaxNode>,
}

#[derive(Debug)]
struct DereferenceNode {
    parent: Option<Weak<AbstractSyntaxNode>>,
    expr: Box<AbstractSyntaxNode>,
}

#[derive(Debug)]
struct TakeAddressNode {
    parent: Option<Weak<AbstractSyntaxNode>>,
    expr: Box<AbstractSyntaxNode>,
}

#[derive(Debug)]
enum UnaryOperationNode {
    Plus(ExpressionNode),
    Minus(UnaryMinusNode),
    Complement(ComplementNode),
    LogicalNot(LogicalNotNode),
    Dereference(DereferenceNode),
    TakeAddress(TakeAddressNode),
}

#[derive(Debug)]
struct WhileNode {
    parent: Option<Weak<AbstractSyntaxNode>>,
    condition: Box<AbstractSyntaxNode>,
    statement: Box<AbstractSyntaxNode>,
}

#[derive(Debug)]
struct IfElseNode {
    parent: Option<Weak<AbstractSyntaxNode>>,
    condition: Box<AbstractSyntaxNode>,
    statement: Box<AbstractSyntaxNode>,
}

#[derive(Debug)]
struct SwitchNode {
    parent: Option<Weak<AbstractSyntaxNode>>,
    variable: Box<AbstractSyntaxNode>,
    cases: Box<AbstractSyntaxNode>,
}

#[derive(Debug)]
struct CaseNode {
    parent: Option<Weak<AbstractSyntaxNode>>,
    value: Box<AbstractSyntaxNode>,
    statement: Box<AbstractSyntaxNode>,
}

#[derive(Debug)]
struct ReturnNode {
    parent: Option<Weak<AbstractSyntaxNode>>,
    value: Option<Box<AbstractSyntaxNode>>,
    curr_function: Box<AbstractSyntaxNode>,
}

#[derive(Debug)]
struct GotoNode {
    parent: Option<Weak<AbstractSyntaxNode>>,
    value: Option<Box<AbstractSyntaxNode>>,
}

#[derive(Debug)]
enum ControlStatementNode {
    While(WhileNode),
    IfElse(IfElseNode),
    Switch(SwitchNode),
    Case(CaseNode),
    Return(ReturnNode),
    Goto(GotoNode),
}

// type Name = Vec<u8>;

#[derive(Debug)]
struct NameNode {
    parent: Option<Weak<AbstractSyntaxNode>>,
    value: Vec<u8>,
}

#[derive(Debug)]
struct VariableDefinitionNode {
    parent: Option<Weak<AbstractSyntaxNode>>,
    name: NameNode,
    value: Option<Box<AbstractSyntaxNode>>,
}

#[derive(Debug)]
struct IvalNode {
    parent: Option<Weak<AbstractSyntaxNode>>,
    value: Box<AbstractSyntaxNode>,
}

#[derive(Debug)]
struct IvalListNode {
    parent: Option<Weak<AbstractSyntaxNode>>,
    values: Vec<Box<IvalNode>>,
}

#[derive(Debug)]
struct VectorDefinitionNode {
    parent: Option<Weak<AbstractSyntaxNode>>,
    name: NameNode,
    specified_element_count: Option<Box<AbstractSyntaxNode>>,
    values: Option<IvalListNode>,
}

#[derive(Debug)]
struct FunctionDefinitionNode {
    parent: Option<Weak<AbstractSyntaxNode>>,
    name: NameNode,
    body: Vec<Box<Box<AbstractSyntaxNode>>>,
}

#[derive(Debug)]
enum DefinitionNode {
    VariableDefinition(VariableDefinitionNode),
    VectorDefinition(VectorDefinitionNode),
    FunctionDefinition(FunctionDefinitionNode),
}

#[derive(Debug)]
struct LabelDeclarationNode {
    parent: Option<Weak<AbstractSyntaxNode>>,
    name: NameNode,
}

#[derive(Debug)]
struct ExternDeclarationNode {
    parent: Option<Weak<AbstractSyntaxNode>>,
    name: NameNode,
}

#[derive(Debug)]
struct VectorDeclarationNode {
    parent: Option<Weak<AbstractSyntaxNode>>,
    name: NameNode,
    element_count: Box<AbstractSyntaxNode>,
}

#[derive(Debug)]
struct VariableDeclarationNode {
    parent: Option<Weak<AbstractSyntaxNode>>,
    name: NameNode,
}

#[derive(Debug)]
enum AutoDeclarationNode {
    Variable(VariableDeclarationNode),
    Vector(VectorDeclarationNode),
}

#[derive(Debug)]
enum DeclarationNode {
    Label(LabelDeclarationNode),
    Extern(ExternDeclarationNode),
    Auto(AutoDeclarationNode),
}

#[derive(Debug)]
struct VectorElementNode {
    parent: Option<Weak<AbstractSyntaxNode>>,
    vector_name: NameNode,
    element_no: Box<AbstractSyntaxNode>,
}

#[derive(Debug)]
struct ConditionalStatementNode {
    parent: Option<Weak<AbstractSyntaxNode>>,
    condition: Box<AbstractSyntaxNode>,
    on_true: Box<AbstractSyntaxNode>,
    on_false: Box<AbstractSyntaxNode>,
}

#[derive(Debug)]
struct ProgramNode {
    children: Vec<Box<AbstractSyntaxNode>>,
}

#[derive(Debug)]
struct FunctionCallNode {
    parent: Option<Weak<AbstractSyntaxNode>>,
    function: Box<AbstractSyntaxNode>,
    parameter_list: Vec<Box<AbstractSyntaxNode>>,
}

#[derive(Debug)]
enum AbstractSyntaxNode {
    Value(ValueNode),
    Binary(BinaryOperationNode),
    Expression(ExpressionNode),
    ControlStatement(ControlStatementNode),
    Statement(StatementNode),
    // CompoundStatement(CompoundStatementNode),
    ConditionalStatement(ConditionalStatementNode),
    VectorElement(VectorElementNode),
    Declaration(DeclarationNode),
    Definition(DefinitionNode),
    Program(ProgramNode),
    Name(NameNode),
    Ival(IvalNode),
    IvalList(IvalListNode),
    FunctionCall(FunctionCallNode),
}

struct DummyNode {
    parent: Option<Weak<AbstractSyntaxNode>>,
}

#[derive(Debug)]
pub(crate) struct AbstractSyntaxTree {
    root: Option<AbstractSyntaxNode>
}

pub(crate) struct Parser<'a> {
    pub(crate) compiler_options: &'a CompilerOptions,
    pub(crate) symbol_table: &'a SymbolTable,
    pub(crate) source_code: Option<&'a Vec<u8>>,
}

#[derive(Eq, PartialEq)]
enum BracketsStatus {
    Ok,
    NotClosed(TokenPos),
    NotOpened(TokenPos),
}

// pub(crate) enum DefinitionNode {
//     VariableDefinition(VariableDefinitionNode),
//     VectorDefinition(VectorDefinitionNode),
//     FunctionDefinition(FunctionDefinitionNode),
// }

// pub(crate) enum DefinitionOrToken {
//     Definition(DefinitionNode),
//     Token(Token),
// }
//
// pub(crate) struct DefinitionOrTokenWithPos {
//     definition_or_token: DefinitionOrToken,
//     position: TokenPos,
// }

// struct DefinitionsDefinitions {
//     data: Vec<DefinitionOrTokenWithPos>,
//     productions: Vec<Vec<DefinitionOrToken>>,
// }

#[derive(Debug)]
struct MatchedNonTerminal {
    non_terminal_type: NonTerminal,
    value: AbstractSyntaxNode,
}

#[derive(Debug)]
struct MatchedTerminal {
    terminal_type: token::TokenType,
    value: Option<Vec<u8>>,
}

#[derive(Debug)]
enum MatchedGrammarSymbol {
    NonTerminal(MatchedNonTerminal),
    Terminal(MatchedTerminal),
    Other,
}

#[derive(Debug)]
struct ParseNonTerminalSuccess {
    syntax_node: AbstractSyntaxNode,
    tokens_read: usize,
}

impl grammar::Grammar {
    fn recursive_descent_non_terminal(
        &self,
        tokens_with_pos: &[(token::Token, TokenPos)],
        nt: &grammar::NonTerminal,
    ) -> Result<ParseNonTerminalSuccess, String> {
        use grammar::*;

        for prod in self.productions_table.get(nt).unwrap_or(&vec![]).into_iter() {
            let mut matched = Vec::<MatchedGrammarSymbol>::new();
            let mut i: usize = 0;
            for prod_symbol in prod {
                match prod_symbol {
                    GrammarSymbol::NonTerminal(non_term) => {
                        match self.recursive_descent_non_terminal(&tokens_with_pos[i..],
                                                                  &non_term) {
                            Ok(parse_res) => {
                                let ast = parse_res.syntax_node;
                                let adv = parse_res.tokens_read;
                                i += adv;
                                matched.push(MatchedGrammarSymbol::NonTerminal(
                                    MatchedNonTerminal {
                                        non_terminal_type: non_term.clone(),
                                        value: ast,
                                    }))
                            }
                            _ => break,
                        }
                    }
                    GrammarSymbol::Terminal(t) => {
                        let input_symbol = tokens_with_pos.get(i);
                        if input_symbol == None {
                            break;
                        }
                        let (input_symbol, _) = input_symbol.unwrap();
                        if *t == input_symbol.token_type {
                            matched.push(MatchedGrammarSymbol::Terminal(
                                MatchedTerminal {
                                    terminal_type: t.clone(),
                                    value: input_symbol.value.clone(),
                                }
                            ));
                            i += 1;
                        } else {
                            break;
                        }
                    }
                    GrammarSymbol::RightEndmarker =>
                        if tokens_with_pos.is_empty() {
                            matched.push(MatchedGrammarSymbol::Other);
                        } else {
                            break;
                        }
                }
            }
            if matched.len() != prod.len() {
                continue;
            }
            let res = match nt {
                NonTerminal::Program => {
                    if let Some(MatchedGrammarSymbol::Other) = matched.last() {
                        matched.pop();
                    }
                    AbstractSyntaxNode::Program(
                        ProgramNode {
                            children: matched.into_iter()
                                .map(|x| match x {
                                    MatchedGrammarSymbol::NonTerminal(x) => Box::new(x.value),
                                    _ => unreachable!()
                                }).collect(),
                        }
                    )
                }
                NonTerminal::Definition(def_type) => {
                    match def_type {
                        DataType::Variable => {
                            let name_node;
                            let value;
                            match prod.len() {
                                2 => {  // name ;
                                    value = None;
                                    matched.pop();  // drop semicolon
                                }
                                3 => {  // name value ;
                                    matched.pop();  // drop semicolon
                                    if let MatchedGrammarSymbol::NonTerminal(x) = matched.pop().unwrap() {
                                        value = Some(Box::new(x.value));
                                    } else {
                                        unreachable!()
                                    }
                                }
                                _ => unreachable!()
                            }
                            if let MatchedGrammarSymbol::NonTerminal(
                                MatchedNonTerminal {
                                    value: AbstractSyntaxNode::Name(nn),
                                    ..
                                }) = matched.pop().unwrap() {
                                name_node = nn;
                            } else {
                                unreachable!();
                            }
                            AbstractSyntaxNode::Definition(
                                DefinitionNode::VariableDefinition(
                                    VariableDefinitionNode {
                                        parent: None,
                                        name: name_node,
                                        value,
                                    }
                                )
                            )
                        }

                        DataType::Vector => {
                            let name_node;
                            let specified_element_count;
                            let values;
                            match prod.len() {
                                4 => {  // name [] ;
                                    matched.pop();  // drop semicolon
                                    matched.pop();  // drop right bracket
                                    specified_element_count = None;
                                    matched.pop();  // drop left bracket
                                    values = None;
                                }
                                5 => {  // name [constant] | name [] ival_list ;
                                    matched.pop();  // drop semicolon
                                    match matched.pop() {
                                        Some(m) => {
                                            match m {
                                                MatchedGrammarSymbol::NonTerminal(
                                                    non_term
                                                ) =>
                                                    match non_term.value {
                                                        AbstractSyntaxNode::IvalList(ival_list) => {
                                                            matched.pop();  // drop right bracket
                                                            specified_element_count = None;
                                                            matched.pop();  // drop left bracket
                                                            values = Some(ival_list);
                                                        }
                                                        _ => unreachable!()
                                                    }

                                                MatchedGrammarSymbol::Terminal(
                                                    MatchedTerminal {
                                                        terminal_type: TokenType::Character(token::Character::Bracket(_)),
                                                        ..
                                                    }
                                                ) =>
                                                    match matched.pop() {
                                                        Some(MatchedGrammarSymbol::NonTerminal(
                                                                 MatchedNonTerminal {
                                                                     non_terminal_type: NonTerminal::Constant,
                                                                     value: c
                                                                 })) => {
                                                            specified_element_count = Some(Box::new(c));
                                                            matched.pop();  // drop left bracket
                                                            values = None;
                                                        }
                                                        _ => unreachable!()
                                                    }
                                                _ => unreachable!()
                                            }
                                        }
                                        _ => unreachable!(),
                                    }
                                }

                                6 => {  // name [constant] IvalList ;
                                    matched.pop();  // drop semicolon
                                    if let Some(MatchedGrammarSymbol::NonTerminal(
                                                    MatchedNonTerminal {
                                                        value: AbstractSyntaxNode::IvalList(ival_list), ..
                                                    })) = matched.pop() {
                                        values = Some(ival_list);
                                    } else {
                                        unreachable!()
                                    }
                                    matched.pop();  // drop right bracket
                                    if let MatchedGrammarSymbol::NonTerminal(
                                        MatchedNonTerminal {
                                            non_terminal_type: NonTerminal::Constant,
                                            value: v
                                        }) = matched.pop().unwrap() {
                                        specified_element_count = Some(Box::new(v));
                                    } else {
                                        unreachable!()
                                    }
                                    matched.pop();  // drop left bracket
                                }
                                _ => unreachable!()
                            }
                            if let MatchedGrammarSymbol::NonTerminal(
                                MatchedNonTerminal {
                                    value: AbstractSyntaxNode::Name(nn),
                                    ..
                                }) = matched.pop().unwrap() {
                                name_node = nn;
                            } else {
                                unreachable!();
                            }
                            AbstractSyntaxNode::Definition(
                                DefinitionNode::VectorDefinition(
                                    VectorDefinitionNode {
                                        parent: None,
                                        name: name_node,
                                        specified_element_count,
                                        values,
                                    }
                                )
                            )
                        }

                        DataType::Function => {
                            unimplemented!()
                        }
                    }
                }
                NonTerminal::Constant => {
                    if let MatchedGrammarSymbol::Terminal(c) = matched.pop().unwrap() {
                        if let TokenType::Constant(c_type) = c.terminal_type {
                            AbstractSyntaxNode::Value(
                                ValueNode::Constant(
                                    ConstantNode {
                                        parent: None,
                                        constant_type: c_type,
                                        value: c.value.unwrap(),
                                    }))
                        } else {
                            unreachable!()
                        }
                    } else {
                        unreachable!()
                    }
                }
                NonTerminal::Name => {
                    if let MatchedGrammarSymbol::Terminal(n) = matched.pop().unwrap() {
                        if let TokenType::Name = n.terminal_type {
                            AbstractSyntaxNode::Name(
                                NameNode {
                                    parent: None,
                                    value: n.value.unwrap(),
                                })
                        } else {
                            unreachable!()
                        }
                    } else {
                        unreachable!()
                    }
                }
                NonTerminal::Ival => {
                    let ival = matched.pop().unwrap();
                    match ival {
                        MatchedGrammarSymbol::NonTerminal(non_term) =>
                            {
                                match non_term.non_terminal_type {
                                    NonTerminal::Constant | NonTerminal::Name =>
                                        AbstractSyntaxNode::Ival(IvalNode {
                                            parent: None,
                                            value: Box::new(non_term.value),
                                        }),
                                    _ => unreachable!()
                                }
                            }
                        _ => unreachable!()
                    }
                }
                NonTerminal::IvalList => {
                    let values: Vec<Box<IvalNode>>;
                    match prod.len() {
                        1 =>
                            if let Some(MatchedGrammarSymbol::NonTerminal(
                                            MatchedNonTerminal {
                                                non_terminal_type: NonTerminal::Ival,
                                                value: AbstractSyntaxNode::Ival(ival_node)
                                            }
                                        )) = matched.pop() {
                                values = vec![Box::new(ival_node)]
                            } else {
                                unreachable!()
                            }
                        3 =>
                            values = matched.into_iter()
                                .enumerate()
                                .filter_map(
                                    |(i, x)| if i % 2 == 0 {
                                        match x {
                                            MatchedGrammarSymbol::NonTerminal(
                                                MatchedNonTerminal {
                                                    non_terminal_type: NonTerminal::IvalList,
                                                    value: AbstractSyntaxNode::IvalList(ival_list)
                                                }
                                            ) => Some(ival_list.values),
                                            MatchedGrammarSymbol::NonTerminal(
                                                MatchedNonTerminal {
                                                    non_terminal_type: NonTerminal::Ival,
                                                    value: AbstractSyntaxNode::Ival(ival_node)
                                                }
                                            ) => Some(vec![Box::new(ival_node)]),
                                            _ => unreachable!()
                                        }
                                    } else {
                                        None
                                    }
                                ).flatten().collect(),
                        _ => unimplemented!()
                    }
                    AbstractSyntaxNode::IvalList(
                        IvalListNode {
                            parent: None,
                            values,
                        }
                    )
                }
                _ => unimplemented!()
            };
            return Ok(ParseNonTerminalSuccess {
                syntax_node: res,
                tokens_read: i,
            });
        }
        return Err("failed to proceed".to_string());
    }

    fn recursive_descent(
        &self,
        tokens_with_pos: &[(token::Token, TokenPos)],
    ) -> Result<AbstractSyntaxTree, String> {
        if tokens_with_pos.is_empty() {
            return Err("an input of recursive descent method is empty".parse().unwrap());
        }
        match self.recursive_descent_non_terminal(tokens_with_pos, &self.start_symbol) {
            Err(msg) => Err(msg),
            Ok(parse_res) => Ok(AbstractSyntaxTree {
                root: Some(parse_res.syntax_node)
            }),
        }
    }
}

impl Parser<'_> {
    // fn extract_bracket_expressions<'b, I>(&self, tok_it: &mut I) -> Vec<TokenOrBracketExpression>
    //     where I: Iterator<Item=&'b (Token, TokenPos)> {
    //     let mut res = Vec::<TokenOrBracketExpression>::new();
    //     while let Some((t, pos)) = tok_it.next() {
    //         match t.token_type {
    //             TokenType::Bracket(Bracket {
    //                                    bracket_type: br_type,
    //                                    left_or_right: left_or_right
    //                                }) => {
    //                 if left_or_right == LeftOrRight::Right {
    //                     return res;
    //                 }
    //                 let nested_expressions = self.extract_bracket_expressions(tok_it);
    //                 res.push(TokenOrBracketExpression::BracketExpression(
    //                     BracketExpression {
    //                         data: nested_expressions,
    //                         brackets_type: br_type,
    //                     }));
    //             }
    //             _ => res.push(TokenOrBracketExpression::TokenAndPos((t.clone(), *pos))),
    //         }
    //     }
    //     res
    // }

    fn are_brackets_balanced<'b, I>(mut tok_it: I) -> BracketsStatus
        where I: Iterator<Item=&'b (Token, TokenPos)> {
        use LeftOrRight::*;

        let mut stack = Vec::<(&Bracket, &TokenPos)>::new();
        while let Some(token) = tok_it.next() {
            if let (Token {
                token_type: TokenType::Character(token::Character::Bracket(br)),
                ..
            }, pos) = token {
                if br.left_or_right == Left {
                    stack.push((br, pos));
                } else {
                    match stack.pop() {
                        None => return BracketsStatus::NotOpened(pos.clone()),
                        Some((last, _)) => {
                            if *last != Bracket::paired_bracket(&br) {
                                return BracketsStatus::NotOpened(pos.clone());
                            }
                        }
                    }
                }
            }
        }
        if stack.is_empty() {
            return BracketsStatus::Ok;
        }
        BracketsStatus::NotClosed(stack.last().unwrap().1.clone())
    }

    pub(crate) fn run(
        &mut self,
        tokens: &Vec<(token::Token, TokenPos)>,
    ) -> Result<(AbstractSyntaxTree, &SymbolTable), String> {
        use crate::grammar::DataType::*;
        use crate::token::{Constant};

        // match Parser::are_brackets_balanced(tokens.into_iter()) {
        //     BracketsStatus::Ok => (),
        //     BracketsStatus::NotClosed(pos) => {
        //         return Err(format!(
        //             "bracket opened in line {}, column {}, not closed",
        //             pos.line.to_string(), pos.column.to_string()));
        //     }
        //     BracketsStatus::NotOpened(pos) => {
        //         return Err(format!(
        //             "closing bracket in line {}, column {}, doesn't have a matching opening bracket",
        //             pos.line.to_string(), pos.column.to_string()));
        //     }
        // }

        if tokens.is_empty() {
            return Ok((AbstractSyntaxTree { root: None }, self.symbol_table));
        }

        let productions_table: grammar::ProductionsTable =
            vec![(NonTerminal::Program,
                  vec![
                      vec![GrammarSymbol::NonTerminal(NonTerminal::Definition(Variable)),
                           GrammarSymbol::NonTerminal(NonTerminal::Program)],
                      vec![GrammarSymbol::NonTerminal(NonTerminal::Definition(Vector)),
                           GrammarSymbol::NonTerminal(NonTerminal::Program)],
                      vec![GrammarSymbol::NonTerminal(NonTerminal::Definition(Function)),
                           GrammarSymbol::NonTerminal(NonTerminal::Program)],
                      vec![GrammarSymbol::RightEndmarker]
                  ]),
                 (NonTerminal::Definition(Variable),
                  vec![
                      vec![GrammarSymbol::NonTerminal(NonTerminal::Name),
                           GrammarSymbol::Terminal(TokenType::Character(
                               token::Character::Punctuation(token::PunctuationSymbol::Semicolon)))],
                      vec![GrammarSymbol::NonTerminal(NonTerminal::Name),
                           GrammarSymbol::NonTerminal(NonTerminal::Ival),
                           GrammarSymbol::Terminal(TokenType::Character(
                               token::Character::Punctuation(token::PunctuationSymbol::Semicolon)))],
                  ]),
                 (NonTerminal::Definition(Vector),
                  vec![
                      vec![GrammarSymbol::NonTerminal(NonTerminal::Name),
                           GrammarSymbol::Terminal(TokenType::Character(
                               token::Character::Bracket(Bracket::from(b'[' as char).unwrap()))),
                           GrammarSymbol::Terminal(TokenType::Character(
                               token::Character::Bracket(Bracket::from(b']' as char).unwrap()))),
                           GrammarSymbol::Terminal(TokenType::Character(
                               token::Character::Punctuation(token::PunctuationSymbol::Semicolon)))],
                      vec![GrammarSymbol::NonTerminal(NonTerminal::Name),
                           GrammarSymbol::Terminal(TokenType::Character(
                               token::Character::Bracket(Bracket::from(b'[' as char).unwrap()))),
                           GrammarSymbol::NonTerminal(NonTerminal::Constant),
                           GrammarSymbol::Terminal(TokenType::Character(
                               token::Character::Bracket(Bracket::from(b']' as char).unwrap()))),
                           GrammarSymbol::Terminal(TokenType::Character(
                               token::Character::Punctuation(token::PunctuationSymbol::Semicolon)))],
                      vec![GrammarSymbol::NonTerminal(NonTerminal::Name),
                           GrammarSymbol::Terminal(TokenType::Character(
                               token::Character::Bracket(Bracket::from(b'[' as char).unwrap()))),
                           GrammarSymbol::Terminal(TokenType::Character(
                               token::Character::Bracket(Bracket::from(b']' as char).unwrap()))),
                           GrammarSymbol::NonTerminal(NonTerminal::IvalList),
                           GrammarSymbol::Terminal(TokenType::Character(
                               token::Character::Punctuation(token::PunctuationSymbol::Semicolon)))],
                      vec![GrammarSymbol::NonTerminal(NonTerminal::Name),
                           GrammarSymbol::Terminal(TokenType::Character(
                               token::Character::Bracket(Bracket::from(b'[' as char).unwrap()))),
                           GrammarSymbol::NonTerminal(NonTerminal::Constant),
                           GrammarSymbol::Terminal(TokenType::Character(
                               token::Character::Bracket(Bracket::from(b']' as char).unwrap()))),
                           GrammarSymbol::NonTerminal(NonTerminal::IvalList),
                           GrammarSymbol::Terminal(TokenType::Character(
                               token::Character::Punctuation(token::PunctuationSymbol::Semicolon)))]
                  ]),
                 (NonTerminal::IvalList,
                  vec![
                      vec![GrammarSymbol::NonTerminal(NonTerminal::Ival),
                           GrammarSymbol::Terminal(TokenType::Character(
                               token::Character::Punctuation(token::PunctuationSymbol::Comma))),
                           GrammarSymbol::NonTerminal(NonTerminal::IvalList)],
                      vec![GrammarSymbol::NonTerminal(NonTerminal::Ival)],
                  ]),
                 (NonTerminal::Ival,
                  vec![
                      vec![GrammarSymbol::NonTerminal(NonTerminal::Constant)],
                      vec![GrammarSymbol::NonTerminal(NonTerminal::Name)],
                  ]),
                 (NonTerminal::Constant,
                  vec![
                      vec![GrammarSymbol::Terminal(TokenType::Constant(Constant::Decimal))],
                      vec![GrammarSymbol::Terminal(TokenType::Constant(Constant::Octal))],
                      vec![GrammarSymbol::Terminal(TokenType::Constant(Constant::Char))],
                      vec![GrammarSymbol::Terminal(TokenType::Constant(Constant::String))],
                  ]),
                 (NonTerminal::Name,
                  vec![
                      vec![GrammarSymbol::Terminal(TokenType::Name)],
                  ]),
            ].into_iter().collect();

        let grammar = grammar::Grammar {
            start_symbol: NonTerminal::Program,
            productions_table,
        };

        return match grammar.recursive_descent(&tokens[..]) {
            Ok(ast) => Ok((ast, self.symbol_table)),
            Err(msg) => Err(msg),
        };
    }
}