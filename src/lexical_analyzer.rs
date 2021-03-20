use std::collections::HashMap;
use std::fmt;
use std::string::String;

use crate::config::{CompilerOptions, SymbolTable};
use crate::generate_error_message_with_pos;
use crate::token::{Token, TokenType};

pub(crate) struct LexicalAnalyzer<'a> {
    pub(crate) compiler_options: &'a CompilerOptions,
    pub(crate) second_symbol_of_escape_sequence_to_character_mapping: &'a HashMap<char, char>,
    pub(crate) keywords: &'a SymbolTable,
}

#[derive(Ord, PartialOrd, Eq, PartialEq, Clone, Copy, Debug, Hash)]
pub(crate) struct TokenPos {
    pub(crate) line: usize,
    pub(crate) column: usize,
}

impl TokenPos {
    pub(crate) fn repr(&self) -> String {
        format!("{},{}", self.line, self.column)
    }

    pub(crate) fn parse<T: AsRef<str>>(s: T) -> Option<Self> {
        let line_and_col: Vec::<&str> = s.as_ref().split(',').collect();
        if line_and_col.len() != 2 {
            return None;
        }
        match line_and_col[..2] {
            [line, col] => {
                let line = line.parse::<usize>();
                let column = col.parse::<usize>();
                if line.is_ok() && column.is_ok() {
                    let line = line.unwrap();
                    let column = column.unwrap();
                    return Some(TokenPos {
                        line,
                        column,
                    });
                }
            }
            _ => unreachable!()
        }
        None
    }
}

impl TokenPos {
    pub(crate) fn from(line: usize, column: usize) -> Self {
        TokenPos { line, column }
    }
}

impl fmt::Display for TokenPos {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "line {}, column {}", self.line, self.column)
    }
}

fn parse_binary_operator<'a, I>(op: I) -> Option<(crate::token::RichBinaryOperation, usize)>
    where I: Iterator<Item=char> {
    use crate::token::BinaryOperation::*;
    use crate::token::BinaryRelation::*;
    use crate::token::RichBinaryOperation::*;
    use crate::token::LeftOrRight::*;

    let s: Vec<char> = op.take(2).collect();
    return Some(
        match s[..2] {
            ['=', '='] => (RegularBinary(Cmp(Eq)), 2),
            ['!', '='] => (RegularBinary(Cmp(Ne)), 2),

            ['<', '<'] => (RegularBinary(Shift(Left)), 2),
            ['>', '>'] => (RegularBinary(Shift(Right)), 2),

            ['<', '='] => (RegularBinary(Cmp(Le)), 2),
            ['>', '='] => (RegularBinary(Cmp(Ge)), 2),

            ['&', _] => (And, 1),
            ['|', _] => (RegularBinary(Or), 1),
            ['^', _] => (RegularBinary(Xor), 1),

            ['+', _] => (Add, 1),
            ['-', _] => (Sub, 1),

            ['<', _] => (RegularBinary(Cmp(Lt)), 1),
            ['>', _] => (RegularBinary(Cmp(Gt)), 1),

            ['*', _] => (Mul, 1),
            ['/', _] => (RegularBinary(Div), 1),
            ['%', _] => (RegularBinary(Mod), 1),
            _ => return None
        }
    );
}

fn parse_operator<'a, I>(op: I) -> Option<(crate::token::Operator, usize)>
    where I: Iterator<Item=char> {
    use crate::token::Operator::*;
    use crate::token::BinaryOperation::*;
    use crate::token::BinaryRelation::*;
    use crate::token::RichBinaryOperation::*;

    let s: Vec<char> = op.take(3).collect();
    return Some(
        match s[..3] {
            ['=', '=', '='] => (Assign(Some(RegularBinary(Cmp(Eq)))), 3),
            ['=', '=', _] => (Binary(Cmp(Eq)), 2),

            ['=', _, _] => {
                if let Some((rich_bin_op, rich_bin_op_len)) = parse_binary_operator(
                    s[1..].into_iter().cloned()) {
                    (Assign(Some(rich_bin_op)), 1 + rich_bin_op_len)
                } else {
                    (Assign(None), 1)
                }
            }

            _ => {
                if let Some((rich_bin_op, rich_bin_op_len)) = parse_binary_operator(
                    s.into_iter()) {
                    if let RegularBinary(bin_op) = rich_bin_op {
                        (Binary(bin_op), rich_bin_op_len)
                    } else {
                        return None;
                    }
                } else {
                    return None;
                }
            }
        }
    );
}

// #[derive(PartialEq, Eq, Clone, Copy)]
// enum CharOrString {
//     Char,
//     String,
// }

impl LexicalAnalyzer<'_> {
    fn tokenize(&self, source_code: &str) -> Result<Vec<Token>, String> {
        use crate::symbol::SymbolType;
        use crate::token::{Constant, Bracket};
        use crate::token::Operator::*;
        use crate::token::UnaryOperation::*;

        let mut res = Vec::<Token>::new();
        let mut buffer = String::new();
        let mut line_no: usize = 1;
        let mut i: usize = 0;
        let mut curr_line_started_at: usize = 0;
        let mut currently_reading: Option<Constant> = None;

        let read_name = |s: &str| s.chars()
            .into_iter()
            .take_while(|c| c.is_ascii_alphanumeric() || ['_', '.'].contains(c))
            .collect::<String>();

        while i < source_code.len() {
            let token_pos = TokenPos { line: line_no, column: i - curr_line_started_at + 1 };
            let curr_char = source_code.chars().nth(i).unwrap_or_default();
            if curr_char == '\n' {
                curr_line_started_at = i + 1;
                line_no += 1;
            } else {
                match curr_char {
                    '\'' | '"' => {
                        let this_literal_type = if curr_char == '"' { Constant::String } else { Constant::Char };
                        match currently_reading {
                            None => {
                                res.push(Token {
                                    r#type: TokenType::Constant(this_literal_type),
                                    val: None,
                                    pos: token_pos,
                                });
                                currently_reading = Some(this_literal_type);
                            }
                            Some(currently_reading_literal_type) => {
                                if currently_reading_literal_type != this_literal_type {
                                    buffer.push(curr_char);
                                } else {
                                    let token = res.last_mut().unwrap();
                                    (*token).val = Some(buffer.clone());
                                    buffer.clear();
                                    currently_reading = None;
                                }
                            }
                        }
                    }
                    '*' =>
                        if currently_reading == None {
                            res.push(Token {
                                r#type: TokenType::Operator(Asterisk),
                                val: None,
                                pos: token_pos,
                            })
                        } else {
                            match source_code.chars().into_iter().nth(i + 1) {
                                None => buffer.push(curr_char),
                                Some(next_char) => {
                                    match self.second_symbol_of_escape_sequence_to_character_mapping.get(&next_char) {
                                        None => {
                                            buffer.push(curr_char);
                                            buffer.push(next_char);
                                        }
                                        Some(processed_escape_seq) => buffer.push(*processed_escape_seq),
                                    }
                                    i += 2;
                                    continue;
                                }
                            }
                        }
                    _ =>
                        if let Some(_) = currently_reading {
                            buffer.push(curr_char);
                        } else {
                            {
                                let word = read_name(&source_code[i..]);
                                let word_len = word.len();
                                if word_len > 0 {
                                    if let Some(
                                        SymbolType::Reserved(token_type)
                                    ) = self.keywords.get(&word) {
                                        res.push(Token {
                                            r#type: token_type.clone(),
                                            val: None,
                                            pos: token_pos,
                                        });
                                    } else {
                                        res.push(Token {
                                            val: Some(word),
                                            r#type: TokenType::Name,
                                            pos: token_pos,
                                        });
                                    }
                                    i += word_len;
                                    continue;
                                }
                            }
                            match curr_char {
                                ' ' => (),
                                ',' => res.push(Token {
                                    r#type: TokenType::Comma,
                                    val: None,
                                    pos: token_pos,
                                }),
                                ';' => res.push(Token {
                                    r#type: TokenType::Semicolon,
                                    val: None,
                                    pos: token_pos,
                                }),
                                ':' => res.push(Token {
                                    r#type: TokenType::Colon,
                                    val: None,
                                    pos: token_pos,
                                }),
                                '0'..='9' => {
                                    let number_as_str: String = source_code[i..].chars().into_iter()
                                        .take_while(|c| c.is_ascii_digit())
                                        .collect();
                                    let number_len = number_as_str.len();
                                    res.push(
                                        Token {
                                            r#type: TokenType::Constant(
                                                if curr_char == '0' {
                                                    Constant::Octal
                                                } else {
                                                    Constant::Decimal
                                                }),
                                            val: Some(number_as_str),
                                            pos: token_pos,
                                        });
                                    i += number_len;
                                    continue;
                                }
                                '+' => {
                                    match source_code.chars().into_iter().nth(i + 1) {
                                        Some('+') => {
                                            res.push(Token {
                                                r#type: TokenType::Operator(Inc),
                                                val: None,
                                                pos: token_pos,
                                            });
                                            i += 2;
                                            continue;
                                        }
                                        _ => res.push(Token {
                                            r#type: TokenType::Operator(Plus),
                                            val: None,
                                            pos: token_pos,
                                        })
                                    }
                                }
                                '-' => {
                                    match source_code.chars().into_iter().nth(i + 1) {
                                        Some('-') => {
                                            res.push(Token {
                                                r#type: TokenType::Operator(Dec),
                                                val: None,
                                                pos: token_pos,
                                            });
                                            i += 2;
                                            continue;
                                        }
                                        _ => res.push(Token {
                                            r#type: TokenType::Operator(Minus),
                                            val: None,
                                            pos: token_pos,
                                        })
                                    }
                                }
                                '!' => res.push(Token {
                                    r#type: TokenType::Operator(Unary(LogicalNot)),
                                    val: None,
                                    pos: token_pos,
                                }),
                                '~' => res.push(Token {
                                    r#type: TokenType::Operator(Unary(Complement)),
                                    val: None,
                                    pos: token_pos,
                                }),
                                '&' => res.push(Token {
                                    r#type: TokenType::Operator(Ampersand),
                                    val: None,
                                    pos: token_pos,
                                }),
                                '?' => res.push(Token {
                                    r#type: TokenType::QuestionMark,
                                    val: None,
                                    pos: token_pos,
                                }),
                                '[' | ']' | '(' | ')' | '{' | '}' =>
                                    res.push(Token {
                                        r#type: TokenType::Bracket(Bracket::from_char_unchecked(curr_char)),
                                        val: None,
                                        pos: token_pos,
                                    }),
                                _ =>
                                    if let Some((op, tokens_read)) = parse_operator(
                                        source_code[i..].chars().into_iter()) {
                                        res.push(Token {
                                            r#type: TokenType::Operator(op),
                                            val: None,
                                            pos: token_pos,
                                        });
                                        i += tokens_read;
                                        continue;
                                    } else {
                                        return Err(generate_error_message_with_pos(
                                            format!("unknown character encountered: {}",
                                                    curr_char as char),
                                            token_pos));
                                    }
                            }
                        }
                }
            }
            i += 1;
        }
        if let Some(t) = currently_reading {
            let literal = if t == Constant::Char { '\'' } else { '"' };
            return Err(generate_error_message_with_pos(
                format!("expected {} (opening literal found in {}), found end of file",
                        literal, res.last().unwrap().pos),
                TokenPos { line: line_no, column: 0 }));
        }
        Ok(res)
    }

    fn remove_comments(
        &self,
        source_code: &str,
    ) -> Result<String, String> {
        let mut res = String::with_capacity(source_code.len());
        let mut line_no: usize = 1;
        let mut comment = false;
        let mut char = false;
        let mut string = false;
        let mut i: usize = 0;
        let mut curr_line_started_at: usize = 0;
        let mut opening_literal_pos: Option<TokenPos> = None;

        while i < source_code.len() {
            let curr_char = source_code.chars().nth(i).unwrap();
            if curr_char == '\n' {
                res.push(curr_char);
                line_no += 1;
                curr_line_started_at = i + 1;
                i += 1;
                continue;
            }
            if !comment {
                match curr_char {
                    '\'' =>
                        if !string && res.chars().last().unwrap_or('\0') != '*' {
                            if !char {
                                opening_literal_pos = Some(TokenPos {
                                    line: line_no,
                                    column: i - curr_line_started_at + 1,
                                })
                            }
                            char = !char;
                        }
                    '"' =>
                        if !char && res.chars().last().unwrap_or('\0') != '*' {
                            if !string {
                                opening_literal_pos = Some(TokenPos {
                                    line: line_no,
                                    column: i - curr_line_started_at + 1,
                                });
                            }
                            string = !string;
                        }
                    '/' =>
                        if !string && !char {
                            if let Some('*') = source_code.chars().nth(i + 1) {
                                comment = true;
                                i += 2;
                                continue;
                            }
                        }
                    _ => ()
                }
            } else {  // currently reading a comment
                if curr_char == '*' {
                    if let Some('/') = source_code.chars().nth(i + 1) {
                        comment = false;
                        i += 2;
                        continue;
                    }
                }
            }
            if !comment {
                res.push(source_code.chars().nth(i).unwrap());
            }
            i += 1;
        }
        if comment {
            return Err(generate_error_message_with_pos("expected */, found end of file",
                                                       TokenPos { line: line_no, column: 0 }));
        }
        if char || string {
            let literal = if char { '\'' } else { '"' };
            return Err(generate_error_message_with_pos(
                format!("expected {} (opening literal found in line {}, column {}), found end of file",
                        literal, opening_literal_pos.unwrap().line, opening_literal_pos.unwrap().column),
                TokenPos { line: line_no, column: 0 }));
        }
        res.shrink_to_fit();
        Ok(res)
    }

    pub(crate) fn run(
        &mut self,
        source_code: &str,
    ) -> Result<Vec<Token>, String> {
        let source_code = self.remove_comments(source_code)?;
        Ok(self.tokenize(&source_code)?)
    }
}

#[cfg(test)]
mod tests {
    use std::borrow::Borrow;

    use crate::config::{get_default_symbols, get_second_symbol_of_escape_sequence_to_character_mapping};

    use super::*;

    fn lexical_analyzer_test<S: AsRef<String>, Z: AsRef<str>>(
        inp: S,
        exp_out: Result<&Vec<Token>, String>,
        panic_msg: Option<Z>,
        scan_first: bool,
        compiler_options: &CompilerOptions,
        keywords: &SymbolTable,
        second_symbol_of_escape_sequence_to_character_mapping: &HashMap<char, char>,
    ) -> Result<(), String> {
        let inp = inp.as_ref();
        let exp_out = exp_out.as_ref();
        let mut la = LexicalAnalyzer {
            compiler_options,
            second_symbol_of_escape_sequence_to_character_mapping,
            keywords,
        };

        let mut after_scan: String;
        if scan_first {
            after_scan = la.remove_comments(inp)?;
        } else {
            after_scan = inp.clone();
        }
        let res = la.tokenize(&after_scan);
        if let Err(exp_err) = exp_out {
            if let Ok(_) = res {
                return Err(String::from(format!("generate_tokens() returned tokens instead of expected error message {}", exp_err)));
            }
            match panic_msg {
                None => assert_eq!(&res.unwrap_err(), exp_err),
                Some(msg) => assert_eq!(&res.unwrap_err(), exp_err, "{}", msg.as_ref()),
            }
        } else {
            if let Err(_) = res {
                return Err(String::from(format!("generate_tokens() returned error instead of expected tokens")));
            }
            match panic_msg {
                None => assert_eq!(res.unwrap(), **exp_out.unwrap()),
                Some(msg) => assert_eq!(res.unwrap(), **exp_out.unwrap(), "{}", msg.as_ref()),
            }
        }
        Ok(())
    }

    fn scanner_test<S: Borrow<String>, T: Borrow<String>, Z: Borrow<str>>(
        inp: S,
        exp_out: T,
        panic_msg: Option<Z>,
    ) -> Result<(), String> {
        let inp = inp.borrow();
        let exp_out = exp_out.borrow();
        let co = CompilerOptions::default();
        let esm = get_second_symbol_of_escape_sequence_to_character_mapping();
        let kw = get_default_symbols();
        let la = LexicalAnalyzer {
            compiler_options: &co,
            second_symbol_of_escape_sequence_to_character_mapping: &esm,
            keywords: &kw,
        };

        let res = la.remove_comments(inp)?;
        let ans = exp_out;
        match panic_msg {
            None => assert_eq!(res, *ans),
            Some(msg) => assert_eq!(res, *ans, "{}", msg.borrow()),
        }
        Ok(())
    }

    #[test]
    fn test_scanner_remove_comments() -> Result<(), String> {
        scanner_test(String::from("\
        main() {\
        auto a, b  /* my comment\

        */;\
        }\
        "), String::from("\
        main() {\
        auto a, b  \

        ;\
        }\
"), None::<Box<str>>)
    }

    #[test]
    fn test_scanner_strings_containing_comments() -> Result<(), String> {
        let test_res = scanner_test(String::from("\
        main() {
            a \" /* my string containing comment */ \";
        }"),
                                    String::from("\
        main() {
            a \" /* my string containing comment */ \";
        }"),
                                    None::<Box<str>>)?;

        scanner_test(String::from("\
        main() {
        a \"a     long    string  \";
        b \"a long string that contains a /*comment*/\";
        }"),
                     String::from("\
        main() {
        a \"a     long    string  \";
        b \"a long string that contains a /*comment*/\";
        }"), None::<Box<str>>)
    }
}
