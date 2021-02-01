use std::collections::HashMap;
use std::string::String;

use crate::config::{CompilerOptions, SymbolTable, TypeOfLineNo};
use crate::generate_error_message_with_line_no;
use crate::token::*;
use crate::token::ConstantType::*;
use crate::token::LeftOrRight::*;

pub struct LexicalAnalyzer<'a> {
    pub compiler_options: &'a CompilerOptions,
    pub second_symbol_of_escape_sequence_to_character_mapping: HashMap<u8, u8>,
    pub keywords: SymbolTable,
}

fn parse_binary_operator<'a, I>(op: I) -> Option<(Binary, usize)>
    where I: Iterator<Item=&'a u8> {
    let s: Vec<&u8> = op.take(2).collect();
    match s[..2] {
        [b'=', b'='] => Some((Binary::Eq, 2)),
        [b'!', b'='] => Some((Binary::Ne, 2)),

        [b'<', b'<'] => Some((Binary::Shift(Left), 2)),
        [b'>', b'>'] => Some((Binary::Shift(Right), 2)),

        [b'<', b'='] => Some((Binary::Le, 2)),
        [b'>', b'='] => Some((Binary::Ge, 2)),

        [b'&', _] => Some((Binary::And, 1)),
        [b'|', _] => Some((Binary::Or, 1)),
        [b'^', _] => Some((Binary::Xor, 1)),

        [b'+', _] => Some((Binary::Add, 1)),
        [b'-', _] => Some((Binary::Sub, 1)),

        [b'<', _] => Some((Binary::Less, 1)),
        [b'>', _] => Some((Binary::Greater, 1)),

        [b'*', _] => Some((Binary::Mul, 1)),
        [b'/', _] => Some((Binary::Div, 1)),
        [b'%', _] => Some((Binary::Mod, 1)),

        _ => None
    }
}

fn parse_operator<'a, I>(op: I) -> Option<(Token, usize)>
    where I: Iterator<Item=&'a u8> {
    let s: Vec<&u8> = op.take(3).collect();
    match s[..3] {
        [b'=', b'=', b'='] => Some((Token::Assign(Some(
            Binary::Eq)), 3)),

        [b'=', b'=', _] => Some((Token::
                                 Binary(Binary::Eq), 2)),

        [b'=', _, _] => {
            if let Some((bin_op, bin_op_len)) = parse_binary_operator(s[1..].into_iter().cloned()) {
                Some((Token::Binary(bin_op), 1 + bin_op_len))
            } else {
                Some((Token::Assign(None), 1))
            }
        }

        _ => {
            if let Some((bin_op, bin_op_len)) = parse_binary_operator(s.into_iter()) {
                Some((Token::Binary(bin_op), bin_op_len))
            } else {
                None
            }
        }
    }
}

fn ensure_right_operand_exists(s: &Vec<u8>, i: usize, line_no: TypeOfLineNo) -> Result<(), String> {
    if i + 1 == s.len() {
        return Err(generate_error_message_with_line_no(
            format!("no right operand for '{}' operator", s[i] as char), line_no));
    }
    return Ok(());
}

impl LexicalAnalyzer<'_> {
    fn tokenize(
        &mut self,
        source_code: &Vec<u8>,
    ) -> Result<Vec<(Token, TypeOfLineNo)>, String> {
        let mut res = Vec::<(Token, TypeOfLineNo)>::new();
        let mut buffer = Vec::<u8>::new();
        let mut line_no: TypeOfLineNo = 1;
        let mut char = false;
        let mut string = false;
        let mut i: usize = 0;
        while i < source_code.len() {
            let curr_char = source_code[i];
            match curr_char {
                b'\'' => {
                    if string {
                        buffer.push(curr_char);
                    } else {
                        if char {
                            res.push((Token::Constant(Constant { constant_type: Char, value: buffer.clone() }), line_no));
                            buffer.clear();
                        }
                        char = !char
                    }
                }
                b'"' => {
                    if char {
                        buffer.push(curr_char);
                    } else {
                        if string {
                            res.push((Token::Constant(Constant { constant_type: String, value: buffer.clone() }), line_no));
                            buffer.clear();
                        }
                        string = !string
                    }
                }
                b'\n' => line_no += 1,
                b'*' => {  // process escape sequence
                    if let Err(s) = ensure_right_operand_exists(source_code, i, line_no) {
                        return Err(s);
                    }
                    let next_char = source_code[i + 1];
                    if !string && !char {
                        res.push((Token::Asterisk, line_no))
                    } else {
                        if let Some(processed_escape_seq) = self.second_symbol_of_escape_sequence_to_character_mapping.get(&next_char) {
                            buffer.push(*processed_escape_seq);
                            i += 2;
                            continue;
                        }
                        res.push((Token::Asterisk, line_no));
                    }
                }
                _ => {
                    if string || char {
                        buffer.push(curr_char);
                    } else {
                        match curr_char {
                            b' ' => (),
                            b',' => res.push((Token::Comma, line_no)),
                            b';' => res.push((Token::Semicolon, line_no)),
                            b':' => res.push((Token::Colon, line_no)),
                            b'a'..=b'z' | b'A'..=b'Z' => {
                                let word: Vec<u8> = source_code[i..].iter().take_while(
                                    |c| c.is_ascii_alphanumeric())
                                    .cloned().collect();
                                let word_len = word.len();
                                if let Some(token) = self.keywords.get(&word) {
                                    res.push((token.clone(), line_no));
                                } else {
                                    res.push((Token::Id(word), line_no));
                                }
                                i += word_len;
                                continue;
                            }
                            b'0'..=b'9' => {
                                let number_as_vec: Vec<u8> = source_code[i..].iter().take_while(
                                    |c| c.is_ascii_digit())
                                    .cloned().collect();
                                let number_len = number_as_vec.len();
                                if curr_char == b'0' {
                                    res.push((Token::Constant(Constant {
                                        constant_type: Octal,
                                        value: number_as_vec,
                                    }), line_no))
                                } else {
                                    res.push((Token::Constant(Constant {
                                        constant_type: Decimal,
                                        value: number_as_vec,
                                    }), line_no))
                                }
                                i += number_len;
                                continue;
                            }
                            b'(' => res.push((Token::Bracket(Bracket {
                                left_or_right: LeftOrRight::Left,
                                bracket_type: BracketType::Round,
                            }), line_no)),
                            b')' => res.push((Token::Bracket(Bracket {
                                left_or_right: LeftOrRight::Right,
                                bracket_type: BracketType::Round,
                            }), line_no)),
                            b'{' => res.push((Token::Bracket(Bracket {
                                left_or_right: LeftOrRight::Left,
                                bracket_type: BracketType::Curly,
                            }), line_no)),
                            b'}' => res.push((Token::Bracket(Bracket {
                                left_or_right: LeftOrRight::Right,
                                bracket_type: BracketType::Curly,
                            }), line_no)),
                            b'[' => res.push((Token::Bracket(Bracket {
                                left_or_right: LeftOrRight::Left,
                                bracket_type: BracketType::Square,
                            }), line_no)),
                            b']' => res.push((Token::Bracket(Bracket {
                                left_or_right: LeftOrRight::Right,
                                bracket_type: BracketType::Square,
                            }), line_no)),
                            b'+' => {
                                if let Err(s) = ensure_right_operand_exists(source_code, i, line_no) {
                                    return Err(s);
                                }
                                if source_code[i + 1] == curr_char {
                                    res.push((Token::Unary(Unary::Increment), line_no));
                                } else {
                                    res.push((Token::Plus, line_no));
                                }
                            }
                            b'-' => {
                                if let Err(s) = ensure_right_operand_exists(source_code, i, line_no) {
                                    return Err(s);
                                }
                                if source_code[i + 1] == curr_char {
                                    res.push((Token::Unary(Unary::Decrement), line_no));
                                } else {
                                    res.push((Token::Minus, line_no));
                                }
                            }
                            b'!' => res.push((Token::Unary(Unary::LogicalNot), line_no)),
                            b'~' => res.push((Token::Unary(Unary::Complement), line_no)),
                            b'&' => res.push((Token::Ampersand, line_no)),
                            b'.' => res.push((Token::Dot, line_no)),
                            b'?' => res.push((Token::QuestionMark, line_no)),
                            _ => {
                                if let Some((op, op_len)) = parse_operator(source_code[i..].iter()) {
                                    res.push((op, line_no));
                                    i += op_len;
                                    continue;
                                } else {
                                    return Err(generate_error_message_with_line_no(
                                        format!("unknown character encountered: {}", curr_char as char), line_no));
                                }
                            }
                        }
                    }
                }
            }
            i += 1;
        }
        Ok(res)
    }

    fn remove_comments(
        &self,
        source_code: &Vec<u8>,
    ) -> Result<Vec<u8>, String> {
        let mut res = Vec::<u8>::with_capacity(source_code.len());
        let mut line_no: TypeOfLineNo = 1;
        let mut comment = false;
        let mut char = false;
        let mut string = false;
        let mut i: usize = 0;
        while i < source_code.len() {
            if source_code[i] == b'\n' {
                line_no += 1;
            }
            if !comment {
                match source_code[i] {
                    b'\'' => {
                        if res.is_empty() || *res.last().unwrap() != b'*' {
                            char = !char;
                        }
                    }
                    b'"' => {
                        if res.is_empty() || *res.last().unwrap() != b'*' {
                            string = !string;
                        }
                    }
                    b'/' => {
                        if let Err(s) = ensure_right_operand_exists(source_code, i, line_no) {
                            return Err(s);
                        }
                        if source_code[i + 1] == b'*'
                            && !string {
                            comment = true;
                            i += 2;
                            continue;
                        }
                    }
                    _ => ()
                }
            } else {  // currently reading a comment
                if source_code[i] == b'*' {
                    if let Err(s) = ensure_right_operand_exists(source_code, i, line_no) {
                        return Err(s);
                    }
                    if source_code[i + 1] == b'/' {
                        comment = false;
                        i += 2;
                        continue;
                    }
                }
            }
            if !comment || source_code[i] == b'\n' {
                res.push(source_code[i]);
            }
            i += 1;
        }
        res.shrink_to_fit();
        Ok(res)
    }

    pub fn run(
        &mut self,
        source_code: &Vec<u8>,
    ) -> Result<Vec<(Token, TypeOfLineNo)>, String> {
        let source_code = self.remove_comments(source_code)?;
        Ok(self.tokenize(&source_code)?)
    }
}


#[cfg(test)]
mod tests {
    use crate::config::{get_keywords, get_second_symbol_of_escape_sequence_to_character_mapping};

    use super::*;

    fn lexical_analyzer_test<S: AsRef<Vec<u8>>, Z: AsRef<str>>(
        inp: S,
        exp_out: Result<&Vec<(Token, TypeOfLineNo)>, String>,
        panic_msg: Option<Z>,
        scan_first: bool,
        compiler_options: &CompilerOptions,
        keywords: SymbolTable,
        second_symbol_of_escape_sequence_to_character_mapping: HashMap<u8, u8>,
    ) -> Result<(), String> {
        let inp = inp.as_ref();
        let exp_out = exp_out.as_ref();
        let mut la = LexicalAnalyzer {
            compiler_options,
            second_symbol_of_escape_sequence_to_character_mapping,
            keywords,
        };

        let mut after_scan: Vec<u8>;
        if scan_first {
            after_scan = la.remove_comments(inp)?;
        } else {
            after_scan = inp.clone();
        }
        let res = la.tokenize(&after_scan);
        if let Err(exp_err) = exp_out {
            if let Ok(v) = res {
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

    fn scanner_test<S: AsRef<Vec<u8>>, T: AsRef<Vec<u8>>, Z: AsRef<str>>(
        inp: S,
        exp_out: T,
        panic_msg: Option<Z>,
    ) -> Result<(), String> {
        let inp = inp.as_ref();
        let exp_out = exp_out.as_ref();
        let co = CompilerOptions::default();
        let esm = get_second_symbol_of_escape_sequence_to_character_mapping();
        let kw = get_keywords();
        let la = LexicalAnalyzer {
            compiler_options: &co,
            second_symbol_of_escape_sequence_to_character_mapping: esm,
            keywords: kw,
        };

        let res = la.remove_comments(inp)?;
        let res = String::from_utf8(res).unwrap();
        let ans = String::from_utf8(exp_out.clone()).unwrap();
        match panic_msg {
            None => assert_eq!(res, ans),
            Some(msg) => assert_eq!(res, ans, "{}", msg.as_ref()),
        }
        Ok(())
    }

    #[test]
    fn test_scanner_remove_comments() -> Result<(), String> {
        scanner_test(Vec::<u8>::from("\
        main() {\
        auto a, b  /* my comment\

        */;\
        }\
        "), Vec::<u8>::from("\
        main() {\
        auto a, b  \

        ;\
        }\
"), None::<Box<str>>)
    }

    #[test]
    fn test_scanner_strings_containing_comments() -> Result<(), String> {
        let test_res = scanner_test(Vec::<u8>::from("\
        main() {
            a \" /* my string containing comment */ \";
        }"),
                                    Vec::<u8>::from("\
        main() {
            a \" /* my string containing comment */ \";
        }"),
                                    None::<Box<str>>)?;

        scanner_test(Vec::<u8>::from("\
        main() {
        a \"a     long    string  \";
        b \"a long string that contains a /*comment*/\";
        }"),
                     Vec::<u8>::from("\
        main() {
        a \"a     long    string  \";
        b \"a long string that contains a /*comment*/\";
        }"), None::<Box<str>>)
    }
}