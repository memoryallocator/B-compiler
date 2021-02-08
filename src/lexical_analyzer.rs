use std::collections::HashMap;
use std::fmt;
use std::string::String;

use crate::config::{CompilerOptions, SymbolTable, TypeOfColumnNo, TypeOfLineNo};
use crate::generate_error_message_with_line_no;
use crate::token::*;

pub(crate) struct LexicalAnalyzer<'a> {
    pub(crate) compiler_options: &'a CompilerOptions,
    pub(crate) second_symbol_of_escape_sequence_to_character_mapping: &'a HashMap<u8, u8>,
    pub(crate) keywords: &'a SymbolTable,
}

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub(crate) struct TokenPos {
    pub(crate) line: TypeOfLineNo,
    pub(crate) column: TypeOfColumnNo,
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

// #[derive(PartialEq, Eq, Clone, Copy)]
// enum CharOrString {
//     Char,
//     String,
// }

impl LexicalAnalyzer<'_> {
    fn tokenize(
        &self,
        source_code: &Vec<u8>,
    ) -> Result<Vec<(Token, TokenPos)>, String> {
        use crate::symbol::SymbolType;

        let mut res = Vec::<(Token, TokenPos)>::new();
        let mut buffer = Vec::<u8>::new();
        let mut line_no: TypeOfLineNo = 1;
        let mut i: usize = 0;
        let mut curr_line_started_at: usize = 0;
        let mut currently_reading: Option<Constant> = None;

        while i < source_code.len() {
            let token_pos = TokenPos { line: line_no, column: i - curr_line_started_at + 1 };
            let curr_char = source_code[i];
            if curr_char == b'\n' {
                curr_line_started_at = i + 1;
                line_no += 1;
            } else {
                match curr_char {
                    b'\'' | b'"' => {
                        let this_literal_type = if curr_char == b'"' { Constant::String } else { Constant::Char };
                        match currently_reading {
                            None => {
                                res.push((Token {
                                    token_type: TokenType::Constant(this_literal_type),
                                    value: None,
                                }, token_pos));
                                currently_reading = Some(this_literal_type);
                            }
                            Some(currently_reading_literal_type) => {
                                if currently_reading_literal_type != this_literal_type {
                                    buffer.push(curr_char);
                                } else {
                                    let (token, _) = res.last_mut().unwrap();
                                    (*token).value = Some(buffer.clone());
                                    buffer.clear();
                                    currently_reading = None;
                                }
                            }
                        }
                    }
                    b'*' =>
                        if currently_reading == None {
                            res.push((Token {
                                token_type: TokenType::Character(Character::Asterisk),
                                value: None,
                            }, token_pos))
                        } else {
                            match source_code.get(i + 1) {
                                None => buffer.push(curr_char),
                                Some(next_char) => {
                                    match self.second_symbol_of_escape_sequence_to_character_mapping.get(next_char) {
                                        None => {
                                            buffer.push(curr_char);
                                            buffer.push(*next_char);
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
                            match curr_char {
                                b' ' => (),
                                b'a'..=b'z' | b'A'..=b'Z' | b'.' => {
                                    let word: Vec<u8> = source_code[i..].iter().take_while(
                                        |c| c.is_ascii_alphanumeric() || **c == b'.')
                                        .cloned().collect();
                                    let word_len = word.len();
                                    if let Some(SymbolType::Reserved(token_type)) = self.keywords.get(&word) {
                                        res.push((Token {
                                            token_type: token_type.clone(),
                                            value: None,
                                        }, token_pos));
                                    } else {
                                        res.push((Token {
                                            value: Some(word),
                                            token_type: TokenType::Name,
                                        }, token_pos));
                                    }
                                    i += word_len;
                                    continue;
                                }
                                b'0'..=b'9' => {
                                    let number_as_vec: Vec<u8> = source_code[i..].iter().take_while(
                                        |c| c.is_ascii_digit())
                                        .cloned().collect();
                                    let number_len = number_as_vec.len();
                                    res.push((
                                        Token {
                                            token_type: TokenType::Constant(if curr_char == b'0' { Constant::Octal } else { Constant::Decimal }),
                                            value: Some(number_as_vec),
                                        }, token_pos));
                                    i += number_len;
                                    continue;
                                }
                                _ => {
                                    if let Some(t) = Character::from_u8(curr_char) {
                                        res.push((Token {
                                            token_type: TokenType::Character(t),
                                            value: None,
                                        }, token_pos));
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
        if let Some(t) = currently_reading {
            let literal = if t == Constant::Char { '\'' } else { '"' };
            return Err(generate_error_message_with_line_no(
                format!("expected {} (opening literal found in line {}, column {}), found end of file",
                        literal, res.last().unwrap().1.line, res.last().unwrap().1.column), line_no));
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
        let mut curr_line_started_at: usize = 0;
        let mut opening_literal_pos: Option<TokenPos> = None;

        while i < source_code.len() {
            let curr_char = source_code[i];
            if curr_char == b'\n' {
                res.push(curr_char);
                line_no += 1;
                curr_line_started_at = i + 1;
                i += 1;
                continue;
            }
            if !comment {
                match curr_char {
                    b'\'' =>
                        if !string && *res.last().unwrap_or(&b'\0') != b'*' {
                            if !char {
                                opening_literal_pos = Some(TokenPos {
                                    line: line_no,
                                    column: i - curr_line_started_at + 1,
                                })
                            }
                            char = !char;
                        }
                    b'"' =>
                        if !char && *res.last().unwrap_or(&b'\0') != b'*' {
                            if !string {
                                opening_literal_pos = Some(TokenPos {
                                    line: line_no,
                                    column: i - curr_line_started_at + 1,
                                });
                            }
                            string = !string;
                        }
                    b'/' =>
                        if !string && !char {
                            if let Some(b'*') = source_code.get(i + 1) {
                                comment = true;
                                i += 2;
                                continue;
                            }
                        }
                    _ => ()
                }
            } else {  // currently reading a comment
                if curr_char == b'*' {
                    if let Some(b'/') = source_code.get(i + 1) {
                        comment = false;
                        i += 2;
                        continue;
                    }
                }
            }
            if !comment {
                res.push(source_code[i]);
            }
            i += 1;
        }
        if comment {
            return Err(generate_error_message_with_line_no("expected */, found end of file", line_no));
        }
        if char || string {
            let literal = if char { '\'' } else { '"' };
            return Err(generate_error_message_with_line_no(
                format!("expected {} (opening literal found in line {}, column {}), found end of file",
                        literal, opening_literal_pos.unwrap().line, opening_literal_pos.unwrap().column), line_no));
        }
        res.shrink_to_fit();
        Ok(res)
    }

    pub(crate) fn run(
        &mut self,
        source_code: &Vec<u8>,
    ) -> Result<Vec<(Token, TokenPos)>, String> {
        let source_code = self.remove_comments(source_code)?;
        Ok(self.tokenize(&source_code)?)
    }
}


#[cfg(test)]
mod tests {
    use crate::config::{get_default_symbols, get_second_symbol_of_escape_sequence_to_character_mapping};

    use super::*;

    fn lexical_analyzer_test<S: AsRef<Vec<u8>>, Z: AsRef<str>>(
        inp: S,
        exp_out: Result<&Vec<(Token, TokenPos)>, String>,
        panic_msg: Option<Z>,
        scan_first: bool,
        compiler_options: &CompilerOptions,
        keywords: &SymbolTable,
        second_symbol_of_escape_sequence_to_character_mapping: &HashMap<u8, u8>,
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

    fn scanner_test<S: AsRef<Vec<u8>>, T: AsRef<Vec<u8>>, Z: AsRef<str>>(
        inp: S,
        exp_out: T,
        panic_msg: Option<Z>,
    ) -> Result<(), String> {
        let inp = inp.as_ref();
        let exp_out = exp_out.as_ref();
        let co = CompilerOptions::default();
        let esm = get_second_symbol_of_escape_sequence_to_character_mapping();
        let kw = get_default_symbols();
        let la = LexicalAnalyzer {
            compiler_options: &co,
            second_symbol_of_escape_sequence_to_character_mapping: &esm,
            keywords: &kw,
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
