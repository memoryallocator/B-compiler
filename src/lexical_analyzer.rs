use std::cell::Cell;
use std::collections::HashMap;
use std::iter::FromIterator;
use std::string::String;

use crate::config::{CompilerOptions, TypeOfLineNo};
use crate::config::get_second_symbol_of_escape_sequence_to_character_mapping;
use crate::generate_error_message_with_line_no;
use crate::token::*;
use crate::token::ConstantType::*;

pub struct LexicalAnalyzer<'a> {
    pub compiler_options: &'a CompilerOptions,
    pub second_symbol_of_escape_sequence_to_character_mapping: HashMap<u8, u8>,
}

fn read_number_to_u8_vec(source_code_slice: &Vec<u8>) -> Vec<u8> {
    let mut buffer = Vec::<u8>::new();
    for i in 0..source_code_slice.len() {
        let curr_char = source_code_slice[i];
        match curr_char {
            b'0'..=b'9' => buffer.push(curr_char),
            _ => break
        }
    }
    return buffer;
}

impl LexicalAnalyzer<'_> {
    fn generate_tokens(
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
                b'\'' => char = !char,
                b'"' => string = !string,
                b'\n' => line_no += 1,
                b'*' => {  // process escape sequence
                    if i + 1 == source_code.len() {
                        return Err(generate_error_message_with_line_no(
                            "no right operand for '*' operator", line_no));
                    }
                    let next_char = source_code[i + 1];
                    if !(string || char) {
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
                b' ' => (),
                b'0'..=b'9' => {
                    let number_as_vec = read_number_to_u8_vec(&source_code[i..].to_vec());
                    let number_len = number_as_vec.len();
                    if curr_char == b'0' {
                        res.push((Token::Constant(Constant { constant_type: Octal, value: number_as_vec }), line_no))
                    } else {
                        res.push((Token::Constant(Constant { constant_type: Decimal, value: number_as_vec }), line_no))
                    }
                    i += number_len;
                    continue;
                }
                _ => ()
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
                        if i + 1 == source_code.len() {
                            return Err(generate_error_message_with_line_no(
                                "no right operand for '/' operator", line_no));
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
                    if i + 1 == source_code.len() {
                        return Err(generate_error_message_with_line_no(
                            "no right operand for '*' operator", line_no));
                    }
                    if source_code[i + 1] == b'/' {
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
        res.shrink_to_fit();
        Ok(res)
    }

    pub fn run(
        &mut self,
        source_code: &Vec<u8>,
    ) -> Result<Vec<(Token, TypeOfLineNo)>, String> {
        let source_code = self.remove_comments(source_code)?;
        Ok(self.generate_tokens(&source_code)?)
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    fn lexical_analyzer_test<S, T, Z: AsRef<str>>(
        inp: S,
        exp_out: T,
        panic_msg: Option<Z>,
        scan_first: bool,
        compiler_options: Option<&CompilerOptions>)
    {
        unimplemented!();
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
        let la = LexicalAnalyzer {
            compiler_options: &co,
            second_symbol_of_escape_sequence_to_character_mapping: esm,
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