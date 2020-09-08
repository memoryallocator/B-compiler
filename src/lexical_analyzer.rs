use std::collections::HashSet;

use crate::{CompilerOptions, Token, SymbolTable, TypeOfLineNo};
use crate::generate_error_message_with_line_no;

pub struct LexicalAnalyzer<'a> {
    pub compiler_options: &'a CompilerOptions,
    pub tokens: Vec<Token>,
    pub reserved: &'a SymbolTable<'a>,
}

impl LexicalAnalyzer<'_> {
    fn generate_tokens(
        &self,
        source_code: &Vec<u8>,
    ) -> Result<Vec<(Token, TypeOfLineNo)>, &str> {
        let mut res = Vec::<(Token, TypeOfLineNo)>::new();
        // let curr_token  &str=;
        // if RESERVED.contains(curr_token) {}
        let mut line_no: usize = 1;
        let mut comment = false;
        for i in 0..source_code.len() {
            if source_code[i] == b'\n' {
                line_no += 1;
            }
        }
        Ok(res)
    }

    fn remove_extra_whitespaces_and_comments(
        &self,
        source_code: &Vec<u8>,
    ) -> Result<Vec<u8>, String> {
        let mut res = Vec::<u8>::with_capacity(source_code.len());
        let mut line_no: usize = 1;
        let mut comment = false;
        let mut i: usize = 0;
        while i < source_code.len() {
            println!("INNER RES is:\n{:?}", &res);
            println!("INNER RES is:\n{}", String::from_utf8_lossy(&res));
            println!();
            if source_code[i] == b'\n' {
                println!("Newline symbol has been encountered.");
                res.push(b'\n');
                line_no += 1;
                i += 1;
                continue;
            }
            if !comment {
                match source_code[i] {
                    b'/' => {
                        if i + 1 == source_code.len() {
                            return Err(generate_error_message_with_line_no(
                                "no right operand for '/' operator", line_no));
                        }
                        if source_code[i + 1] == b'*' {
                            comment = true;
                            i += 2;
                            continue;
                        }
                    }
                    b' ' => {
                        if i + 1 == source_code.len() {
                            break;
                        }
                        // println!("PREV is {}", res[i - 1].clone());
                        if res.is_empty() {
                            i += 1;
                            continue;
                        } else if [b'\n', b' '].contains(res.last().unwrap()) {
                            i += 1;
                            continue;
                        }
                        if source_code[i + 1] == b'\n' {
                            i += 1;
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
                println!("Pushing {} to res...", source_code[i].clone());
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
        self.tokens = Vec::new();
        let source_code = self.remove_extra_whitespaces_and_comments(source_code)?;
        Ok(self.generate_tokens(&source_code)?)
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_scanner_remove_comments() -> Result<(), String> {
        let st = SymbolTable::new();
        let co = CompilerOptions::default();
        let la = LexicalAnalyzer {
            compiler_options: &co,
            reserved: &st,
            tokens: Vec::new(),
        };

        let res = la.remove_extra_whitespaces_and_comments(&Vec::<u8>::from("\
        main() {\
        auto a, b  /* my exciting comment\

        */;\
        }\
        "))?;
        let ans = &Vec::<u8>::from("\
main() {\
auto a, b \

;\
}\
");
        {
            let res = String::from_utf8_lossy(&res);
            // let res = String::from(res);
            println!("RES is:\n{}", res);
        }
        assert_eq!(&res, ans);
        Ok(())
    }

    #[test]
    fn test_scanner_remove_whitespaces() -> Result<(), String> {
        let st = SymbolTable::new();
        let co = CompilerOptions::default();
        let la = LexicalAnalyzer {
            compiler_options: &co,
            reserved: &st,
            tokens: Vec::new(),
        };

        let res = la.remove_extra_whitespaces_and_comments(&Vec::<u8>::from("\
        main() {
        auto   a,   b  /* my exciting comment

        */;
        a = 0;
        b = 100;
        a   =   b +   1;
        b  =-  2  ;
        b     =    -      a;
        a = = = b;
        }
        "))?;
        let ans: Vec<u8> = ("\
main() {
auto a, b \n".to_owned()
            + "\n"
            + ";
a = 0;
b = 100;
a = b + 1;
b =- 2 ;
b = - a;
a = = = b;
}
").into_bytes();
        assert_eq!(res, ans);
        Ok(())
    }
}