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
        let mut string = false;
        let mut i: usize = 0;
        while i < source_code.len() {
            println!("INNER RES is:\n{:?}", &res);
            println!("INNER RES is:\n{}", String::from_utf8_lossy(&res));
            println!();
            if source_code[i] == b'\n' {
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
                        if source_code[i + 1] == b'*'
                            && !string {
                            comment = true;
                            i += 2;
                            continue;
                        }
                    }
                    b' ' => {
                        if !string {
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
                    }
                    b'"' => {
                        if res.is_empty() {
                            return Err(generate_error_message_with_line_no("expected name, found '\"'",
                                                                           line_no));
                        }
                        if *res.last().unwrap() != b'*' {
                            string = !string;
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

    fn lexical_analyzer_test<S, T, Z: AsRef<str>>(
        inp: S,
        exp_out: T,
        panic_msg: Option<Z>,
        scan_first: bool,
        symbol_table: Option<&SymbolTable>,
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
        let st = SymbolTable::new();
        let co = CompilerOptions::default();
        let la = LexicalAnalyzer {
            compiler_options: &co,
            reserved: &st,
            tokens: Vec::new(),
        };

        let res = la.remove_extra_whitespaces_and_comments(inp)?;
        let res = &res;
        let ans = exp_out;
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
        auto a, b  /* my exciting comment\

        */;\
        }\
        "), Vec::<u8>::from("\
main() {\
auto a, b \

;\
}\
"), None::<Box<str>>)
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

        scanner_test(Vec::<u8>::from("\
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
        "), Vec::<u8>::from("\
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
"), None::<Box<str>>)
    }

    #[test]
    fn test_scanner_remove_whitespaces_inside_strings() -> Result<(), String> {
        let test_res = scanner_test(Vec::<u8>::from("\
        main() {
        a \"my exciting string\";
        }"),
                                    Vec::<u8>::from("\
main() {
a \"my exciting string\";
}"),
                                    None::<Box<str>>)?;

        scanner_test(Vec::<u8>::from("\
        main() {
        a \"a     long    string  \";
        }"),
                     Vec::<u8>::from("\
main() {
a \"a     long    string  \";
}"), None::<Box<str>>)
    }
}