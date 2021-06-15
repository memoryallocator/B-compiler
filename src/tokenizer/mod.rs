use std::*;
use cell::Cell;
use collections::HashMap;

use crate::config::*;
use token::*;

pub mod token;

pub(crate) struct Tokenizer<'a> {
    pub(crate) escape_sequences: &'a HashMap<String, String>,
    pub(crate) reserved_symbols: &'a ReservedSymbolsTable,
    pub(crate) compiler_options: CompilerOptions,
}

fn parse_assign_binary_operator<'a, I>(op: I) -> Option<(RichBinaryOperation, usize)>
    where I: Iterator<Item=char> {
    use BinaryOperation::*;
    use BinaryRelation::*;
    use RichBinaryOperation::*;
    use LeftOrRight::*;

    let s: Vec<char> = op.take(2).collect();
    return Some(
        match s[..2] {
            ['=', '='] => (RegularBinary(Cmp(Eq)), 2),
            ['!', '='] => (RegularBinary(Cmp(Ne)), 2),

            ['<', '<'] => (RegularBinary(Shift(Left)), 2),
            ['>', '>'] => (RegularBinary(Shift(Right)), 2),

            ['<', '='] => (RegularBinary(Cmp(Le)), 2),
            ['>', '='] => (RegularBinary(Cmp(Ge)), 2),

            ['&', _] => (BitwiseAnd, 1),
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

fn parse_operator<'a, I>(op: I) -> Option<(Operator, usize)>
    where I: Iterator<Item=char> {
    use Operator::*;
    use UnaryOperation::*;
    use BinaryOperation::*;
    use BinaryRelation::*;
    use RichBinaryOperation::*;
    use token::IncOrDec::*;
    use token::Assign as AssignStruct;

    let s: Vec<char> = op.take(3).collect();
    return Some(
        match s[..3] {
            ['+', '+', _] => (IncDec(Increment), 2),
            ['-', '-', _] => (IncDec(Decrement), 2),

            ['+', _, _] => (Plus, 1),
            ['-', _, _] => (Minus, 1),

            ['!', '=', _] => (Binary(Cmp(Ne)), 2),
            ['!', _, _] => (Unary(LogicalNot), 1),
            ['~', _, _] => (Unary(Complement), 1),

            ['&', _, _] => (Ampersand, 1),
            ['*', _, _] => (Asterisk, 1),

            ['=', '=', '='] => (Assign(AssignStruct::from(RegularBinary(Cmp(Eq)))), 3),
            ['=', '=', _] => (Binary(Cmp(Eq)), 2),

            ['=', _, _] => {
                if let Some(
                    (rich_bin_op, rich_bin_op_len)
                ) = parse_assign_binary_operator(
                    s[1..].into_iter().cloned()) {
                    (Assign(AssignStruct::from(rich_bin_op)), 1 + rich_bin_op_len)
                } else {
                    (Assign(AssignStruct::from(None)), 1)
                }
            }

            _ => {
                if let Some(
                    (rich_bin_op, rich_bin_op_len)
                ) = parse_assign_binary_operator(
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

pub(crate) fn char_to_u64(char: &str) -> u64 {
    let mut char_as_u64: u64 = 0;
    for (i, char) in char.chars().enumerate() {
        char_as_u64 |= (char as u64) << (i * 8);
    }
    char_as_u64
}

impl Tokenizer<'_> {
    fn tokenize(&self, source_code: &str, issues: &mut Vec<Issue>) -> Result<Vec<Token>, String> {
        let mut res = vec![];
        let mut buffer = Cell::default();
        let mut line_no: usize = 1;
        let mut i: usize = 0;
        let mut curr_line_started_at: usize = 0;

        #[derive(Eq, PartialEq, Copy, Clone, Debug)]
        enum CommentOrCharOrString {
            Comment,
            Char,
            String,
        }

        impl CommentOrCharOrString {
            fn get_opening_literal(&self) -> &'static str {
                match self {
                    CommentOrCharOrString::Comment => "/*",
                    CommentOrCharOrString::Char => "'",
                    CommentOrCharOrString::String => "\"",
                }
            }

            fn get_closing_literal(&self) -> &'static str {
                match self {
                    CommentOrCharOrString::Comment => "*/",
                    CommentOrCharOrString::Char | CommentOrCharOrString::String =>
                        self.get_opening_literal(),
                }
            }
        }

        let mut currently_reading: Option<(CommentOrCharOrString, TokenPos)> = None;

        fn read_name(s: &str) -> Option<String> {
            let first_symbol = s.chars().next()?;
            if first_symbol.is_ascii_digit() {
                return None;
            }

            let res = s.chars()
                .into_iter()
                .take_while(|c| c.is_ascii_alphanumeric() || ['_', '.'].contains(c))
                .collect();

            Some(res)
        }

        let comment_or_char_or_string_variants =
            vec![CommentOrCharOrString::Comment,
                 CommentOrCharOrString::Char,
                 CommentOrCharOrString::String];

        while i < source_code.len() {
            let token_pos = TokenPos::from((line_no, i - curr_line_started_at + 1));
            let curr_char = source_code.chars().nth(i).unwrap_or_default();

            if curr_char == '\n' {
                curr_line_started_at = i + 1;
                line_no += 1;
            } else if let Some(
                (ref curr_reading, pos)
            ) = currently_reading {
                let closing_literal = curr_reading.get_closing_literal();

                if source_code[i..].starts_with(closing_literal) {
                    if *curr_reading != CommentOrCharOrString::Comment {
                        res.push(Token {
                            token: WrappedToken::Constant(
                                match curr_reading {
                                    CommentOrCharOrString::String =>
                                        Constant::String(buffer.take()),

                                    CommentOrCharOrString::Char => {
                                        let char = buffer.take();
                                        if char.len() > self.compiler_options
                                            .target_platform.arch.word_size() as usize {
                                            issues.push(Issue::LiteralTooLong(pos))
                                        }
                                        Constant::Number(char_to_u64(&char))
                                    }
                                    _ => unreachable!(),
                                }),
                            pos,
                        });
                    }

                    currently_reading = None;
                    i += closing_literal.len();
                    continue;
                }

                if *curr_reading != CommentOrCharOrString::Comment {
                    let mut esc_seq_matched = false;
                    // TODO: finite state machine
                    for (esc_seq, symbols) in self.escape_sequences {
                        if source_code[i..].starts_with(esc_seq) {
                            buffer.get_mut().push_str(symbols);
                            i += esc_seq.len();
                            esc_seq_matched = true;
                            break;
                        }
                    }
                    if esc_seq_matched {
                        continue;
                    }
                    buffer.get_mut().push(curr_char);
                }
            } else {
                for comment_or_char_or_string in &comment_or_char_or_string_variants {
                    let opening_literal = comment_or_char_or_string.get_opening_literal();

                    if source_code[i..].starts_with(opening_literal) {
                        currently_reading = Some((*comment_or_char_or_string, token_pos));

                        i += opening_literal.len();
                        break;
                    }
                }
                if currently_reading.is_some() {
                    continue;
                }
                if let Some(name) = read_name(&source_code[i..]) {
                    let name_len = name.len();
                    if name_len > 0 {
                        if let Some(
                            keyword
                        ) = self.reserved_symbols.get(&name) {
                            res.push(Token {
                                token: WrappedToken::ReservedName(*keyword),
                                pos: token_pos,
                            });
                        } else {
                            res.push(Token {
                                token: WrappedToken::Name(name),
                                pos: token_pos,
                            });
                        }
                        i += name_len;
                        continue;
                    }
                }
                match curr_char {
                    ' ' => (),
                    ',' => res.push(Token {
                        token: WrappedToken::Comma,
                        pos: token_pos,
                    }),
                    ';' => res.push(Token {
                        token: WrappedToken::Semicolon,
                        pos: token_pos,
                    }),
                    ':' => res.push(Token {
                        token: WrappedToken::Colon,
                        pos: token_pos,
                    }),
                    '0'..='9' => {
                        let number_as_str: String = source_code[i..].chars()
                            .into_iter()
                            .take_while(|c| c.is_ascii_digit())
                            .collect();
                        let number_len = number_as_str.len();

                        let radix =
                            if curr_char == '0' {
                                8
                            } else {
                                10
                            };
                        let val = u64::from_str_radix(&number_as_str, radix)
                            .unwrap_or_else(|_| {
                                issues.push(Issue::LiteralTooLong(token_pos));
                                0
                            });
                        res.push(
                            Token {
                                token: WrappedToken::Constant(Constant::Number(val)),
                                pos: token_pos,
                            });
                        i += number_len;
                        continue;
                    }
                    '?' => res.push(Token {
                        token: WrappedToken::QuestionMark,
                        pos: token_pos,
                    }),
                    '[' | ']' | '(' | ')' | '{' | '}' =>
                        res.push(Token {
                            token: WrappedToken::Bracket(
                                Bracket::from_char_unchecked(curr_char)),
                            pos: token_pos,
                        }),
                    '\r' => (), // for Windows
                    _ =>
                        if let Some(
                            (op, tokens_read)
                        ) = parse_operator(source_code[i..].chars().into_iter()) {
                            res.push(Token {
                                token: WrappedToken::Operator(op),
                                pos: token_pos,
                            });

                            i += tokens_read;
                            continue;
                        } else {
                            return Err(format!("{}: unknown character encountered: {}",
                                               token_pos, curr_char as char));
                        }
                }
            }

            i += 1;
        }

        if let Some((currently_reading, opening_literal_pos)) = currently_reading {
            let closing_literal = currently_reading.get_closing_literal();

            return Err(format!("expected {} (opening literal found in {}), found end of file",
                               closing_literal, opening_literal_pos), );
        }

        Ok(res)
    }

    pub(crate) fn run(
        &self,
        source_code: &str,
        issues: &mut Vec<Issue>,
    ) -> Result<Vec<Token>, String> {
        Ok(self.tokenize(&source_code, issues)?)
    }
}

#[cfg(test)]
mod tests {
    use std::borrow::Borrow;

    use crate::config::{get_reserved_symbols, get_second_symbol_of_escape_sequence_to_character_mapping};

    use super::*;

    fn lexical_analyzer_test<S: AsRef<String>, Z: AsRef<str>>(
        inp: S,
        exp_out: Result<&Vec<Token>, String>,
        panic_msg: Option<Z>,
        scan_first: bool,
        compiler_options: &CompilerOptions,
        keywords: &ReservedSymbolsTable,
        second_symbol_of_escape_sequence_to_character_mapping: &HashMap<char, char>,
    ) -> Result<(), String> {
        let inp = inp.as_ref();
        let exp_out = exp_out.as_ref();
        let mut la = Tokenizer {
            compiler_options,
            second_symbol_of_escape_sequence_to_character_mapping,
            reserved_symbols: keywords,
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
        let kw = get_reserved_symbols();
        let la = Tokenizer {
            compiler_options: &co,
            second_symbol_of_escape_sequence_to_character_mapping: &esm,
            reserved_symbols: &kw,
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