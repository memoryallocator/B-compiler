use std::cell::Cell;
use std::collections::HashMap;

use token::{
    Assign, BinaryOperation, BinaryRelation, CtrlStmtIdent, IncOrDec, LeftOrRight, Operator,
    RichBinaryOperation, UnaryOperation, WrappedToken,
};

pub mod token;

pub(crate) struct Tokenizer<'a> {
    pub(crate) escape_sequences: &'a HashMap<String, String>,
    pub(crate) reserved_symbols: &'a ReservedSymbolsTable,
    pub(crate) compiler_options: CompilerOptions,
}

fn parse_assign_binary_operator(s: &str) -> Option<(RichBinaryOperation, usize)> {
    use BinaryOperation::*;
    use BinaryRelation::*;
    use LeftOrRight::*;
    use RichBinaryOperation::*;

    return Some(match s.chars().take(2).collect::<Vec<char>>().as_slice() {
        ['=', '='] => (RegularBinary(Cmp(Eq)), 2),
        ['!', '='] => (RegularBinary(Cmp(Ne)), 2),

        ['<', '='] => (RegularBinary(Cmp(Le)), 2),
        ['>', '='] => (RegularBinary(Cmp(Ge)), 2),

        ['<', '<'] => (RegularBinary(Shift(Left)), 2),
        ['>', '>'] => (RegularBinary(Shift(Right)), 2),

        ['<', _] => (RegularBinary(Cmp(Lt)), 1),
        ['>', _] => (RegularBinary(Cmp(Gt)), 1),

        ['+', _] => (Add, 1),
        ['-', _] => (Sub, 1),

        ['&', _] => (BitwiseAnd, 1),
        ['|', _] => (RegularBinary(Or), 1),
        ['^', _] => (RegularBinary(Xor), 1),

        ['*', _] => (Mul, 1),
        ['/', _] => (RegularBinary(Div), 1),
        ['%', _] => (RegularBinary(Mod), 1),
        _ => return None,
    });
}

fn parse_operator(s: &str) -> Option<(Operator, usize)> {
    use Assign as AssignStruct;
    use BinaryOperation::*;
    use BinaryRelation::*;
    use IncOrDec::*;
    use Operator::*;
    use RichBinaryOperation::*;
    use UnaryOperation::*;

    return Some(match s.chars().take(3).collect::<Vec<char>>().as_slice() {
        ['+', '+', _] => (IncDec(Increment), 2),
        ['-', '-', _] => (IncDec(Decrement), 2),

        ['+', _, _] => (Plus, 1),
        ['-', _, _] => (Minus, 1),

        ['&', _, _] => (Ampersand, 1),
        ['*', _, _] => (Asterisk, 1),

        ['!', '=', _] => (Binary(Cmp(Ne)), 2),
        ['!', _, _] => (Unary(LogicalNot), 1),
        ['~', _, _] => (Unary(Complement), 1),

        ['=', '=', '='] => (Assign(AssignStruct::from(RegularBinary(Cmp(Eq)))), 3),
        ['=', '=', _] => (Binary(Cmp(Eq)), 2),

        ['=', _, _] => {
            if let Some((rich_bin_op, rich_bin_op_len)) = parse_assign_binary_operator(&s[1..]) {
                (Assign(AssignStruct::from(rich_bin_op)), 1 + rich_bin_op_len)
            } else {
                (Assign(AssignStruct::from(None)), 1)
            }
        }

        _ => {
            if let Some((RegularBinary(bin_op), rich_bin_op_len)) = parse_assign_binary_operator(s)
            {
                (Binary(bin_op), rich_bin_op_len)
            } else {
                return None;
            }
        }
    });
}

pub(crate) fn char_to_u64(char: &str) -> u64 {
    let mut char_as_u64: u64 = 0;
    for (i, char) in char.chars().enumerate() {
        char_as_u64 |= (char as u64).wrapping_shl((i * 8) as u32);
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
                    CommentOrCharOrString::Char | CommentOrCharOrString::String => {
                        self.get_opening_literal()
                    }
                }
            }
        }

        let mut currently_reading: Option<(CommentOrCharOrString, TokenPos)> = None;

        fn read_name(s: &str) -> Option<String> {
            let first_symbol = s.chars().next()?;
            if first_symbol.is_ascii_digit() {
                return None;
            }

            let res = s
                .chars()
                .into_iter()
                .take_while(|c| c.is_ascii_alphanumeric() || ['_', '.'].contains(c))
                .collect();

            Some(res)
        }

        let comment_or_char_or_string_variants = vec![
            CommentOrCharOrString::Comment,
            CommentOrCharOrString::Char,
            CommentOrCharOrString::String,
        ];

        while i < source_code.len() {
            let token_pos = TokenPos::from((line_no, i - curr_line_started_at + 1));
            let curr_char = source_code.chars().nth(i).unwrap_or_default();

            if curr_char == '\n' {
                curr_line_started_at = i + 1;
                line_no += 1;
            }
            if let Some((ref curr_reading, pos)) = currently_reading {
                let closing_literal = curr_reading.get_closing_literal();

                if source_code[i..].starts_with(closing_literal) {
                    if *curr_reading != CommentOrCharOrString::Comment {
                        res.push(Token {
                            token: WrappedToken::Constant(match curr_reading {
                                CommentOrCharOrString::String => Constant::String(buffer.take()),

                                CommentOrCharOrString::Char => {
                                    let char = buffer.take();
                                    if char.len()
                                        > self.compiler_options.target_platform.arch.word_size()
                                            as usize
                                    {
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
                        if let Some(keyword) = self.reserved_symbols.get(&name) {
                            if *keyword == ReservedName::CtrlStmt(CtrlStmtIdent::Continue)
                                && !self.compiler_options.continue_is_enabled
                            {
                                res.push(Token {
                                    token: WrappedToken::Name(name),
                                    pos: token_pos,
                                })
                            } else {
                                res.push(Token {
                                    token: WrappedToken::ReservedName(*keyword),
                                    pos: token_pos,
                                });
                            }
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
                    ' ' | '\t' | '\r' | '\n' => (),
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
                        let number_as_str: String = source_code[i..]
                            .chars()
                            .into_iter()
                            .take_while(|c| c.is_ascii_digit())
                            .collect();
                        let number_len = number_as_str.len();

                        let radix = if curr_char == '0' { 8 } else { 10 };
                        let val = u64::from_str_radix(&number_as_str, radix).unwrap_or_else(|_| {
                            issues.push(Issue::LiteralTooLong(token_pos));
                            0
                        });
                        res.push(Token {
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
                    '[' | ']' | '(' | ')' | '{' | '}' => res.push(Token {
                        token: WrappedToken::Bracket(Bracket::from_char_unchecked(curr_char)),
                        pos: token_pos,
                    }),
                    _ => {
                        if let Some((op, tokens_read)) = parse_operator(&source_code[i..]) {
                            res.push(Token {
                                token: WrappedToken::Operator(op),
                                pos: token_pos,
                            });

                            i += tokens_read;
                            continue;
                        } else {
                            return Err(format!(
                                "{}: unknown character encountered: {}",
                                token_pos, curr_char as char
                            ));
                        }
                    }
                }
            }

            i += 1;
        }
        if let Some((currently_reading, opening_literal_pos)) = currently_reading {
            let closing_literal = currently_reading.get_closing_literal();
            return Err(format!(
                "expected {} (opening literal found in {}), found end of file",
                closing_literal, opening_literal_pos
            ));
        }
        Ok(res)
    }

    pub(crate) fn run(
        &self,
        source_code: &str,
        issues: &mut Vec<Issue>,
    ) -> Result<Vec<Token>, String> {
        self.tokenize(source_code, issues)
    }
}
