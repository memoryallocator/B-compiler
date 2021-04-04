use std::*;
use borrow::Borrow;
use hash::Hash;
use collections::{HashMap, HashSet};
use convert::TryFrom;

use crate::config::*;
use ast::*;
use crate::lexical_analyzer::token;
use token::*;

pub(crate) mod ast;
mod expression_parser;

pub(crate) struct Parser<'a> {
    pub(crate) compiler_options: CompilerOptions,
    pub(crate) standard_library_names: &'a HashSet<StandardLibraryName>,
    pub(crate) source_code: Option<&'a str>,
}

#[derive(PartialEq)]
enum BracketsStatus {
    Ok,
    NotClosed(TokenPos),
    NotOpened(TokenPos),
}

#[derive(Debug, Clone)]
pub(crate) struct MultiMap<K: cmp::Eq + Hash, V> {
    data: HashMap<K, Vec<V>>,
    size: usize,
}

impl<K: Eq + Hash + Clone, V: Clone> TryFrom<MultiMap<K, V>> for HashMap<K, V> {
    type Error = ();

    fn try_from(value: MultiMap<K, V>) -> Result<Self, Self::Error> {
        let mut res = HashMap::<K, V>::new();

        for (k, v) in value.get_inner() {
            if v.len() != 1 {
                return Err(());
            }

            res.insert(k.clone(), v[0].clone());
        }

        Ok(res)
    }
}

impl<K: Eq + Hash, V> MultiMap<K, V> {
    pub fn new() -> Self {
        MultiMap {
            data: Default::default(),
            size: 0,
        }
    }

    pub fn get_inner(&self) -> &HashMap<K, Vec<V>> {
        &self.data
    }
}

impl<K: Eq + Hash, V> MultiMap<K, V> {
    fn len(&self) -> usize {
        self.size
    }

    fn insert(&mut self, key: K, value: V) {
        self.size += 1;

        if let Some(vals) = self.data.get_mut(&key) {
            vals.push(value)
        } else {
            self.data.insert(key, vec![value]);
        }
    }

    fn get_last<Q: ?Sized>(&self, k: &Q) -> Option<&V>
        where K: Borrow<Q>, Q: Hash + Eq {
        self.data.get(k)?.last()
    }

    fn contains_key<Q: ?Sized>(&self, k: &Q) -> bool
        where K: Borrow<Q>, Q: Hash + Eq {
        self.data.contains_key(k)
    }
}

impl<K: Eq + Hash, V> Default for MultiMap<K, V> {
    fn default() -> Self {
        MultiMap {
            data: Default::default(),
            size: 0,
        }
    }
}

impl<K, V> Extend<(K, V)> for MultiMap<K, V>
    where K: Eq + Hash {
    fn extend<T: IntoIterator<Item=(K, V)>>(&mut self, iter: T) {
        for (k, v) in iter {
            self.insert(k, v)
        }
    }
}

// type ScopeTable = HashMap<TokenPos, Rc<Scope>>;

impl Parser<'_> {
    fn find_brackets_pairs<'b, I>(tok_it: I) -> (BracketsStatus, Option<Vec<Token>>)
        where I: Iterator<Item=&'b Token> {
        use LeftOrRight::*;

        let mut stack = Vec::<(&Bracket, &TokenPos, usize)>::new();
        let mut processed_tokens = Vec::<Token>::new();
        let mut tok_it = tok_it.enumerate();

        while let Some((i, token)) = tok_it.next() {
            if let Token {
                token: WrappedToken::Bracket(br),
                pos,
            } = token {
                if br.left_or_right == Left {
                    stack.push((br, pos, i));
                    processed_tokens.push(token.clone());
                } else {
                    let pos = pos.clone();

                    match stack.pop() {
                        None => return (BracketsStatus::NotOpened(pos.clone()), None),
                        Some((
                                 &left_br,
                                 &left_br_pos,
                                 left_br_idx)
                        ) => {
                            if !left_br.is_pair(br) {
                                return (BracketsStatus::NotOpened(pos), None);
                            }

                            let left_br = &mut processed_tokens[left_br_idx];
                            if let WrappedToken::Bracket(br) = &mut left_br.token {
                                br.pair_pos = Some(pos);
                            } else {
                                unreachable!()
                            }

                            let mut br = *br;
                            br.pair_pos = Some(left_br_pos);

                            processed_tokens.push(Token {
                                token: WrappedToken::Bracket(br),
                                pos,
                            });
                        }
                    }
                }
            } else {
                processed_tokens.push(token.clone());
            }
        }

        if stack.is_empty() {
            return (BracketsStatus::Ok, Some(processed_tokens));
        }
        (BracketsStatus::NotClosed(stack.pop().unwrap().1.clone()), None)
    }

    pub(crate) fn run(
        &mut self,
        tokens: &[Token],
    ) -> Result<ProgramNode, Error> {
        let tokens =
            match Parser::find_brackets_pairs(tokens.into_iter()) {
                (BracketsStatus::Ok, processed_tokens) => processed_tokens.unwrap(),
                (BracketsStatus::NotClosed(pos), _) => {
                    return Err(Error::BracketNotClosed(pos));
                }
                (BracketsStatus::NotOpened(pos), _) => {
                    return Err(Error::BracketNotOpened(pos));
                }
            };

        if tokens.is_empty() {
            return Err(Error::EmptyTokenStream);
        }

        let prog_node = ProgramNode::parse_exact(&tokens);
        if prog_node.is_none() {
            return Err(Error::ParsingError);
            // TODO: failure analysis & detailed error report
        }

        Ok(prog_node.unwrap())
    }
}