#[cfg(test)]
#[allow(clippy::single_component_path_imports)]
use rstest_reuse;

use std::fs::read_to_string;
use tree_sitter::Parser;

fn main() {
    let mut parser = Parser::new();
    parser.set_language(tree_sitter_rust::language()).unwrap();

    let source = read_to_string("src/main.rs").unwrap();

    let tree = parser.parse(&source, None).unwrap();

    println!("-----------------");

    let sexp = tree.root_node().to_sexp();
    println!("{}", &sexp[0..100]);

    println!("-----------------");

    let sexp = indent_sexp(&sexp);
    println!("{}", &sexp[0..100]);

    println!("-----------------");
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum TokenKind {
    OpenParen,
    CloseParen,
    Symbol,
    Number,
    String,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Token<'a> {
    pub kind: TokenKind,
    pub range: (usize, usize),
    pub content: &'a str,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Line<'a> {
    pub indent: usize,
    pub content: &'a str,
}

#[derive(Debug, Clone)]
pub struct NotSexprError;

fn parse_sexp(sexp: &str) -> Result<Vec<Token>, NotSexprError> {
    let mut tokens = vec![];
    let mut token = None;
    let mut inside_string = false;
    for (i, c) in sexp.chars().enumerate() {
        match c {
            '(' => {
                if let Some(token) = token.take() {
                    tokens.push(token);
                }
                let next_token = Token {
                    kind: TokenKind::OpenParen,
                    content: &sexp[i..i + 1],
                    range: (i, i + 1),
                };
                println!("pushing token: {:?}", next_token);
                tokens.push(next_token);
            }
            ')' => {
                if let Some(token) = token.take() {
                    tokens.push(token);
                }
                let next_token = Token {
                    kind: TokenKind::CloseParen,
                    content: sexp[i..i + 1].as_ref(),
                    range: (i, i + 1),
                };
                println!("pushing token: {:?}", next_token);
                tokens.push(next_token);
            }
            ' ' | '\n' | '\t' => {
                if inside_string {
                    let token: &mut Token =
                        token.as_mut().expect("token should exist inside string");
                    token.content = &sexp[token.content.len()..i];
                } else if let Some(token) = token.take() {
                    println!("pushing token: {:?}", token);
                    tokens.push(token);
                }
            }
            '"' => {
                if inside_string {
                    let token: Token = token.take().expect("token should exist inside string");
                    println!("pushing token: {:?}", token);
                    tokens.push(Token {
                        kind: TokenKind::String,
                        content: token.content,
                        range: (token.range.0, i + 1),
                    });
                    inside_string = false;
                } else {
                    token = Some(Token {
                        kind: TokenKind::String,
                        content: &sexp[i..i + 1],
                        range: (i, i + 1),
                    });
                    inside_string = true;
                }
            }
            _ => {
                if let Some(token) = token.as_mut() {
                    token.content = &sexp[token.content.len()..i];
                    println!("updated token.content: '{:?}'", token.content);
                } else {
                    token = Some(Token {
                        kind: TokenKind::Symbol,
                        content: &sexp[i..i + 1],
                        range: (i, i + 1),
                    });
                }
            }
        }
    }
    if token.is_some() {
        return Err(NotSexprError);
    }

    Ok(tokens)
}

fn indent_sexp(sexp: &str) -> String {
    unimplemented!("index_sexp(sexp)");
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::rstest;
    use rstest_reuse::{self, *};
    use speculoos::prelude::*;

    #[template]
    #[rstest]
    #[case("()")]
    #[case("(single)")]
    #[case("(one two)")]
    #[case("(+ 1 2)")]
    #[case("(+ 1 (+ 2 3))")]
    #[case("(+ 1 (+ 2 3) (+ 4 5))")]
    #[case("(fn symbol 4 \"string\")")]
    fn basic_expressions(#[case] sexp: &str) {}

    #[rstest]
    #[case("()", 2)]
    #[case("(single)", 3)]
    #[case("(one two)", 4)]
    #[case("(+ 1 2)", 5)]
    #[case("(+ 1 (+ 2 3))", 9)]
    #[case("(+ 1 (+ 2 3) (+ 4 5))", 14)]
    #[case("(fn symbol 4 \"string\")", 6)]
    fn test_basics(#[case] sexp: &str, #[case] expected_token_count: usize) {
        assert_that!(parse_sexp(sexp))
            .is_ok()
            .has_length(expected_token_count);
    }

    #[apply(basic_expressions)]
    fn test_parse_sexp_tree(#[case] sexp: &str) {
        insta::assert_debug_snapshot!(parse_sexp(sexp));
    }
}
