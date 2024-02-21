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
    println!("parsing sexp: {:?}", sexp);
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
                println!("( pushing token: {:?}", next_token);
                tokens.push(next_token);
            }
            ')' => {
                if let Some(mut token) = token.take() {
                    token.content = &sexp[token.range.0..i];
                    println!(") pushing token: {:?}", token);
                    tokens.push(token);
                }
                let next_token = Token {
                    kind: TokenKind::CloseParen,
                    content: sexp[i..i + 1].as_ref(),
                    range: (i, i + 1),
                };
                println!(") pushing token: {:?}", next_token);
                tokens.push(next_token);
            }
            ' ' | '\n' | '\t' => {
                if inside_string {
                    let token: &mut Token =
                        token.as_mut().expect("token should exist inside string");
                    token.content = &sexp[token.range.0..i];
                } else if let Some(mut token) = token.take() {
                    token.content = &sexp[token.range.0..i];
                    println!("\\s pushing token: {:?}", token);
                    tokens.push(token);
                }
            }
            '"' => {
                if inside_string {
                    let token: Token = token.take().expect("token should exist inside string");
                    println!("\" pushing token: {:?}", token);
                    tokens.push(Token {
                        kind: TokenKind::String,
                        content: &sexp[token.range.0..i],
                        range: (token.range.0, i),
                    });
                    inside_string = false;
                } else {
                    token = Some(Token {
                        kind: TokenKind::String,
                        content: &sexp[i + 1..i + 1],
                        range: (i + 1, i + 1),
                    });
                    inside_string = true;
                    println!("start string: {:?}", token);
                }
            }
            _ => {
                if let Some(token) = token.as_mut() {
                    token.content = &sexp[token.range.0..i];
                    println!("_ updated token.content: '{:?}'", token.content);
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
    use insta::assert_debug_snapshot;
    use rstest::rstest;
    use rstest_reuse::{self, *};
    use speculoos::prelude::*;

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

    #[rstest]
    fn test_parse_sexp_empty() {
        assert_debug_snapshot!(
            parse_sexp("()"),
            @r###"
        Ok(
            [
                Token {
                    kind: OpenParen,
                    range: (
                        0,
                        1,
                    ),
                    content: "(",
                },
                Token {
                    kind: CloseParen,
                    range: (
                        1,
                        2,
                    ),
                    content: ")",
                },
            ],
        )
        "###);
    }

    #[rstest]
    fn test_parse_sexp_single() {
        assert_debug_snapshot!(parse_sexp("(single)"), @r###"
        Ok(
            [
                Token {
                    kind: OpenParen,
                    range: (
                        0,
                        1,
                    ),
                    content: "(",
                },
                Token {
                    kind: Symbol,
                    range: (
                        1,
                        2,
                    ),
                    content: "single",
                },
                Token {
                    kind: CloseParen,
                    range: (
                        7,
                        8,
                    ),
                    content: ")",
                },
            ],
        )
        "###);
    }

    #[rstest]
    fn test_parse_sexp_double() {
        assert_debug_snapshot!(parse_sexp("(one two)"), @r###"
        Ok(
            [
                Token {
                    kind: OpenParen,
                    range: (
                        0,
                        1,
                    ),
                    content: "(",
                },
                Token {
                    kind: Symbol,
                    range: (
                        1,
                        2,
                    ),
                    content: "one",
                },
                Token {
                    kind: Symbol,
                    range: (
                        5,
                        6,
                    ),
                    content: "two",
                },
                Token {
                    kind: CloseParen,
                    range: (
                        8,
                        9,
                    ),
                    content: ")",
                },
            ],
        )
        "###);
    }

    #[rstest]
    fn test_parse_sexp_addition() {
        assert_debug_snapshot!(parse_sexp("(+ 1 2)"), @r###"
        Ok(
            [
                Token {
                    kind: OpenParen,
                    range: (
                        0,
                        1,
                    ),
                    content: "(",
                },
                Token {
                    kind: Symbol,
                    range: (
                        1,
                        2,
                    ),
                    content: "+",
                },
                Token {
                    kind: Symbol,
                    range: (
                        3,
                        4,
                    ),
                    content: "1",
                },
                Token {
                    kind: Symbol,
                    range: (
                        5,
                        6,
                    ),
                    content: "2",
                },
                Token {
                    kind: CloseParen,
                    range: (
                        6,
                        7,
                    ),
                    content: ")",
                },
            ],
        )
        "###);
    }

    #[rstest]
    fn test_parse_sexp_addition_nested() {
        assert_debug_snapshot!(parse_sexp("(+ 1 (+ 2 3))"),@r###"
        Ok(
            [
                Token {
                    kind: OpenParen,
                    range: (
                        0,
                        1,
                    ),
                    content: "(",
                },
                Token {
                    kind: Symbol,
                    range: (
                        1,
                        2,
                    ),
                    content: "+",
                },
                Token {
                    kind: Symbol,
                    range: (
                        3,
                        4,
                    ),
                    content: "1",
                },
                Token {
                    kind: OpenParen,
                    range: (
                        5,
                        6,
                    ),
                    content: "(",
                },
                Token {
                    kind: Symbol,
                    range: (
                        6,
                        7,
                    ),
                    content: "+",
                },
                Token {
                    kind: Symbol,
                    range: (
                        8,
                        9,
                    ),
                    content: "2",
                },
                Token {
                    kind: Symbol,
                    range: (
                        10,
                        11,
                    ),
                    content: "3",
                },
                Token {
                    kind: CloseParen,
                    range: (
                        11,
                        12,
                    ),
                    content: ")",
                },
                Token {
                    kind: CloseParen,
                    range: (
                        12,
                        13,
                    ),
                    content: ")",
                },
            ],
        )
        "###);
    }

    #[rstest]
    fn test_parse_sexp_addition_nested_doule() {
        assert_debug_snapshot!(parse_sexp("(+ 1 (+ 2 3) (+ 4 5))"), @r###"
        Ok(
            [
                Token {
                    kind: OpenParen,
                    range: (
                        0,
                        1,
                    ),
                    content: "(",
                },
                Token {
                    kind: Symbol,
                    range: (
                        1,
                        2,
                    ),
                    content: "+",
                },
                Token {
                    kind: Symbol,
                    range: (
                        3,
                        4,
                    ),
                    content: "1",
                },
                Token {
                    kind: OpenParen,
                    range: (
                        5,
                        6,
                    ),
                    content: "(",
                },
                Token {
                    kind: Symbol,
                    range: (
                        6,
                        7,
                    ),
                    content: "+",
                },
                Token {
                    kind: Symbol,
                    range: (
                        8,
                        9,
                    ),
                    content: "2",
                },
                Token {
                    kind: Symbol,
                    range: (
                        10,
                        11,
                    ),
                    content: "3",
                },
                Token {
                    kind: CloseParen,
                    range: (
                        11,
                        12,
                    ),
                    content: ")",
                },
                Token {
                    kind: OpenParen,
                    range: (
                        13,
                        14,
                    ),
                    content: "(",
                },
                Token {
                    kind: Symbol,
                    range: (
                        14,
                        15,
                    ),
                    content: "+",
                },
                Token {
                    kind: Symbol,
                    range: (
                        16,
                        17,
                    ),
                    content: "4",
                },
                Token {
                    kind: Symbol,
                    range: (
                        18,
                        19,
                    ),
                    content: "5",
                },
                Token {
                    kind: CloseParen,
                    range: (
                        19,
                        20,
                    ),
                    content: ")",
                },
                Token {
                    kind: CloseParen,
                    range: (
                        20,
                        21,
                    ),
                    content: ")",
                },
            ],
        )
        "###);
    }

    #[rstest]
    fn test_parse_sexp_symbols() {
        assert_debug_snapshot!(parse_sexp("(fn symbol 4 \"string\")"), @r###"
        Ok(
            [
                Token {
                    kind: OpenParen,
                    range: (
                        0,
                        1,
                    ),
                    content: "(",
                },
                Token {
                    kind: Symbol,
                    range: (
                        1,
                        2,
                    ),
                    content: "fn",
                },
                Token {
                    kind: Symbol,
                    range: (
                        4,
                        5,
                    ),
                    content: "symbol",
                },
                Token {
                    kind: Symbol,
                    range: (
                        11,
                        12,
                    ),
                    content: "4",
                },
                Token {
                    kind: String,
                    range: (
                        14,
                        20,
                    ),
                    content: "string",
                },
                Token {
                    kind: CloseParen,
                    range: (
                        21,
                        22,
                    ),
                    content: ")",
                },
            ],
        )
        "###);
    }

    #[rstest]
    fn test_parse_sexp_symbols_2() {
        assert_debug_snapshot!(parse_sexp("(fn symbol \"string\" :atom)"), @r###"
        Ok(
            [
                Token {
                    kind: OpenParen,
                    range: (
                        0,
                        1,
                    ),
                    content: "(",
                },
                Token {
                    kind: Symbol,
                    range: (
                        1,
                        2,
                    ),
                    content: "fn",
                },
                Token {
                    kind: Symbol,
                    range: (
                        4,
                        5,
                    ),
                    content: "symbol",
                },
                Token {
                    kind: String,
                    range: (
                        12,
                        18,
                    ),
                    content: "string",
                },
                Token {
                    kind: Symbol,
                    range: (
                        20,
                        21,
                    ),
                    content: ":atom",
                },
                Token {
                    kind: CloseParen,
                    range: (
                        25,
                        26,
                    ),
                    content: ")",
                },
            ],
        )
        "###);
    }
}
