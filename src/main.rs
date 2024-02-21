

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
    
    

    #[rstest]
    fn test_parse_sexp_empty() {
        assert_debug_snapshot!(parse_sexp("()"));
    }

    #[rstest]
    fn test_parse_sexp_single() {
        assert_debug_snapshot!(parse_sexp("(single)"));
    }

    #[rstest]
    fn test_parse_sexp_double() {
        assert_debug_snapshot!(parse_sexp("(one two)"));
    }

    #[rstest]
    fn test_parse_sexp_addition() {
        assert_debug_snapshot!(parse_sexp("(+ 1 2)")
       );
    }

    #[rstest]
    fn test_parse_sexp_addition_nested() {
        assert_debug_snapshot!(parse_sexp("(+ 1 (+ 2 3))"));
    }

    #[rstest]
    fn test_parse_sexp_addition_nested_doule() {
        assert_debug_snapshot!(parse_sexp("(+ 1 (+ 2 3) (+ 4 5))"));
    }

    #[rstest]
    fn test_parse_sexp_symbols() {
        assert_debug_snapshot!(parse_sexp("(fn symbol 4 \"string\")"));
    }

    #[rstest]
    fn test_parse_sexp_symbols_atom() {
        assert_debug_snapshot!(parse_sexp("(fn symbol \"string\" :atom)"));
    }
}
