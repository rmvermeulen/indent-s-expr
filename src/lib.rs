pub fn indent_sexp(sexp: &str, tabsize: usize) -> Result<String, NotSexprError> {
    parse_sexp(sexp).map(|tokens| Sexp(tokens).indented(tabsize))
}

#[derive(Debug)]
pub struct Sexp<'a>(Vec<Token<'a>>);

impl Sexp<'_> {
    fn indented(&self, tabsize: usize) -> String {
        let indent = " ".repeat(tabsize);
        let mut current_indent = 0;
        let mut result = String::new();
        for token in self.0.iter() {
            match token.kind {
                TokenKind::OpenParen => {
                    result += &format!("{}(\n", indent.repeat(current_indent));
                    current_indent += 1;
                }
                TokenKind::CloseParen => {
                    current_indent -= 1;
                    result += &format!("{})\n", indent.repeat(current_indent));
                }
                TokenKind::Symbol => {
                    result += &format!("{}{}\n", indent.repeat(current_indent), token.content);
                }
                TokenKind::Number => {
                    result += &format!("{}{}\n", indent.repeat(current_indent), token.content);
                }
                TokenKind::String => {
                    result += &format!("{}{}\n", indent.repeat(current_indent), token.content);
                }
            }
        }
        result
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
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
                tokens.push(next_token);
            }
            ')' => {
                if let Some(mut token) = token.take() {
                    token.content = &sexp[token.range.0..i];
                    tokens.push(token);
                }
                let next_token = Token {
                    kind: TokenKind::CloseParen,
                    content: sexp[i..i + 1].as_ref(),
                    range: (i, i + 1),
                };
                tokens.push(next_token);
            }
            ' ' | '\n' | '\t' => {
                if inside_string {
                    let token: &mut Token =
                        token.as_mut().expect("token should exist inside string");
                    token.content = &sexp[token.range.0..i];
                } else if let Some(mut token) = token.take() {
                    token.content = &sexp[token.range.0..i];
                    tokens.push(token);
                }
            }
            '"' => {
                if inside_string {
                    let token: Token = token.take().expect("token should exist inside string");
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
                }
            }
            c => {
                if let Some(token) = token.as_mut() {
                    token.content = &sexp[token.range.0..i];
                } else {
                    token = Some(Token {
                        kind: if c.is_ascii_digit() {
                            TokenKind::Number
                        } else {
                            TokenKind::Symbol
                        },
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
        assert_debug_snapshot!(parse_sexp("(+ 1 2)"));
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