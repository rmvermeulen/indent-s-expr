pub fn indent_s_expr<S: AsRef<str>>(sexp: S, tabsize: usize) -> Result<String, NotSexprError> {
    let tokens = parse_sexp(sexp.as_ref())?;
    Ok(Sexp(tokens).indented(tabsize))
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
    let mut token: Option<Token<'_>> = None;
    let mut escaped = false;
    let mut inside_string = false;
    for (i, c) in sexp.chars().enumerate() {
        if escaped {
            escaped = false;
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
            continue;
        }
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
            '\\' => {
                assert!(!escaped, "cannot escape twice");
                escaped = true;
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
    fn test_indent_sexp_directly() {
        assert_debug_snapshot!(indent_s_expr(r#"(one (two) (+ 1 :atom) "string")"#, 2));
    }

    #[rstest]
    fn test_parse_sexp_nested_string() {
        assert_debug_snapshot!(parse_sexp(r#"(join "abc\"" "\"def")"#));
    }

    #[rstest]
    fn test_parse_empty_string() {
        assert_debug_snapshot!(parse_sexp(""));
    }

    #[rstest]
    fn test_parse_partial_sexp() {
        assert_debug_snapshot!(parse_sexp("(+ 1 2 3"));
    }

    #[rstest]
    fn test_parse_sexp_numerics() {
        assert_debug_snapshot!(parse_sexp("(+ 123ax ax123)"));
        assert_debug_snapshot!(parse_sexp("(+ 123ax(ax123))"));
    }

    #[rstest]
    fn test_parse_sexp_empty() {
        assert_debug_snapshot!(parse_sexp("()"));
    }

    #[rstest]
    fn test_parse_sexp_empty_nested() {
        assert_debug_snapshot!(parse_sexp("(())"));
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
        assert_debug_snapshot!(parse_sexp("(+ 1 26)"));
    }

    #[rstest]
    fn test_parse_sexp_addition_nested() {
        assert_debug_snapshot!(parse_sexp("(+ 10 (+ 82 43))"));
    }

    #[rstest]
    fn test_parse_sexp_escaping() {
        assert_debug_snapshot!(parse_sexp(r#"(\a)"#));
        assert_debug_snapshot!(parse_sexp(r#"(\1)"#));
        assert_debug_snapshot!(parse_sexp(r#"(\\)"#));
        assert_debug_snapshot!(parse_sexp(r#"(\\\)"#));
        assert_debug_snapshot!(parse_sexp(r#"(\\\\)"#));
        assert_debug_snapshot!(parse_sexp(r#"(\\\\\)"#));
        assert_debug_snapshot!(parse_sexp(r#"(fn "string with\ spaces")"#));
    }

    #[rstest]
    fn test_parse_sexp_addition_nested_doule() {
        assert_debug_snapshot!(parse_sexp("(+ 10 (+ 22 31) (+ 47 52))"));
    }

    #[rstest]
    fn test_parse_sexp_symbols() {
        assert_debug_snapshot!(parse_sexp(r#"(fn "string" "string with spaces")"#));
    }

    #[rstest]
    fn test_parse_sexp_symbols_atom() {
        assert_debug_snapshot!(parse_sexp("(fn symbol :atom)"));
        assert_debug_snapshot!(
            indent_s_expr("(+ (alpha / beta) (gamma * delta))", 2),
            @r###"
        Ok(
            "(\n  +\n  (\n    alpha\n    /\n    beta\n  )\n  (\n    gamma\n    *\n    delta\n  )\n)\n",
        )
        "###
        );
    }
}
