// pest. The Elegant Parser
// Copyright (c) 2018 Dragoș Tiselice
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

//! Types for the pest's abstract syntax tree.

/// A grammar rule
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Rule {
    /// The name of the rule
    pub name: String,
    /// The rule's type (silent, atomic, ...)
    pub ty: RuleType,
    /// The rule's expression
    pub expr: Expr,
}

/// All possible rule types
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum RuleType {
    /// The normal rule type
    Normal,
    /// Silent rules are just like normal rules
    /// — when run, they function the same way —
    /// except they do not produce pairs or tokens.
    /// If a rule is silent, it will never appear in a parse result.
    /// (their syntax is `_{ ... }`)
    Silent,
    /// atomic rule prevent implicit whitespace: inside an atomic rule,
    /// the tilde ~ means "immediately followed by",
    /// and repetition operators (asterisk * and plus sign +)
    /// have no implicit separation. In addition, all other rules
    /// called from an atomic rule are also treated as atomic.
    /// In an atomic rule, interior matching rules are silent.
    /// (their syntax is `@{ ... }`)
    Atomic,
    /// Compound atomic rules are similar to atomic rules,
    /// but they produce inner tokens as normal.
    /// (their syntax is `${ ... }`)
    CompoundAtomic,
    /// Non-atomic rules cancel the effect of atomic rules.
    /// (their syntax is `!{ ... }`)
    NonAtomic,
}

/// All possible rule expressions
///
/// # Warning: Semantic Versioning
/// There may be non-breaking changes to the meta-grammar
/// between minor versions. Those non-breaking changes, however,
/// may translate into semver-breaking changes due to the additional variants
/// propagated from the `Rule` enum. This is a known issue and will be fixed in the
/// future (e.g. by increasing MSRV and non_exhaustive annotations).
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Expr {
    /// Matches an exact string, e.g. `"a"`
    Str(String),
    /// Matches an exact string, case insensitively (ASCII only), e.g. `^"a"`
    Insens(String),
    /// Matches one character in the range, e.g. `'a'..'z'`
    Range(String, String),
    /// Matches the rule with the given name, e.g. `a`
    Ident(String),
    /// Matches a custom part of the stack, e.g. `PEEK[..]`
    PeekSlice(i32, Option<i32>),
    /// Positive lookahead; matches expression without making progress, e.g. `&e`
    PosPred(Box<Expr>),
    /// Negative lookahead; matches if expression doesn't match, without making progress, e.g. `!e`
    NegPred(Box<Expr>),
    /// Matches a sequence of two expressions, e.g. `e1 ~ e2`
    Seq(Box<Expr>, Box<Expr>),
    /// Matches either of two expressions, e.g. `e1 | e2`
    Choice(Box<Expr>, Box<Expr>),
    /// Optionally matches an expression, e.g. `e?`
    Opt(Box<Expr>),
    /// Matches an expression zero or more times, e.g. `e*`
    Rep(Box<Expr>),
    /// Matches an expression one or more times, e.g. `e+`
    RepOnce(Box<Expr>),
    /// Matches an expression an exact number of times, e.g. `e{n}`
    RepExact(Box<Expr>, u32),
    /// Matches an expression at least a number of times, e.g. `e{n,}`
    RepMin(Box<Expr>, u32),
    /// Matches an expression at most a number of times, e.g. `e{,n}`
    RepMax(Box<Expr>, u32),
    /// Matches an expression a number of times within a range, e.g. `e{m, n}`
    RepMinMax(Box<Expr>, u32, u32),
    /// Continues to match expressions until one of the strings in the `Vec` is found
    Skip(Vec<String>),
    /// Matches an expression and pushes it to the stack, e.g. `push(e)`
    Push(Box<Expr>),
    /// Pushes a literal string to the stack, e.g. `push_literal("a")`
    #[cfg(feature = "grammar-extras")]
    PushLiteral(String),
    /// Matches an expression and assigns a label to it, e.g. #label = exp
    #[cfg(feature = "grammar-extras")]
    NodeTag(Box<Expr>, String),
}

impl Expr {
    /// Returns the iterator that steps the expression from top to bottom.
    pub fn iter_top_down(&self) -> ExprTopDownIterator {
        ExprTopDownIterator::new(self)
    }

    /// Applies `f` to the expression and all its children (top to bottom).
    pub fn map_top_down<F>(self, mut f: F) -> Expr
    where
        F: FnMut(Expr) -> Expr,
    {
        fn map_internal<F>(expr: Expr, f: &mut F) -> Expr
        where
            F: FnMut(Expr) -> Expr,
        {
            let expr = f(expr);

            match expr {
                Expr::PosPred(expr) => {
                    let mapped = Box::new(map_internal(*expr, f));
                    Expr::PosPred(mapped)
                }
                Expr::NegPred(expr) => {
                    let mapped = Box::new(map_internal(*expr, f));
                    Expr::NegPred(mapped)
                }
                Expr::Seq(lhs, rhs) => {
                    let mapped_lhs = Box::new(map_internal(*lhs, f));
                    let mapped_rhs = Box::new(map_internal(*rhs, f));
                    Expr::Seq(mapped_lhs, mapped_rhs)
                }
                Expr::Choice(lhs, rhs) => {
                    let mapped_lhs = Box::new(map_internal(*lhs, f));
                    let mapped_rhs = Box::new(map_internal(*rhs, f));
                    Expr::Choice(mapped_lhs, mapped_rhs)
                }
                Expr::Rep(expr) => {
                    let mapped = Box::new(map_internal(*expr, f));
                    Expr::Rep(mapped)
                }
                Expr::RepOnce(expr) => {
                    let mapped = Box::new(map_internal(*expr, f));
                    Expr::RepOnce(mapped)
                }
                Expr::RepExact(expr, max) => {
                    let mapped = Box::new(map_internal(*expr, f));
                    Expr::RepExact(mapped, max)
                }
                Expr::RepMin(expr, num) => {
                    let mapped = Box::new(map_internal(*expr, f));
                    Expr::RepMin(mapped, num)
                }
                Expr::RepMax(expr, num) => {
                    let mapped = Box::new(map_internal(*expr, f));
                    Expr::RepMax(mapped, num)
                }
                Expr::RepMinMax(expr, min, max) => {
                    let mapped = Box::new(map_internal(*expr, f));
                    Expr::RepMinMax(mapped, min, max)
                }
                Expr::Opt(expr) => {
                    let mapped = Box::new(map_internal(*expr, f));
                    Expr::Opt(mapped)
                }
                Expr::Push(expr) => {
                    let mapped = Box::new(map_internal(*expr, f));
                    Expr::Push(mapped)
                }
                #[cfg(feature = "grammar-extras")]
                Expr::NodeTag(expr, tag) => {
                    let mapped = Box::new(map_internal(*expr, f));
                    Expr::NodeTag(mapped, tag)
                }
                expr => expr,
            }
        }

        map_internal(self, &mut f)
    }

    /// Applies `f` to the expression and all its children (bottom up).
    pub fn map_bottom_up<F>(self, mut f: F) -> Expr
    where
        F: FnMut(Expr) -> Expr,
    {
        fn map_internal<F>(expr: Expr, f: &mut F) -> Expr
        where
            F: FnMut(Expr) -> Expr,
        {
            let mapped = match expr {
                Expr::PosPred(expr) => {
                    let mapped = Box::new(map_internal(*expr, f));
                    Expr::PosPred(mapped)
                }
                Expr::NegPred(expr) => {
                    let mapped = Box::new(map_internal(*expr, f));
                    Expr::NegPred(mapped)
                }
                Expr::Seq(lhs, rhs) => {
                    let mapped_lhs = Box::new(map_internal(*lhs, f));
                    let mapped_rhs = Box::new(map_internal(*rhs, f));
                    Expr::Seq(mapped_lhs, mapped_rhs)
                }
                Expr::Choice(lhs, rhs) => {
                    let mapped_lhs = Box::new(map_internal(*lhs, f));
                    let mapped_rhs = Box::new(map_internal(*rhs, f));
                    Expr::Choice(mapped_lhs, mapped_rhs)
                }
                Expr::Rep(expr) => {
                    let mapped = Box::new(map_internal(*expr, f));
                    Expr::Rep(mapped)
                }
                Expr::RepOnce(expr) => {
                    let mapped = Box::new(map_internal(*expr, f));
                    Expr::RepOnce(mapped)
                }
                Expr::RepExact(expr, num) => {
                    let mapped = Box::new(map_internal(*expr, f));
                    Expr::RepExact(mapped, num)
                }
                Expr::RepMin(expr, max) => {
                    let mapped = Box::new(map_internal(*expr, f));
                    Expr::RepMin(mapped, max)
                }
                Expr::RepMax(expr, max) => {
                    let mapped = Box::new(map_internal(*expr, f));
                    Expr::RepMax(mapped, max)
                }
                Expr::RepMinMax(expr, min, max) => {
                    let mapped = Box::new(map_internal(*expr, f));
                    Expr::RepMinMax(mapped, min, max)
                }
                Expr::Opt(expr) => {
                    let mapped = Box::new(map_internal(*expr, f));
                    Expr::Opt(mapped)
                }
                Expr::Push(expr) => {
                    let mapped = Box::new(map_internal(*expr, f));
                    Expr::Push(mapped)
                }
                #[cfg(feature = "grammar-extras")]
                Expr::NodeTag(expr, tag) => {
                    let mapped = Box::new(map_internal(*expr, f));
                    Expr::NodeTag(mapped, tag)
                }
                expr => expr,
            };

            f(mapped)
        }

        map_internal(self, &mut f)
    }
}

/// Writes `string` as a pest string or character literal delimited by `quote`.
///
/// `{:?}` cannot be used directly: for `U+0001..=U+000F` it emits the single-hex-digit form
/// `\u{1}`, and pest's own `unicode` rule is `"u" ~ "{" ~ hex_digit{2, 6} ~ "}"`, so that form
/// is not valid pest. Everything else is escaped exactly as `{:?}` escapes it, which is what
/// keeps control characters out of generated doc comments.
pub(crate) fn fmt_literal(
    f: &mut core::fmt::Formatter<'_>,
    string: &str,
    quote: char,
) -> core::fmt::Result {
    write!(f, "{quote}")?;
    for c in string.chars() {
        match c {
            _ if c == quote => write!(f, "\\{c}")?,
            // The delimiter that is not in use needs no escape, just as `{:?}` leaves `'`
            // alone inside a string and `"` alone inside a character.
            '\'' | '"' => write!(f, "{c}")?,
            '\u{1}'..='\u{8}' | '\u{b}' | '\u{c}' | '\u{e}' | '\u{f}' => {
                write!(f, "\\u{{{:02x}}}", c as u32)?
            }
            _ => write!(f, "{}", c.escape_debug())?,
        }
    }
    write!(f, "{quote}")
}

impl Expr {
    /// Renders `self` as the operand of a postfix operator (`?`, `*`, `+`, `{n}`, ...).
    ///
    /// pest binds postfix operators tighter than the `&` and `!` prefixes, so a predicate
    /// operand has to be parenthesised: without this, `Opt(NegPred(e))` would print as `!e?`,
    /// which re-parses as `NegPred(Opt(e))` — a rule that always succeeds turned into one that
    /// always fails.
    fn as_postfix_operand(&self) -> String {
        match self {
            Expr::PosPred(_) | Expr::NegPred(_) => format!("({self})"),
            _ => format!("{self}"),
        }
    }
}

impl core::fmt::Display for Expr {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Expr::Str(s) => fmt_literal(f, s, '"'),
            Expr::Insens(s) => {
                write!(f, "^")?;
                fmt_literal(f, s, '"')
            }
            Expr::Range(start, end) => {
                let start = start.chars().next().expect("Empty range start.");
                let end = end.chars().next().expect("Empty range end.");
                write!(f, "(")?;
                fmt_literal(f, start.encode_utf8(&mut [0; 4]), '\'')?;
                write!(f, "..")?;
                fmt_literal(f, end.encode_utf8(&mut [0; 4]), '\'')?;
                write!(f, ")")
            }
            Expr::Ident(id) => write!(f, "{id}"),
            Expr::PeekSlice(start, end) => match end {
                Some(end) => write!(f, "PEEK[{start}..{end}]"),
                None => write!(f, "PEEK[{start}..]"),
            },
            Expr::PosPred(expr) => write!(f, "&{}", expr.as_ref()),
            Expr::NegPred(expr) => write!(f, "!{}", expr.as_ref()),
            Expr::Seq(lhs, rhs) => {
                let mut nodes = Vec::new();
                nodes.push(lhs);
                let mut current = rhs;
                while let Expr::Seq(lhs, rhs) = current.as_ref() {
                    nodes.push(lhs);
                    current = rhs;
                }
                nodes.push(current);
                let sequence = nodes
                    .iter()
                    .map(|node| format!("{node}"))
                    .collect::<Vec<_>>()
                    .join(" ~ ");
                write!(f, "({sequence})")
            }
            Expr::Choice(lhs, rhs) => {
                let mut nodes = Vec::new();
                nodes.push(lhs);
                let mut current = rhs;
                while let Expr::Choice(lhs, rhs) = current.as_ref() {
                    nodes.push(lhs);
                    current = rhs;
                }
                nodes.push(current);
                let sequence = nodes
                    .iter()
                    .map(|node| format!("{node}"))
                    .collect::<Vec<_>>()
                    .join(" | ");
                write!(f, "({sequence})")
            }
            Expr::Opt(expr) => write!(f, "{}?", expr.as_postfix_operand()),
            Expr::Rep(expr) => write!(f, "{}*", expr.as_postfix_operand()),
            Expr::RepOnce(expr) => write!(f, "{}+", expr.as_postfix_operand()),
            Expr::RepExact(expr, n) => write!(f, "{}{{{n}}}", expr.as_postfix_operand()),
            Expr::RepMin(expr, min) => write!(f, "{}{{{min},}}", expr.as_postfix_operand()),
            Expr::RepMax(expr, max) => write!(f, "{}{{,{max}}}", expr.as_postfix_operand()),
            Expr::RepMinMax(expr, min, max) => {
                write!(f, "{}{{{min}, {max}}}", expr.as_postfix_operand())
            }
            Expr::Skip(strings) => {
                write!(f, "(!(")?;
                for (i, s) in strings.iter().enumerate() {
                    if i > 0 {
                        write!(f, " | ")?;
                    }
                    fmt_literal(f, s, '"')?;
                }
                write!(f, ") ~ ANY)*")
            }
            Expr::Push(expr) => write!(f, "PUSH({expr})"),
            #[cfg(feature = "grammar-extras")]
            Expr::PushLiteral(s) => {
                write!(f, "PUSH_LITERAL(")?;
                fmt_literal(f, s, '"')?;
                write!(f, ")")
            }
            #[cfg(feature = "grammar-extras")]
            Expr::NodeTag(expr, tag) => {
                write!(f, "(#{tag} = {expr})")
            }
        }
    }
}

/// The top down iterator for an expression.
pub struct ExprTopDownIterator {
    current: Option<Expr>,
    next: Option<Expr>,
    right_branches: Vec<Expr>,
}

impl ExprTopDownIterator {
    /// Constructs a top-down iterator from the expression.
    pub fn new(expr: &Expr) -> Self {
        let mut iter = ExprTopDownIterator {
            current: None,
            next: None,
            right_branches: vec![],
        };
        iter.iterate_expr(expr.clone());
        iter
    }

    fn iterate_expr(&mut self, expr: Expr) {
        self.current = Some(expr.clone());
        match expr {
            Expr::Seq(lhs, rhs) => {
                self.right_branches.push(*rhs);
                self.next = Some(*lhs);
            }
            Expr::Choice(lhs, rhs) => {
                self.right_branches.push(*rhs);
                self.next = Some(*lhs);
            }
            Expr::PosPred(expr)
            | Expr::NegPred(expr)
            | Expr::Rep(expr)
            | Expr::RepOnce(expr)
            | Expr::RepExact(expr, _)
            | Expr::RepMin(expr, _)
            | Expr::RepMax(expr, _)
            | Expr::RepMinMax(expr, ..)
            | Expr::Opt(expr)
            | Expr::Push(expr) => {
                self.next = Some(*expr);
            }
            #[cfg(feature = "grammar-extras")]
            Expr::NodeTag(expr, _) => {
                self.next = Some(*expr);
            }
            _ => {
                self.next = None;
            }
        }
    }
}

impl Iterator for ExprTopDownIterator {
    type Item = Expr;

    fn next(&mut self) -> Option<Self::Item> {
        let result = self.current.take();

        if let Some(expr) = self.next.take() {
            self.iterate_expr(expr);
        } else if let Some(expr) = self.right_branches.pop() {
            self.iterate_expr(expr);
        }

        result
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn top_down_iterator() {
        let expr = Expr::Choice(
            Box::new(Expr::Str(String::from("a"))),
            Box::new(Expr::Str(String::from("b"))),
        );
        let mut top_down = expr.iter_top_down();
        assert_eq!(top_down.next(), Some(expr));
        assert_eq!(top_down.next(), Some(Expr::Str(String::from("a"))));
        assert_eq!(top_down.next(), Some(Expr::Str(String::from("b"))));
        assert_eq!(top_down.next(), None);
    }

    #[test]
    fn identity() {
        let expr = Expr::Choice(
            Box::new(Expr::Seq(
                Box::new(Expr::Ident("a".to_owned())),
                Box::new(Expr::Str("b".to_owned())),
            )),
            Box::new(Expr::PosPred(Box::new(Expr::NegPred(Box::new(Expr::Rep(
                Box::new(Expr::RepOnce(Box::new(Expr::Opt(Box::new(Expr::Choice(
                    Box::new(Expr::Insens("c".to_owned())),
                    Box::new(Expr::Push(Box::new(Expr::Range(
                        "'d'".to_owned(),
                        "'e'".to_owned(),
                    )))),
                )))))),
            )))))),
        );

        assert_eq!(
            expr.clone()
                .map_bottom_up(|expr| expr)
                .map_top_down(|expr| expr),
            expr,
        );
    }

    mod display {
        use super::super::*;

        #[test]
        fn string() {
            assert_eq!(Expr::Str("a".to_owned()).to_string(), r#""a""#);
        }

        #[test]
        fn insens() {
            assert_eq!(Expr::Insens("a".to_owned()).to_string(), r#"^"a""#);
        }

        #[test]
        fn range() {
            assert_eq!(
                Expr::Range("a".to_owned(), "z".to_owned()).to_string(),
                r#"('a'..'z')"#,
            );
        }

        #[test]
        fn ident() {
            assert_eq!(Expr::Ident("a".to_owned()).to_string(), "a");
        }

        #[test]
        fn peek_slice() {
            assert_eq!(Expr::PeekSlice(0, None).to_string(), "PEEK[0..]");
            assert_eq!(Expr::PeekSlice(0, Some(-1)).to_string(), "PEEK[0..-1]");
        }

        #[test]
        fn postfix_over_predicate_is_parenthesised() {
            // pest binds postfix operators tighter than `&` and `!`, so an unparenthesised
            // `!e?` would re-parse as `NegPred(Opt(e))` instead of `Opt(NegPred(e))`.
            let neg = || Box::new(Expr::NegPred(Box::new(Expr::Ident("e".to_owned()))));
            let pos = || Box::new(Expr::PosPred(Box::new(Expr::Ident("e".to_owned()))));

            assert_eq!(Expr::Opt(neg()).to_string(), "(!e)?");
            assert_eq!(Expr::Rep(neg()).to_string(), "(!e)*");
            assert_eq!(Expr::RepOnce(neg()).to_string(), "(!e)+");
            assert_eq!(Expr::RepExact(neg(), 2).to_string(), "(!e){2}");
            assert_eq!(Expr::RepMin(neg(), 2).to_string(), "(!e){2,}");
            assert_eq!(Expr::RepMax(neg(), 2).to_string(), "(!e){,2}");
            assert_eq!(Expr::RepMinMax(pos(), 2, 3).to_string(), "(&e){2, 3}");

            // Non-predicate operands keep their unparenthesised form.
            assert_eq!(
                Expr::Opt(Box::new(Expr::Ident("e".to_owned()))).to_string(),
                "e?"
            );
        }

        #[test]
        fn control_characters_use_a_two_digit_unicode_escape() {
            // pest's own rule is `unicode = @{ "u" ~ "{" ~ hex_digit{2, 6} ~ "}" }`, so the
            // single-digit form `{:?}` emits for U+0001..=U+000F is not valid pest.
            assert_eq!(Expr::Str("\u{1}".to_owned()).to_string(), r#""\u{01}""#);
            assert_eq!(Expr::Insens("\u{f}".to_owned()).to_string(), r#"^"\u{0f}""#);
            assert_eq!(
                Expr::Range("\u{1}".to_owned(), "\u{b}".to_owned()).to_string(),
                r#"('\u{01}'..'\u{0b}')"#,
            );
            // The escapes `{:?}` already gets right are unchanged.
            assert_eq!(
                Expr::Str("\0\t\n\r\\\"".to_owned()).to_string(),
                r#""\0\t\n\r\\\"""#
            );
            assert_eq!(Expr::Str("a'b".to_owned()).to_string(), r#""a'b""#);
            assert_eq!(
                Expr::Range("'".to_owned(), "\"".to_owned()).to_string(),
                r#"('\''..'"')"#,
            );
        }

        /// Every rule of every grammar below is printed and parsed back; the AST has to survive.
        #[test]
        fn display_round_trips_through_the_parser() {
            use crate::parser::{consume_rules, parse, Rule as PRule};

            let sources = [
                r#"r = { (!"a")? ~ "b" }"#,
                r#"r = { (&"a"){2, 3} }"#,
                r#"r = @{ (!"a"){2} }"#,
                r#"r = { "\u{0001}" ~ ^"\u{000b}" ~ '\u{0007}'..'\u{000f}' }"#,
                r#"r = { PUSH("a") ~ PEEK[0..-1] ~ POP }"#,
                r#"r = { ("a" ~ "b") | ("a" ~ "c") }"#,
            ];

            for source in sources {
                let rules = consume_rules(parse(PRule::grammar_rules, source).unwrap()).unwrap();
                let printed = format!("r = {{ {} }}", rules[0].expr);
                let reparsed = consume_rules(parse(PRule::grammar_rules, &printed).unwrap())
                    .unwrap_or_else(|e| panic!("{printed} did not parse: {e:?}"));
                assert_eq!(rules[0].expr, reparsed[0].expr, "printed as {printed}");
            }
        }

        #[test]
        fn pos_pred() {
            assert_eq!(
                Expr::PosPred(Box::new(Expr::Ident("e".to_owned()))).to_string(),
                "&e",
            );
        }

        #[test]
        fn neg_pred() {
            assert_eq!(
                Expr::NegPred(Box::new(Expr::Ident("e".to_owned()))).to_string(),
                "!e",
            );
        }

        #[test]
        fn seq() {
            assert_eq!(
                Expr::Seq(
                    Box::new(Expr::Ident("e1".to_owned())),
                    Box::new(Expr::Ident("e2".to_owned())),
                )
                .to_string(),
                "(e1 ~ e2)",
            );
            assert_eq!(
                Expr::Seq(
                    Box::new(Expr::Ident("e1".to_owned())),
                    Box::new(Expr::Seq(
                        Box::new(Expr::Ident("e2".to_owned())),
                        Box::new(Expr::Ident("e3".to_owned())),
                    )),
                )
                .to_string(),
                "(e1 ~ e2 ~ e3)",
            );
            assert_eq!(
                Expr::Seq(
                    Box::new(Expr::Ident("e1".to_owned())),
                    Box::new(Expr::Seq(
                        Box::new(Expr::Ident("e2".to_owned())),
                        Box::new(Expr::Seq(
                            Box::new(Expr::Ident("e3".to_owned())),
                            Box::new(Expr::Ident("e4".to_owned())),
                        )),
                    )),
                )
                .to_string(),
                "(e1 ~ e2 ~ e3 ~ e4)",
            );
            assert_eq!(
                Expr::Seq(
                    Box::new(Expr::Ident("e1".to_owned())),
                    Box::new(Expr::Choice(
                        Box::new(Expr::Ident("e2".to_owned())),
                        Box::new(Expr::Seq(
                            Box::new(Expr::Ident("e3".to_owned())),
                            Box::new(Expr::Ident("e4".to_owned())),
                        )),
                    )),
                )
                .to_string(),
                "(e1 ~ (e2 | (e3 ~ e4)))",
            );
            assert_eq!(
                Expr::Seq(
                    Box::new(Expr::Ident("e1".to_owned())),
                    Box::new(Expr::Seq(
                        Box::new(Expr::Ident("e2".to_owned())),
                        Box::new(Expr::Choice(
                            Box::new(Expr::Ident("e3".to_owned())),
                            Box::new(Expr::Ident("e4".to_owned())),
                        )),
                    )),
                )
                .to_string(),
                "(e1 ~ e2 ~ (e3 | e4))",
            );
        }

        #[test]
        fn choice() {
            assert_eq!(
                Expr::Choice(
                    Box::new(Expr::Ident("e1".to_owned())),
                    Box::new(Expr::Ident("e2".to_owned())),
                )
                .to_string(),
                "(e1 | e2)",
            );
            assert_eq!(
                Expr::Choice(
                    Box::new(Expr::Ident("e1".to_owned())),
                    Box::new(Expr::Choice(
                        Box::new(Expr::Ident("e2".to_owned())),
                        Box::new(Expr::Ident("e3".to_owned())),
                    )),
                )
                .to_string(),
                "(e1 | e2 | e3)",
            );
            assert_eq!(
                Expr::Choice(
                    Box::new(Expr::Ident("e1".to_owned())),
                    Box::new(Expr::Choice(
                        Box::new(Expr::Ident("e2".to_owned())),
                        Box::new(Expr::Choice(
                            Box::new(Expr::Ident("e3".to_owned())),
                            Box::new(Expr::Ident("e4".to_owned())),
                        )),
                    )),
                )
                .to_string(),
                "(e1 | e2 | e3 | e4)",
            );
            assert_eq!(
                Expr::Choice(
                    Box::new(Expr::Ident("e1".to_owned())),
                    Box::new(Expr::Seq(
                        Box::new(Expr::Ident("e2".to_owned())),
                        Box::new(Expr::Choice(
                            Box::new(Expr::Ident("e3".to_owned())),
                            Box::new(Expr::Ident("e4".to_owned())),
                        )),
                    )),
                )
                .to_string(),
                "(e1 | (e2 ~ (e3 | e4)))",
            );
        }

        #[test]
        fn opt() {
            assert_eq!(
                Expr::Opt(Box::new(Expr::Ident("e".to_owned()))).to_string(),
                "e?",
            );
        }

        #[test]
        fn rep() {
            assert_eq!(
                Expr::Rep(Box::new(Expr::Ident("e".to_owned()))).to_string(),
                "e*",
            );
        }

        #[test]
        fn rep_once() {
            assert_eq!(
                Expr::RepOnce(Box::new(Expr::Ident("e".to_owned()))).to_string(),
                "e+",
            );
        }

        #[test]
        fn rep_exact() {
            assert_eq!(
                Expr::RepExact(Box::new(Expr::Ident("e".to_owned())), 1).to_string(),
                "e{1}",
            );
        }

        #[test]
        fn rep_min() {
            assert_eq!(
                Expr::RepMin(Box::new(Expr::Ident("e".to_owned())), 1).to_string(),
                "e{1,}",
            );
        }

        #[test]
        fn rep_max() {
            assert_eq!(
                Expr::RepMax(Box::new(Expr::Ident("e".to_owned())), 1).to_string(),
                "e{,1}",
            );
        }

        #[test]
        fn rep_min_max() {
            assert_eq!(
                Expr::RepMinMax(Box::new(Expr::Ident("e".to_owned())), 1, 2).to_string(),
                "e{1, 2}",
            );
        }

        #[test]
        fn skip() {
            assert_eq!(
                Expr::Skip(
                    ["a", "bc"]
                        .into_iter()
                        .map(|s| s.to_owned())
                        .collect::<Vec<_>>(),
                )
                .to_string(),
                r#"(!("a" | "bc") ~ ANY)*"#,
            );
        }

        #[test]
        fn push() {
            assert_eq!(
                Expr::Push(Box::new(Expr::Ident("e".to_owned()))).to_string(),
                "PUSH(e)",
            );
        }

        #[test]
        #[cfg(feature = "grammar-extras")]
        fn push_literal() {
            assert_eq!(
                Expr::PushLiteral("one \" ' two".to_string()).to_string(),
                r#"PUSH_LITERAL("one \" ' two")"#
            )
        }

        #[test]
        #[cfg(feature = "grammar-extras")]
        fn node_tag() {
            assert_eq!(
                Expr::NodeTag(Box::new(Expr::Ident("expr".to_owned())), "label".to_owned())
                    .to_string(),
                "(#label = expr)",
            );
        }
    }
}
