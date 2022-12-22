// pest. The Elegant Parser
// Copyright (c) 2018 Dragoș Tiselice
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.
//! # pest meta
//!
//! This crate parses, validates, optimizes, and converts pest's own grammars to ASTs.

#![doc(
    html_logo_url = "https://raw.githubusercontent.com/pest-parser/pest/master/pest-logo.svg",
    html_favicon_url = "https://raw.githubusercontent.com/pest-parser/pest/master/pest-logo.svg"
)]
#![warn(missing_docs, rust_2018_idioms, unused_qualifications)]

#[cfg(test)]
#[macro_use]
extern crate pest;

use std::fmt::Display;

use pest::error::Error;

pub mod ast;
pub mod optimizer;
pub mod parser;
pub mod validator;

/// A helper that will unwrap the result or panic
/// with the nicely formatted error message.
pub fn unwrap_or_report<T, E>(result: Result<T, E>) -> T
where
    E: IntoIterator,
    E::Item: Display,
{
    result.unwrap_or_else(|e| {
        panic!(
            "{}{}",
            "grammar error\n\n".to_owned(),
            &e.into_iter()
                .map(|error| format!("{}", error))
                .collect::<Vec<_>>()
                .join("\n\n")
        )
    })
}

/// A tuple returned by the validation and processing of the parsed grammar.
/// The first element is the vector of used builtin rule names,
/// the second element is the vector of optimized rules.
type UsedBuiltinAndOptimized<'i> = (Vec<&'i str>, Vec<optimizer::OptimizedRule>);

/// Parses, validates, processes and optimizes the provided grammar.
pub fn parse_and_optimize(
    grammar: &str,
) -> Result<UsedBuiltinAndOptimized<'_>, Vec<Error<parser::Rule>>> {
    let pairs = match parser::parse(parser::Rule::grammar_rules, grammar) {
        Ok(pairs) => Ok(pairs),
        Err(error) => Err(vec![error]),
    }?;

    let defaults = validator::validate_pairs(pairs.clone())?;
    let ast = parser::consume_rules(pairs)?;

    Ok((defaults, optimizer::optimize(ast)))
}

#[doc(hidden)]
pub static UNICODE_PROPERTY_NAMES: &[&str] = &[
    /* BINARY */
    "ALPHABETIC",
    "BIDI_CONTROL",
    "CASE_IGNORABLE",
    "CASED",
    "CHANGES_WHEN_CASEFOLDED",
    "CHANGES_WHEN_CASEMAPPED",
    "CHANGES_WHEN_LOWERCASED",
    "CHANGES_WHEN_TITLECASED",
    "CHANGES_WHEN_UPPERCASED",
    "DASH",
    "DEFAULT_IGNORABLE_CODE_POINT",
    "DEPRECATED",
    "DIACRITIC",
    "EXTENDER",
    "GRAPHEME_BASE",
    "GRAPHEME_EXTEND",
    "GRAPHEME_LINK",
    "HEX_DIGIT",
    "HYPHEN",
    "IDS_BINARY_OPERATOR",
    "IDS_TRINARY_OPERATOR",
    "ID_CONTINUE",
    "ID_START",
    "IDEOGRAPHIC",
    "JOIN_CONTROL",
    "LOGICAL_ORDER_EXCEPTION",
    "LOWERCASE",
    "MATH",
    "NONCHARACTER_CODE_POINT",
    "OTHER_ALPHABETIC",
    "OTHER_DEFAULT_IGNORABLE_CODE_POINT",
    "OTHER_GRAPHEME_EXTEND",
    "OTHER_ID_CONTINUE",
    "OTHER_ID_START",
    "OTHER_LOWERCASE",
    "OTHER_MATH",
    "OTHER_UPPERCASE",
    "PATTERN_SYNTAX",
    "PATTERN_WHITE_SPACE",
    "PREPENDED_CONCATENATION_MARK",
    "QUOTATION_MARK",
    "RADICAL",
    "REGIONAL_INDICATOR",
    "SENTENCE_TERMINAL",
    "SOFT_DOTTED",
    "TERMINAL_PUNCTUATION",
    "UNIFIED_IDEOGRAPH",
    "UPPERCASE",
    "VARIATION_SELECTOR",
    "WHITE_SPACE",
    "XID_CONTINUE",
    "XID_START",
    /* CATEGORY */
    "CASED_LETTER",
    "CLOSE_PUNCTUATION",
    "CONNECTOR_PUNCTUATION",
    "CONTROL",
    "CURRENCY_SYMBOL",
    "DASH_PUNCTUATION",
    "DECIMAL_NUMBER",
    "ENCLOSING_MARK",
    "FINAL_PUNCTUATION",
    "FORMAT",
    "INITIAL_PUNCTUATION",
    "LETTER",
    "LETTER_NUMBER",
    "LINE_SEPARATOR",
    "LOWERCASE_LETTER",
    "MARK",
    "MATH_SYMBOL",
    "MODIFIER_LETTER",
    "MODIFIER_SYMBOL",
    "NONSPACING_MARK",
    "NUMBER",
    "OPEN_PUNCTUATION",
    "OTHER",
    "OTHER_LETTER",
    "OTHER_NUMBER",
    "OTHER_PUNCTUATION",
    "OTHER_SYMBOL",
    "PARAGRAPH_SEPARATOR",
    "PRIVATE_USE",
    "PUNCTUATION",
    "SEPARATOR",
    "SPACE_SEPARATOR",
    "SPACING_MARK",
    "SURROGATE",
    "SYMBOL",
    "TITLECASE_LETTER",
    "UNASSIGNED",
    "UPPERCASE_LETTER",
    /* Script */
    "CJK",
    "HAN",
    "HANGUL",
    "HIRAGANA",
    "KATAKANA",
];
