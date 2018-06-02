// pest. The Elegant Parser
// Copyright (c) 2018 Dragoș Tiselice
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

extern crate maplit;
#[cfg(test)]
#[macro_use]
extern crate pest;
#[cfg(not(test))]
extern crate pest;
#[macro_use]
extern crate pest_derive;

use std::fmt::Display;

pub mod parser;
pub mod ast;
pub mod optimizer;
pub mod validator;

pub fn unwrap_or_report<T, E>(result: Result<T, E>) -> T
where
    E: IntoIterator,
    E::Item: Display
{
    result.unwrap_or_else(|e| {
        panic!(
            "grammar error\n\n".to_owned()
                + &e.into_iter()
                    .map(|error| format!("{}", error))
                    .collect::<Vec<_>>()
                    .join("\n\n")
        )
    })
}

#[doc(hidden)]
pub static UNICODE_PROPERTY_NAMES: &[&str] = &[
    /* binary */ "ASCII_Hex_Digit", "Alphabetic", "Bidi_Control", "Case_Ignorable", "Cased",
    "Changes_When_Casefolded", "Changes_When_Casemapped", "Changes_When_Lowercased",
    "Changes_When_Titlecased", "Changes_When_Uppercased", "Dash", "Default_Ignorable_Code_Point",
    "Deprecated", "Diacritic", "Extender", "Grapheme_Base", "Grapheme_Extend", "Grapheme_Link",
    "Hex_Digit", "Hyphen", "IDS_Binary_Operator", "IDS_Trinary_Operator", "ID_Continue", "ID_Start",
    "Ideographic", "Join_Control", "Logical_Order_Exception", "Lowercase", "Math",
    "Noncharacter_Code_Point", "Other_Alphabetic", "Other_Default_Ignorable_Code_Point",
    "Other_Grapheme_Extend", "Other_ID_Continue", "Other_ID_Start", "Other_Lowercase", "Other_Math",
    "Other_Uppercase", "Pattern_Syntax", "Pattern_White_Space", "Prepended_Concatenation_Mark",
    "Quotation_Mark", "Radical", "Regional_Indicator", "Sentence_Terminal", "Soft_Dotted",
    "Terminal_Punctuation", "Unified_Ideograph", "Uppercase", "Variation_Selector", "White_Space",
    "XID_Continue", "XID_Start", /* category */ "Cased_Letter", "Close_Punctuation",
    "Connector_Punctuation", "Control", "Currency_Symbol", "Dash_Punctuation", "Decimal_Number",
    "Enclosing_Mark", "Final_Punctuation", "Format", "Initial_Punctuation", "Letter",
    "Letter_Number", "Line_Separator", "Lowercase_Letter", "Mark", "Math_Symbol", "Modifier_Letter",
    "Modifier_Symbol", "Nonspacing_Mark", "Number", "Open_Punctuation", "Other", "Other_Letter",
    "Other_Number", "Other_Punctuation", "Other_Symbol", "Paragraph_Separator", "Private_Use",
    "Punctuation", "Separator", "Space_Separator", "Spacing_Mark", "Surrogate", "Symbol",
    "Titlecase_Letter", "Unassigned", "Uppercase_Letter",
];
