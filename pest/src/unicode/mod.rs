//! Character inclusion in binary or General_Category value Unicode sets.
//!
//! We rely on dead code elimination to remove the tables that aren't needed.

#![allow(bad_style)]
#![allow(clippy::all)]

macro_rules! char_property_functions {
    {$(
        mod $module:ident;
        [$(
            $prop:ident,
        )*];
    )*} => {$(
        #[allow(unused)]
        mod $module;
        $(pub fn $prop(c: char) -> bool {
            self::$module::$prop.contains_char(c)
        })*
    )*};
}

char_property_functions! {
    mod binary;
    [
        // ASCII_HEX_DIGIT, // let this one be stripped out -- the full trie is wasteful for ASCII
        ALPHABETIC, BIDI_CONTROL, CASE_IGNORABLE, CASED, CHANGES_WHEN_CASEFOLDED,
        CHANGES_WHEN_CASEMAPPED, CHANGES_WHEN_LOWERCASED, CHANGES_WHEN_TITLECASED,
        CHANGES_WHEN_UPPERCASED, DASH, DEFAULT_IGNORABLE_CODE_POINT, DEPRECATED, DIACRITIC,
        EXTENDER, GRAPHEME_BASE, GRAPHEME_EXTEND, GRAPHEME_LINK, HEX_DIGIT, HYPHEN,
        IDS_BINARY_OPERATOR, IDS_TRINARY_OPERATOR, ID_CONTINUE, ID_START, IDEOGRAPHIC, JOIN_CONTROL,
        LOGICAL_ORDER_EXCEPTION, LOWERCASE, MATH, NONCHARACTER_CODE_POINT, OTHER_ALPHABETIC,
        OTHER_DEFAULT_IGNORABLE_CODE_POINT, OTHER_GRAPHEME_EXTEND, OTHER_ID_CONTINUE,
        OTHER_ID_START, OTHER_LOWERCASE, OTHER_MATH, OTHER_UPPERCASE, PATTERN_SYNTAX,
        PATTERN_WHITE_SPACE, PREPENDED_CONCATENATION_MARK, QUOTATION_MARK, RADICAL,
        REGIONAL_INDICATOR, SENTENCE_TERMINAL, SOFT_DOTTED, TERMINAL_PUNCTUATION, UNIFIED_IDEOGRAPH,
        UPPERCASE, VARIATION_SELECTOR, WHITE_SPACE, XID_CONTINUE, XID_START,
    ];

    mod category;
    [
        CASED_LETTER, CLOSE_PUNCTUATION, CONNECTOR_PUNCTUATION, CONTROL, CURRENCY_SYMBOL,
        DASH_PUNCTUATION, DECIMAL_NUMBER, ENCLOSING_MARK, FINAL_PUNCTUATION, FORMAT,
        INITIAL_PUNCTUATION, LETTER, LETTER_NUMBER, LINE_SEPARATOR, LOWERCASE_LETTER, MARK,
        MATH_SYMBOL, MODIFIER_LETTER, MODIFIER_SYMBOL, NONSPACING_MARK, NUMBER, OPEN_PUNCTUATION,
        OTHER, OTHER_LETTER, OTHER_NUMBER, OTHER_PUNCTUATION, OTHER_SYMBOL, PARAGRAPH_SEPARATOR,
        PRIVATE_USE, PUNCTUATION, SEPARATOR, SPACE_SEPARATOR, SPACING_MARK, SURROGATE, SYMBOL,
        TITLECASE_LETTER, UNASSIGNED, UPPERCASE_LETTER,
    ];
}

pub fn by_name(name: &str) -> Option<Box<dyn Fn(char) -> bool>> {
    for property in binary::BY_NAME {
        if name == property.0.to_uppercase() {
            return Some(Box::new(move |c| property.1.contains_char(c)));
        }
    }

    for property in category::BY_NAME {
        if name == property.0.to_uppercase() {
            return Some(Box::new(move |c| property.1.contains_char(c)));
        }
    }

    None
}
