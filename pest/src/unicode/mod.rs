//! Character inclusion in binary or General_Category value Unicode sets.
//!
//! We rely on dead code elimination to remove the tables that aren't needed.

#![allow(bad_style)]

macro_rules! char_property_functions {
    {$(
        mod $module:ident;
        [$(
            ($func:ident, $trie:ident),
        )*];
    )*} => {$(
        #[allow(unused)]
        mod $module;
        $(pub fn $func(c: char) -> bool {
            self::$module::$trie.contains_char(c)
        })*
    )*};
}

char_property_functions! {
    mod binary;
    [
      (ASCII_Hex_Digit, ASCII_HEX_DIGIT), (Alphabetic, ALPHABETIC),
      (Bidi_Control, BIDI_CONTROL), (Case_Ignorable, CASE_IGNORABLE),
      (Cased, CASED), (Changes_When_Casefolded, CHANGES_WHEN_CASEFOLDED),
      (Changes_When_Casemapped, CHANGES_WHEN_CASEMAPPED),
      (Changes_When_Lowercased, CHANGES_WHEN_LOWERCASED),
      (Changes_When_Titlecased, CHANGES_WHEN_TITLECASED),
      (Changes_When_Uppercased, CHANGES_WHEN_UPPERCASED), (Dash, DASH),
      (Default_Ignorable_Code_Point, DEFAULT_IGNORABLE_CODE_POINT),
      (Deprecated, DEPRECATED), (Diacritic, DIACRITIC),
      (Extender, EXTENDER), (Grapheme_Base, GRAPHEME_BASE),
      (Grapheme_Extend, GRAPHEME_EXTEND), (Grapheme_Link, GRAPHEME_LINK),
      (Hex_Digit, HEX_DIGIT), (Hyphen, HYPHEN),
      (IDS_Binary_Operator, IDS_BINARY_OPERATOR),
      (IDS_Trinary_Operator, IDS_TRINARY_OPERATOR),
      (ID_Continue, ID_CONTINUE), (ID_Start, ID_START),
      (Ideographic, IDEOGRAPHIC), (Join_Control, JOIN_CONTROL),
      (Logical_Order_Exception, LOGICAL_ORDER_EXCEPTION),
      (Lowercase, LOWERCASE), (Math, MATH),
      (Noncharacter_Code_Point, NONCHARACTER_CODE_POINT),
      (Other_Alphabetic, OTHER_ALPHABETIC),
      (Other_Default_Ignorable_Code_Point, OTHER_DEFAULT_IGNORABLE_CODE_POINT),
      (Other_Grapheme_Extend, OTHER_GRAPHEME_EXTEND),
      (Other_ID_Continue, OTHER_ID_CONTINUE),
      (Other_ID_Start, OTHER_ID_START), (Other_Lowercase, OTHER_LOWERCASE),
      (Other_Math, OTHER_MATH), (Other_Uppercase, OTHER_UPPERCASE),
      (Pattern_Syntax, PATTERN_SYNTAX),
      (Pattern_White_Space, PATTERN_WHITE_SPACE),
      (Prepended_Concatenation_Mark, PREPENDED_CONCATENATION_MARK),
      (Quotation_Mark, QUOTATION_MARK), (Radical, RADICAL),
      (Regional_Indicator, REGIONAL_INDICATOR),
      (Sentence_Terminal, SENTENCE_TERMINAL), (Soft_Dotted, SOFT_DOTTED),
      (Terminal_Punctuation, TERMINAL_PUNCTUATION),
      (Unified_Ideograph, UNIFIED_IDEOGRAPH), (Uppercase, UPPERCASE),
      (Variation_Selector, VARIATION_SELECTOR), (White_Space, WHITE_SPACE),
      (XID_Continue, XID_CONTINUE), (XID_Start, XID_START),
    ];
    
    mod category;
    [
      (Cased_Letter, CASED_LETTER), (Close_Punctuation, CLOSE_PUNCTUATION),
      (Connector_Punctuation, CONNECTOR_PUNCTUATION), (Control, CONTROL),
      (Currency_Symbol, CURRENCY_SYMBOL),
      (Dash_Punctuation, DASH_PUNCTUATION), (Decimal_Number, DECIMAL_NUMBER),
      (Enclosing_Mark, ENCLOSING_MARK),
      (Final_Punctuation, FINAL_PUNCTUATION), (Format, FORMAT),
      (Initial_Punctuation, INITIAL_PUNCTUATION), (Letter, LETTER),
      (Letter_Number, LETTER_NUMBER), (Line_Separator, LINE_SEPARATOR),
      (Lowercase_Letter, LOWERCASE_LETTER), (Mark, MARK),
      (Math_Symbol, MATH_SYMBOL), (Modifier_Letter, MODIFIER_LETTER),
      (Modifier_Symbol, MODIFIER_SYMBOL), (Nonspacing_Mark, NONSPACING_MARK),
      (Number, NUMBER), (Open_Punctuation, OPEN_PUNCTUATION),
      (Other, OTHER), (Other_Letter, OTHER_LETTER),
      (Other_Number, OTHER_NUMBER), (Other_Punctuation, OTHER_PUNCTUATION),
      (Other_Symbol, OTHER_SYMBOL),
      (Paragraph_Separator, PARAGRAPH_SEPARATOR), (Private_Use, PRIVATE_USE),
      (Punctuation, PUNCTUATION), (Separator, SEPARATOR),
      (Space_Separator, SPACE_SEPARATOR), (Spacing_Mark, SPACING_MARK),
      (Surrogate, SURROGATE), (Symbol, SYMBOL),
      (Titlecase_Letter, TITLECASE_LETTER), (Unassigned, UNASSIGNED),
      (Uppercase_Letter, UPPERCASE_LETTER),
    ];
}
