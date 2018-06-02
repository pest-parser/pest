//! Character inclusion in binary or General_Category value Unicode sets.
//!
//! We rely on dead code elimination to remove the tables that aren't needed.

#![allow(bad_style)]

#[allow(unused)]
mod binary;
pub fn ASCII_Hex_Digit(c: char) -> bool {
    self::binary::ASCII_HEX_DIGIT.contains_char(c)
}
pub fn Alphabetic(c: char) -> bool {
    self::binary::ALPHABETIC.contains_char(c)
}
pub fn Bidi_Control(c: char) -> bool {
    self::binary::BIDI_CONTROL.contains_char(c)
}
pub fn Case_Ignorable(c: char) -> bool {
    self::binary::CASE_IGNORABLE.contains_char(c)
}
pub fn Cased(c: char) -> bool {
    self::binary::CASED.contains_char(c)
}
pub fn Changes_When_Casefolded(c: char) -> bool {
    self::binary::CHANGES_WHEN_CASEFOLDED.contains_char(c)
}
pub fn Changes_When_Casemapped(c: char) -> bool {
    self::binary::CHANGES_WHEN_CASEMAPPED.contains_char(c)
}
pub fn Changes_When_Lowercased(c: char) -> bool {
    self::binary::CHANGES_WHEN_LOWERCASED.contains_char(c)
}
pub fn Changes_When_Titlecased(c: char) -> bool {
    self::binary::CHANGES_WHEN_TITLECASED.contains_char(c)
}
pub fn Changes_When_Uppercased(c: char) -> bool {
    self::binary::CHANGES_WHEN_UPPERCASED.contains_char(c)
}
pub fn Dash(c: char) -> bool {
    self::binary::DASH.contains_char(c)
}
pub fn Default_Ignorable_Code_Point(c: char) -> bool {
    self::binary::DEFAULT_IGNORABLE_CODE_POINT.contains_char(c)
}
pub fn Deprecated(c: char) -> bool {
    self::binary::DEPRECATED.contains_char(c)
}
pub fn Diacritic(c: char) -> bool {
    self::binary::DIACRITIC.contains_char(c)
}
pub fn Extender(c: char) -> bool {
    self::binary::EXTENDER.contains_char(c)
}
pub fn Grapheme_Base(c: char) -> bool {
    self::binary::GRAPHEME_BASE.contains_char(c)
}
pub fn Grapheme_Extend(c: char) -> bool {
    self::binary::GRAPHEME_EXTEND.contains_char(c)
}
pub fn Grapheme_Link(c: char) -> bool {
    self::binary::GRAPHEME_LINK.contains_char(c)
}
pub fn Hex_Digit(c: char) -> bool {
    self::binary::HEX_DIGIT.contains_char(c)
}
pub fn Hyphen(c: char) -> bool {
    self::binary::HYPHEN.contains_char(c)
}
pub fn IDS_Binary_Operator(c: char) -> bool {
    self::binary::IDS_BINARY_OPERATOR.contains_char(c)
}
pub fn IDS_Trinary_Operator(c: char) -> bool {
    self::binary::IDS_TRINARY_OPERATOR.contains_char(c)
}
pub fn ID_Continue(c: char) -> bool {
    self::binary::ID_CONTINUE.contains_char(c)
}
pub fn ID_Start(c: char) -> bool {
    self::binary::ID_START.contains_char(c)
}
pub fn Ideographic(c: char) -> bool {
    self::binary::IDEOGRAPHIC.contains_char(c)
}
pub fn Join_Control(c: char) -> bool {
    self::binary::JOIN_CONTROL.contains_char(c)
}
pub fn Logical_Order_Exception(c: char) -> bool {
    self::binary::LOGICAL_ORDER_EXCEPTION.contains_char(c)
}
pub fn Lowercase(c: char) -> bool {
    self::binary::LOWERCASE.contains_char(c)
}
pub fn Math(c: char) -> bool {
    self::binary::MATH.contains_char(c)
}
pub fn Noncharacter_Code_Point(c: char) -> bool {
    self::binary::NONCHARACTER_CODE_POINT.contains_char(c)
}
pub fn Other_Alphabetic(c: char) -> bool {
    self::binary::OTHER_ALPHABETIC.contains_char(c)
}
pub fn Other_Default_Ignorable_Code_Point(c: char) -> bool {
    self::binary::OTHER_DEFAULT_IGNORABLE_CODE_POINT.contains_char(c)
}
pub fn Other_Grapheme_Extend(c: char) -> bool {
    self::binary::OTHER_GRAPHEME_EXTEND.contains_char(c)
}
pub fn Other_ID_Continue(c: char) -> bool {
    self::binary::OTHER_ID_CONTINUE.contains_char(c)
}
pub fn Other_ID_Start(c: char) -> bool {
    self::binary::OTHER_ID_START.contains_char(c)
}
pub fn Other_Lowercase(c: char) -> bool {
    self::binary::OTHER_LOWERCASE.contains_char(c)
}
pub fn Other_Math(c: char) -> bool {
    self::binary::OTHER_MATH.contains_char(c)
}
pub fn Other_Uppercase(c: char) -> bool {
    self::binary::OTHER_UPPERCASE.contains_char(c)
}
pub fn Pattern_Syntax(c: char) -> bool {
    self::binary::PATTERN_SYNTAX.contains_char(c)
}
pub fn Pattern_White_Space(c: char) -> bool {
    self::binary::PATTERN_WHITE_SPACE.contains_char(c)
}
pub fn Prepended_Concatenation_Mark(c: char) -> bool {
    self::binary::PREPENDED_CONCATENATION_MARK.contains_char(c)
}
pub fn Quotation_Mark(c: char) -> bool {
    self::binary::QUOTATION_MARK.contains_char(c)
}
pub fn Radical(c: char) -> bool {
    self::binary::RADICAL.contains_char(c)
}
pub fn Regional_Indicator(c: char) -> bool {
    self::binary::REGIONAL_INDICATOR.contains_char(c)
}
pub fn Sentence_Terminal(c: char) -> bool {
    self::binary::SENTENCE_TERMINAL.contains_char(c)
}
pub fn Soft_Dotted(c: char) -> bool {
    self::binary::SOFT_DOTTED.contains_char(c)
}
pub fn Terminal_Punctuation(c: char) -> bool {
    self::binary::TERMINAL_PUNCTUATION.contains_char(c)
}
pub fn Unified_Ideograph(c: char) -> bool {
    self::binary::UNIFIED_IDEOGRAPH.contains_char(c)
}
pub fn Uppercase(c: char) -> bool {
    self::binary::UPPERCASE.contains_char(c)
}
pub fn Variation_Selector(c: char) -> bool {
    self::binary::VARIATION_SELECTOR.contains_char(c)
}
pub fn White_Space(c: char) -> bool {
    self::binary::WHITE_SPACE.contains_char(c)
}
pub fn XID_Continue(c: char) -> bool {
    self::binary::XID_CONTINUE.contains_char(c)
}
pub fn XID_Start(c: char) -> bool {
    self::binary::XID_START.contains_char(c)
}

#[allow(unused)]
mod category;
pub fn Cased_Letter(c: char) -> bool {
    self::category::CASED_LETTER.contains_char(c)
}
pub fn Close_Punctuation(c: char) -> bool {
    self::category::CLOSE_PUNCTUATION.contains_char(c)
}
pub fn Connector_Punctuation(c: char) -> bool {
    self::category::CONNECTOR_PUNCTUATION.contains_char(c)
}
pub fn Control(c: char) -> bool {
    self::category::CONTROL.contains_char(c)
}
pub fn Currency_Symbol(c: char) -> bool {
    self::category::CURRENCY_SYMBOL.contains_char(c)
}
pub fn Dash_Punctuation(c: char) -> bool {
    self::category::DASH_PUNCTUATION.contains_char(c)
}
pub fn Decimal_Number(c: char) -> bool {
    self::category::DECIMAL_NUMBER.contains_char(c)
}
pub fn Enclosing_Mark(c: char) -> bool {
    self::category::ENCLOSING_MARK.contains_char(c)
}
pub fn Final_Punctuation(c: char) -> bool {
    self::category::FINAL_PUNCTUATION.contains_char(c)
}
pub fn Format(c: char) -> bool {
    self::category::FORMAT.contains_char(c)
}
pub fn Initial_Punctuation(c: char) -> bool {
    self::category::INITIAL_PUNCTUATION.contains_char(c)
}
pub fn Letter(c: char) -> bool {
    self::category::LETTER.contains_char(c)
}
pub fn Letter_Number(c: char) -> bool {
    self::category::LETTER_NUMBER.contains_char(c)
}
pub fn Line_Separator(c: char) -> bool {
    self::category::LINE_SEPARATOR.contains_char(c)
}
pub fn Lowercase_Letter(c: char) -> bool {
    self::category::LOWERCASE_LETTER.contains_char(c)
}
pub fn Mark(c: char) -> bool {
    self::category::MARK.contains_char(c)
}
pub fn Math_Symbol(c: char) -> bool {
    self::category::MATH_SYMBOL.contains_char(c)
}
pub fn Modifier_Letter(c: char) -> bool {
    self::category::MODIFIER_LETTER.contains_char(c)
}
pub fn Modifier_Symbol(c: char) -> bool {
    self::category::MODIFIER_SYMBOL.contains_char(c)
}
pub fn Nonspacing_Mark(c: char) -> bool {
    self::category::NONSPACING_MARK.contains_char(c)
}
pub fn Number(c: char) -> bool {
    self::category::NUMBER.contains_char(c)
}
pub fn Open_Punctuation(c: char) -> bool {
    self::category::OPEN_PUNCTUATION.contains_char(c)
}
pub fn Other(c: char) -> bool {
    self::category::OTHER.contains_char(c)
}
pub fn Other_Letter(c: char) -> bool {
    self::category::OTHER_LETTER.contains_char(c)
}
pub fn Other_Number(c: char) -> bool {
    self::category::OTHER_NUMBER.contains_char(c)
}
pub fn Other_Punctuation(c: char) -> bool {
    self::category::OTHER_PUNCTUATION.contains_char(c)
}
pub fn Other_Symbol(c: char) -> bool {
    self::category::OTHER_SYMBOL.contains_char(c)
}
pub fn Paragraph_Separator(c: char) -> bool {
    self::category::PARAGRAPH_SEPARATOR.contains_char(c)
}
pub fn Private_Use(c: char) -> bool {
    self::category::PRIVATE_USE.contains_char(c)
}
pub fn Punctuation(c: char) -> bool {
    self::category::PUNCTUATION.contains_char(c)
}
pub fn Separator(c: char) -> bool {
    self::category::SEPARATOR.contains_char(c)
}
pub fn Space_Separator(c: char) -> bool {
    self::category::SPACE_SEPARATOR.contains_char(c)
}
pub fn Spacing_Mark(c: char) -> bool {
    self::category::SPACING_MARK.contains_char(c)
}
pub fn Surrogate(c: char) -> bool {
    self::category::SURROGATE.contains_char(c)
}
pub fn Symbol(c: char) -> bool {
    self::category::SYMBOL.contains_char(c)
}
pub fn Titlecase_Letter(c: char) -> bool {
    self::category::TITLECASE_LETTER.contains_char(c)
}
pub fn Unassigned(c: char) -> bool {
    self::category::UNASSIGNED.contains_char(c)
}
pub fn Uppercase_Letter(c: char) -> bool {
    self::category::UPPERCASE_LETTER.contains_char(c)
}
