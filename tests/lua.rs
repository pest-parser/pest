// pest. Elegant, efficient grammars
// Copyright (C) 2016  Dragoș Tiselice
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Semi-compliant (UTF-8 only) Lua 5.2 grammar inspired from:
//
//     https://github.com/antlr/grammars-v4/blob/master/lua/Lua.g4

#![recursion_limit = "400"]

#[macro_use]
extern crate pest;

use pest::prelude::*;

impl_rdp! {
    grammar! {
        chunk = { block ~ eoi }
        block = { stat* ~ retstat? }

        stat = {
            [";"] |
            varlist ~ ["="] ~ explist |
            functioncall |
            label |
            ["break"] |
            ["goto"] ~ name |
            ["do"] ~ block ~ ["end"] |
            ["while"] ~ exp ~ ["do"] ~ block ~ ["end"] |
            ["repeat"] ~ block ~ ["until"] ~ exp |
            ["if"] ~ exp ~ ["then"] ~ block ~ (["elseif"] ~ exp ~ ["then"] ~ block)* ~
                (["else"] ~ block)? ~ ["end"] |
            ["for"] ~ name ~ ["="] ~ exp ~ [","] ~ exp ~ ([","] ~ exp)? ~ ["do"] ~ block ~
                ["end"] |
            ["for"] ~ namelist ~ ["in"] ~ explist ~ ["do"] ~ block ~ ["end"] |
            ["function"] ~ funcname ~ funcbody |
            ["local"] ~ ["function"] ~ name ~ funcbody |
            ["local"] ~ namelist ~ (["="] ~ explist)?
        }

        retstat  = { ["return"] ~ explist? ~ [";"]? }
        label    = { ["::"] ~ name ~ ["::"] }
        funcname = { name ~ (["."] ~ name)* ~ ([":"] ~ name)? }
        varlist  = { var ~ ([","] ~ var)* }
        namelist = { name ~ ([","] ~ name)* }
        explist  = { !(["end"]) ~ exp ~ ([","] ~ exp)* }

        _power = _{
            {
                ["nil"] | ["false"] | ["true"] | number | string |
                ["..."] | functiondef | prefixexp | tableconstructor
            }
            power = {< op_power }
        }

        _unary = _{ unary | _power }
        unary  = { op_unary ~ _unary }

        exp = {
            { _unary }
            or          = {  op_or }
            and         = {  op_and }
            comparison  = {  op_comparison }
            strcat      = {< op_strcat }
            add_sub     = {  op_add_sub }
            mul_div_mod = {  op_mul_div_mod }
        }

        var              =  { (name | ["("] ~ exp ~ [")"] ~ varsuffix) ~ varsuffix* }
        prefixexp        =  { varorexp ~ nameandargs* }
        functioncall     =  { varorexp ~ nameandargs+ }
        varorexp         =  { var | ["("] ~ exp ~ [")"] }
        nameandargs      =  { ([":"] ~ name)? ~ args }
        varsuffix        =  { nameandargs* ~ (["["] ~ exp ~ ["]"] | ["."] ~ name) }
        args             =  { ["("] ~ explist? ~ [")"] | tableconstructor | string }
        functiondef      =  { ["function"] ~ funcbody }
        funcbody         =  { ["("] ~ parlist? ~ [")"] ~ block ~ ["end"] }
        parlist          =  { namelist ~ ([","] ~ ["..."])? | ["..."] }
        tableconstructor =  { ["{"] ~ fieldlist? ~ ["}"] }
        fieldlist        =  { field ~ (fieldsep ~ field)* ~ fieldsep* }
        field            =  { ["["] ~ exp ~ ["]"] ~ ["="] ~ exp | name ~ ["="] ~ exp | exp }
        fieldsep         = _{ [","] | [";"] }

        op_or          = { ["or"] }
        op_and         = { ["and"] }
        op_comparison  = { ["<="] | [">="] | ["<"] | [">"] | ["~="] | ["=="] }
        op_strcat      = { [".."] }
        op_add_sub     = { ["+"] | ["-"] }
        op_mul_div_mod = { ["*"] | ["/"] | ["%"] }
        op_unary       = { ["not"] | ["#"] | ["-"] }
        op_power       = { ["^"] }

        name = @{
            (['a'..'z'] | ['A'..'Z'] | ["_"]) ~ (['a'..'z'] | ['A'..'Z'] | ["_"] | ['0'..'9'])*
        }

        string       = @{ normalstring | charstring | longstring }
        normalstring =  { ["\""] ~ (escape_sequence | !(["\""] | ["\\"]) ~ any)* ~ ["\""] }
        charstring   =  { ["'"] ~ (escape_sequence | !(["'"] | ["\\"]) ~ any)* ~ ["'"] }
        longstring   =  { ["["] ~ nested_str ~ ["]"] }
        nested_str   = _{
            (["="] ~ ["["] ~ (!(["]"] ~ ["="]) ~ any)* ~ ["]"] ~ ["="]) |
            (["="] ~ nested_str ~ ["="])
        }

        number        = @{ hex_float | hex | float | int }
        int           =  { digits }
        hex           =  { ["0"] ~ (["x"] | ["X"]) ~ hex_digits }
        float         =  {
            digits ~ ["."] ~ digits? ~ exponent? |
            ["."] ~ digits ~ exponent? |
            digits ~ exponent
        }
        hex_float     =  { ["0"] ~ (["x"] | ["X"]) ~ (
            hex_digits ~ ["."] ~ hex_digits? ~ hex_exponent? |
            ["."] ~ hex_digits ~ hex_exponent? |
            hex_digits ~ hex_exponent
        ) }
        exponent     =  { (["e"] | ["E"]) ~ (["+"] | ["-"])? ~ digits }
        hex_exponent =  { (["p"] | ["P"]) ~ (["+"] | ["-"])? ~ digits }
        digits       =  { digit+ }
        hex_digits   =  { hex_digit+ }

        escape_sequence =  {
            ["\\"] ~ (["a"] | ["b"] | ["f"] | ["n"] | ["r"] | ["t"] | ["v"] | ["z"] | ["\""] |
                      ["'"] | ["\\"]) |
            ["\\"] ~ ["\r"]? ~ ["\n"] |
            decimal_escape |
            hex_escape
        }
        decimal_escape  =  {
            ["\\"] ~ ['0'..'2'] ~ digit ~ digit |
            ["\\"] ~ digit ~ digit |
            ["\\"] ~ digit
        }
        hex_escape     =  { ["\\"] ~ ["x"] ~ hex_digit ~ hex_digit }
        digit          = _{ ['0'..'9'] }
        hex_digit      = _{ ['0'..'9'] | ['a'..'f'] | ['A'..'F'] }

        comment = _{
            ["--"] ~ (
                ["["] ~ nested_str ~ ["]"] |
                (!(["\r"] | ["\n"]) ~ any)* ~ (["\n"] | ["\r\n"] | ["\r"] | eoi)
            ) |
            ["#"] ~ ["!"]? ~ (!(["\r"] | ["\n"]) ~ any)* ~ (["\n"] | ["\r\n"] | ["\r"] | eoi)
        }

        whitespace = _{ [" "] | ["\t"] | ["\u{000C}"] | ["\r"] | ["\n"] }
    }
}

#[test]
fn _power_number() {
    let mut parser = Rdp::new(StringInput::new("123"));

    assert!(parser._power());
    assert!(parser.end());

    let queue = vec![
        Token::new(Rule::number, 0, 3),
        Token::new(Rule::int, 0, 3),
        Token::new(Rule::digits, 0, 3)
    ];

    assert_eq!(parser.queue(), &queue);
}

#[test]
fn _power_power() {
    let mut parser = Rdp::new(StringInput::new("1 ^ 3"));

    assert!(parser._power());
    assert!(parser.end());

    let queue = vec![
        Token::new(Rule::power, 0, 5),
        Token::new(Rule::number, 0, 1),
        Token::new(Rule::int, 0, 1),
        Token::new(Rule::digits, 0, 1),
        Token::new(Rule::op_power, 2, 3),
        Token::new(Rule::number, 4, 5),
        Token::new(Rule::int, 4, 5),
        Token::new(Rule::digits, 4, 5)
    ];

    assert_eq!(parser.queue(), &queue);
}

#[test]
fn _unary_power() {
    let mut parser = Rdp::new(StringInput::new("1 ^ 3"));

    assert!(parser._unary());
    assert!(parser.end());

    let queue = vec![
        Token::new(Rule::power, 0, 5),
        Token::new(Rule::number, 0, 1),
        Token::new(Rule::int, 0, 1),
        Token::new(Rule::digits, 0, 1),
        Token::new(Rule::op_power, 2, 3),
        Token::new(Rule::number, 4, 5),
        Token::new(Rule::int, 4, 5),
        Token::new(Rule::digits, 4, 5)
    ];

    assert_eq!(parser.queue(), &queue);
}

#[test]
fn _unary_unary() {
    let mut parser = Rdp::new(StringInput::new("not 1"));

    assert!(parser._unary());
    assert!(parser.end());

    let queue = vec![
        Token::new(Rule::unary, 0, 5),
        Token::new(Rule::op_unary, 0, 3),
        Token::new(Rule::number, 4, 5),
        Token::new(Rule::int, 4, 5),
        Token::new(Rule::digits, 4, 5)
    ];

    assert_eq!(parser.queue(), &queue);
}

#[test]
fn _unary_unary_unary() {
    let mut parser = Rdp::new(StringInput::new("not not 1"));

    assert!(parser._unary());
    assert!(parser.end());

    let queue = vec![
        Token::new(Rule::unary, 0, 9),
        Token::new(Rule::op_unary, 0, 3),
        Token::new(Rule::unary, 4, 9),
        Token::new(Rule::op_unary, 4, 7),
        Token::new(Rule::number, 8, 9),
        Token::new(Rule::int, 8, 9),
        Token::new(Rule::digits, 8, 9)
    ];

    assert_eq!(parser.queue(), &queue);
}

#[test]
fn exp_number() {
    let mut parser = Rdp::new(StringInput::new("123"));

    assert!(parser.exp());
    assert!(parser.end());

    let queue = vec![
        Token::new(Rule::exp, 0, 3),
        Token::new(Rule::number, 0, 3),
        Token::new(Rule::int, 0, 3),
        Token::new(Rule::digits, 0, 3)
    ];

    assert_eq!(parser.queue(), &queue);
}

#[test]
fn exp_power() {
    let mut parser = Rdp::new(StringInput::new("1 ^ 3"));

    assert!(parser.exp());
    assert!(parser.end());

    let queue = vec![
        Token::new(Rule::exp, 0, 5),
        Token::new(Rule::power, 0, 5),
        Token::new(Rule::number, 0, 1),
        Token::new(Rule::int, 0, 1),
        Token::new(Rule::digits, 0, 1),
        Token::new(Rule::op_power, 2, 3),
        Token::new(Rule::number, 4, 5),
        Token::new(Rule::int, 4, 5),
        Token::new(Rule::digits, 4, 5)
    ];

    assert_eq!(parser.queue(), &queue);
}

#[test]
fn exp_unary() {
    let mut parser = Rdp::new(StringInput::new("not 1"));

    assert!(parser.exp());
    assert!(parser.end());

    let queue = vec![
        Token::new(Rule::exp, 0, 5),
        Token::new(Rule::unary, 0, 5),
        Token::new(Rule::op_unary, 0, 3),
        Token::new(Rule::number, 4, 5),
        Token::new(Rule::int, 4, 5),
        Token::new(Rule::digits, 4, 5)
    ];

    assert_eq!(parser.queue(), &queue);
}

#[test]
fn exp_unary_unary() {
    let mut parser = Rdp::new(StringInput::new("not not 1"));

    assert!(parser.exp());
    assert!(parser.end());

    let queue = vec![
        Token::new(Rule::exp, 0, 9),
        Token::new(Rule::unary, 0, 9),
        Token::new(Rule::op_unary, 0, 3),
        Token::new(Rule::unary, 4, 9),
        Token::new(Rule::op_unary, 4, 7),
        Token::new(Rule::number, 8, 9),
        Token::new(Rule::int, 8, 9),
        Token::new(Rule::digits, 8, 9)
    ];

    assert_eq!(parser.queue(), &queue);
}

#[test]
fn exp_infix() {
    let mut parser = Rdp::new(StringInput::new("3 + 3 -1^2 * 2 %64 and 3 or a..b <= 2"));

    assert!(parser.exp());
    assert!(parser.end());

    let queue = vec![
        Token::new(Rule::exp, 0, 37),
        Token::new(Rule::or, 0, 37),
        Token::new(Rule::and, 0, 24),
        Token::new(Rule::add_sub, 0, 14),
        Token::new(Rule::add_sub, 0, 5),
        Token::new(Rule::number, 0, 1),
        Token::new(Rule::int, 0, 1),
        Token::new(Rule::digits, 0, 1),
        Token::new(Rule::op_add_sub, 2, 3),
        Token::new(Rule::number, 4, 5),
        Token::new(Rule::int, 4, 5),
        Token::new(Rule::digits, 4, 5),
        Token::new(Rule::op_add_sub, 6, 7),
        Token::new(Rule::mul_div_mod, 7, 18),
        Token::new(Rule::mul_div_mod, 7, 14),
        Token::new(Rule::power, 7, 10),
        Token::new(Rule::number, 7, 8),
        Token::new(Rule::int, 7, 8),
        Token::new(Rule::digits, 7, 8),
        Token::new(Rule::op_power, 8, 9),
        Token::new(Rule::number, 9, 10),
        Token::new(Rule::int, 9, 10),
        Token::new(Rule::digits, 9, 10),
        Token::new(Rule::op_mul_div_mod, 11, 12),
        Token::new(Rule::number, 13, 14),
        Token::new(Rule::int, 13, 14),
        Token::new(Rule::digits, 13, 14),
        Token::new(Rule::op_mul_div_mod, 15, 16),
        Token::new(Rule::number, 16, 18),
        Token::new(Rule::int, 16, 18),
        Token::new(Rule::digits, 16, 18),
        Token::new(Rule::op_and, 19, 22),
        Token::new(Rule::number, 23, 24),
        Token::new(Rule::int, 23, 24),
        Token::new(Rule::digits, 23, 24),
        Token::new(Rule::op_or, 25, 27),
        Token::new(Rule::comparison, 28, 37),
        Token::new(Rule::strcat, 28, 32),
        Token::new(Rule::prefixexp, 28, 29),
        Token::new(Rule::varorexp, 28, 29),
        Token::new(Rule::var, 28, 29),
        Token::new(Rule::name, 28, 29),
        Token::new(Rule::op_strcat, 29, 31),
        Token::new(Rule::prefixexp, 31, 32),
        Token::new(Rule::varorexp, 31, 32),
        Token::new(Rule::var, 31, 32),
        Token::new(Rule::name, 31, 32),
        Token::new(Rule::op_comparison, 33, 35),
        Token::new(Rule::number, 36, 37),
        Token::new(Rule::int, 36, 37),
        Token::new(Rule::digits, 36, 37)
    ];

    assert_eq!(parser.queue(), &queue);
}

#[test]
fn exp_functiondef() {
    let mut parser = Rdp::new(StringInput::new("function (a, b, ...); end"));

    assert!(parser.exp());
    assert!(parser.end());

    let queue = vec![
        Token::new(Rule::exp, 0, 25),
        Token::new(Rule::functiondef, 0, 25),
        Token::new(Rule::funcbody, 9, 25),
        Token::new(Rule::parlist, 10, 19),
        Token::new(Rule::namelist, 10, 14),
        Token::new(Rule::name, 10, 11),
        Token::new(Rule::name, 13, 14),
        Token::new(Rule::block, 20, 21),
        Token::new(Rule::stat, 20, 21)
    ];

    assert_eq!(parser.queue(), &queue);
}

#[test]
fn exp_prefixexp() {
    let mut parser = Rdp::new(StringInput::new("a()"));

    assert!(parser.exp());
    assert!(parser.end());

    let queue = vec![
        Token::new(Rule::exp, 0, 3),
        Token::new(Rule::prefixexp, 0, 3),
        Token::new(Rule::varorexp, 0, 1),
        Token::new(Rule::var, 0, 1),
        Token::new(Rule::name, 0, 1),
        Token::new(Rule::nameandargs, 1, 3),
        Token::new(Rule::args, 1, 3)

    ];

    assert_eq!(parser.queue(), &queue);
}

#[test]
fn exp_tableconstructor() {
    let mut parser = Rdp::new(StringInput::new("{ }"));

    assert!(parser.exp());
    assert!(parser.end());

    let queue = vec![
        Token::new(Rule::exp, 0, 3),
        Token::new(Rule::tableconstructor, 0, 3)
    ];

    assert_eq!(parser.queue(), &queue);
}

#[test]
fn retstat() {
    let mut parser = Rdp::new(StringInput::new("return 1, 2;"));

    assert!(parser.retstat());
    assert!(parser.end());

    let queue = vec![
        Token::new(Rule::retstat, 0, 12),
        Token::new(Rule::explist, 7, 11),
        Token::new(Rule::exp, 7, 8),
        Token::new(Rule::number, 7, 8),
        Token::new(Rule::int, 7, 8),
        Token::new(Rule::digits, 7, 8),
        Token::new(Rule::exp, 10, 11),
        Token::new(Rule::number, 10, 11),
        Token::new(Rule::int, 10, 11),
        Token::new(Rule::digits, 10, 11)
    ];

    assert_eq!(parser.queue(), &queue);
}

#[test]
fn label() {
    let mut parser = Rdp::new(StringInput::new("::a::"));

    assert!(parser.label());
    assert!(parser.end());

    let queue = vec![
        Token::new(Rule::label, 0, 5),
        Token::new(Rule::name, 2, 3)
    ];

    assert_eq!(parser.queue(), &queue);
}

#[test]
fn funcname() {
    let mut parser = Rdp::new(StringInput::new("a.b :c"));

    assert!(parser.funcname());
    assert!(parser.end());

    let queue = vec![
        Token::new(Rule::funcname, 0, 6),
        Token::new(Rule::name, 0, 1),
        Token::new(Rule::name, 2, 3),
        Token::new(Rule::name, 5, 6)
    ];

    assert_eq!(parser.queue(), &queue);
}

#[test]
fn var() {
    let mut parser = Rdp::new(StringInput::new("(1) :a ().hi"));

    assert!(parser.var());
    assert!(parser.end());

    let queue = vec![
        Token::new(Rule::var, 0, 12),
        Token::new(Rule::exp, 1, 2),
        Token::new(Rule::number, 1, 2),
        Token::new(Rule::int, 1, 2),
        Token::new(Rule::digits, 1, 2),
        Token::new(Rule::varsuffix, 4, 12),
        Token::new(Rule::nameandargs, 4, 9),
        Token::new(Rule::name, 5, 6),
        Token::new(Rule::args, 7, 9),
        Token::new(Rule::name, 10, 12)
    ];

    assert_eq!(parser.queue(), &queue);
}

#[test]
fn functioncall_var() {
    let mut parser = Rdp::new(StringInput::new("a()"));

    assert!(parser.functioncall());
    assert!(parser.end());

    let queue = vec![
        Token::new(Rule::functioncall, 0, 3),
        Token::new(Rule::varorexp, 0, 1),
        Token::new(Rule::var, 0, 1),
        Token::new(Rule::name, 0, 1),
        Token::new(Rule::nameandargs, 1, 3),
        Token::new(Rule::args, 1, 3)

    ];

    assert_eq!(parser.queue(), &queue);
}

#[test]
fn functioncall_exp() {
    let mut parser = Rdp::new(StringInput::new("(1)()"));

    assert!(parser.functioncall());
    assert!(parser.end());

    let queue = vec![
        Token::new(Rule::functioncall, 0, 5),
        Token::new(Rule::varorexp, 0, 3),
        Token::new(Rule::exp, 1, 2),
        Token::new(Rule::number, 1, 2),
        Token::new(Rule::int, 1, 2),
        Token::new(Rule::digits, 1, 2),
        Token::new(Rule::nameandargs, 3, 5),
        Token::new(Rule::args, 3, 5)

    ];

    assert_eq!(parser.queue(), &queue);
}

#[test]
fn varsuffix() {
    let mut parser = Rdp::new(StringInput::new(":a () () [1]"));

    assert!(parser.varsuffix());
    assert!(parser.end());

    let queue = vec![
        Token::new(Rule::varsuffix, 0, 12),
        Token::new(Rule::nameandargs, 0, 5),
        Token::new(Rule::name, 1, 2),
        Token::new(Rule::args, 3, 5),
        Token::new(Rule::nameandargs, 6, 8),
        Token::new(Rule::args, 6, 8),
        Token::new(Rule::exp, 10, 11),
        Token::new(Rule::number, 10, 11),
        Token::new(Rule::int, 10, 11),
        Token::new(Rule::digits, 10, 11)

    ];

    assert_eq!(parser.queue(), &queue);
}

#[test]
fn nameandargs() {
    let mut parser = Rdp::new(StringInput::new(":a ()"));

    assert!(parser.nameandargs());
    assert!(parser.end());

    let queue = vec![
        Token::new(Rule::nameandargs, 0, 5),
        Token::new(Rule::name, 1, 2),
        Token::new(Rule::args, 3, 5)

    ];

    assert_eq!(parser.queue(), &queue);
}

#[test]
fn args() {
    let mut parser = Rdp::new(StringInput::new("(1, 2)"));

    assert!(parser.args());
    assert!(parser.end());

    let queue = vec![
        Token::new(Rule::args, 0, 6),
        Token::new(Rule::explist, 1, 5),
        Token::new(Rule::exp, 1, 2),
        Token::new(Rule::number, 1, 2),
        Token::new(Rule::int, 1, 2),
        Token::new(Rule::digits, 1, 2),
        Token::new(Rule::exp, 4, 5),
        Token::new(Rule::number, 4, 5),
        Token::new(Rule::int, 4, 5),
        Token::new(Rule::digits, 4, 5)

    ];

    assert_eq!(parser.queue(), &queue);
}

#[test]
fn explist() {
    let mut parser = Rdp::new(StringInput::new("1, 2, 3"));

    assert!(parser.explist());
    assert!(parser.end());

    let queue = vec![
        Token::new(Rule::explist, 0, 7),
        Token::new(Rule::exp, 0, 1),
        Token::new(Rule::number, 0, 1),
        Token::new(Rule::int, 0, 1),
        Token::new(Rule::digits, 0, 1),
        Token::new(Rule::exp, 3, 4),
        Token::new(Rule::number, 3, 4),
        Token::new(Rule::int, 3, 4),
        Token::new(Rule::digits, 3, 4),
        Token::new(Rule::exp, 6, 7),
        Token::new(Rule::number, 6, 7),
        Token::new(Rule::int, 6, 7),
        Token::new(Rule::digits, 6, 7)

    ];

    assert_eq!(parser.queue(), &queue);
}

#[test]
fn functiondef() {
    let mut parser = Rdp::new(StringInput::new("function (a, b, ...); end"));

    assert!(parser.functiondef());
    assert!(parser.end());

    let queue = vec![
        Token::new(Rule::functiondef, 0, 25),
        Token::new(Rule::funcbody, 9, 25),
        Token::new(Rule::parlist, 10, 19),
        Token::new(Rule::namelist, 10, 14),
        Token::new(Rule::name, 10, 11),
        Token::new(Rule::name, 13, 14),
        Token::new(Rule::block, 20, 21),
        Token::new(Rule::stat, 20, 21)
    ];

    assert_eq!(parser.queue(), &queue);
}

#[test]
fn funcbody() {
    let mut parser = Rdp::new(StringInput::new("(a, b, ...); end"));

    assert!(parser.funcbody());
    assert!(parser.end());

    let queue = vec![
        Token::new(Rule::funcbody, 0, 16),
        Token::new(Rule::parlist, 1, 10),
        Token::new(Rule::namelist, 1, 5),
        Token::new(Rule::name, 1, 2),
        Token::new(Rule::name, 4, 5),
        Token::new(Rule::block, 11, 12),
        Token::new(Rule::stat, 11, 12)
    ];

    assert_eq!(parser.queue(), &queue);
}

#[test]
fn parlist_dots() {
    let mut parser = Rdp::new(StringInput::new("..."));

    assert!(parser.parlist());
    assert!(parser.end());

    let queue = vec![
        Token::new(Rule::parlist, 0, 3)
    ];

    assert_eq!(parser.queue(), &queue);
}

#[test]
fn parlist() {
    let mut parser = Rdp::new(StringInput::new("a, b, ..."));

    assert!(parser.parlist());
    assert!(parser.end());

    let queue = vec![
        Token::new(Rule::parlist, 0, 9),
        Token::new(Rule::namelist, 0, 4),
        Token::new(Rule::name, 0, 1),
        Token::new(Rule::name, 3, 4)
    ];

    assert_eq!(parser.queue(), &queue);
}

#[test]
fn namelist_one() {
    let mut parser = Rdp::new(StringInput::new("abc"));

    assert!(parser.namelist());
    assert!(parser.end());

    let queue = vec![
        Token::new(Rule::namelist, 0, 3),
        Token::new(Rule::name, 0, 3)
    ];

    assert_eq!(parser.queue(), &queue);
}

#[test]
fn namelist_many() {
    let mut parser = Rdp::new(StringInput::new("abc, def"));

    assert!(parser.namelist());
    assert!(parser.end());

    let queue = vec![
        Token::new(Rule::namelist, 0, 8),
        Token::new(Rule::name, 0, 3),
        Token::new(Rule::name, 5, 8)
    ];

    assert_eq!(parser.queue(), &queue);
}

#[test]
fn tableconstructor_empty() {
    let mut parser = Rdp::new(StringInput::new("{ }"));

    assert!(parser.tableconstructor());
    assert!(parser.end());

    let queue = vec![
        Token::new(Rule::tableconstructor, 0, 3)
    ];

    assert_eq!(parser.queue(), &queue);
}

#[test]
fn tableconstructor() {
    let mut parser = Rdp::new(StringInput::new("{ [1] = 1 }"));

    assert!(parser.tableconstructor());
    assert!(parser.end());

    let queue = vec![
        Token::new(Rule::tableconstructor, 0, 11),
        Token::new(Rule::fieldlist, 2, 9),
        Token::new(Rule::field, 2, 9),
        Token::new(Rule::exp, 3, 4),
        Token::new(Rule::number, 3, 4),
        Token::new(Rule::int, 3, 4),
        Token::new(Rule::digits, 3, 4),
        Token::new(Rule::exp, 8, 9),
        Token::new(Rule::number, 8, 9),
        Token::new(Rule::int, 8, 9),
        Token::new(Rule::digits, 8, 9)
    ];

    assert_eq!(parser.queue(), &queue);
}

#[test]
fn fieldlist() {
    let mut parser = Rdp::new(StringInput::new("[1] = 1, [1] = 1, ,;,"));

    assert!(parser.fieldlist());
    assert!(parser.end());

    let queue = vec![
        Token::new(Rule::fieldlist, 0, 21),
        Token::new(Rule::field, 0, 7),
        Token::new(Rule::exp, 1, 2),
        Token::new(Rule::number, 1, 2),
        Token::new(Rule::int, 1, 2),
        Token::new(Rule::digits, 1, 2),
        Token::new(Rule::exp, 6, 7),
        Token::new(Rule::number, 6, 7),
        Token::new(Rule::int, 6, 7),
        Token::new(Rule::digits, 6, 7),
        Token::new(Rule::field, 9, 16),
        Token::new(Rule::exp, 10, 11),
        Token::new(Rule::number, 10, 11),
        Token::new(Rule::int, 10, 11),
        Token::new(Rule::digits, 10, 11),
        Token::new(Rule::exp, 15, 16),
        Token::new(Rule::number, 15, 16),
        Token::new(Rule::int, 15, 16),
        Token::new(Rule::digits, 15, 16)
    ];

    assert_eq!(parser.queue(), &queue);
}

#[test]
fn field1() {
    let mut parser = Rdp::new(StringInput::new("[1] = 1"));

    assert!(parser.field());
    assert!(parser.end());

    let queue = vec![
        Token::new(Rule::field, 0, 7),
        Token::new(Rule::exp, 1, 2),
        Token::new(Rule::number, 1, 2),
        Token::new(Rule::int, 1, 2),
        Token::new(Rule::digits, 1, 2),
        Token::new(Rule::exp, 6, 7),
        Token::new(Rule::number, 6, 7),
        Token::new(Rule::int, 6, 7),
        Token::new(Rule::digits, 6, 7)
    ];

    assert_eq!(parser.queue(), &queue);
}

#[test]
fn field2() {
    let mut parser = Rdp::new(StringInput::new("a = 1"));

    assert!(parser.field());
    assert!(parser.end());

    let queue = vec![
        Token::new(Rule::field, 0, 5),
        Token::new(Rule::name, 0, 1),
        Token::new(Rule::exp, 4, 5),
        Token::new(Rule::number, 4, 5),
        Token::new(Rule::int, 4, 5),
        Token::new(Rule::digits, 4, 5)
    ];

    assert_eq!(parser.queue(), &queue);
}

#[test]
fn field3() {
    let mut parser = Rdp::new(StringInput::new("1"));

    assert!(parser.field());
    assert!(parser.end());

    let queue = vec![
        Token::new(Rule::field, 0, 1),
        Token::new(Rule::exp, 0, 1),
        Token::new(Rule::number, 0, 1),
        Token::new(Rule::int, 0, 1),
        Token::new(Rule::digits, 0, 1)
    ];

    assert_eq!(parser.queue(), &queue);
}

#[test]
fn name() {
    let mut parser = Rdp::new(StringInput::new("__hell0"));

    assert!(parser.name());
    assert!(parser.end());

    let queue = vec![
        Token::new(Rule::name, 0, 7)
    ];

    assert_eq!(parser.queue(), &queue);
}

#[test]
fn string_normal() {
    let mut parser = Rdp::new(StringInput::new("\"a \\b\\n\\099\\xfF\""));

    assert!(parser.string());
    assert!(parser.end());

    let queue = vec![
        Token::new(Rule::string, 0, 16),
        Token::new(Rule::normalstring, 0, 16),
        Token::new(Rule::escape_sequence, 3, 5),
        Token::new(Rule::escape_sequence, 5, 7),
        Token::new(Rule::escape_sequence, 7, 11),
        Token::new(Rule::decimal_escape, 7, 11),
        Token::new(Rule::escape_sequence, 11, 15),
        Token::new(Rule::hex_escape, 11, 15)
    ];

    assert_eq!(parser.queue(), &queue);
}

#[test]
fn string_char() {
    let mut parser = Rdp::new(StringInput::new("'a \\b\\n\\099\\xfF'"));

    assert!(parser.string());
    assert!(parser.end());

    let queue = vec![
        Token::new(Rule::string, 0, 16),
        Token::new(Rule::charstring, 0, 16),
        Token::new(Rule::escape_sequence, 3, 5),
        Token::new(Rule::escape_sequence, 5, 7),
        Token::new(Rule::escape_sequence, 7, 11),
        Token::new(Rule::decimal_escape, 7, 11),
        Token::new(Rule::escape_sequence, 11, 15),
        Token::new(Rule::hex_escape, 11, 15)
    ];

    assert_eq!(parser.queue(), &queue);
}

#[test]
fn string_long() {
    let mut parser = Rdp::new(StringInput::new("[==[a d]==]"));

    assert!(parser.string());
    assert!(parser.end());

    let queue = vec![
        Token::new(Rule::string, 0, 11),
        Token::new(Rule::longstring, 0, 11)
    ];

    assert_eq!(parser.queue(), &queue);
}

#[test]
fn int() {
    let mut parser = Rdp::new(StringInput::new("00845"));

    assert!(parser.number());
    assert!(parser.end());

    let queue = vec![
        Token::new(Rule::number, 0, 5),
        Token::new(Rule::int, 0, 5),
        Token::new(Rule::digits, 0, 5)
    ];

    assert_eq!(parser.queue(), &queue);
}

#[test]
fn float_digits_point_digits() {
    let mut parser = Rdp::new(StringInput::new("10.01"));

    assert!(parser.number());
    assert!(parser.end());

    let queue = vec![
        Token::new(Rule::number, 0, 5),
        Token::new(Rule::float, 0, 5),
        Token::new(Rule::digits, 0, 2),
        Token::new(Rule::digits, 3, 5)
    ];

    assert_eq!(parser.queue(), &queue);
}

#[test]
fn float_digits_point_exp() {
    let mut parser = Rdp::new(StringInput::new("01.E-1"));

    assert!(parser.number());
    assert!(parser.end());

    let queue = vec![
        Token::new(Rule::number, 0, 6),
        Token::new(Rule::float, 0, 6),
        Token::new(Rule::digits, 0, 2),
        Token::new(Rule::exponent, 3, 6),
        Token::new(Rule::digits, 5, 6)
    ];

    assert_eq!(parser.queue(), &queue);
}

#[test]
fn float_point_digits_exp() {
    let mut parser = Rdp::new(StringInput::new(".02e-4"));

    assert!(parser.number());
    assert!(parser.end());

    let queue = vec![
        Token::new(Rule::number, 0, 6),
        Token::new(Rule::float, 0, 6),
        Token::new(Rule::digits, 1, 3),
        Token::new(Rule::exponent, 3, 6),
        Token::new(Rule::digits, 5, 6)
    ];

    assert_eq!(parser.queue(), &queue);
}

#[test]
fn float_digits_exp() {
    let mut parser = Rdp::new(StringInput::new("9e-01"));

    assert!(parser.number());
    assert!(parser.end());

    let queue = vec![
        Token::new(Rule::number, 0, 5),
        Token::new(Rule::float, 0, 5),
        Token::new(Rule::digits, 0, 1),
        Token::new(Rule::exponent, 1, 5),
        Token::new(Rule::digits, 3, 5)
    ];

    assert_eq!(parser.queue(), &queue);
}

#[test]
fn hex() {
    let mut parser = Rdp::new(StringInput::new("0x0aF"));

    assert!(parser.number());
    assert!(parser.end());

    let queue = vec![
        Token::new(Rule::number, 0, 5),
        Token::new(Rule::hex, 0, 5),
        Token::new(Rule::hex_digits, 2, 5)
    ];

    assert_eq!(parser.queue(), &queue);
}

#[test]
fn hex_float_digits_point_digits() {
    let mut parser = Rdp::new(StringInput::new("0x0a.F1"));

    assert!(parser.number());
    assert!(parser.end());

    let queue = vec![
        Token::new(Rule::number, 0, 7),
        Token::new(Rule::hex_float, 0, 7),
        Token::new(Rule::hex_digits, 2, 4),
        Token::new(Rule::hex_digits, 5, 7)
    ];

    assert_eq!(parser.queue(), &queue);
}

#[test]
fn hex_float_digits_point_exp() {
    let mut parser = Rdp::new(StringInput::new("0x0a.P-2"));

    assert!(parser.number());
    assert!(parser.end());

    let queue = vec![
        Token::new(Rule::number, 0, 8),
        Token::new(Rule::hex_float, 0, 8),
        Token::new(Rule::hex_digits, 2, 4),
        Token::new(Rule::hex_exponent, 5, 8),
        Token::new(Rule::digits, 7, 8)
    ];

    assert_eq!(parser.queue(), &queue);
}

#[test]
fn hex_float_point_digits_exp() {
    let mut parser = Rdp::new(StringInput::new("0x.afp-2"));

    assert!(parser.number());
    assert!(parser.end());

    let queue = vec![
        Token::new(Rule::number, 0, 8),
        Token::new(Rule::hex_float, 0, 8),
        Token::new(Rule::hex_digits, 3, 5),
        Token::new(Rule::hex_exponent, 5, 8),
        Token::new(Rule::digits, 7, 8)
    ];

    assert_eq!(parser.queue(), &queue);
}

#[test]
fn hex_float_digits_exp() {
    let mut parser = Rdp::new(StringInput::new("0x9ep-01"));

    assert!(parser.number());
    assert!(parser.end());

    let queue = vec![
        Token::new(Rule::number, 0, 8),
        Token::new(Rule::hex_float, 0, 8),
        Token::new(Rule::hex_digits, 2, 4),
        Token::new(Rule::hex_exponent, 4, 8),
        Token::new(Rule::digits, 6, 8)
    ];

    assert_eq!(parser.queue(), &queue);
}

#[test]
fn nested_str1_empty() {
    let mut parser = Rdp::new(StringInput::new("=[]="));

    assert!(parser.nested_str());
    assert!(parser.end());

    let queue = vec![];

    assert_eq!(parser.queue(), &queue);
}

#[test]
fn nested_str1() {
    let mut parser = Rdp::new(StringInput::new("=[asd asd]="));

    assert!(parser.nested_str());
    assert!(parser.end());

    let queue = vec![];

    assert_eq!(parser.queue(), &queue);
}

#[test]
fn nested_str2_empty() {
    let mut parser = Rdp::new(StringInput::new("==[]=="));

    assert!(parser.nested_str());
    assert!(parser.end());

    let queue = vec![];

    assert_eq!(parser.queue(), &queue);
}

#[test]
fn nested_str2() {
    let mut parser = Rdp::new(StringInput::new("==[asd asd]=="));

    assert!(parser.nested_str());
    assert!(parser.end());

    let queue = vec![];

    assert_eq!(parser.queue(), &queue);
}

#[test]
fn nested_str2_spaced() {
    let mut parser = Rdp::new(StringInput::new("  =  = [asd asd] =="));

    assert!(parser.nested_str());
    assert!(parser.end());

    let queue = vec![];

    assert_eq!(parser.queue(), &queue);
}

#[test]
fn comment() {
    let mut parser = Rdp::new(StringInput::new("-- [ [simple] ]"));

    assert!(parser.comment());
    assert!(parser.end());

    let queue = vec![];

    assert_eq!(parser.queue(), &queue);
}

#[test]
fn line_comment1() {
    let mut parser = Rdp::new(StringInput::new("--"));

    assert!(parser.comment());
    assert!(parser.end());

    let queue = vec![];

    assert_eq!(parser.queue(), &queue);
}

#[test]
fn line_comment2() {
    let mut parser = Rdp::new(StringInput::new("--[====="));

    assert!(parser.comment());
    assert!(parser.end());

    let queue = vec![];

    assert_eq!(parser.queue(), &queue);
}

#[test]
fn line_comment3() {
    let mut parser = Rdp::new(StringInput::new("--[=====hi"));

    assert!(parser.comment());
    assert!(parser.end());

    let queue = vec![];

    assert_eq!(parser.queue(), &queue);
}

#[test]
fn line_comment4() {
    let mut parser = Rdp::new(StringInput::new("--hi"));

    assert!(parser.comment());
    assert!(parser.end());

    let queue = vec![];

    assert_eq!(parser.queue(), &queue);
}

#[test]
fn shebang() {
    let mut parser = Rdp::new(StringInput::new("#!/bin/not-lua"));

    assert!(parser.comment());
    assert!(parser.end());

    let queue = vec![];

    assert_eq!(parser.queue(), &queue);
}
