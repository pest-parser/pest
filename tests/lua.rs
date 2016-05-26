// pest. Smart PEGs in Rust
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

use pest::Parser;
use pest::Queues;
use pest::Token;
use pest::Input;
use pest::StringInput;

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
    let mut parser = Rdp::new(Box::new(StringInput::new("123")));

    assert!(parser._power());
    assert!(parser.end());

    let queue = vec![
        Token { rule: Rule::number, start: 0, end: 3 },
        Token { rule: Rule::int, start: 0, end: 3 },
        Token { rule: Rule::digits, start: 0, end: 3 }
    ];

    assert!(parser.queue().iter().eq(&queue));
}

#[test]
fn _power_power() {
    let mut parser = Rdp::new(Box::new(StringInput::new("1 ^ 3")));

    assert!(parser._power());
    assert!(parser.end());

    let queue = vec![
        Token { rule: Rule::power, start: 0, end: 5 },
        Token { rule: Rule::number, start: 0, end: 1 },
        Token { rule: Rule::int, start: 0, end: 1 },
        Token { rule: Rule::digits, start: 0, end: 1 },
        Token { rule: Rule::op_power, start: 2, end: 3 },
        Token { rule: Rule::number, start: 4, end: 5 },
        Token { rule: Rule::int, start: 4, end: 5 },
        Token { rule: Rule::digits, start: 4, end: 5 }
    ];

    assert!(parser.queue().iter().eq(&queue));
}

#[test]
fn _unary_power() {
    let mut parser = Rdp::new(Box::new(StringInput::new("1 ^ 3")));

    assert!(parser._unary());
    assert!(parser.end());

    let queue = vec![
        Token { rule: Rule::power, start: 0, end: 5 },
        Token { rule: Rule::number, start: 0, end: 1 },
        Token { rule: Rule::int, start: 0, end: 1 },
        Token { rule: Rule::digits, start: 0, end: 1 },
        Token { rule: Rule::op_power, start: 2, end: 3 },
        Token { rule: Rule::number, start: 4, end: 5 },
        Token { rule: Rule::int, start: 4, end: 5 },
        Token { rule: Rule::digits, start: 4, end: 5 }
    ];

    assert!(parser.queue().iter().eq(&queue));
}

#[test]
fn _unary_unary() {
    let mut parser = Rdp::new(Box::new(StringInput::new("not 1")));

    assert!(parser._unary());
    assert!(parser.end());

    let queue = vec![
        Token { rule: Rule::unary, start: 0, end: 5 },
        Token { rule: Rule::op_unary, start: 0, end: 3 },
        Token { rule: Rule::number, start: 4, end: 5 },
        Token { rule: Rule::int, start: 4, end: 5 },
        Token { rule: Rule::digits, start: 4, end: 5 }
    ];

    assert!(parser.queue().iter().eq(&queue));
}

#[test]
fn _unary_unary_unary() {
    let mut parser = Rdp::new(Box::new(StringInput::new("not not 1")));

    assert!(parser._unary());
    assert!(parser.end());

    let queue = vec![
        Token { rule: Rule::unary, start: 0, end: 9 },
        Token { rule: Rule::op_unary, start: 0, end: 3 },
        Token { rule: Rule::unary, start: 4, end: 9 },
        Token { rule: Rule::op_unary, start: 4, end: 7 },
        Token { rule: Rule::number, start: 8, end: 9 },
        Token { rule: Rule::int, start: 8, end: 9 },
        Token { rule: Rule::digits, start: 8, end: 9 }
    ];

    assert!(parser.queue().iter().eq(&queue));
}

#[test]
fn exp_number() {
    let mut parser = Rdp::new(Box::new(StringInput::new("123")));

    assert!(parser.exp());
    assert!(parser.end());

    let queue = vec![
        Token { rule: Rule::exp, start: 0, end: 3 },
        Token { rule: Rule::number, start: 0, end: 3 },
        Token { rule: Rule::int, start: 0, end: 3 },
        Token { rule: Rule::digits, start: 0, end: 3 }
    ];

    assert!(parser.queue().iter().eq(&queue));
}

#[test]
fn exp_power() {
    let mut parser = Rdp::new(Box::new(StringInput::new("1 ^ 3")));

    assert!(parser.exp());
    assert!(parser.end());

    let queue = vec![
        Token { rule: Rule::exp, start: 0, end: 5 },
        Token { rule: Rule::power, start: 0, end: 5 },
        Token { rule: Rule::number, start: 0, end: 1 },
        Token { rule: Rule::int, start: 0, end: 1 },
        Token { rule: Rule::digits, start: 0, end: 1 },
        Token { rule: Rule::op_power, start: 2, end: 3 },
        Token { rule: Rule::number, start: 4, end: 5 },
        Token { rule: Rule::int, start: 4, end: 5 },
        Token { rule: Rule::digits, start: 4, end: 5 }
    ];

    assert!(parser.queue().iter().eq(&queue));
}

#[test]
fn exp_unary() {
    let mut parser = Rdp::new(Box::new(StringInput::new("not 1")));

    assert!(parser.exp());
    assert!(parser.end());

    let queue = vec![
        Token { rule: Rule::exp, start: 0, end: 5 },
        Token { rule: Rule::unary, start: 0, end: 5 },
        Token { rule: Rule::op_unary, start: 0, end: 3 },
        Token { rule: Rule::number, start: 4, end: 5 },
        Token { rule: Rule::int, start: 4, end: 5 },
        Token { rule: Rule::digits, start: 4, end: 5 }
    ];

    assert!(parser.queue().iter().eq(&queue));
}

#[test]
fn exp_unary_unary() {
    let mut parser = Rdp::new(Box::new(StringInput::new("not not 1")));

    assert!(parser.exp());
    assert!(parser.end());

    let queue = vec![
        Token { rule: Rule::exp, start: 0, end: 9 },
        Token { rule: Rule::unary, start: 0, end: 9 },
        Token { rule: Rule::op_unary, start: 0, end: 3 },
        Token { rule: Rule::unary, start: 4, end: 9 },
        Token { rule: Rule::op_unary, start: 4, end: 7 },
        Token { rule: Rule::number, start: 8, end: 9 },
        Token { rule: Rule::int, start: 8, end: 9 },
        Token { rule: Rule::digits, start: 8, end: 9 }
    ];

    assert!(parser.queue().iter().eq(&queue));
}

#[test]
fn exp_infix() {
    let mut parser = Rdp::new(Box::new(StringInput::new("3 + 3 -1^2 * 2 %64 and 3 or a..b <= 2")));

    assert!(parser.exp());
    assert!(parser.end());

    let queue = vec![
        Token { rule: Rule::exp, start: 0, end: 37 },
        Token { rule: Rule::or, start: 0, end: 37 },
        Token { rule: Rule::add_sub, start: 0, end: 24 },
        Token { rule: Rule::number, start: 0, end: 1 },
        Token { rule: Rule::int, start: 0, end: 1 },
        Token { rule: Rule::digits, start: 0, end: 1 },
        Token { rule: Rule::op_add_sub, start: 2, end: 3 },
        Token { rule: Rule::and, start: 4, end: 24 },
        Token { rule: Rule::add_sub, start: 4, end: 14 },
        Token { rule: Rule::number, start: 4, end: 5 },
        Token { rule: Rule::int, start: 4, end: 5 },
        Token { rule: Rule::digits, start: 4, end: 5 },
        Token { rule: Rule::op_add_sub, start: 6, end: 7 },
        Token { rule: Rule::mul_div_mod, start: 7, end: 18 },
        Token { rule: Rule::mul_div_mod, start: 7, end: 14 },
        Token { rule: Rule::power, start: 7, end: 10 },
        Token { rule: Rule::number, start: 7, end: 8 },
        Token { rule: Rule::int, start: 7, end: 8 },
        Token { rule: Rule::digits, start: 7, end: 8 },
        Token { rule: Rule::op_power, start: 8, end: 9 },
        Token { rule: Rule::number, start: 9, end: 10 },
        Token { rule: Rule::int, start: 9, end: 10 },
        Token { rule: Rule::digits, start: 9, end: 10 },
        Token { rule: Rule::op_mul_div_mod, start: 11, end: 12 },
        Token { rule: Rule::number, start: 13, end: 14 },
        Token { rule: Rule::int, start: 13, end: 14 },
        Token { rule: Rule::digits, start: 13, end: 14 },
        Token { rule: Rule::op_mul_div_mod, start: 15, end: 16 },
        Token { rule: Rule::number, start: 16, end: 18 },
        Token { rule: Rule::int, start: 16, end: 18 },
        Token { rule: Rule::digits, start: 16, end: 18 },
        Token { rule: Rule::op_and, start: 19, end: 22 },
        Token { rule: Rule::number, start: 23, end: 24 },
        Token { rule: Rule::int, start: 23, end: 24 },
        Token { rule: Rule::digits, start: 23, end: 24 },
        Token { rule: Rule::op_or, start: 25, end: 27 },
        Token { rule: Rule::comparison, start: 28, end: 37 },
        Token { rule: Rule::strcat, start: 28, end: 32 },
        Token { rule: Rule::prefixexp, start: 28, end: 29 },
        Token { rule: Rule::varorexp, start: 28, end: 29 },
        Token { rule: Rule::var, start: 28, end: 29 },
        Token { rule: Rule::name, start: 28, end: 29 },
        Token { rule: Rule::op_strcat, start: 29, end: 31 },
        Token { rule: Rule::prefixexp, start: 31, end: 32 },
        Token { rule: Rule::varorexp, start: 31, end: 32 },
        Token { rule: Rule::var, start: 31, end: 32 },
        Token { rule: Rule::name, start: 31, end: 32 },
        Token { rule: Rule::op_comparison, start: 33, end: 35 },
        Token { rule: Rule::number, start: 36, end: 37 },
        Token { rule: Rule::int, start: 36, end: 37 },
        Token { rule: Rule::digits, start: 36, end: 37 }
    ];

    assert!(parser.queue().iter().eq(&queue));
}

#[test]
fn exp_functiondef() {
    let mut parser = Rdp::new(Box::new(StringInput::new("function (a, b, ...); end")));

    assert!(parser.exp());
    assert!(parser.end());

    let queue = vec![
        Token { rule: Rule::exp, start: 0, end: 25 },
        Token { rule: Rule::functiondef, start: 0, end: 25 },
        Token { rule: Rule::funcbody, start: 9, end: 25 },
        Token { rule: Rule::parlist, start: 10, end: 19 },
        Token { rule: Rule::namelist, start: 10, end: 14 },
        Token { rule: Rule::name, start: 10, end: 11 },
        Token { rule: Rule::name, start: 13, end: 14 },
        Token { rule: Rule::block, start: 20, end: 21 },
        Token { rule: Rule::stat, start: 20, end: 21 }
    ];

    assert!(parser.queue().iter().eq(&queue));
}

#[test]
fn exp_prefixexp() {
    let mut parser = Rdp::new(Box::new(StringInput::new("a()")));

    assert!(parser.exp());
    assert!(parser.end());

    let queue = vec![
        Token { rule: Rule::exp, start: 0, end: 3 },
        Token { rule: Rule::prefixexp, start: 0, end: 3 },
        Token { rule: Rule::varorexp, start: 0, end: 1 },
        Token { rule: Rule::var, start: 0, end: 1 },
        Token { rule: Rule::name, start: 0, end: 1 },
        Token { rule: Rule::nameandargs, start: 1, end: 3 },
        Token { rule: Rule::args, start: 1, end: 3 }

    ];

    assert!(parser.queue().iter().eq(&queue));
}

#[test]
fn exp_tableconstructor() {
    let mut parser = Rdp::new(Box::new(StringInput::new("{ }")));

    assert!(parser.exp());
    assert!(parser.end());

    let queue = vec![
        Token { rule: Rule::exp, start: 0, end: 3 },
        Token { rule: Rule::tableconstructor, start: 0, end: 3 }
    ];

    assert!(parser.queue().iter().eq(&queue));
}

#[test]
fn retstat() {
    let mut parser = Rdp::new(Box::new(StringInput::new("return 1, 2;")));

    assert!(parser.retstat());
    assert!(parser.end());

    let queue = vec![
        Token { rule: Rule::retstat, start: 0, end: 12 },
        Token { rule: Rule::explist, start: 7, end: 11 },
        Token { rule: Rule::exp, start: 7, end: 8 },
        Token { rule: Rule::number, start: 7, end: 8 },
        Token { rule: Rule::int, start: 7, end: 8 },
        Token { rule: Rule::digits, start: 7, end: 8 },
        Token { rule: Rule::exp, start: 10, end: 11 },
        Token { rule: Rule::number, start: 10, end: 11 },
        Token { rule: Rule::int, start: 10, end: 11 },
        Token { rule: Rule::digits, start: 10, end: 11 }
    ];

    assert!(parser.queue().iter().eq(&queue));
}

#[test]
fn label() {
    let mut parser = Rdp::new(Box::new(StringInput::new("::a::")));

    assert!(parser.label());
    assert!(parser.end());

    let queue = vec![
        Token { rule: Rule::label, start: 0, end: 5 },
        Token { rule: Rule::name, start: 2, end: 3 }
    ];

    assert!(parser.queue().iter().eq(&queue));
}

#[test]
fn funcname() {
    let mut parser = Rdp::new(Box::new(StringInput::new("a.b :c")));

    assert!(parser.funcname());
    assert!(parser.end());

    let queue = vec![
        Token { rule: Rule::funcname, start: 0, end: 6 },
        Token { rule: Rule::name, start: 0, end: 1 },
        Token { rule: Rule::name, start: 2, end: 3 },
        Token { rule: Rule::name, start: 5, end: 6 }
    ];

    assert!(parser.queue().iter().eq(&queue));
}

#[test]
fn var() {
    let mut parser = Rdp::new(Box::new(StringInput::new("(1) :a ().hi")));

    assert!(parser.var());
    assert!(parser.end());

    let queue = vec![
        Token { rule: Rule::var, start: 0, end: 12 },
        Token { rule: Rule::exp, start: 1, end: 2 },
        Token { rule: Rule::number, start: 1, end: 2 },
        Token { rule: Rule::int, start: 1, end: 2 },
        Token { rule: Rule::digits, start: 1, end: 2 },
        Token { rule: Rule::varsuffix, start: 4, end: 12 },
        Token { rule: Rule::nameandargs, start: 4, end: 9 },
        Token { rule: Rule::name, start: 5, end: 6 },
        Token { rule: Rule::args, start: 7, end: 9 },
        Token { rule: Rule::name, start: 10, end: 12 }
    ];

    assert!(parser.queue().iter().eq(&queue));
}

#[test]
fn functioncall_var() {
    let mut parser = Rdp::new(Box::new(StringInput::new("a()")));

    assert!(parser.functioncall());
    assert!(parser.end());

    let queue = vec![
        Token { rule: Rule::functioncall, start: 0, end: 3 },
        Token { rule: Rule::varorexp, start: 0, end: 1 },
        Token { rule: Rule::var, start: 0, end: 1 },
        Token { rule: Rule::name, start: 0, end: 1 },
        Token { rule: Rule::nameandargs, start: 1, end: 3 },
        Token { rule: Rule::args, start: 1, end: 3 }

    ];

    assert!(parser.queue().iter().eq(&queue));
}

#[test]
fn functioncall_exp() {
    let mut parser = Rdp::new(Box::new(StringInput::new("(1)()")));

    assert!(parser.functioncall());
    assert!(parser.end());

    let queue = vec![
        Token { rule: Rule::functioncall, start: 0, end: 5 },
        Token { rule: Rule::varorexp, start: 0, end: 3 },
        Token { rule: Rule::exp, start: 1, end: 2 },
        Token { rule: Rule::number, start: 1, end: 2 },
        Token { rule: Rule::int, start: 1, end: 2 },
        Token { rule: Rule::digits, start: 1, end: 2 },
        Token { rule: Rule::nameandargs, start: 3, end: 5 },
        Token { rule: Rule::args, start: 3, end: 5 }

    ];

    assert!(parser.queue().iter().eq(&queue));
}

#[test]
fn varsuffix() {
    let mut parser = Rdp::new(Box::new(StringInput::new(":a () () [1]")));

    assert!(parser.varsuffix());
    assert!(parser.end());

    let queue = vec![
        Token { rule: Rule::varsuffix, start: 0, end: 12 },
        Token { rule: Rule::nameandargs, start: 0, end: 5 },
        Token { rule: Rule::name, start: 1, end: 2 },
        Token { rule: Rule::args, start: 3, end: 5 },
        Token { rule: Rule::nameandargs, start: 6, end: 8 },
        Token { rule: Rule::args, start: 6, end: 8 },
        Token { rule: Rule::exp, start: 10, end: 11 },
        Token { rule: Rule::number, start: 10, end: 11 },
        Token { rule: Rule::int, start: 10, end: 11 },
        Token { rule: Rule::digits, start: 10, end: 11 }

    ];

    assert!(parser.queue().iter().eq(&queue));
}

#[test]
fn nameandargs() {
    let mut parser = Rdp::new(Box::new(StringInput::new(":a ()")));

    assert!(parser.nameandargs());
    assert!(parser.end());

    let queue = vec![
        Token { rule: Rule::nameandargs, start: 0, end: 5 },
        Token { rule: Rule::name, start: 1, end: 2 },
        Token { rule: Rule::args, start: 3, end: 5 }

    ];

    assert!(parser.queue().iter().eq(&queue));
}

#[test]
fn args() {
    let mut parser = Rdp::new(Box::new(StringInput::new("(1, 2)")));

    assert!(parser.args());
    assert!(parser.end());

    let queue = vec![
        Token { rule: Rule::args, start: 0, end: 6 },
        Token { rule: Rule::explist, start: 1, end: 5 },
        Token { rule: Rule::exp, start: 1, end: 2 },
        Token { rule: Rule::number, start: 1, end: 2 },
        Token { rule: Rule::int, start: 1, end: 2 },
        Token { rule: Rule::digits, start: 1, end: 2 },
        Token { rule: Rule::exp, start: 4, end: 5 },
        Token { rule: Rule::number, start: 4, end: 5 },
        Token { rule: Rule::int, start: 4, end: 5 },
        Token { rule: Rule::digits, start: 4, end: 5 }

    ];

    assert!(parser.queue().iter().eq(&queue));
}

#[test]
fn explist() {
    let mut parser = Rdp::new(Box::new(StringInput::new("1, 2, 3")));

    assert!(parser.explist());
    assert!(parser.end());

    let queue = vec![
        Token { rule: Rule::explist, start: 0, end: 7 },
        Token { rule: Rule::exp, start: 0, end: 1 },
        Token { rule: Rule::number, start: 0, end: 1 },
        Token { rule: Rule::int, start: 0, end: 1 },
        Token { rule: Rule::digits, start: 0, end: 1 },
        Token { rule: Rule::exp, start: 3, end: 4 },
        Token { rule: Rule::number, start: 3, end: 4 },
        Token { rule: Rule::int, start: 3, end: 4 },
        Token { rule: Rule::digits, start: 3, end: 4 },
        Token { rule: Rule::exp, start: 6, end: 7 },
        Token { rule: Rule::number, start: 6, end: 7 },
        Token { rule: Rule::int, start: 6, end: 7 },
        Token { rule: Rule::digits, start: 6, end: 7 }

    ];

    assert!(parser.queue().iter().eq(&queue));
}

#[test]
fn functiondef() {
    let mut parser = Rdp::new(Box::new(StringInput::new("function (a, b, ...); end")));

    assert!(parser.functiondef());
    assert!(parser.end());

    let queue = vec![
        Token { rule: Rule::functiondef, start: 0, end: 25 },
        Token { rule: Rule::funcbody, start: 9, end: 25 },
        Token { rule: Rule::parlist, start: 10, end: 19 },
        Token { rule: Rule::namelist, start: 10, end: 14 },
        Token { rule: Rule::name, start: 10, end: 11 },
        Token { rule: Rule::name, start: 13, end: 14 },
        Token { rule: Rule::block, start: 20, end: 21 },
        Token { rule: Rule::stat, start: 20, end: 21 }
    ];

    assert!(parser.queue().iter().eq(&queue));
}

#[test]
fn funcbody() {
    let mut parser = Rdp::new(Box::new(StringInput::new("(a, b, ...); end")));

    assert!(parser.funcbody());
    assert!(parser.end());

    let queue = vec![
        Token { rule: Rule::funcbody, start: 0, end: 16 },
        Token { rule: Rule::parlist, start: 1, end: 10 },
        Token { rule: Rule::namelist, start: 1, end: 5 },
        Token { rule: Rule::name, start: 1, end: 2 },
        Token { rule: Rule::name, start: 4, end: 5 },
        Token { rule: Rule::block, start: 11, end: 12 },
        Token { rule: Rule::stat, start: 11, end: 12 }
    ];

    assert!(parser.queue().iter().eq(&queue));
}

#[test]
fn parlist_dots() {
    let mut parser = Rdp::new(Box::new(StringInput::new("...")));

    assert!(parser.parlist());
    assert!(parser.end());

    let queue = vec![
        Token { rule: Rule::parlist, start: 0, end: 3 }
    ];

    assert!(parser.queue().iter().eq(&queue));
}

#[test]
fn parlist() {
    let mut parser = Rdp::new(Box::new(StringInput::new("a, b, ...")));

    assert!(parser.parlist());
    assert!(parser.end());

    let queue = vec![
        Token { rule: Rule::parlist, start: 0, end: 9 },
        Token { rule: Rule::namelist, start: 0, end: 4 },
        Token { rule: Rule::name, start: 0, end: 1 },
        Token { rule: Rule::name, start: 3, end: 4 }
    ];

    assert!(parser.queue().iter().eq(&queue));
}

#[test]
fn namelist_one() {
    let mut parser = Rdp::new(Box::new(StringInput::new("abc")));

    assert!(parser.namelist());
    assert!(parser.end());

    let queue = vec![
        Token { rule: Rule::namelist, start: 0, end: 3 },
        Token { rule: Rule::name, start: 0, end: 3 }
    ];

    assert!(parser.queue().iter().eq(&queue));
}

#[test]
fn namelist_many() {
    let mut parser = Rdp::new(Box::new(StringInput::new("abc, def")));

    assert!(parser.namelist());
    assert!(parser.end());

    let queue = vec![
        Token { rule: Rule::namelist, start: 0, end: 8 },
        Token { rule: Rule::name, start: 0, end: 3 },
        Token { rule: Rule::name, start: 5, end: 8 }
    ];

    assert!(parser.queue().iter().eq(&queue));
}

#[test]
fn tableconstructor_empty() {
    let mut parser = Rdp::new(Box::new(StringInput::new("{ }")));

    assert!(parser.tableconstructor());
    assert!(parser.end());

    let queue = vec![
        Token { rule: Rule::tableconstructor, start: 0, end: 3 }
    ];

    assert!(parser.queue().iter().eq(&queue));
}

#[test]
fn tableconstructor() {
    let mut parser = Rdp::new(Box::new(StringInput::new("{ [1] = 1 }")));

    assert!(parser.tableconstructor());
    assert!(parser.end());

    let queue = vec![
        Token { rule: Rule::tableconstructor, start: 0, end: 11 },
        Token { rule: Rule::fieldlist, start: 2, end: 9 },
        Token { rule: Rule::field, start: 2, end: 9 },
        Token { rule: Rule::exp, start: 3, end: 4 },
        Token { rule: Rule::number, start: 3, end: 4 },
        Token { rule: Rule::int, start: 3, end: 4 },
        Token { rule: Rule::digits, start: 3, end: 4 },
        Token { rule: Rule::exp, start: 8, end: 9 },
        Token { rule: Rule::number, start: 8, end: 9 },
        Token { rule: Rule::int, start: 8, end: 9 },
        Token { rule: Rule::digits, start: 8, end: 9 }
    ];

    assert!(parser.queue().iter().eq(&queue));
}

#[test]
fn fieldlist() {
    let mut parser = Rdp::new(Box::new(StringInput::new("[1] = 1, [1] = 1, ,;,")));

    assert!(parser.fieldlist());
    assert!(parser.end());

    let queue = vec![
        Token { rule: Rule::fieldlist, start: 0, end: 21 },
        Token { rule: Rule::field, start: 0, end: 7 },
        Token { rule: Rule::exp, start: 1, end: 2 },
        Token { rule: Rule::number, start: 1, end: 2 },
        Token { rule: Rule::int, start: 1, end: 2 },
        Token { rule: Rule::digits, start: 1, end: 2 },
        Token { rule: Rule::exp, start: 6, end: 7 },
        Token { rule: Rule::number, start: 6, end: 7 },
        Token { rule: Rule::int, start: 6, end: 7 },
        Token { rule: Rule::digits, start: 6, end: 7 },
        Token { rule: Rule::field, start: 9, end: 16 },
        Token { rule: Rule::exp, start: 10, end: 11 },
        Token { rule: Rule::number, start: 10, end: 11 },
        Token { rule: Rule::int, start: 10, end: 11 },
        Token { rule: Rule::digits, start: 10, end: 11 },
        Token { rule: Rule::exp, start: 15, end: 16 },
        Token { rule: Rule::number, start: 15, end: 16 },
        Token { rule: Rule::int, start: 15, end: 16 },
        Token { rule: Rule::digits, start: 15, end: 16 }
    ];

    assert!(parser.queue().iter().eq(&queue));
}

#[test]
fn field1() {
    let mut parser = Rdp::new(Box::new(StringInput::new("[1] = 1")));

    assert!(parser.field());
    assert!(parser.end());

    let queue = vec![
        Token { rule: Rule::field, start: 0, end: 7 },
        Token { rule: Rule::exp, start: 1, end: 2 },
        Token { rule: Rule::number, start: 1, end: 2 },
        Token { rule: Rule::int, start: 1, end: 2 },
        Token { rule: Rule::digits, start: 1, end: 2 },
        Token { rule: Rule::exp, start: 6, end: 7 },
        Token { rule: Rule::number, start: 6, end: 7 },
        Token { rule: Rule::int, start: 6, end: 7 },
        Token { rule: Rule::digits, start: 6, end: 7 }
    ];

    assert!(parser.queue().iter().eq(&queue));
}

#[test]
fn field2() {
    let mut parser = Rdp::new(Box::new(StringInput::new("a = 1")));

    assert!(parser.field());
    assert!(parser.end());

    let queue = vec![
        Token { rule: Rule::field, start: 0, end: 5 },
        Token { rule: Rule::name, start: 0, end: 1 },
        Token { rule: Rule::exp, start: 4, end: 5 },
        Token { rule: Rule::number, start: 4, end: 5 },
        Token { rule: Rule::int, start: 4, end: 5 },
        Token { rule: Rule::digits, start: 4, end: 5 }
    ];

    assert!(parser.queue().iter().eq(&queue));
}

#[test]
fn field3() {
    let mut parser = Rdp::new(Box::new(StringInput::new("1")));

    assert!(parser.field());
    assert!(parser.end());

    let queue = vec![
        Token { rule: Rule::field, start: 0, end: 1 },
        Token { rule: Rule::exp, start: 0, end: 1 },
        Token { rule: Rule::number, start: 0, end: 1 },
        Token { rule: Rule::int, start: 0, end: 1 },
        Token { rule: Rule::digits, start: 0, end: 1 }
    ];

    assert!(parser.queue().iter().eq(&queue));
}

#[test]
fn name() {
    let mut parser = Rdp::new(Box::new(StringInput::new("__hell0")));

    assert!(parser.name());
    assert!(parser.end());

    let queue = vec![
        Token { rule: Rule::name, start: 0, end: 7 }
    ];

    assert!(parser.queue().iter().eq(&queue));
}

#[test]
fn string_normal() {
    let mut parser = Rdp::new(Box::new(StringInput::new("\"a \\b\\n\\099\\xfF\"")));

    assert!(parser.string());
    assert!(parser.end());

    let queue = vec![
        Token { rule: Rule::string, start: 0, end: 16 },
        Token { rule: Rule::normalstring, start: 0, end: 16 },
        Token { rule: Rule::escape_sequence, start: 3, end: 5 },
        Token { rule: Rule::escape_sequence, start: 5, end: 7 },
        Token { rule: Rule::escape_sequence, start: 7, end: 11 },
        Token { rule: Rule::decimal_escape, start: 7, end: 11 },
        Token { rule: Rule::escape_sequence, start: 11, end: 15 },
        Token { rule: Rule::hex_escape, start: 11, end: 15 }
    ];

    assert!(parser.queue().iter().eq(&queue));
}

#[test]
fn string_char() {
    let mut parser = Rdp::new(Box::new(StringInput::new("'a \\b\\n\\099\\xfF'")));

    assert!(parser.string());
    assert!(parser.end());

    let queue = vec![
        Token { rule: Rule::string, start: 0, end: 16 },
        Token { rule: Rule::charstring, start: 0, end: 16 },
        Token { rule: Rule::escape_sequence, start: 3, end: 5 },
        Token { rule: Rule::escape_sequence, start: 5, end: 7 },
        Token { rule: Rule::escape_sequence, start: 7, end: 11 },
        Token { rule: Rule::decimal_escape, start: 7, end: 11 },
        Token { rule: Rule::escape_sequence, start: 11, end: 15 },
        Token { rule: Rule::hex_escape, start: 11, end: 15 }
    ];

    assert!(parser.queue().iter().eq(&queue));
}

#[test]
fn string_long() {
    let mut parser = Rdp::new(Box::new(StringInput::new("[==[a d]==]")));

    assert!(parser.string());
    assert!(parser.end());

    let queue = vec![
        Token { rule: Rule::string, start: 0, end: 11 },
        Token { rule: Rule::longstring, start: 0, end: 11 }
    ];

    assert!(parser.queue().iter().eq(&queue));
}

#[test]
fn int() {
    let mut parser = Rdp::new(Box::new(StringInput::new("00845")));

    assert!(parser.number());
    assert!(parser.end());

    let queue = vec![
        Token { rule: Rule::number, start: 0, end: 5 },
        Token { rule: Rule::int, start: 0, end: 5 },
        Token { rule: Rule::digits, start: 0, end: 5 }
    ];

    assert!(parser.queue().iter().eq(&queue));
}

#[test]
fn float_digits_point_digits() {
    let mut parser = Rdp::new(Box::new(StringInput::new("10.01")));

    assert!(parser.number());
    assert!(parser.end());

    let queue = vec![
        Token { rule: Rule::number, start: 0, end: 5 },
        Token { rule: Rule::float, start: 0, end: 5 },
        Token { rule: Rule::digits, start: 0, end: 2 },
        Token { rule: Rule::digits, start: 3, end: 5 }
    ];

    assert!(parser.queue().iter().eq(&queue));
}

#[test]
fn float_digits_point_exp() {
    let mut parser = Rdp::new(Box::new(StringInput::new("01.E-1")));

    assert!(parser.number());
    assert!(parser.end());

    let queue = vec![
        Token { rule: Rule::number, start: 0, end: 6 },
        Token { rule: Rule::float, start: 0, end: 6 },
        Token { rule: Rule::digits, start: 0, end: 2 },
        Token { rule: Rule::exponent, start: 3, end: 6 },
        Token { rule: Rule::digits, start: 5, end: 6 }
    ];

    assert!(parser.queue().iter().eq(&queue));
}

#[test]
fn float_point_digits_exp() {
    let mut parser = Rdp::new(Box::new(StringInput::new(".02e-4")));

    assert!(parser.number());
    assert!(parser.end());

    let queue = vec![
        Token { rule: Rule::number, start: 0, end: 6 },
        Token { rule: Rule::float, start: 0, end: 6 },
        Token { rule: Rule::digits, start: 1, end: 3 },
        Token { rule: Rule::exponent, start: 3, end: 6 },
        Token { rule: Rule::digits, start: 5, end: 6 }
    ];

    assert!(parser.queue().iter().eq(&queue));
}

#[test]
fn float_digits_exp() {
    let mut parser = Rdp::new(Box::new(StringInput::new("9e-01")));

    assert!(parser.number());
    assert!(parser.end());

    let queue = vec![
        Token { rule: Rule::number, start: 0, end: 5 },
        Token { rule: Rule::float, start: 0, end: 5 },
        Token { rule: Rule::digits, start: 0, end: 1 },
        Token { rule: Rule::exponent, start: 1, end: 5 },
        Token { rule: Rule::digits, start: 3, end: 5 }
    ];

    assert!(parser.queue().iter().eq(&queue));
}

#[test]
fn hex() {
    let mut parser = Rdp::new(Box::new(StringInput::new("0x0aF")));

    assert!(parser.number());
    assert!(parser.end());

    let queue = vec![
        Token { rule: Rule::number, start: 0, end: 5 },
        Token { rule: Rule::hex, start: 0, end: 5 },
        Token { rule: Rule::hex_digits, start: 2, end: 5 }
    ];

    assert!(parser.queue().iter().eq(&queue));
}

#[test]
fn hex_float_digits_point_digits() {
    let mut parser = Rdp::new(Box::new(StringInput::new("0x0a.F1")));

    assert!(parser.number());
    assert!(parser.end());

    let queue = vec![
        Token { rule: Rule::number, start: 0, end: 7 },
        Token { rule: Rule::hex_float, start: 0, end: 7 },
        Token { rule: Rule::hex_digits, start: 2, end: 4 },
        Token { rule: Rule::hex_digits, start: 5, end: 7 }
    ];

    assert!(parser.queue().iter().eq(&queue));
}

#[test]
fn hex_float_digits_point_exp() {
    let mut parser = Rdp::new(Box::new(StringInput::new("0x0a.P-2")));

    assert!(parser.number());
    assert!(parser.end());

    let queue = vec![
        Token { rule: Rule::number, start: 0, end: 8 },
        Token { rule: Rule::hex_float, start: 0, end: 8 },
        Token { rule: Rule::hex_digits, start: 2, end: 4 },
        Token { rule: Rule::hex_exponent, start: 5, end: 8 },
        Token { rule: Rule::digits, start: 7, end: 8 }
    ];

    assert!(parser.queue().iter().eq(&queue));
}

#[test]
fn hex_float_point_digits_exp() {
    let mut parser = Rdp::new(Box::new(StringInput::new("0x.afp-2")));

    assert!(parser.number());
    assert!(parser.end());

    let queue = vec![
        Token { rule: Rule::number, start: 0, end: 8 },
        Token { rule: Rule::hex_float, start: 0, end: 8 },
        Token { rule: Rule::hex_digits, start: 3, end: 5 },
        Token { rule: Rule::hex_exponent, start: 5, end: 8 },
        Token { rule: Rule::digits, start: 7, end: 8 }
    ];

    assert!(parser.queue().iter().eq(&queue));
}

#[test]
fn hex_float_digits_exp() {
    let mut parser = Rdp::new(Box::new(StringInput::new("0x9ep-01")));

    assert!(parser.number());
    assert!(parser.end());

    let queue = vec![
        Token { rule: Rule::number, start: 0, end: 8 },
        Token { rule: Rule::hex_float, start: 0, end: 8 },
        Token { rule: Rule::hex_digits, start: 2, end: 4 },
        Token { rule: Rule::hex_exponent, start: 4, end: 8 },
        Token { rule: Rule::digits, start: 6, end: 8 }
    ];

    assert!(parser.queue().iter().eq(&queue));
}

#[test]
fn nested_str1_empty() {
    let mut parser = Rdp::new(Box::new(StringInput::new("=[]=")));

    assert!(parser.nested_str());
    assert!(parser.end());

    let queue = vec![];

    assert!(parser.queue().iter().eq(&queue));
}

#[test]
fn nested_str1() {
    let mut parser = Rdp::new(Box::new(StringInput::new("=[asd asd]=")));

    assert!(parser.nested_str());
    assert!(parser.end());

    let queue = vec![];

    assert!(parser.queue().iter().eq(&queue));
}

#[test]
fn nested_str2_empty() {
    let mut parser = Rdp::new(Box::new(StringInput::new("==[]==")));

    assert!(parser.nested_str());
    assert!(parser.end());

    let queue = vec![];

    assert!(parser.queue().iter().eq(&queue));
}

#[test]
fn nested_str2() {
    let mut parser = Rdp::new(Box::new(StringInput::new("==[asd asd]==")));

    assert!(parser.nested_str());
    assert!(parser.end());

    let queue = vec![];

    assert!(parser.queue().iter().eq(&queue));
}

#[test]
fn nested_str2_spaced() {
    let mut parser = Rdp::new(Box::new(StringInput::new("  =  = [asd asd] ==")));

    assert!(parser.nested_str());
    assert!(parser.end());

    let queue = vec![];

    assert!(parser.queue().iter().eq(&queue));
}

#[test]
fn comment() {
    let mut parser = Rdp::new(Box::new(StringInput::new("-- [ [simple] ]")));

    assert!(parser.comment());
    assert!(parser.end());

    let queue = vec![];

    assert!(parser.queue().iter().eq(&queue));
}

#[test]
fn line_comment1() {
    let mut parser = Rdp::new(Box::new(StringInput::new("--")));

    assert!(parser.comment());
    assert!(parser.end());

    let queue = vec![];

    assert!(parser.queue().iter().eq(&queue));
}

#[test]
fn line_comment2() {
    let mut parser = Rdp::new(Box::new(StringInput::new("--[=====")));

    assert!(parser.comment());
    assert!(parser.end());

    let queue = vec![];

    assert!(parser.queue().iter().eq(&queue));
}

#[test]
fn line_comment3() {
    let mut parser = Rdp::new(Box::new(StringInput::new("--[=====hi")));

    assert!(parser.comment());
    assert!(parser.end());

    let queue = vec![];

    assert!(parser.queue().iter().eq(&queue));
}

#[test]
fn line_comment4() {
    let mut parser = Rdp::new(Box::new(StringInput::new("--hi")));

    assert!(parser.comment());
    assert!(parser.end());

    let queue = vec![];

    assert!(parser.queue().iter().eq(&queue));
}

#[test]
fn shebang() {
    let mut parser = Rdp::new(Box::new(StringInput::new("#!/bin/not-lua")));

    assert!(parser.comment());
    assert!(parser.end());

    let queue = vec![];

    assert!(parser.queue().iter().eq(&queue));
}
