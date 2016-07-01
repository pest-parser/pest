// pest. Elegant, efficient grammars
// Copyright (C) 2016  Dragoș Tiselice
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use pest::prelude::*;

impl_rdp! {
    grammar! {
        file = _{ soi ~ (!macro_impl_rdp ~ any)* ~ rdp ~ any* ~ eoi }

        rdp    = { macro_impl_rdp ~ block_start ~ macros ~ block_end }
        macros = { grammar ~ process? }

        grammar         = { macro_grammar ~ block_start ~ definition* ~ block_end }
        definition      = {
            name ~ assign ~ modifier? ~ block_start ~ (expression | prec_rule) ~ block_end
        }
        prec_rule       = { block_start ~ (expression | prec_rule) ~ block_end ~ prec_definition* }
        prec_definition = {
            name ~ assign ~ modifier? ~ block_start ~ ["<"]? ~ (expression | prec_rule) ~ block_end
        }
        modifier        = { ["_"] | ["@"] }

        process = { macro_process ~ block_start ~ (!block_end ~ any)* ~ block_end }

        expression = {
            { prefix* ~ (name | terminal | ["("] ~ expression ~ [")"]) ~ postfix* }
            choice   = { op_choice }
            sequence = { op_sequence }
        }

        name  = @{ (alpha | ["_"]) ~ (alpha | ["_"] | digit)* }
        alpha = _{ ['a'..'z'] | ['A'..'Z'] }
        digit = _{ ['0'..'9'] }

        terminal  =  { terminal_start ~ (string | range) ~ terminal_end }
        string    = @{ ["\""] ~ (["\\"] ~ any | !["\""] ~ any )* ~ ["\""] }
        range     =  { character ~ [".."] ~ character }
        character = @{ ["'"]  ~ (["\\"] ~ any | !["'"]  ~ any )* ~ ["'"] }

        prefix  = { ["&"] | ["!"] }
        postfix = { ["?"] | ["*"] | ["+"] }

        op_choice   = { ["|"] }
        op_sequence = { ["~"] }

        macro_impl_rdp = { ["impl_rdp!"] }
        macro_grammar  = { ["grammar!"] }
        macro_process  = { ["process!"] }

        block_start    = { ["{"] }
        block_end      = { ["}"] }
        terminal_start = { ["["] }
        terminal_end   = { ["]"] }
        assign         = { ["="] }

        whitespace = _{ [" "] | ["\n"] | ["\t"] }
        comment    = _{
            ["//"] ~ (!["\n"] ~ any)* | ["/*"] ~ (!["*/"] ~ any)* ~ ["*/"]
        }
    }
}
