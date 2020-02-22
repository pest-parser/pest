// pest. The Elegant Parser
// Copyright (c) 2018 Dragoș Tiselice
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

macro_rules! insert_builtin {
    ($builtin: expr, $name: ident, $pattern: expr) => {
        $builtin.push((stringify!($name), generate_rule!($name, $pattern)));
    };
}

macro_rules! insert_public_builtin {
    ($builtin: expr, $name: ident, $pattern: expr) => {
        $builtin.push((stringify!($name), generate_public_rule!($name, $pattern)));
    };
}

macro_rules! generate_rule {
    ($name: ident, $pattern: expr) => {
        quote! {
            #[inline]
            #[allow(dead_code, non_snake_case, unused_variables)]
            pub fn $name(state: Box<::pest::ParserState<Rule>>) -> ::pest::ParseResult<Box<::pest::ParserState<Rule>>> {
                $pattern
            }
        }
    }
}

macro_rules! generate_public_rule {
    ($name: ident, $pattern: expr) => {
        quote! {
            #[inline]
            #[allow(dead_code, non_snake_case, unused_variables)]
            pub fn $name(state: Box<::pest::ParserState<Rule>>) -> ::pest::ParseResult<Box<::pest::ParserState<Rule>>> {
                $pattern
            }
        }
    }
}
