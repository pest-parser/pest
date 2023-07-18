// pest. The Elegant Parser
// Copyright (c) 2018 Dragoș Tiselice
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

//! Adapted from [generator.rs](./generator.rs).

use std::path::PathBuf;

use proc_macro2::TokenStream;
use quote::{ToTokens, TokenStreamExt};
use syn::{self, Generics, Ident};

use pest::unicode::unicode_property_names;
use pest_meta::ast::*;
use pest_meta::optimizer::*;

use crate::docs::DocComment;
use crate::generator::{generate_enum, generate_include};
use crate::types::{box_type, option_type, result_type};

use crate::graph::generate_typed_pair_from_rule;

/// Generate codes for Parser.
pub(crate) fn generate_typed(
    name: Ident,
    generics: &Generics,
    paths: Vec<PathBuf>,
    rules: Vec<OptimizedRule>,
    defaults: Vec<&str>,
    doc_comment: &DocComment,
    include_grammar: bool,
    emit_rule_reference: bool,
    emit_tagged_node_reference: bool,
) -> TokenStream {
    let include_fix = if include_grammar {
        generate_include(&name, paths)
    } else {
        quote!()
    };
    let rule_enum = generate_enum(&rules, doc_comment, true);
    let pairs =
        generate_typed_pair_from_rule(&rules, emit_rule_reference, emit_tagged_node_reference);
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    let result = result_type();

    let parser_impl = quote! {
        #[allow(clippy::all)]
        impl #impl_generics ::pest::typed::TypedParser<Rule> for #name #ty_generics #where_clause {
            fn parse<'i, T: ::pest::typed::ParsableTypedNode<'i, Rule>>(
                input: &'i str
            ) -> #result<
                T,
                ::pest::error::Error<Rule>
            > {
                T::parse(input)
            }
        }
    };

    let res = quote! {
        #include_fix
        #rule_enum
        #pairs
        #parser_impl
    };
    res
}
