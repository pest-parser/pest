// pest. The Elegant Parser
// Copyright (c) 2018 Dragoș Tiselice
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

//! Adapted from [generator.rs](./generator.rs).

use crate::docs::DocComment;
use crate::generator::{generate_enum, generate_include};
use crate::graph::generate_typed_pair_from_rule;
use crate::types::result_type;
use crate::{collect_data, get_attribute, GrammarSource};
use pest_meta::optimizer::OptimizedRule;
use pest_meta::parser::{self, rename_meta_rule, Rule};
use pest_meta::{optimizer, unwrap_or_report};
use proc_macro2::TokenStream;
use std::path::PathBuf;
use syn::{self, Generics, Ident};
use syn::{Attribute, DeriveInput};

/// Processes the derive/proc macro input and generates the corresponding typed parser based
/// on the parsed grammar. If `include_grammar` is set to true, it'll generate an explicit
/// "include_str" statement (done in pest_derive, but turned off in the local bootstrap).
pub fn derive_typed_parser(input: TokenStream, include_grammar: bool) -> TokenStream {
    let ast: DeriveInput = syn::parse2(input).unwrap();
    let (name, generics, contents, emit_rule_reference, emit_tagged_node_reference) =
        parse_typed_derive(ast);

    let (data, paths) = collect_data(contents);

    let pairs = match parser::parse(Rule::grammar_rules, &data) {
        Ok(pairs) => pairs,
        Err(error) => panic!("error parsing \n{}", error.renamed_rules(rename_meta_rule)),
    };

    let doc_comment = crate::docs::consume(pairs.clone());
    let ast = unwrap_or_report(parser::consume_rules(pairs));
    let optimized = optimizer::optimize(ast);

    generate_typed(
        name,
        &generics,
        paths,
        optimized,
        &doc_comment,
        include_grammar,
        emit_rule_reference,
        emit_tagged_node_reference,
    )
}

fn parse_typed_derive(ast: DeriveInput) -> (Ident, Generics, Vec<GrammarSource>, bool, bool) {
    let name = ast.ident;
    let generics = ast.generics;

    let grammar: Vec<&Attribute> = ast
        .attrs
        .iter()
        .filter(|attr| {
            let path = attr.meta.path();
            path.is_ident("grammar") || path.is_ident("grammar_inline")
        })
        .collect();

    if grammar.is_empty() {
        panic!("a grammar file needs to be provided with the #[grammar = \"PATH\"] or #[grammar_inline = \"GRAMMAR CONTENTS\"] attribute");
    }

    let mut grammar_sources = Vec::with_capacity(grammar.len());
    for attr in grammar {
        grammar_sources.push(get_attribute(attr))
    }

    let mut emit_rule_reference = false;
    let mut emit_tagged_node_reference = false;
    for attr in ast.attrs.iter() {
        let path = attr.path();
        if path.is_ident("emit_rule_reference") {
            emit_rule_reference = true;
        }
        if path.is_ident("emit_tagged_node_reference") {
            emit_tagged_node_reference = true;
        }
    }
    (
        name,
        generics,
        grammar_sources,
        emit_rule_reference,
        emit_tagged_node_reference,
    )
}

/// Generate codes for Parser.
fn generate_typed(
    name: Ident,
    generics: &Generics,
    paths: Vec<PathBuf>,
    rules: Vec<OptimizedRule>,
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
