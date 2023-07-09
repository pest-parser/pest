// pest. The Elegant Parser
// Copyright (c) 2018 Dragoș Tiselice
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

use crate::types::result_type;
use pest_meta::{ast::RuleType, optimizer::OptimizedRule};
use proc_macro2::{Ident, TokenStream};
pub use std::collections::BTreeMap as Map;

use crate::optimizer::OptimizedExpr;

fn ident(s: &str) -> Ident {
    format_ident!("r#{}", s)
}

fn fn_decl() -> TokenStream {
    let result = result_type();
    quote! {
        #[inline]
        #[allow(unused_variables)]
        fn try_new(
            input: ::pest::Position<'i>,
            stack: &mut ::pest::Stack<::pest::Span<'i>>
        ) -> #result<(::pest::Position<'i>, Self), ::pest::error::Error<super::Rule>>
    }
}

fn attributes() -> TokenStream {
    // quote! {#[allow(non_snake_case)]}
    quote! {
        #[derive(Debug)]
    }
}

struct Output {
    content: Vec<TokenStream>,
    wrappers: Vec<TokenStream>,
}
impl Output {
    fn new() -> Self {
        Self {
            content: Vec::new(),
            wrappers: Vec::new(),
        }
    }
    fn insert(&mut self, tokens: TokenStream) {
        self.content.push(tokens);
    }
    fn insert_wrapper(&mut self, tokens: TokenStream) {
        self.wrappers.push(tokens);
    }
    fn collect(&self) -> TokenStream {
        let content = &self.content;
        let wrappers = &self.wrappers;
        quote! {
            #(#content)*
            pub mod __pest_string_wrapper {
                #(#wrappers)*
            }
        }
    }
}

fn process_single(
    map: &mut Output,
    candidate_name: String,
    type_name: TokenStream,
    fimpl: TokenStream,
    doc: &str,
    silent: bool,
) -> TokenStream {
    let f = fn_decl();
    let name = ident(&candidate_name);
    let inner = type_name.clone();
    let fn_def = if silent {
        quote! {
            #fimpl;
            Ok((input, Self { _phantom: ::core::marker::PhantomData }))
        }
    } else {
        quote! {
            #fimpl;
            Ok((input, Self { span, content }))
        }
    };
    let fields = if silent {
        quote! {
            _phantom: ::core::marker::PhantomData<::pest::Span<'i>>
        }
    } else {
        quote! {
            pub span: ::pest::Span<'i>,
            pub content: #inner,
        }
    };
    let debug = if silent {
        quote! {
            impl<'i> ::core::fmt::Debug for #name<'i> {
                fn fmt(&self, f: &mut ::core::fmt::Formatter<'_>) -> ::core::fmt::Result {
                    f.debug_struct(#candidate_name)
                        .finish()
                }
            }
        }
    } else {
        quote! {
            impl<'i> ::core::fmt::Debug for #name<'i> {
                fn fmt(&self, f: &mut ::core::fmt::Formatter<'_>) -> ::core::fmt::Result {
                    f.debug_struct(#candidate_name)
                        .field("span", &self.span)
                        .field("content", &self.content)
                        .finish()
                }
            }
        }
    };
    let def = quote! {
        #[doc = #doc]
        pub struct #name<'i> {
            #fields
        }
        impl<'i> ::pest::iterators::TypedNode<'i, super::Rule> for #name<'i> {
            #f {
                #fn_def
            }
        }
        #debug
    };
    map.insert(def);
    quote! {
        #name::<'i>
    }
}

/// Returns flle name.
fn process_single_alias(
    map: &mut Output,
    candidate_name: String,
    type_name: TokenStream,
    explicit: bool,
) -> TokenStream {
    let name = ident(&candidate_name);
    if explicit {
        let def = quote! {pub type #name<'i> = #type_name;};
        map.insert(def);
        quote! {#name::<'i>}
    } else {
        type_name
    }
}

/// Returns type name
fn generate_graph_node(
    expr: &OptimizedExpr,
    candidate_name: String,
    // From node name to type definition and implementation
    map: &mut Output,
    explicit: bool,
    inner_spaces: bool,
    inner_tokens: bool,
    silent: bool,
) -> TokenStream {
    macro_rules! walk_tree {
        ($ivar: ident, $ovar: ident) => {{
            let mut nodes: Vec<&OptimizedExpr> = Vec::new();
            let mut names: Vec<TokenStream> = Vec::new();
            let mut i = 0usize;
            let mut current = expr;
            while let OptimizedExpr::$ivar(lhs, rhs) = current {
                nodes.push(lhs);
                names.push(generate_graph_node(
                    lhs,
                    format!("{}_{}", candidate_name, i),
                    map,
                    false,
                    inner_spaces,
                    inner_tokens,
                    silent,
                ));
                current = rhs;
                i += 1;
            }
            nodes.push(current);
            names.push(generate_graph_node(
                current,
                format!("{}_{}", candidate_name, i),
                map,
                false,
                inner_spaces,
                inner_tokens,
                silent,
            ));
            let name = ident(&candidate_name);
            (nodes, names, quote! {#name::<'i>})
        }};
    }

    let f = fn_decl();
    let attr = attributes();

    let spaces = if inner_spaces {
        quote! {
            let (next, _) = ::pest::iterators::predefined_node::Ign::<'i, super::Rule, WHITESPACE, COMMENT>::new(input, stack);
            input = next;
        }
    } else {
        quote! {}
    };

    // Still some compile-time information not taken
    match expr {
        OptimizedExpr::Str(content) => {
            let wrapper = format_ident!("r#{}", candidate_name);
            map.insert_wrapper(quote! {
                pub struct #wrapper();
                impl ::pest::iterators::predefined_node::StringWrapper for #wrapper {
                    const CONTENT: &'static str = #content;
                }
            });
            process_single_alias(
                map,
                candidate_name,
                quote! {
                    ::pest::iterators::predefined_node::Str::<'i, super::Rule, __pest_string_wrapper::#wrapper>
                },
                explicit,
            )
        }
        OptimizedExpr::Insens(content) => {
            let wrapper = format_ident!("__pest__string_wrapper_{}", candidate_name);
            map.insert_wrapper(quote! {
                pub struct #wrapper();
                impl ::pest::iterators::predefined_node::StringWrapper for #wrapper {
                    const CONTENT: &'static str = #content;
                }
            });
            process_single_alias(
                map,
                candidate_name,
                quote! {
                    ::pest::iterators::predefined_node::Insens::<'i, super::Rule, __pest_string_wrapper::#wrapper>
                },
                explicit,
            )
        }
        OptimizedExpr::PeekSlice(start, end) => process_single(
            map,
            candidate_name,
            quote! {()},
            quote! {
                let (input, span) = ::pest::iterators::predefined_node::peek_stack_slice::<super::Rule>(input, #start, #end, stack)?;
                let content = ();
            },
            &match end {
                Some(end) => format!("Match {}..{} of the stack", start, end),
                None => format!("Match {}.. of the stack", start),
            },
            silent,
        ),
        OptimizedExpr::Push(expr) => {
            let inner = generate_graph_node(
                expr,
                format! {"{}_p", candidate_name},
                map,
                false,
                inner_spaces,
                inner_tokens,
                silent,
            );
            process_single(
                map,
                candidate_name,
                quote! {()},
                quote! {
                    let start = input.clone();
                    let (input, res) = #inner::try_new(input, stack)?;
                    let span = start.span(&input);
                    let content = ();
                    stack.push(span);
                },
                "Matches an expression and pushes it to the stack.",
                silent,
            )
        }
        OptimizedExpr::Skip(strings) => process_single(
            map,
            candidate_name,
            quote! {()},
            quote!(
                let (input, span) = ::pest::iterators::predefined_node::skip_until::<super::Rule>(input, &[#(#strings),*])?;
                let content = ();
            ),
            &format!(
                "Continues to match expressions until one of the strings in {:?} is found.",
                strings
            ),
            silent,
        ),
        OptimizedExpr::Range(start, end) => {
            let start = start.chars().next().unwrap();
            let end = end.chars().next().unwrap();
            process_single_alias(
                map,
                candidate_name,
                quote! {
                    ::pest::iterators::predefined_node::Range::<'i, super::Rule, #start, #end>
                },
                explicit,
            )
        }
        OptimizedExpr::Ident(id) => {
            let inner = ident(id);
            process_single_alias(
                map,
                candidate_name,
                quote! {::pest::iterators::predefined_node::Box::<'i, super::Rule, #inner::<'i>>},
                explicit,
            )
        }
        OptimizedExpr::PosPred(expr) => {
            let inner = generate_graph_node(
                expr,
                format! {"{}_P", candidate_name},
                map,
                false,
                inner_spaces,
                inner_tokens,
                silent,
            );
            process_single_alias(
                map,
                candidate_name,
                quote! {
                    ::pest::iterators::predefined_node::Positive::<'i, super::Rule, #inner>
                },
                explicit,
            )
        }
        OptimizedExpr::NegPred(expr) => {
            let inner = generate_graph_node(
                expr,
                format! {"{}_N", candidate_name},
                map,
                false,
                inner_spaces,
                inner_tokens,
                silent,
            );
            process_single_alias(
                map,
                candidate_name,
                quote! {
                    ::pest::iterators::predefined_node::Negative::<'i, super::Rule, #inner>
                },
                explicit,
            )
        }
        OptimizedExpr::RestoreOnErr(expr) => {
            let inner = generate_graph_node(
                expr,
                format! {"{}_E", candidate_name},
                map,
                false,
                inner_spaces,
                inner_tokens,
                silent,
            );
            process_single_alias(
                map,
                candidate_name,
                quote! {
                    ::pest::iterators::predefined_node::Restorable::<'i, super::Rule, #inner>
                },
                explicit,
            )
        }
        OptimizedExpr::Seq(_lhs, _rhs) => {
            let (_nodes, names, res) = walk_tree!(Seq, Sequence);
            let name = ident(&candidate_name);
            // eprintln!("{} contains {:?}", candidate_name, names);
            let (init, fields): (Vec<_>, Vec<_>) = names
                .iter()
                .enumerate()
                .map(|(i, name)| {
                    let field = format_ident!("r#field{}", i);
                    (
                        quote! {
                            // eprintln!("Matching {}.", core::any::type_name::<#name>());
                            let (remained, #field) = match #name::try_new(input, stack) {
                                Ok(res) => res,
                                Err(err) => {
                                    let message = ::pest::iterators::predefined_node::stack_error(err);
                                    return Err(::pest::error::Error::new_from_pos(
                                        ::pest::error::ErrorVariant::CustomError {
                                            message: format!(
                                                "Sequence {} failed in {}-th elements: \n{}",
                                                ::core::any::type_name::<Self>(),
                                                #i,
                                                message,
                                            )
                                        }, input
                                    ))
                                }
                            };
                            input = remained;
                            // eprintln!("Matched {}.", core::any::type_name::<#name>());
                        },
                        field,
                    )
                })
                .unzip();
            let mut inits = Vec::<TokenStream>::new();
            for (i, init) in init.into_iter().enumerate() {
                if i != 0 {
                    inits.push(spaces.clone());
                }
                inits.push(init);
            }
            let doc = format!("Sequence.",);
            let def = quote! {
                #[doc = #doc]
                #attr
                pub struct #name<'i> {
                    pub span: ::pest::Span::<'i>,
                    #(pub #fields: #names),*
                }
                impl<'i> ::pest::iterators::TypedNode<'i, super::Rule> for #name<'i> {
                    #f {
                        let start = input.clone();
                        let mut input = input;
                        #(#inits)*
                        let span = start.span(&input);
                        Ok( (input, Self { span, #(#fields),* }) )
                    }
                }
            };
            // println!("{}", def);
            map.insert(def);

            res
        }
        OptimizedExpr::Choice(_lhs, _rhs) => {
            let (_nodes, names, res) = walk_tree!(Choice, Variant);
            let name = ident(&candidate_name);
            let vars = names
                .iter()
                .enumerate()
                .map(|(i, _n)| format_ident!("var_{}", i));
            let init = names.iter().enumerate().map(|(i, var)| {
                let var_name = format_ident!("var_{}", i);
                quote! {
                    match #var::try_new(input, stack) {
                        Ok((input, res)) => {
                            return Ok((input, #name::#var_name(res)));
                        }
                        Err(e) => {
                            errors.push(e);
                        }
                    }
                }
            });
            let doc = format! {"Choices."};
            let def = quote! {
                #[doc = #doc]
                #attr
                pub enum #name<'i> {
                    #( #vars(#names) ),*
                }
                impl<'i> ::pest::iterators::TypedNode<'i, super::Rule> for #name<'i> {
                    #f {
                        let mut errors = vec![];
                        #(#init)*
                        let message = ::pest::iterators::predefined_node::stack_errors(errors);
                        return Err(::pest::error::Error::new_from_pos(
                            ::pest::error::ErrorVariant::CustomError {
                                message: format!(
                                    "Choices {} failed with errors: \n{}",
                                    ::core::any::type_name::<Self>(),
                                    message,
                                )
                            }, input
                        ))
                    }
                }
            };
            map.insert(def);
            res
        }
        OptimizedExpr::Opt(inner) => {
            let inner_name = generate_graph_node(
                inner,
                format!("{}_o", candidate_name),
                map,
                false,
                inner_spaces,
                inner_tokens,
                silent,
            );
            process_single_alias(
                map,
                candidate_name,
                quote! {::pest::iterators::predefined_node::Opt::<'i, super::Rule, #inner_name>},
                explicit,
            )
        }
        OptimizedExpr::Rep(inner) => {
            let name = ident(&candidate_name);
            let inner_name = generate_graph_node(
                inner,
                format!("{}_r", candidate_name),
                map,
                false,
                inner_spaces,
                inner_tokens,
                silent,
            );
            let def = quote! {
                pub type #name<'i> = ::pest::iterators::predefined_node::Rep::<
                    'i,
                    super::Rule,
                    #inner_name,
                    #inner_spaces,
                    ::pest::iterators::predefined_node::Ign::<
                        'i,
                        super::Rule,
                        COMMENT::<'i>,
                        WHITESPACE::<'i>,
                    >
                >;
            };
            map.insert(def);
            quote! {#name::<'i>}
        }
        #[cfg(feature = "grammar-extras")]
        OptimizedExpr::NodeTag(inner_expr, _tag) => {
            generate_graph_node(
                inner_expr,
                format!("{}_0", candidate_name),
                map,
                explicit,
                inner_spaces,
                inner_tokens,
                silent,
            );
            todo!()
        }
    }
}

fn generate_graph(rules: &[OptimizedRule]) -> Output {
    // println!("{:#?}", rules);
    let mut res = Output::new();
    for rule in rules.iter() {
        match rule.ty {
            RuleType::Normal => {
                generate_graph_node(
                    &rule.expr,
                    rule.name.clone(),
                    &mut res,
                    true,
                    true,
                    true,
                    false,
                );
            }
            RuleType::Silent => {
                generate_graph_node(
                    &rule.expr,
                    rule.name.clone(),
                    &mut res,
                    true,
                    true,
                    true,
                    true,
                );
            }
            RuleType::NonAtomic => {
                todo!();
                generate_graph_node(
                    &rule.expr,
                    rule.name.clone(),
                    &mut res,
                    true,
                    true,
                    true,
                    true,
                );
            }
            RuleType::CompoundAtomic => {
                generate_graph_node(
                    &rule.expr,
                    rule.name.clone(),
                    &mut res,
                    true,
                    false,
                    true,
                    false,
                );
            }
            RuleType::Atomic => {
                generate_graph_node(
                    &rule.expr,
                    rule.name.clone(),
                    &mut res,
                    true,
                    false,
                    false,
                    false,
                );
            }
        }
    }
    res
}

pub fn generate_typed_pair_from_rule(rules: &[OptimizedRule]) -> TokenStream {
    // let names: Vec<_> = rules.iter().map(|rule| &rule.name).collect();
    // eprintln!("{:#?}", names);
    let graph = generate_graph(rules);
    let pairs = graph.collect();
    let builtin = generate_builtin();
    // let names = rules.iter().map(|rule| format_ident!("r#{}", rule.name));
    let res = quote! {
        pub mod pairs {
            use pest::iterators::NeverFailedTypedNode as _;
            #builtin

            #pairs
        }
    };
    // println!("{}", res);
    res
}

pub fn generate_builtin() -> TokenStream {
    quote! {
        use ::pest::iterators::predefined_node::{ANY, SOI, EOI, NEWLINE, PEEK_ALL, DROP};
    }
}
