// pest. The Elegant Parser
// Copyright (c) 2018 Dragoș Tiselice
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

use crate::types::{box_type, option_type, result_type};
use pest_meta::{ast::RuleType, optimizer::OptimizedRule};
use proc_macro2::{Ident, TokenStream};
pub use std::collections::BTreeMap as Map;

use crate::optimizer::OptimizedExpr;

fn ident(s: &str) -> Ident {
    format_ident!("r#{}", s)
}
fn quote_ident(s: &str) -> TokenStream {
    let ident = ident(s);
    quote! {#ident}
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

fn visibility(_explicit: bool) -> TokenStream {
    quote! {pub}
}

fn process_single(
    map: &mut Map<String, TokenStream>,
    candidate_name: String,
    type_name: TokenStream,
    fimpl: TokenStream,
    explicit: bool,
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
    let vis = visibility(explicit);
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
        #vis struct #name<'i> {
            #fields
        }
        impl<'i> ::pest::iterators::TypedNode<'i, super::Rule> for #name<'i> {
            #f {
                #fn_def
            }
        }
        #debug
    };
    map.insert(candidate_name.clone(), def);
    quote! {
        #name::<'i>
    }
}

/// Returns type name
fn generate_graph_node(
    expr: &OptimizedExpr,
    candidate_name: String,
    // From node name to type definition and implementation
    map: &mut Map<String, TokenStream>,
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

    let vis = visibility(explicit);
    let f = fn_decl();
    let attr = attributes();
    let option = option_type();
    let Box = box_type();
    let s = quote!(&'i ::std::primitive::str);

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
        OptimizedExpr::Str(content) => process_single(
            map,
            candidate_name,
            s.clone(),
            quote! {
                let (input, span) = ::pest::iterators::predefined_node::string::<super::Rule>(input, #content)?;
                let content = span.as_str();
            },
            explicit,
            silent,
        ),
        OptimizedExpr::Insens(content) => process_single(
            map,
            candidate_name,
            s.clone(),
            quote! {
                let (input, span) = ::pest::iterators::predefined_node::insensitive::<super::Rule>(input, #content)?;
                let content = span.as_str();
            },
            explicit,
            silent,
        ),
        OptimizedExpr::PeekSlice(start, end) => process_single(
            map,
            candidate_name,
            quote! {()},
            quote! {
                let (input, span) = ::pest::iterators::predefined_node::peek_stack_slice::<super::Rule>(input, #start, #end, stack)?;
                let content = ();
            },
            explicit,
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
                explicit,
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
            explicit,
            silent,
        ),
        OptimizedExpr::Range(start, end) => {
            let start = start.chars().next().unwrap();
            let end = end.chars().next().unwrap();
            process_single(
                map,
                candidate_name,
                quote! {
                    ::std::primitive::char
                },
                quote! {
                    let (input, span, content) = ::pest::iterators::predefined_node::range::<super::Rule>(input, #start, #end)?;
                },
                explicit,
                silent,
            )
        }
        OptimizedExpr::Ident(id) => {
            let name = ident(id);
            if explicit {
                process_single(
                    map,
                    candidate_name,
                    quote! {#Box::<#name::<'i>>},
                    quote! {
                        let start = input.clone();
                        let (input, content) = #name::<'i>::try_new(input, stack)?;
                        let content = #Box::<#name::<'i>>::new(content);
                        let span = start.span(&input);
                    },
                    explicit,
                    silent,
                )
            } else {
                quote! {::pest::iterators::predefined_node::Box::<'i, super::Rule, #name::<'i>>}
            }
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
            process_single(
                map,
                candidate_name,
                quote! {()},
                quote! {
                    let content = ::pest::iterators::predefined_node::positive::<super::Rule, #inner>(input, stack)?;
                    let span = input.span(&input);
                },
                explicit,
                silent,
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
            process_single(
                map,
                candidate_name,
                quote! {()},
                quote! {
                    let content = ::pest::iterators::predefined_node::negative::<super::Rule, #inner>(input, stack)?;
                    let span = input.span(&input);
                },
                explicit,
                silent,
            )
        }
        OptimizedExpr::RestoreOnErr(expr) => {
            let inner = generate_graph_node(
                expr,
                format! {"{}_N", candidate_name},
                map,
                false,
                inner_spaces,
                inner_tokens,
                silent,
            );
            process_single(
                map,
                candidate_name,
                quote! {#option::<#inner>},
                quote! {
                    stack.snapshot();
                    let (input, content) = match #inner::try_new(input, stack) {
                        Ok((input, res)) => {
                            stack.clear_snapshot();
                            (input, Some(res))
                        }
                        Err(_) => {
                            stack.restore();
                            (input, None)
                        }
                    };
                    let span = input.span(&input);
                },
                explicit,
                silent,
            )
        }
        OptimizedExpr::Seq(_lhs, _rhs) => {
            let (_nodes, names, res) = walk_tree!(Seq, Sequence);
            let name = ident(&candidate_name);
            let (init, fields): (Vec<_>, Vec<_>) = names
                .iter()
                .enumerate()
                .map(|(i, name)| {
                    let field = format_ident!("r#field{}", i);
                    (
                        quote! {
                            // eprintln!("Matching {}.", core::any::type_name::<#name>());
                            let (remained, #field) = #name::try_new(input, stack)?;
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
            let def = quote! {
                #attr
                #vis struct #name<'i> {
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
            map.entry(candidate_name.clone()).or_insert(def);

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
            let def = quote! {
                #attr
                #vis enum #name<'i> {
                    #( #vars(#names) ),*
                }
                impl<'i> ::pest::iterators::TypedNode<'i, super::Rule> for #name<'i> {
                    #f {
                        let mut errors = vec![];
                        #(#init)*
                        let messages: Vec<_> = errors.into_iter().map(|e|format!("{}", e)).collect();
                        let message = messages.join("\n");
                        return Err(::pest::error::Error::new_from_pos(
                            ::pest::error::ErrorVariant::CustomError {
                                message: format!("Choices failed with errors: {}", message)
                            }, input
                        ))
                    }
                }
            };
            map.entry(candidate_name.clone()).or_insert(def);
            res
        }
        OptimizedExpr::Opt(inner) => {
            let name = ident(&candidate_name);
            let inner_name = generate_graph_node(
                inner,
                format!("{}_o", candidate_name),
                map,
                false,
                inner_spaces,
                inner_tokens,
                silent,
            );
            let def = quote! {
                pub type #name<'i> = ::pest::iterators::predefined_node::Opt::<'i, super::Rule, #inner_name>;
            };
            map.entry(candidate_name.clone()).or_insert(def);
            quote! {#name::<'i>}
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
            map.entry(candidate_name.clone()).or_insert(def);
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

pub fn generate_graph(rules: &[OptimizedRule]) -> Map<String, TokenStream> {
    // println!("{:#?}", rules);
    let mut res = Map::<String, TokenStream>::new();
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
    let pairs = graph.iter().map(|(_name, rule)| rule);
    let builtin = generate_builtin();
    // let names = rules.iter().map(|rule| format_ident!("r#{}", rule.name));
    let res = quote! {
        pub mod pairs {
            use pest::iterators::NeverFailedTypedNode as _;
            #builtin

            #( #pairs )*
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
