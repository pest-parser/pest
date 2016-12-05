use proc_macro::TokenStream;
use quote::Tokens;

use super::ast::*;

pub fn generate(name: &str, rules: Vec<Rule>) -> Tokens {
    quote! {
        struct #name;
    }
}