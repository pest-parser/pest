// pest. The Elegant Parser
// Copyright (c) 2018 Dragoș Tiselice
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

use proc_macro2::TokenStream;

pub fn box_type() -> TokenStream {
    #[cfg(feature = "std")]
    quote! { ::std::boxed::Box }

    #[cfg(not(feature = "std"))]
    quote! { ::alloc::boxed::Box }
}

pub fn result_type() -> TokenStream {
    #[cfg(feature = "std")]
    quote! { ::std::result::Result }

    #[cfg(not(feature = "std"))]
    quote! { ::core::result::Result }
}

pub fn option_type() -> TokenStream {
    #[cfg(feature = "std")]
    quote! { ::std::option::Option }

    #[cfg(not(feature = "std"))]
    quote! { ::core::option::Option }
}

pub fn vec_type() -> TokenStream {
    #[cfg(feature = "std")]
    quote! { ::std::vec::Vec }

    #[cfg(not(feature = "std"))]
    quote! { ::alloc::vec::Vec }
}

pub fn error_type() -> TokenStream {
    #[cfg(feature = "std")]
    quote! { ::std::result::Result }

    #[cfg(not(feature = "std"))]
    quote! { ::core::result::Result }
}
