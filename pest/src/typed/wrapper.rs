// pest. The Elegant Parser
// Copyright (c) 2018 Dragoș Tiselice
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

pub trait ConstWrapper<T: Sized> {
    /// Wrapped content.
    const CONTENT: T;
}

pub trait ConstUnwrap<T: Sized> {
    /// Get wrapped content.
    fn const_unwrap(&self) -> T;
}

impl<T: Sized, U: ConstWrapper<T>> ConstUnwrap<T> for U {
    fn const_unwrap(&self) -> T {
        Self::CONTENT
    }
}

/// A wrapper for string as a generics argument.
pub trait StringWrapper {
    /// Wrapped string.
    const CONTENT: &'static str;
}

impl<T: StringWrapper> ConstWrapper<&'static str> for T {
    const CONTENT: &'static str = T::CONTENT;
}

/// An object containing a string.
pub trait StringStorage {
    /// Get contained string.
    fn get_content(&self) -> &str;
}

impl<T: StringWrapper> StringStorage for T {
    fn get_content(&self) -> &str {
        Self::CONTENT
    }
}
