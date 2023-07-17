// pest. The Elegant Parser
// Copyright (c) 2018 Dragoș Tiselice
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

use crate::RuleType;

/// An object containing a constant.
pub trait Storage<T> {
    /// Get contained string.
    fn get_content(&self) -> T;
}

/// A wrapper for string as a generics argument.
pub trait StringWrapper {
    /// Wrapped string.
    const CONTENT: &'static str;
}
impl<T: StringWrapper> Storage<&'static str> for T {
    fn get_content(&self) -> &'static str {
        Self::CONTENT
    }
}

/// A wrapper for string array as a generics argument.
pub trait StringArrayWrapper {
    const CONTENT: &'static [&'static str];
}
impl<T: StringArrayWrapper> Storage<&'static [&'static str]> for T {
    fn get_content(&self) -> &'static [&'static str] {
        Self::CONTENT
    }
}

/// Rule wrapper.
pub trait RuleWrapper<R: RuleType> {
    /// Wrapped rule.
    const RULE: R;
}

/// Type wrapper
pub trait TypeWrapper {
    type Inner;
}
