// pest. The Elegant Parser
// Copyright (C) 2017  Dragoș Tiselice
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::convert::AsRef;
use std::ffi::OsString;
use std::fs::File;
use std::io::{self, Read};
use std::ops::Range;
use std::path::Path;

use super::{Input, StringInput};

/// A `struct` useful for matching `File`s by allocating the contents at the beginning.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct FileInput {
    input: StringInput,
    file_name: OsString
}

impl FileInput {
    /// Creates a new `FileInput` from a `File`.
    ///
    /// # Examples
    ///
    /// ```no_run
    /// # use pest::inputs::FileInput;
    /// FileInput::new("file").unwrap();
    /// ```
    pub fn new<P: AsRef<Path>>(path: P) -> io::Result<FileInput> {
        let mut file = File::open(path.as_ref())?;
        let mut string = String::new();
        file.read_to_string(&mut string)?;

        let input = StringInput::new(string);
        let file_name = path.as_ref().file_name().unwrap().to_os_string();

        Ok(FileInput { input, file_name })
    }
}

impl Input for FileInput {
    #[inline]
    fn len(&self) -> usize {
        self.input.len()
    }

    #[inline]
    fn is_empty(&self) -> bool {
        self.input.is_empty()
    }

    #[inline]
    fn file_name(&self) -> Option<OsString> {
        Some(self.file_name.clone())
    }

    #[inline]
    unsafe fn slice(&self, start: usize, end: usize) -> &str {
        self.input.slice(start, end)
    }

    #[inline]
    unsafe fn line_col(&self, pos: usize) -> (usize, usize) {
        self.input.line_col(pos)
    }

    #[inline]
    unsafe fn line_of(&self, pos: usize) -> &str {
        self.input.line_of(pos)
    }

    #[inline]
    unsafe fn skip(&self, n: usize, pos: usize) -> Option<usize> {
        self.input.skip(n, pos)
    }

    #[inline]
    unsafe fn match_string(&self, string: &str, pos: usize) -> bool {
        self.input.match_string(string, pos)
    }

    #[inline]
    unsafe fn match_insensitive(&self, string: &str, pos: usize) -> bool {
        self.input.match_insensitive(string, pos)
    }

    #[inline]
    unsafe fn match_range(&self, range: Range<char>, pos: usize) -> Option<usize> {
        self.input.match_range(range, pos)
    }
}
