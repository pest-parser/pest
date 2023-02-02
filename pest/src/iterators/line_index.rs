//! `LineIndex` to make a line_offsets, each item is an byte offset (start from 0) of the beginning of the line.
//!
//! For example, the text: `"hello你好\nworld"`, the line_offsets will store `[0, 12]`.
//!
//! Then `line_col` with a offset just need to find the line index by binary search.
//!
//! Inspired by rust-analyzer's `LineIndex`:
//! https://github.com/rust-lang/rust/blob/1.67.0/src/tools/rust-analyzer/crates/ide-db/src/line_index.rs
use alloc::vec::Vec;

#[derive(Clone)]
pub struct LineIndex {
    /// Offset (bytes) the the beginning of each line, zero-based
    line_offsets: Vec<usize>,
}

impl LineIndex {
    pub fn new(text: &str) -> LineIndex {
        let mut line_offsets: Vec<usize> = Vec::new();
        line_offsets.push(0);

        let mut offset = 0;

        for c in text.chars() {
            offset += c.len_utf8();
            if c == '\n' {
                line_offsets.push(offset);
            }
        }

        LineIndex { line_offsets }
    }

    /// Returns (line, col) of pos.
    ///
    /// The pos is a byte offset, start from 0, e.g. "ab" is 2, "你好" is 6
    pub fn line_col(&self, input: &str, pos: usize) -> (usize, usize) {
        let line = self.line_offsets.partition_point(|&it| it <= pos) - 1;
        let first_offset = self.line_offsets[line];

        // Get line str from original input, then we can get column offset
        let line_str = &input[first_offset..pos];
        let col = line_str.chars().count();

        (line + 1, col + 1)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_line_index() {
        let text = "hello你好A🎈C\nworld";
        let table = [
            (00, 1, 1, 'h'),
            (01, 1, 2, 'e'),
            (02, 1, 3, 'l'),
            (03, 1, 4, 'l'),
            (04, 1, 5, 'o'),
            (05, 1, 6, '你'),
            (08, 1, 7, '好'),
            (11, 1, 8, 'A'),
            (12, 1, 9, '🎈'),
            (16, 1, 10, 'C'),
            (17, 1, 11, '\n'),
            (18, 2, 1, 'w'),
            (19, 2, 2, 'o'),
            (20, 2, 3, 'r'),
            (21, 2, 4, 'l'),
            (22, 2, 5, 'd'),
        ];

        let index = LineIndex::new(text);
        for &(offset, line, col, c) in table.iter() {
            let res = index.line_col(&text, offset);
            assert_eq!(
                (res.0, res.1),
                (line, col),
                "Expected: ({}, {}, {}, {:?})",
                offset,
                line,
                col,
                c
            );
        }
    }
}
