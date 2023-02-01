//! `LineIndex` to make a line_offsets, each item is an offset (start from 0) of the beginning of the line.
//!
//! For example, the text: `"hello\nworld"`, the line_offsets will store `[0, 8]`.
//!
//! Then `line_col` with a offset just need to find the line index by binary search,
//!
//! - `line` is the index of the line_offsets
//! - `col` is the offset minus the line start offset
//!
//! Inspired by rust-analyzer's `LineIndex`:
//! https://github.com/rust-lang/rust/blob/1.67.0/src/tools/rust-analyzer/crates/ide-db/src/line_index.rs
use alloc::vec::Vec;

#[derive(Clone)]
pub struct LineIndex {
    /// Offset the the beginning of each line, zero-based
    line_offsets: Vec<usize>,
}

impl LineIndex {
    pub fn new(text: &str) -> LineIndex {
        let mut line_offsets = Vec::new();
        line_offsets.push(0);

        let mut offset = 0;

        for c in text.chars() {
            offset += 1;
            if c == '\n' {
                line_offsets.push(offset)
            }
        }

        LineIndex { line_offsets }
    }

    pub fn line_col(&self, offset: usize) -> (usize, usize) {
        let line = self.line_offsets.partition_point(|&it| it <= offset) - 1;
        let line_start_offset = self.line_offsets[line];
        let col = offset - line_start_offset;

        (line + 1, col + 1)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_line_index() {
        let text = "hello\nworld";
        let table = [
            (00, 1, 1),
            (01, 1, 2),
            (05, 1, 6),
            (06, 2, 1),
            (07, 2, 2),
            (08, 2, 3),
            (10, 2, 5),
            (11, 2, 6),
            (12, 2, 7),
        ];

        let index = LineIndex::new(text);
        for &(offset, line, col) in &table {
            assert_eq!(index.line_col(offset), (line, col));
        }
    }
}
