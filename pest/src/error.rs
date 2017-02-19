use super::Input;

/// An `enum` which defines possible errors.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Error<Rule> {
    /// Generated parsing error with expected and unexpected `Rule`s and a position
    ParsingError(Vec<Rule>, Vec<Rule>, usize),
    /// Custom error with a message and a position
    CustomPos(String, usize),
    /// Custom error with a message and a span defined by a start and end position
    CustomSpan(String, usize, usize)
}

impl<Rule> Error<Rule> {
    /// Formats an `Error` according to an `Input`.
    ///
    /// # Examples
    ///
    /// ```
    /// # use pest::{Error, StringInput};
    /// let input = StringInput::new("ab\ncd\nef");
    /// let error: Error<()> = Error::CustomPos("error: big one".to_owned(), 4);
    ///
    /// assert_eq!(error.format(&input), vec![
    ///     " --> 2:2",
    ///     "  |",
    ///     "2 | cd",
    ///     "  |  ^---",
    ///     "  |",
    ///     "  = error: big one"
    /// ].join("\n"));
    /// ```
    pub fn format(&self, input: &Input) -> String {
        self.format_option(input, None)
    }

    /// Formats an `Error` according to an `Input` while including a filename.
    ///
    /// # Examples
    ///
    /// ```
    /// # use pest::{Error, StringInput};
    /// let input = StringInput::new("ab\ncd\nefghi");
    /// let error: Error<()> = Error::CustomSpan("error: big one".to_owned(), 7, 11);
    ///
    /// assert_eq!(error.format_with_filename(&input, "file.ext"), vec![
    ///     " --> file.ext:3:2",
    ///     "  |",
    ///     "3 | efghi",
    ///     "  |  ^--^",
    ///     "  |",
    ///     "  = error: big one"
    /// ].join("\n"));
    /// ```
    pub fn format_with_filename(&self, input: &Input, filename: &str) -> String {
        self.format_option(input, Some(filename))
    }

    fn message(&self) -> &str {
        match self {
            &Error::ParsingError(_, _, _)         => unimplemented!(),
            &Error::CustomPos(ref message, _)     => message,
            &Error::CustomSpan(ref message, _, _) => message
        }
    }

    fn underline(&self, line: &str, offset: usize) -> String {
        let mut underline = String::new();

        for _ in 0..offset { underline.push(' '); }

        match self {
            &Error::CustomSpan(_, start, end) => {
                underline.push('^');
                for _ in 2..(end - start) { underline.push('-'); }
                underline.push('^');
            },
            _ => underline.push_str("^---")
        };

        underline
    }

    fn format_option(&self, input: &Input, option: Option<&str>) -> String {
        let pos = match self {
            &Error::ParsingError(_, _, pos) => pos,
            &Error::CustomPos(_, pos)       => pos,
            &Error::CustomSpan(_, pos, _)   => pos
        };
        let (line, col) = input.line_col(pos);
        let line_str_len = format!("{}", line).len();

        let mut spacing = String::new();
        for _ in 0..line_str_len { spacing.push(' '); }

        let mut result = match option {
            Some(filename) => format!("{}--> {}:{}:{}\n", spacing, filename, line, col),
            None           => format!("{}--> {}:{}\n", spacing, line, col)
        };

        result.push_str(&format!("{} |\n", spacing));
        result.push_str(&format!("{} | ", line));

        let line = input.line_of(pos);
        result.push_str(&format!("{}\n", line));
        result.push_str(&format!("{} | {}\n", spacing, self.underline(&line, col - 1)));
        result.push_str(&format!("{} |\n", spacing));
        result.push_str(&format!("{} = {}", spacing, self.message()));

        result
    }
}
