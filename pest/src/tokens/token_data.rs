use super::super::inputs::Input;

/// A `struct` that captures the essential data of a `Token` pair.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct TokenData<Rule> {
    /// The `Rule` of the `Token` pair
    pub rule:  Rule,
    /// The start position
    pub start: usize,
    /// The end position
    pub end:   usize
}

impl<Rule> TokenData<Rule> {
    pub fn capture<'a, I: Input>(&self, input: &'a I) -> &'a str {
        input.slice(self.start, self.end)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use super::super::super::inputs::StringInput;

    #[test]
    fn capture() {
        let input = StringInput::new("asdasdf");
        let data = TokenData { rule: (), start: 1, end: 3 };

        assert_eq!(data.capture(&input), "sd");
    }
}
