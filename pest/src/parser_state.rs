use futures::sync::mpsc::UnboundedSender;

use super::inputs::Input;
use super::token::Token;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum TokenDestination {
    Stream,
    Queue,
    Ignore
}

/// A `struct` which contains the complete state of a `Parser`.
pub struct ParserState<'a, Rule> {
    input:     &'a Input,
    pos:       usize,
    sender:    UnboundedSender<Token<Rule>>,
    queue:     Vec<Token<Rule>>,
    dest:      TokenDestination,
    is_atomic: bool
}

impl<'a, Rule> ParserState<'a, Rule> {
    /// Creates a new `ParserState` from an `Input` and an `UnboundedSender`.
    ///
    /// # Examples
    ///
    /// ```
    /// # extern crate futures;
    /// # extern crate pest;
    /// # use futures::sync::mpsc::unbounded;
    /// # use pest::{ParserState, StringInput, Token};
    /// # fn main() {
    /// let (s, _) = unbounded::<Token<()>>();
    /// let input = StringInput::new("a");
    ///
    /// let state = ParserState::new(&input, s);
    /// # }
    /// ```
    pub fn new(input: &'a Input, sender: UnboundedSender<Token<Rule>>) -> ParserState<'a, Rule> {
        ParserState {
            input:     input,
            pos:       0,
            sender:    sender,
            queue:     vec![],
            dest:      TokenDestination::Stream,
            is_atomic: false
        }
    }

    /// Sends `token` according to the state's destination. The `Token` will get sent to the
    /// `TokenStream`, queued up to be sent later, or ignored.
    ///
    /// # Examples
    ///
    /// ```no_run
    /// # extern crate futures;
    /// # extern crate pest;
    /// # use futures::sync::mpsc::unbounded;
    /// # use pest::{ParserState, StringInput, Token};
    /// # fn main() {
    /// let (s, _) = unbounded::<Token<()>>();
    /// let input = StringInput::new("");
    /// let mut state = ParserState::new(&input, s);
    ///
    /// state.send(Token::Start((), 0));
    /// # }
    /// ```
    #[inline]
    pub fn send(&mut self, token: Token<Rule>) {
        match self.dest {
            TokenDestination::Stream => self.sender.send(token).unwrap(),
            TokenDestination::Queue  => self.queue.push(token),
            TokenDestination::Ignore => ()
        };
    }

    /// Matches `string`, returns whether it matched, and advances the position with `string.len()`
    /// in case it did.
    ///
    /// # Examples
    ///
    /// ```
    /// # extern crate futures;
    /// # extern crate pest;
    /// # use futures::sync::mpsc::unbounded;
    /// # use pest::{ParserState, StringInput, Token};
    /// # fn main() {
    /// let (s, _) = unbounded::<Token<()>>();
    /// let input = StringInput::new("abcd");
    /// let mut state = ParserState::new(&input, s);
    ///
    /// assert!(state.match_string("ab"));
    /// assert!(state.match_string("cd"));
    /// # }
    /// ```
    #[inline]
    pub fn match_string(&mut self, string: &str) -> bool {
        let result = self.input.match_string(string, self.pos);

        if result {
            self.pos += string.len();
        }

        result
    }

    /// Matches `string` case insensitively, returns whether it matched, and advances the position
    /// with `string.len()` in case it did.
    ///
    /// # Examples
    ///
    /// ```
    /// # extern crate futures;
    /// # extern crate pest;
    /// # use futures::sync::mpsc::unbounded;
    /// # use pest::{ParserState, StringInput, Token};
    /// # fn main() {
    /// let (s, _) = unbounded::<Token<()>>();
    /// let input = StringInput::new("AbcD");
    /// let mut state = ParserState::new(&input, s);
    ///
    /// assert!(state.match_insensitive("ab"));
    /// assert!(state.match_insensitive("cd"));
    /// # }
    /// ```
    #[inline]
    pub fn match_insensitive(&mut self, string: &str) -> bool {
        let result = self.input.match_insensitive(string, self.pos);

        if result {
            self.pos += string.len();
        }

        result
    }

    /// Matches if the current `char` is between `left` and `right`, and advances the position with
    /// one `char` length in case it did.
    ///
    /// # Examples
    ///
    /// ```
    /// # extern crate futures;
    /// # extern crate pest;
    /// # use futures::sync::mpsc::unbounded;
    /// # use pest::{ParserState, StringInput, Token};
    /// # fn main() {
    /// let (s, _) = unbounded::<Token<()>>();
    /// let input = StringInput::new("Cd");
    /// let mut state = ParserState::new(&input, s);
    ///
    /// assert!(state.match_range('A', 'Z'));
    /// assert!(state.match_range('a', 'z'));
    /// # }
    /// ```
    #[inline]
    pub fn match_range(&mut self, left: char, right: char) -> bool {
        let result = self.input.match_range(left, right, self.pos);

        if result {
            self.pos += left.len_utf8();
        }

        result
    }

    /// Matches the current `rule`, queues up all generated `Token`s, and reverts the state if the
    /// `rule` fails.
    ///
    /// # Examples
    ///
    /// ```
    /// # extern crate futures;
    /// # extern crate pest;
    /// # use futures::sync::mpsc::unbounded;
    /// # use pest::{ParserState, StringInput, Token};
    /// # fn main() {
    /// let (s, _) = unbounded::<Token<()>>();
    /// let input = StringInput::new("abacad");
    /// let mut state = ParserState::new(&input, s);
    ///
    /// assert!(state.queued(|state| {
    ///     state.match_string("a") && state.match_string("b")
    /// }));
    /// assert!(state.queued(|state| {
    ///     state.match_string("a") && state.match_string("c")
    /// }));
    /// assert!(!state.queued(|state| {
    ///     state.match_string("a") && state.match_string("c")
    /// }));
    /// # }
    /// ```
    #[inline]
    pub fn queued<F>(&mut self, rule: F) -> bool
        where F: FnOnce(&mut ParserState<'a, Rule>) -> bool {

        let initial_pos = self.pos;
        let should_toggle = self.dest == TokenDestination::Stream;

        if should_toggle {
            self.dest = TokenDestination::Queue;
        }

        let result = rule(self);

        if should_toggle {
            self.dest = TokenDestination::Stream;

            if result {
                for token in self.queue.drain(..) {
                    self.sender.send(token).unwrap();
                }
            } else {
                self.queue.clear();
            }
        }

        if !result {
            self.pos = initial_pos;
        }

        result
    }

    /// Matches the current `rule`, ignores all `Token`s, and reverts the state.
    ///
    /// # Examples
    ///
    /// ```
    /// # extern crate futures;
    /// # extern crate pest;
    /// # use futures::sync::mpsc::unbounded;
    /// # use pest::{ParserState, StringInput, Token};
    /// # fn main() {
    /// let (s, _) = unbounded::<Token<()>>();
    /// let input = StringInput::new("ab");
    /// let mut state = ParserState::new(&input, s);
    ///
    /// assert!(state.ignored(|state| {
    ///     state.send(Token::Start((), 0));
    ///     state.match_string("a") && state.match_string("b")
    /// }));
    /// assert!(state.ignored(|state| {
    ///     state.send(Token::Start((), 0));
    ///     state.match_string("a") && state.match_string("b")
    /// }));
    /// assert!(!state.ignored(|state| {
    ///     state.send(Token::Start((), 0));
    ///     state.match_string("a") && state.match_string("c")
    /// }));
    /// # }
    /// ```
    #[inline]
    pub fn ignored<F>(&mut self, rule: F) -> bool
        where F: FnOnce(&mut ParserState<'a, Rule>) -> bool {

        let initial_pos = self.pos;
        let should_toggle = self.dest != TokenDestination::Ignore;
        let initial_dest = self.dest;

        if should_toggle {
            self.dest = TokenDestination::Ignore;
        }

        let result = rule(self);

        if should_toggle {
            self.dest = initial_dest;
        }

        self.pos = initial_pos;

        result
    }
}
