use futures::sync::mpsc::UnboundedSender;

use super::inputs::Input;
use super::tokens::Token;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum TokenDestination {
    Stream,
    Queue,
    Ignore
}

/// A `struct` which contains the complete state of a `Parser`.
pub struct ParserState<'a, Rule> {
    input:           &'a Input,
    pos:             usize,
    sender:          UnboundedSender<Token<Rule>>,
    queue:           Vec<Token<Rule>>,
    dest:            TokenDestination,
    is_atomic:       bool,
    pos_attempts:    Vec<Rule>,
    neg_attempts:    Vec<Rule>,
    attempt_pos:     usize,
    /// Stack of captured strings
    pub stack:       Vec<&'a str>,
    /// End-of-input matched flag
    pub eoi_matched: bool
}

impl<'a, Rule: Clone + Ord> ParserState<'a, Rule> {
    /// Creates a new `ParserState` from an `Input` and an `UnboundedSender`.
    ///
    /// # Examples
    ///
    /// ```
    /// # extern crate futures;
    /// # extern crate pest;
    /// # use futures::sync::mpsc::unbounded;
    /// # use pest::{ParserState, StringInput};
    /// # use pest::tokens::Token;
    /// # fn main() {
    /// let (s, _) = unbounded::<Token<()>>();
    /// let input = StringInput::new("a");
    ///
    /// let state = ParserState::new(&input, s);
    /// # }
    /// ```
    pub fn new(input: &'a Input, sender: UnboundedSender<Token<Rule>>) -> ParserState<'a, Rule> {
        ParserState {
            input:        input,
            pos:          0,
            sender:       sender,
            queue:        vec![],
            dest:         TokenDestination::Stream,
            is_atomic:    false,
            pos_attempts: vec![],
            neg_attempts: vec![],
            attempt_pos:  0,
            stack:        vec![],
            eoi_matched:  false
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
    /// # use pest::{ParserState, StringInput};
    /// # use pest::tokens::Token;
    /// # fn main() {
    /// let (s, _) = unbounded::<Token<()>>();
    /// let input = StringInput::new("");
    /// let mut state = ParserState::new(&input, s);
    ///
    /// state.send(Token::Start { rule: (), pos: 0 });
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

    /// Returns whether the position is at the start of the `Input`.
    ///
    /// # Examples
    ///
    /// ```no_run
    /// # extern crate futures;
    /// # extern crate pest;
    /// # use futures::sync::mpsc::unbounded;
    /// # use pest::{ParserState, StringInput};
    /// # use pest::tokens::Token;
    /// # fn main() {
    /// let (s, _) = unbounded::<Token<()>>();
    /// let input = StringInput::new("ab");
    /// let mut state = ParserState::new(&input, s);
    ///
    /// assert!(state.at_start());
    /// state.match_string("ab");
    /// assert!(!state.at_start());
    /// # }
    /// ```
    #[inline]
    pub fn at_start(&self) -> bool {
        self.pos == 0
    }

    /// Returns whether the position is at the end of the `Input`.
    ///
    /// # Examples
    ///
    /// ```no_run
    /// # extern crate futures;
    /// # extern crate pest;
    /// # use futures::sync::mpsc::unbounded;
    /// # use pest::{ParserState, StringInput};
    /// # use pest::tokens::Token;
    /// # fn main() {
    /// let (s, _) = unbounded::<Token<()>>();
    /// let input = StringInput::new("ab");
    /// let mut state = ParserState::new(&input, s);
    ///
    /// assert!(!state.at_end());
    /// state.match_string("ab");
    /// assert!(state.at_end());
    /// # }
    /// ```
    #[inline]
    pub fn at_end(&self) -> bool {
        self.pos == self.input.len()
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
    /// # use pest::{ParserState, StringInput};
    /// # use pest::tokens::Token;
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
    /// # use pest::{ParserState, StringInput};
    /// # use pest::tokens::Token;
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
    /// # use pest::{ParserState, StringInput};
    /// # use pest::tokens::Token;
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
    /// # use pest::{ParserState, StringInput};
    /// # use pest::tokens::Token;
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
    /// # use pest::{ParserState, StringInput};
    /// # use pest::tokens::Token;
    /// # fn main() {
    /// let (s, _) = unbounded::<Token<()>>();
    /// let input = StringInput::new("ab");
    /// let mut state = ParserState::new(&input, s);
    ///
    /// assert!(state.ignored(|state| {
    ///     state.send(Token::Start { rule: (), pos: 0 });
    ///     state.match_string("a") && state.match_string("b")
    /// }));
    /// assert!(state.ignored(|state| {
    ///     state.send(Token::Start { rule: (), pos: 0 });
    ///     state.match_string("a") && state.match_string("b")
    /// }));
    /// assert!(!state.ignored(|state| {
    ///     state.send(Token::Start { rule: (), pos: 0 });
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

    /// Matches the current `rule` while toggling atomicity.
    ///
    /// # Examples
    ///
    /// ```
    /// # extern crate futures;
    /// # extern crate pest;
    /// # use futures::sync::mpsc::unbounded;
    /// # use pest::{ParserState, StringInput};
    /// # use pest::tokens::Token;
    /// # fn main() {
    /// let (s, _) = unbounded::<Token<()>>();
    /// let input = StringInput::new("");
    /// let mut state = ParserState::new(&input, s);
    ///
    /// assert!(!state.is_atomic());
    /// state.atomic(true, |state| {
    ///     assert!(state.is_atomic());
    ///     true
    /// });
    /// assert!(!state.is_atomic());
    /// # }
    /// ```
    #[inline]
    pub fn atomic<F>(&mut self, is_atomic: bool, rule: F) -> bool
        where F: FnOnce(&mut ParserState<'a, Rule>) -> bool {

        let should_toggle = self.is_atomic != is_atomic;

        if should_toggle {
            self.is_atomic = is_atomic;
        }

        let result = rule(self);

        if should_toggle {
            self.is_atomic = !is_atomic;
        }

        result
    }

    /// Returns the state's current atomicity.
    ///
    /// # Examples
    ///
    /// ```
    /// # extern crate futures;
    /// # extern crate pest;
    /// # use futures::sync::mpsc::unbounded;
    /// # use pest::{ParserState, StringInput};
    /// # use pest::tokens::Token;
    /// # fn main() {
    /// let (s, _) = unbounded::<Token<()>>();
    /// let input = StringInput::new("");
    /// let mut state = ParserState::new(&input, s);
    ///
    /// assert!(!state.is_atomic());
    /// # }
    /// ```
    #[inline]
    pub fn is_atomic(&self) -> bool {
        self.is_atomic
    }

    /// Keeps track of failed positive rule attempts. It should be called when a `Rule` fails at the
    /// current position.
    ///
    /// # Examples
    ///
    /// ```
    /// # extern crate futures;
    /// # extern crate pest;
    /// # use futures::sync::mpsc::unbounded;
    /// # use pest::{ParserState, StringInput};
    /// # use pest::tokens::Token;
    /// # fn main() {
    /// #[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
    /// enum Rule {
    ///     a,
    ///     b
    /// }
    ///
    /// let (s, _) = unbounded::<Token<Rule>>();
    /// let input = StringInput::new("a");
    /// let mut state = ParserState::new(&input, s);
    ///
    /// state.track_pos(Rule::a);
    /// # }
    /// ```
    #[inline]
    pub fn track_pos(&mut self, rule: Rule) {
        if self.is_atomic || self.dest == TokenDestination::Ignore {
            return
        }

        if self.pos_attempts.is_empty() {
            self.pos_attempts.push(rule);

            self.attempt_pos = self.pos;
        } else {
            if self.pos == self.attempt_pos {
                self.pos_attempts.push(rule);
            } else if self.pos > self.attempt_pos {
                self.pos_attempts.clear();
                self.neg_attempts.clear();
                self.pos_attempts.push(rule);

                self.attempt_pos = self.pos;
            }
        }
    }

    /// Keeps track of failed negative rule attempts. It should be called when a `Rule` fails at the
    /// current position.
    ///
    /// # Examples
    ///
    /// ```
    /// # extern crate futures;
    /// # extern crate pest;
    /// # use futures::sync::mpsc::unbounded;
    /// # use pest::{ParserState, StringInput};
    /// # use pest::tokens::Token;
    /// # fn main() {
    /// #[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
    /// enum Rule {
    ///     a,
    ///     b
    /// }
    ///
    /// let (s, _) = unbounded::<Token<Rule>>();
    /// let input = StringInput::new("a");
    /// let mut state = ParserState::new(&input, s);
    ///
    /// state.track_neg(Rule::a);
    /// # }
    /// ```
    #[inline]
    pub fn track_neg(&mut self, rule: Rule) {
        if self.is_atomic || self.dest == TokenDestination::Ignore {
            return
        }

        if self.neg_attempts.is_empty() {
            self.neg_attempts.push(rule);

            self.attempt_pos = self.pos;
        } else {
            if self.pos == self.attempt_pos {
                self.neg_attempts.push(rule);
            } else if self.pos > self.attempt_pos {
                self.pos_attempts.clear();
                self.neg_attempts.clear();
                self.neg_attempts.push(rule);

                self.attempt_pos = self.pos;
            }
        }
    }
}
