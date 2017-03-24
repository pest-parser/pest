// pest. The Elegant Parser
// Copyright (C) 2017  Dragoș Tiselice
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::cell::RefCell;
use std::fmt::Debug;
use std::rc::Rc;

use futures::stream::Stream;

use super::expandable_stream::ExpandableStream;
use super::expanded_stream as es;
use super::super::inputs::Input;
use super::peek_rule_future as prf;
use super::sliced_stream as ss;
use super::sliceable_stream::SliceableStream;
use super::tail_stream as ts;
use super::token_data_future as tdf;
use super::super::error::Error;
use super::super::RuleType;
use super::super::tokens::Token;

/// A `trait` that defines common methods on `Token` `Stream`s.
pub trait TokenStream<R: RuleType, I: Input>:
    Stream<Item=Token<R, I>, Error=Error<R, I>> + Sized {

    /// Peeks at the following `Token`'s `Rule` and returns a `PeekRuleFuture` containing the
    /// possible `Rule` and a `Peekable` stream.
    ///
    /// This method is used primarily as an easy means to decide what to do at any certain point
    /// in the stream.
    ///
    /// # Panics
    ///
    /// Panics if the first `Token` in the stream is not a `Token::Start`.
    ///
    /// # Examples
    ///
    /// ```
    /// # extern crate futures;
    /// # extern crate pest;
    /// # use futures::future::Future;
    /// # use futures::stream::Stream;
    /// # use pest::inputs::StringInput;
    /// # use pest::state;
    /// # use pest::streams::TokenStream;
    /// # use pest::tokens::Token;
    /// # fn main() {
    /// # #[allow(non_camel_case_types)]
    /// #[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
    /// enum Rule {
    ///     a
    /// }
    ///
    /// let input = StringInput::new("");
    /// let (mut state, stream) = state(input);
    ///
    /// let pos = state.start();
    /// state.send(Token::Start { rule: Rule::a, pos: pos.clone() });
    /// state.send(Token::End   { rule: Rule::a, pos: pos });
    ///
    /// stream.peek_rule().and_then(|(rule, stream)| {
    ///     assert_eq!(rule, Some(Rule::a));
    ///     assert_eq!(stream.take(2).collect().wait().unwrap(), vec![
    ///         Token::Start { rule: Rule::a, pos: state.start() },
    ///         Token::End   { rule: Rule::a, pos: state.start() }
    ///     ]);
    ///
    ///     Ok(())
    /// }).wait().unwrap();
    /// # }
    /// ```
    fn peek_rule(self) -> prf::PeekRuleFuture<R, I, Self> {
        prf::new(self)
    }

    /// Expands a matching `Token` pair to a `TokenDataFuture`, containing the `TokenData` crated
    /// from the pair, and an `ExpandedStream`, containing all the `Token`s between the matching
    /// pair.
    ///
    /// A matching `Token` pair is formed by a `Token::Start` followed by a `Token::End` with the
    /// same `Rule` with the condition that all `Token`s between them can form such pairs as well.
    /// This is similar to the
    /// [brace matching problem](https://en.wikipedia.org/wiki/Brace_matching) in editors.
    ///
    /// # Panics
    ///
    /// Panics if the first `Token` in the stream is not a `Token::Start` with the matching `rule`
    /// or if a pair if not found before the stream ends.
    ///
    /// # Examples
    ///
    /// ```
    /// # extern crate futures;
    /// # extern crate pest;
    /// # use futures::future::Future;
    /// # use futures::stream::Stream;
    /// # use pest::inputs::StringInput;
    /// # use pest::state;
    /// # use pest::streams::TokenStream;
    /// # use pest::tokens::Token;
    /// # fn main() {
    /// # #[allow(non_camel_case_types)]
    /// #[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
    /// enum Rule {
    ///     a,
    ///     b
    /// }
    ///
    /// let input = StringInput::new("abc");
    /// let (mut state, stream) = state(input);
    ///
    /// let pos0 = state.start();
    /// let pos1 = pos0.clone().match_string("a").unwrap();
    /// let pos2 = pos1.clone().match_string("b").unwrap();
    /// let pos3 = pos2.clone().match_string("c").unwrap();
    ///
    /// state.send(Token::Start { rule: Rule::a, pos: pos0 });
    /// state.send(Token::Start { rule: Rule::b, pos: pos1.clone() });
    /// state.send(Token::End   { rule: Rule::b, pos: pos2.clone() });
    /// state.send(Token::End   { rule: Rule::a, pos: pos3 });
    ///
    /// stream.expand(Rule::a, |data, stream| {
    ///     let data = data.wait().unwrap();
    ///
    ///     assert_eq!(data.rule, Rule::a);
    ///     assert_eq!(data.span.start(), 0);
    ///     assert_eq!(data.span.end(), 3);
    ///
    ///     let inner = stream.collect().wait().unwrap();
    ///
    ///     assert_eq!(inner, vec![
    ///         Token::Start { rule: Rule::b, pos: pos1 },
    ///         Token::End   { rule: Rule::b, pos: pos2 }
    ///     ]);
    /// });
    /// # }
    /// ```
    fn expand<F, T>(self, rule: R, f: F) -> (T, ts::TailStream<R, I, Self>)
        where F: FnOnce(tdf::TokenDataFuture<R, I, Self>,
                        es::ExpandedStream<R, I, Self>) -> T {

        let stream = Rc::new(RefCell::new(ExpandableStream::new(self, rule)));

        let token_data_future = tdf::new(stream.clone());
        let expanded_stream = es::new(stream.clone());
        let tail_stream = ts::new(stream.clone());

        (f(token_data_future, expanded_stream), tail_stream)
    }

    /// Returns a `SlicedStream` of `PairStream`s, with each `PairStream` containing all the
    /// `Token`s between the matching pair *inclusively*.
    ///
    /// A matching `Token` pair is formed by a `Token::Start` followed by a `Token::End` with the
    /// same `Rule` with the condition that all `Token`s between them can form such pairs as well.
    /// This is similar to the
    /// [brace matching problem](https://en.wikipedia.org/wiki/Brace_matching) in editors.
    ///
    /// # Panics
    ///
    /// Panics if the first `Token` in the stream is not a `Token::Start` or if a pair if not found
    /// before the stream ends.
    ///
    /// # Examples
    ///
    /// ```
    /// # extern crate futures;
    /// # extern crate pest;
    /// # use futures::future::Future;
    /// # use futures::stream::Stream;
    /// # use pest::inputs::StringInput;
    /// # use pest::state;
    /// # use pest::streams::TokenStream;
    /// # use pest::tokens::Token;
    /// # fn main() {
    /// # #[allow(non_camel_case_types)]
    /// #[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
    /// enum Rule {
    ///     a,
    ///     b
    /// }
    ///
    /// let input = StringInput::new("abc");
    /// let (mut state, stream) = state(input);
    ///
    /// let pos0 = state.start();
    /// let pos1 = pos0.clone().match_string("a").unwrap();
    /// let pos2 = pos1.clone().match_string("b").unwrap();
    /// let pos3 = pos2.clone().match_string("c").unwrap();
    ///
    /// state.send(Token::Start { rule: Rule::a, pos: pos0.clone() });
    /// state.send(Token::End   { rule: Rule::a, pos: pos1.clone() });
    /// state.send(Token::Start { rule: Rule::b, pos: pos2.clone() });
    /// state.send(Token::End   { rule: Rule::b, pos: pos3.clone() });
    ///
    /// let pairs = stream.sliced().map(|stream| {
    ///     stream.collect().wait().unwrap()
    /// }).take(2).collect().wait().unwrap();
    ///
    /// assert_eq!(pairs, vec![
    ///     vec![
    ///         Token::Start { rule: Rule::a, pos: pos0 },
    ///         Token::End   { rule: Rule::a, pos: pos1 }
    ///     ],
    ///     vec![
    ///         Token::Start { rule: Rule::b, pos: pos2 },
    ///         Token::End   { rule: Rule::b, pos: pos3 }
    ///     ]
    /// ]);
    /// # }
    /// ```
    fn sliced(self) -> ss::SlicedStream<R, I, Self> {
        let stream = Rc::new(RefCell::new(SliceableStream::new(self)));

        ss::new(stream)
    }
}

impl<R: RuleType, I: Input, S> TokenStream<R, I> for S
    where S: Stream<Item=Token<R, I>, Error=Error<R, I>> {}

#[cfg(test)]
mod tests {
    use std::rc::Rc;
    use std::sync::Arc;
    use std::thread;
    use std::time::Duration;

    use futures::future::Future;
    use futures::stream::Stream;

    use super::TokenStream;
    use super::super::buffered::{buffered, SendableError, SendableToken};
    use super::super::parser_stream;
    use super::super::super::error::Error;
    use super::super::super::inputs::StringInput;
    use super::super::super::inputs_private::{position, span};
    use super::super::super::tokens::{Token, TokenData};


    #[allow(non_camel_case_types)]
    #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
    enum Rule {
        a,
        b,
        c,
        d
    }

    #[test]
    fn peek_rule_sleep() {
        let arc = Arc::new(StringInput::new(""));
        let input = Rc::new(arc.clone());
        
        let r = {
            let (s, r) = buffered(16);

            thread::spawn(move || {
                thread::sleep(Duration::from_millis(10));

                s.send(SendableToken::Start { rule: Rule::a, pos: 0 });
            });

            r
        };

        let stream = parser_stream::new(r, arc);

        stream.peek_rule().and_then(|(rule, stream)| {
            assert_eq!(rule, Some(Rule::a));
            assert_eq!(stream.collect().wait().unwrap(), vec![
                Token::Start { rule: Rule::a, pos: position::new(input.clone(), 0) }
            ]);

            Ok(())
        }).wait().unwrap();
    }

    #[test]
    fn peek_rule_empty() {
        let arc = Arc::new(StringInput::new(""));

        let r = {
            let (_, r) = buffered::<SendableToken<Rule>, SendableError<Rule>>(16);

            r
        };

        let stream = parser_stream::new(r, arc);

        stream.peek_rule().and_then(|(rule, stream)| {
            assert_eq!(rule, None);
            assert_eq!(stream.collect().wait().unwrap(), vec![]);

            Ok(())
        }).wait().unwrap();
    }

    #[test]
    #[should_panic(expected = "expected Start { .. }, \
                               but found End { rule: a, pos: Position { pos: 0 } } instead")]
    fn peek_rule_end() {
        let arc = Arc::new(StringInput::new(""));

        let r = {
            let (s, r) = buffered::<SendableToken<Rule>, SendableError<Rule>>(16);

            s.send(SendableToken::End { rule: Rule::a, pos: 0 });

            r
        };

        let stream = parser_stream::new(r, arc);

        stream.peek_rule().wait().unwrap();
    }

    #[test]
    fn peek_rule_error() {
        let arc = Arc::new(StringInput::new(""));
        let input = Rc::new(arc.clone());

        let r = {
            let (s, r) = buffered::<SendableToken<Rule>, SendableError<Rule>>(16);

            s.fail(SendableError::CustomErrorPos { message: "e".to_owned(), pos: 2 });

            r
        };

        let stream = parser_stream::new(r, arc);

        let err = stream.peek_rule().wait().err().unwrap();

        assert_eq!(err, Error::CustomErrorPos {
            message: "e".to_owned(),
            pos: position::new(input.clone(), 2)
        });
    }

    #[test]
    fn expand_sleep() {
        let arc = Arc::new(StringInput::new(""));
        let input = Rc::new(arc.clone());

        let r = {
            let (s, r) = buffered(16);

            thread::spawn(move || {
                thread::sleep(Duration::from_millis(10));

                s.send(SendableToken::Start { rule: Rule::a, pos: 0 });
                s.send(SendableToken::Start { rule: Rule::b, pos: 1 });
                s.send(SendableToken::End   { rule: Rule::b, pos: 2 });
                s.send(SendableToken::End   { rule: Rule::a, pos: 3 });
                s.send(SendableToken::Start { rule: Rule::c, pos: 4 });
                s.send(SendableToken::End   { rule: Rule::c, pos: 5 });
            });

            r
        };

        let stream = parser_stream::new(r, arc);

        let (result, stream) = stream.expand(Rule::a, |data, stream| {
            assert_eq!(data.wait().unwrap(), TokenData {
                rule: Rule::a,
                span: span::new(input.clone(), 0, 3)
            });

            let (result, _) = stream.expand(Rule::b, |data, _| {
                assert_eq!(data.wait().unwrap(), TokenData {
                    rule: Rule::b,
                    span: span::new(input.clone(), 1, 2)
                });

                2
            });

            3 + result
        });

        assert_eq!(result, 5);

        stream.expand(Rule::c, |data, _| {
            assert_eq!(data.wait().unwrap(), TokenData {
                rule: Rule::c,
                span: span::new(input, 4, 5)
            });
        });
    }

    #[test]
    fn expand_tail() {
        let arc = Arc::new(StringInput::new(""));
        let input = Rc::new(arc.clone());

        let r = {
            let (s, r) = buffered(16);

            s.send(SendableToken::Start { rule: Rule::a, pos: 0 });
            s.send(SendableToken::Start { rule: Rule::b, pos: 1 });
            s.send(SendableToken::End   { rule: Rule::b, pos: 2 });
            s.send(SendableToken::End   { rule: Rule::a, pos: 3 });
            s.send(SendableToken::Start { rule: Rule::c, pos: 4 });
            s.send(SendableToken::End   { rule: Rule::c, pos: 5 });

            r
        };

        let stream = parser_stream::new(r, arc);

        let (_, stream) = stream.expand(Rule::a, |_, _| {});

        assert_eq!(stream.collect().wait().unwrap(), vec![
            Token::Start { rule: Rule::c, pos: position::new(input.clone(), 4) },
            Token::End   { rule: Rule::c, pos: position::new(input.clone(), 5) }
        ]);
    }

    #[test]
    fn expand_streams() {
        let arc = Arc::new(StringInput::new(""));
        let input = Rc::new(arc.clone());

        let r = {
            let (s, r) = buffered(16);

            s.send(SendableToken::Start { rule: Rule::a, pos: 0 });
            s.send(SendableToken::Start { rule: Rule::a, pos: 1 });
            s.send(SendableToken::End   { rule: Rule::a, pos: 2 });
            s.send(SendableToken::End   { rule: Rule::a, pos: 3 });
            s.send(SendableToken::Start { rule: Rule::c, pos: 4 });
            s.send(SendableToken::End   { rule: Rule::c, pos: 5 });

            r
        };

        let stream = parser_stream::new(r, arc);

        let (_, stream) = stream.expand(Rule::a, |_, stream| {
            assert_eq!(stream.collect().wait().unwrap(), vec![
                Token::Start { rule: Rule::a, pos: position::new(input.clone(), 1) },
                Token::End   { rule: Rule::a, pos: position::new(input.clone(), 2) }
            ]);
        });

        stream.expand(Rule::c, |_, stream| {
            assert_eq!(stream.collect().wait().unwrap(), vec![]);
        });
    }

    #[test]
    #[should_panic(expected = "expected Start { rule: b, .. }, \
                               but found Start { rule: a, pos: Position { pos: 0 } } instead")]
    fn expand_wrong_start_future() {
        let arc = Arc::new(StringInput::new(""));

        let r = {
            let (s, r) = buffered(16);

            s.send(SendableToken::Start { rule: Rule::a, pos: 0 });
            s.send(SendableToken::End   { rule: Rule::a, pos: 1 });

            r
        };

        let stream = parser_stream::new(r, arc);

        stream.expand(Rule::b, |data, _| {
            data.wait().unwrap();
        });
    }

    #[test]
    #[should_panic(expected = "expected Start { rule: a, .. }, \
                               but found nothing")]
    fn expand_no_start_future() {
        let arc = Arc::new(StringInput::new(""));

        let r = {
            let (_, r) = buffered(16);

            r
        };

        let stream = parser_stream::new(r, arc);

        stream.expand(Rule::a, |data, _| {
            data.wait().unwrap();
        });
    }

    #[test]
    #[should_panic(expected = "expected End { rule: a, .. }, \
                               but found nothing")]
    fn expand_no_end_future() {
        let arc = Arc::new(StringInput::new(""));

        let r = {
            let (s, r) = buffered(16);

            s.send(SendableToken::Start { rule: Rule::a, pos: 0 });

            r
        };

        let stream = parser_stream::new(r, arc);

        stream.expand(Rule::a, |data, _| {
            data.wait().unwrap();
        });
    }

    #[test]
    #[should_panic(expected = "expected Start { rule: b, .. }, \
                               but found Start { rule: a, pos: Position { pos: 0 } } instead")]
    fn expand_wrong_start_expanded_stream() {
        let arc = Arc::new(StringInput::new(""));

        let r = {
            let (s, r) = buffered(16);

            s.send(SendableToken::Start { rule: Rule::a, pos: 0 });
            s.send(SendableToken::End   { rule: Rule::a, pos: 1 });

            r
        };

        let stream = parser_stream::new(r, arc);

        stream.expand(Rule::b, |_, stream| {
            stream.collect().wait().unwrap();
        });
    }

    #[test]
    #[should_panic(expected = "expected Start { rule: a, .. }, \
                               but found nothing")]
    fn expand_no_start_expanded_stream() {
        let arc = Arc::new(StringInput::new(""));

        let r = {
            let (_, r) = buffered(16);

            r
        };

        let stream = parser_stream::new(r, arc);

        stream.expand(Rule::a, |_, stream| {
            stream.collect().wait().unwrap();
        });
    }

    #[test]
    #[should_panic(expected = "expected End { rule: a, .. }, \
                               but found nothing")]
    fn expand_no_end_expanded_stream() {
        let arc = Arc::new(StringInput::new(""));

        let r = {
            let (s, r) = buffered(16);

            s.send(SendableToken::Start { rule: Rule::a, pos: 0 });

            r
        };

        let stream = parser_stream::new(r, arc);

        stream.expand(Rule::a, |_, stream| {
            stream.collect().wait().unwrap();
        });
    }

    #[test]
    #[should_panic(expected = "expected Start { rule: b, .. }, \
                               but found Start { rule: a, pos: Position { pos: 0 } } instead")]
    fn expand_wrong_start_tail_stream() {
        let arc = Arc::new(StringInput::new(""));

        let r = {
            let (s, r) = buffered(16);

            s.send(SendableToken::Start { rule: Rule::a, pos: 0 });
            s.send(SendableToken::End   { rule: Rule::a, pos: 1 });

            r
        };

        let stream = parser_stream::new(r, arc);

        let (_, stream) = stream.expand(Rule::b, |_, _| {});

        stream.collect().wait().unwrap();
    }

    #[test]
    #[should_panic(expected = "expected Start { rule: a, .. }, \
                               but found nothing")]
    fn expand_no_start_tail_stream() {
        let arc = Arc::new(StringInput::new(""));

        let r = {
            let (_, r) = buffered(16);

            r
        };

        let stream = parser_stream::new(r, arc);

        let (_, stream) = stream.expand(Rule::a, |_, _| {});

        stream.collect().wait().unwrap();
    }

    #[test]
    #[should_panic(expected = "expected End { rule: a, .. }, \
                               but found nothing")]
    fn expand_no_end_tail_stream() {
        let arc = Arc::new(StringInput::new(""));

        let r = {
            let (s, r) = buffered(16);

            s.send(SendableToken::Start { rule: Rule::a, pos: 0 });

            r
        };

        let stream = parser_stream::new(r, arc);

        let (_, stream) = stream.expand(Rule::a, |_, _| {});

        stream.collect().wait().unwrap();
    }

    #[test]
    fn expand_incomplete_future() {
        let arc = Arc::new(StringInput::new(""));
        let input = Rc::new(arc.clone());

        let (s, r) = buffered(16);

        s.send(SendableToken::Start { rule: Rule::a, pos: 0 });
        s.send(SendableToken::Start { rule: Rule::b, pos: 1 });
        s.send(SendableToken::End   { rule: Rule::b, pos: 2 });

        let stream = parser_stream::new(r, arc);

        stream.expand(Rule::a, |_, stream| {
            assert_eq!(stream.take(2).collect().wait().unwrap(), vec![
                Token::Start { rule: Rule::b, pos: position::new(input.clone(), 1) },
                Token::End   { rule: Rule::b, pos: position::new(input.clone(), 2) }
            ]);
        });
    }

    #[test]
    fn expand_future_first() {
        let arc = Arc::new(StringInput::new(""));
        let input = Rc::new(arc.clone());

        let r = {
            let (s, r) = buffered(16);

            s.send(SendableToken::Start { rule: Rule::a, pos: 0 });
            s.send(SendableToken::Start { rule: Rule::a, pos: 1 });
            s.send(SendableToken::End   { rule: Rule::a, pos: 2 });
            s.send(SendableToken::End   { rule: Rule::a, pos: 3 });

            r
        };

        let stream = parser_stream::new(r, arc);

        stream.expand(Rule::a, |data, stream| {
            assert_eq!(data.wait().unwrap(), TokenData {
                rule: Rule::a,
                span: span::new(input.clone(), 0, 3)
            });
            assert_eq!(stream.collect().wait().unwrap(), vec![
                Token::Start { rule: Rule::a, pos: position::new(input.clone(), 1) },
                Token::End   { rule: Rule::a, pos: position::new(input.clone(), 2) }
            ]);
        });
    }

    #[test]
    fn expand_tail_first() {
        let arc = Arc::new(StringInput::new(""));
        let input = Rc::new(arc.clone());

        let r = {
            let (s, r) = buffered(16);

            s.send(SendableToken::Start { rule: Rule::a, pos: 0 });
            s.send(SendableToken::Start { rule: Rule::a, pos: 1 });
            s.send(SendableToken::End   { rule: Rule::a, pos: 2 });
            s.send(SendableToken::End   { rule: Rule::a, pos: 3 });

            r
        };

        let stream = parser_stream::new(r, arc);

        let ((data, expanded_stream), stream) = stream.expand(Rule::a, |data, stream| {
            (data, stream)
        });

        assert_eq!(stream.collect().wait().unwrap(), vec![]);

        assert_eq!(data.wait().unwrap(), TokenData {
            rule: Rule::a,
            span: span::new(input.clone(), 0, 3)
        });
        assert_eq!(expanded_stream.collect().wait().unwrap(), vec![
            Token::Start { rule: Rule::a, pos: position::new(input.clone(), 1) },
            Token::End   { rule: Rule::a, pos: position::new(input.clone(), 2) }
        ]);
    }

    #[test]
    fn expand_error_future_first() {
        let arc = Arc::new(StringInput::new(""));
        let input = Rc::new(arc.clone());

        let r = {
            let (s, r) = buffered(16);

            s.fail(SendableError::CustomErrorPos { message: "e".to_owned(), pos: 2 });

            r
        };

        let stream = parser_stream::new(r, arc);

        let (_, stream) = stream.expand(Rule::a, |data, stream| {
            assert_eq!(data.wait(), Err(Error::CustomErrorPos {
                message: "e".to_owned(),
                pos: position::new(input.clone(), 2)
            }));
            assert_eq!(stream.collect().wait(), Err(Error::CustomErrorPos {
                message: "e".to_owned(),
                pos: position::new(input.clone(), 2)
            }));
        });


        assert_eq!(stream.collect().wait(), Err(Error::CustomErrorPos {
            message: "e".to_owned(),
            pos: position::new(input.clone(), 2)
        }));
    }

    #[test]
    fn expand_error_expand_first() {
        let arc = Arc::new(StringInput::new(""));
        let input = Rc::new(arc.clone());

        let r = {
            let (s, r) = buffered(16);

            s.fail(SendableError::CustomErrorPos { message: "e".to_owned(), pos: 2 });

            r
        };

        let stream = parser_stream::new(r, arc);

        let (_, stream) = stream.expand(Rule::a, |data, stream| {
            assert_eq!(stream.collect().wait(), Err(Error::CustomErrorPos {
                message: "e".to_owned(),
                pos: position::new(input.clone(), 2)
            }));
            assert_eq!(data.wait(), Err(Error::CustomErrorPos {
                message: "e".to_owned(),
                pos: position::new(input.clone(), 2)
            }));
        });


        assert_eq!(stream.collect().wait(), Err(Error::CustomErrorPos {
            message: "e".to_owned(),
            pos: position::new(input.clone(), 2)
        }));
    }

    #[test]
    fn expand_error_tail_first() {
        let arc = Arc::new(StringInput::new(""));
        let input = Rc::new(arc.clone());

        let r = {
            let (s, r) = buffered(16);

            s.fail(SendableError::CustomErrorPos { message: "e".to_owned(), pos: 2 });

            r
        };

        let stream = parser_stream::new(r, arc);

        let ((data, expanded), stream) = stream.expand(Rule::a, |data, stream| {
            (data, stream)
        });


        assert_eq!(stream.collect().wait(), Err(Error::CustomErrorPos {
            message: "e".to_owned(),
            pos: position::new(input.clone(), 2)
        }));
        assert_eq!(data.wait(), Err(Error::CustomErrorPos {
            message: "e".to_owned(),
            pos: position::new(input.clone(), 2)
        }));
        assert_eq!(expanded.collect().wait(), Err(Error::CustomErrorPos {
            message: "e".to_owned(),
            pos: position::new(input.clone(), 2)
        }));
    }

    #[test]
    fn expand_nested() {
        let arc = Arc::new(StringInput::new(""));

        let (s, r) = buffered(16);

        s.send(SendableToken::Start { rule: Rule::a, pos: 0 });
        s.send(SendableToken::Start { rule: Rule::b, pos: 1 });
        s.send(SendableToken::Start { rule: Rule::c, pos: 2 });
        s.send(SendableToken::Start { rule: Rule::d, pos: 3 });
        s.send(SendableToken::End   { rule: Rule::d, pos: 4 });

        let stream = parser_stream::new(r, arc);

        stream.expand(Rule::a, |_, stream| {
            stream.expand(Rule::b, |_, stream| {
                stream.expand(Rule::c, |_, stream| {
                    stream.expand(Rule::d, |_, stream| {
                        assert_eq!(stream.collect().wait().unwrap(), vec![]);
                    })
                })
            })
        });
    }

    #[test]
    fn sliced_sleep() {
        let arc = Arc::new(StringInput::new(""));
        let input = Rc::new(arc.clone());

        let r = {
            let (s, r) = buffered(16);

            thread::spawn(move || {
                thread::sleep(Duration::from_millis(10));

                s.send(SendableToken::Start { rule: Rule::a, pos: 0 });
                s.send(SendableToken::End   { rule: Rule::a, pos: 1 });
                s.send(SendableToken::Start { rule: Rule::b, pos: 2 });
                s.send(SendableToken::End   { rule: Rule::b, pos: 3 });
                s.send(SendableToken::Start { rule: Rule::c, pos: 4 });
                s.send(SendableToken::Start { rule: Rule::c, pos: 5 });
                s.send(SendableToken::Start { rule: Rule::d, pos: 6 });
                s.send(SendableToken::End   { rule: Rule::d, pos: 7 });
                s.send(SendableToken::End   { rule: Rule::c, pos: 8 });
                s.send(SendableToken::End   { rule: Rule::c, pos: 9 });
            });

            r
        };

        let stream = parser_stream::new(r, arc);

        let pairs = stream.sliced().map(|stream| {
            stream.collect().wait().unwrap()
        }).collect().wait().unwrap();

        assert_eq!(pairs, vec![
            vec![
                Token::Start { rule: Rule::a, pos: position::new(input.clone(), 0) },
                Token::End   { rule: Rule::a, pos: position::new(input.clone(), 1) }
            ],
            vec![
                Token::Start { rule: Rule::b, pos: position::new(input.clone(), 2) },
                Token::End   { rule: Rule::b, pos: position::new(input.clone(), 3) }
            ],
            vec![
                Token::Start { rule: Rule::c, pos: position::new(input.clone(), 4) },
                Token::Start { rule: Rule::c, pos: position::new(input.clone(), 5) },
                Token::Start { rule: Rule::d, pos: position::new(input.clone(), 6) },
                Token::End   { rule: Rule::d, pos: position::new(input.clone(), 7) },
                Token::End   { rule: Rule::c, pos: position::new(input.clone(), 8) },
                Token::End   { rule: Rule::c, pos: position::new(input.clone(), 9) }
            ]
        ]);
    }

    #[test]
    fn sliced_wait() {
        let arc = Arc::new(StringInput::new(""));
        let input = Rc::new(arc.clone());

        let r = {
            let (s, r) = buffered(16);
            s.send(SendableToken::Start { rule: Rule::a, pos: 0 });
            s.send(SendableToken::End   { rule: Rule::a, pos: 1 });
            s.send(SendableToken::Start { rule: Rule::b, pos: 2 });
            s.send(SendableToken::End   { rule: Rule::b, pos: 3 });
            s.send(SendableToken::Start { rule: Rule::c, pos: 4 });
            s.send(SendableToken::Start { rule: Rule::c, pos: 5 });
            s.send(SendableToken::Start { rule: Rule::d, pos: 6 });
            s.send(SendableToken::End   { rule: Rule::d, pos: 7 });
            s.send(SendableToken::End   { rule: Rule::c, pos: 8 });
            s.send(SendableToken::End   { rule: Rule::c, pos: 9 });

            r
        };

        let stream = parser_stream::new(r, arc);

        let pairs = stream.sliced().map(|stream| stream).collect().wait().unwrap();
        let pairs: Vec<_> = pairs.into_iter().map(|stream| {
            stream.collect().wait().unwrap()
        }).collect();

        assert_eq!(pairs, vec![
            vec![
                Token::Start { rule: Rule::a, pos: position::new(input.clone(), 0) },
                Token::End   { rule: Rule::a, pos: position::new(input.clone(), 1) }
            ],
            vec![
                Token::Start { rule: Rule::b, pos: position::new(input.clone(), 2) },
                Token::End   { rule: Rule::b, pos: position::new(input.clone(), 3) }
            ],
            vec![
                Token::Start { rule: Rule::c, pos: position::new(input.clone(), 4) },
                Token::Start { rule: Rule::c, pos: position::new(input.clone(), 5) },
                Token::Start { rule: Rule::d, pos: position::new(input.clone(), 6) },
                Token::End   { rule: Rule::d, pos: position::new(input.clone(), 7) },
                Token::End   { rule: Rule::c, pos: position::new(input.clone(), 8) },
                Token::End   { rule: Rule::c, pos: position::new(input.clone(), 9) }
            ]
        ]);
    }

    #[test]
    fn sliced_empty() {
        let arc = Arc::new(StringInput::new(""));

        let r = {
            let (_, r) = buffered::<SendableToken<Rule>, SendableError<Rule>>(16);

            r
        };

        let stream = parser_stream::new(r, arc);

        let pairs = stream.sliced().map(|_| ()).collect().wait().unwrap();

        assert_eq!(pairs, vec![]);
    }

    #[test]
    fn sliced_one_pair() {
        let arc = Arc::new(StringInput::new(""));
        let input = Rc::new(arc.clone());

        let r = {
            let (s, r) = buffered::<SendableToken<Rule>, SendableError<Rule>>(16);

            s.send(SendableToken::Start { rule: Rule::a, pos: 0 });
            s.send(SendableToken::End   { rule: Rule::a, pos: 1 });

            r
        };

        let stream = parser_stream::new(r, arc);

        let pairs = stream.sliced().map(|stream| {
            stream.collect().wait().unwrap()
        }).collect().wait().unwrap();

        assert_eq!(pairs, vec![
            vec![
                Token::Start { rule: Rule::a, pos: position::new(input.clone(), 0) },
                Token::End   { rule: Rule::a, pos: position::new(input.clone(), 1) }
            ]
        ]);
    }

    #[test]
    #[should_panic(expected = "expected Start { .. }, \
                               but found End { rule: a, pos: Position { pos: 0 } } instead")]
    fn sliced_end() {
        let arc = Arc::new(StringInput::new(""));

        let r = {
            let (s, r) = buffered(16);

            s.send(SendableToken::End { rule: Rule::a, pos: 0 });

            r
        };

        let stream = parser_stream::new(r, arc);

        stream.sliced().collect().wait().unwrap();
    }

    #[test]
    #[should_panic(expected = "expected End { rule: a, .. }, \
                               but found nothing")]
    fn sliced_no_end_sliced() {
        let arc = Arc::new(StringInput::new(""));

        let r = {
            let (s, r) = buffered(16);

            s.send(SendableToken::Start { rule: Rule::a, pos: 0 });

            r
        };

        let stream = parser_stream::new(r, arc);

        stream.sliced().collect().wait().unwrap();
    }

    #[test]
    fn sliced_no_end_future_slice_no_panic() {
        let arc = Arc::new(StringInput::new(""));

        let r = {
            let (s, r) = buffered(16);

            s.send(SendableToken::Start { rule: Rule::a, pos: 0 });

            r
        };

        let stream = parser_stream::new(r, arc);

        assert!(stream.sliced().into_future().wait().is_ok());
    }

    #[test]
    #[should_panic(expected = "expected End { rule: a, .. }, \
                               but found nothing")]
    fn sliced_no_end_pair() {
        let arc = Arc::new(StringInput::new(""));

        let r = {
            let (s, r) = buffered(16);

            s.send(SendableToken::Start { rule: Rule::a, pos: 0 });

            r
        };

        let stream = parser_stream::new(r, arc);

        stream.sliced().map(|stream| {
            stream.collect().wait().unwrap()
        }).collect().wait().unwrap();
    }

    #[test]
    fn sliced_incomplete_slice() {
        let arc = Arc::new(StringInput::new(""));
        let input = Rc::new(arc.clone());

        let (s, r) = buffered(16);

        s.send(SendableToken::Start { rule: Rule::a, pos: 0 });
        s.send(SendableToken::End   { rule: Rule::a, pos: 1 });

        let stream = parser_stream::new(r, arc);

        let (pair, _) = stream.sliced().map(|stream| {
            stream.collect().wait().unwrap()
        }).into_future().wait().map_err(|_| ()).unwrap();

        assert_eq!(pair.unwrap(), vec![
            Token::Start { rule: Rule::a, pos: position::new(input.clone(), 0) },
            Token::End   { rule: Rule::a, pos: position::new(input.clone(), 1) }
        ]);
    }

    #[test]
    fn sliced_error() {
        let arc = Arc::new(StringInput::new(""));
        let input = Rc::new(arc.clone());

        let r = {
            let (s, r) = buffered(16);

            s.send(SendableToken::Start { rule: Rule::a, pos: 0 });
            s.fail(SendableError::CustomErrorPos { message: "e".to_owned(), pos: 2 });

            r
        };

        let stream = parser_stream::new(r, arc);

        assert_eq!(stream.sliced().collect().map(|_| ()).wait(),
                   Err(Error::CustomErrorPos {
                       message: "e".to_owned(),
                       pos: position::new(input.clone(), 2)
                   }));
    }
}
