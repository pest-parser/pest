// pest. Elegant, efficient grammars
// Copyright (C) 2016  Dragoș Tiselice
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

#[macro_use]
extern crate pest;

use std::collections::LinkedList;

use pest::prelude::*;

#[derive(Debug, PartialEq)]
pub enum Node {
    Sentence(LinkedList<Node>),
    Word(LinkedList<Node>),
    Letter(char)
}

impl_rdp! {
    grammar! {
        sentence = _{ word ~ ([" "] ~ word)* }
        word     =  { letter* }
        letter   =  { ['a'..'z'] }
    }

    process! {
        main(&self) -> Node {
            (list: _sentence()) => {
                Node::Sentence(list)
            }
        }

        _sentence(&self) -> LinkedList<Node> {
            (_: word, head: _word(), mut tail: _sentence()) => {
                tail.push_front(Node::Word(head));

                tail
            },
            () => {
                LinkedList::new()
            }
        }

        _word(&self) -> LinkedList<Node> {
            (&head: letter, mut tail: _word()) => {
                tail.push_front(Node::Letter(head.chars().next().unwrap()));

                tail
            },
            () => {
                LinkedList::new()
            }
        }
    }
}

#[test]
fn word() {
    let mut parser = Rdp::new(StringInput::new("abc def"));

    assert!(parser.sentence());

    let mut letters = LinkedList::new();

    letters.push_back(Node::Letter('a'));
    letters.push_back(Node::Letter('b'));
    letters.push_back(Node::Letter('c'));

    let word1 = Node::Word(letters);

    let mut letters = LinkedList::new();

    letters.push_back(Node::Letter('d'));
    letters.push_back(Node::Letter('e'));
    letters.push_back(Node::Letter('f'));

    let word2 = Node::Word(letters);

    let mut words = LinkedList::new();

    words.push_back(word1);
    words.push_back(word2);

    let sentence = Node::Sentence(words);

    assert_eq!(parser.main(), sentence);
}
