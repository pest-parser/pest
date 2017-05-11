// pest. The Elegant Parser
// Copyright (C) 2017  Dragoș Tiselice
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

// This structure serves to improve performance over Token objects in two ways:
//
//   * it is smaller than a Token, leading to both less memory use when stored in the queue but also
//     increased speed when pushing to the queue
//   * it finds its pair in O(1) time instead of O(N), since pair positions are known at parse time
//     and can easily be stored instead of recomputed
#[derive(Debug)]
pub enum QueueableToken<R> {
    Start {
        pair: usize,
        pos: usize
    },
    End {
        rule: R,
        pos: usize
    }
}
