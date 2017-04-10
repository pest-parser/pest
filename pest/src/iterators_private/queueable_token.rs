// pest. The Elegant Parser
// Copyright (C) 2017  Dragoș Tiselice
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

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
