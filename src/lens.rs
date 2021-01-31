// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use crate::imports::*;

// this ios going to be replaced with new "hidden" machiner
//
// For now, we are firstly removing all calls to everything except
// new_hidden_todo.
//
// Then we'll adjust all call sites of new_hidden_todo too and Lens
// can be abolished.

pub trait Lens : Debug {
}
#[derive(Debug)]
pub struct TransparentLens {
}
impl Lens for TransparentLens {
}

