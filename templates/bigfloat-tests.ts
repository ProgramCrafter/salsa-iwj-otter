// -*- JavaScript -*-
// Copyright 2020 Ian Jackson
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

function assert_eq(a: string, b: string) {
  if (a != b) throw('unequal ' + a + ' ' + b);
}

let x = "!0000 ffff_ffff_fff0" as any;
let y = "!0000 0000_0000_0040" as any;
let i = Bigfloats.iter_upto(x, y, 4);

assert_eq(i(), "+0000 0000_0000_0000");
assert_eq(i(), "+0000 0000_0000_0010");
assert_eq(i(), "+0000 0000_0000_0020");
assert_eq(i(), "+0000 0000_0000_0030");
