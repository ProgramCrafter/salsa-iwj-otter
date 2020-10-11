// -*- JavaScript -*-
// Copyright 2020 Ian Jackson
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

function assert_eq(a: string, b: string) {
  if (a == b) return;
  console.log(['unequal', a, b]);
  throw('unequal');
}

let x : any;
let y : any
let i : any

x = "!0000 ffff_ffff_fff0" as any;
y = "!0000 0000_0000_0040" as any;
i = Bigfloats.iter_upto(x, y, 4);

assert_eq(i(), "+0000 0000_0000_0000");
assert_eq(i(), "+0000 0000_0000_0010");
assert_eq(i(), "+0000 0000_0000_0020");
assert_eq(i(), "+0000 0000_0000_0030");

x = "!0000 ffff_ffff_fffe" as any;
y = "!0000 0000_0000_0001" as any;
i = Bigfloats.iter_upto(x, y, 4);

assert_eq(i(), "!0000 ffff_ffff_ffff 3333_3333_3333");
assert_eq(i(), "!0000 ffff_ffff_ffff 6666_6666_6666");
assert_eq(i(), "!0000 ffff_ffff_ffff 9999_9999_9999");
assert_eq(i(), "!0000 ffff_ffff_ffff cccc_cccc_cccc");
