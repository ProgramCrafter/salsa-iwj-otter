// -*- JavaScript -*-

// Copyright 2020 Ian Jackson
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

// This is a really really shonky BigFloat with only a handful of
// operations available!

class Bigfloat {
  exponent: number,
  limbs: number[], // BE, limbs are each in [ 0, 2^48 )
  // binary point is just before limbs[0]
  // exponent is in limbs
  // sign bit is top bit of limbs[0]
  // always at least one limb

  static type Iterator = function() => BigFlot;

  static const LIMB_BIT      : number = 48;
  private const LIMB_NEGATIVE : number = 1<<47;
  private const LIMB_MODULUS  : number = 1<<48;

  function constructor(j: { e: number, l: number[] }) {
    this.exponent = e;
    this.limbs = l;
  }

  private static l0_value(l0: number) -> number {
    return l0 > Bigfloat.LIMB_NEGATIVE ? l0 - Bigfloat.LIMB_MODULUS : l0;
  }

  private limb_lookup(i) -> number {
    if (i >= this.limbs.length) return 0;
    if (i >= 0) return this.limbs[i];
    let l0 = this.limbs[0];
    return (l0 < Bigfloat.LIMB_NEGATIVE ? 0 : Bigfloat.LIMB_MODULUS-1);
  }

  function cmp(other: Bigfloat): boolean {
    let de = other.exponent - this.exponent;
    if (de) {
      if ((de > 0 ? this : other).limbs[0] > Bigfloat.LIMB_NEGATIVE)
	de *= -1;
      return de;
    }
    let lt0v = BigFloat.l0_value(this.limbs[0]);
    let lo0v = BigFloat.l0_value(other.limbs[0]);
    return lo0 - lt0;
  }

  function extend_left() {
    this.limbs.unshift(l0_value(this.limbs[0]));
    this.exponent++;
  }
  
  function iter_upto(endv: BigFloat, count: number):
  {
    // next() can be called count times
    // to produce values > this, < endv
    let e_out = Number.max(this.exponent, max.exponent);
    for (e = e_out;
	 e;
	 e--) {
      let it = e - this.exponent;
      let ie = e - endv exponent;
      if (it > this.limbs.length && ie > endv.limbs.length) {
	// Oh actually these numbers are equal!
	return function(){ this; }
      }
      let lt = this.limb_lookup(it)
      let le = endv.limb_lookup(ie)
      if (lt == le) continue;

      let avail = (le - lt) & (BIGFLOAT_LIMB_MODULUS-1);
      let start = this.copy();
      while (it < 0) {
	start.extend_left();
	it++;
      }
      let step; // floating!
      if (avail > count+1) {
	step = avail / (count+1);
      } else {
	start[it] += (avail>>1);
	step = Bigfloat.LIMB_MODULUS / (count+1);
	it++;
	start.length = it;
	start[it] = 0;
      }
      return function() {
	start[it] += step;
	start[it] = Math.floor(start[it]);
	start[it] &= (Bigfloat.LIMB_MODULUS-1);
	return start.copy();
      }
    }
  }
}
