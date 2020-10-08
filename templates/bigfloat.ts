// -*- JavaScript -*-

// Copyright 2020 Ian Jackson
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

// This is a really really shonky BigFloat with only a handful of
// operations available!


// Representation, and model, ought to have these properties
//     CBOR is binary and compact
//  *  JSON is not lossy
//  *  JSON is human-comprehensible
//  *  JavaScript can compare efficiently
//     JavaScript can do arithmetic efficiently
//  *  Limb size is 48 for fast JS arithmetic
//     Limb size is small for not being too full of padding
//  *  Limb size is big so step algorithm rarely encounters limb boundaries
//
// Many of these are not compatible (in theory extending
// the serde data model might help a bit, but not completely).
// We choose those properties marked with "*".
//
// Main representation is a string:
//   VVVVVVVVVV_VVVVVVVVVV ...]
// where
//   VVVVVVVVVV = 50 bits in lowercase base32hex ("0-9a-..."), unsigned
// Value is a whole number of 50-bit groups ("limbs"), at least one sucb.
//
// The abstract value is completed by an infinite number of zero
// limbs to the right.  Trailing zero limbs are forbidden in the
// representation.
//
// Supported operations are:
//
//    Total ordering
//       Values are compared lexically.
//
//    Select initial value:
//       g000000000
//
//    Pick a value (or "n" values) strictly in between any two
//    existing values.
//
//       Find first limb that they differ.  Divide intervening values
//       for tht limb evenly (ie divide the range by n+1).  If this
//       leaves less than an increment of 1 per output value, do
//       differently: choose a value halfway (rounding down) for the
//       first differeing limb, and add a new limb, whose whole range
//       0000000000..vvvvvvvvvv is divided evenly into n+1 (thus
//       guaranteeing that the added limb is nonzero).
//
//    Pick a value later than a specified value.
//
//       Try to add delta to rightmost nonzero limb, with carry.  If
//       this would overflow top limb, start again: add two limbs
//       0000000000 and then redo (this guarantees that one of the
//       added limbs iis nonzero).  Delta is 0001000000.
//
//    Pick a value earlier than a specified value.
//
//       Try to subtract delta from rightmost nonzero limb, with
//       borrow.  If this would underflow, or would leave leftmost
//       limb equal to 0000000000, start again: decrement rightmost
//       nonzero limb by 1, with borrow, then add two limbs
//       vvvvvvvvvv, and redo.

type Bigfloat = Bigfloats.Packed;

namespace Bigfloats {
  type Json = string;
  export type Packed = string & { readonly phantom: unique symbol };

  export function from_json(s: Json): Packed { return s as Packed; }
  export function to_json(p: Packed): Json { return p; }

  const LIMB_BIT      : number = 50;
  const LIMB_MODULUS  : number = 0x4000000000000;
  const LIMB_DIGITS   : number = 10;
  const RADIX         : number = 32;
  const DELTA         : number = 0x40000000;
/*
  var UNPACK_HEAD_RE = /^([!\+])([0-9a-f]{4}) /;
  var UNPACK_LIMB_RE = / ([0-9a-f]{4})_([0-9a-f]{4})_([0-9a-f]{4})/g;
*/
  type Limb = number;

  type Unpacked = { limbs: Limb[], };

  export function unpack(p: Packed): Unpacked {
    let limbs = [];
    for (var lt of p.split('_')) {
      limbs.push(parseInt(lt, RADIX));
    }
    return { limbs };
  }

  export function pack(v: Unpacked): Packed {
    return v.limbs.map(
      l => (l + LIMB_MODULUS).toString(RADIX).slice(-LIMB_DIGITS)
    ).join('_') as Packed;
  }

  function limb_mask(v: Limb): Limb {
    return (v + LIMB_MODULUS*2) % LIMB_MODULUS;
  }

  function clone(v: Unpacked): Unpacked {
    return { limbs: v.limbs.slice() };
  }

  export function increment(p: Bigfloat): Bigfloat {
    for (;;) {
      let v = unpack(p);
      if (add_with_carry(v, v.limbs.length-1, DELTA))
	return pack(v);
      v.limbs.push(0);
      v.limbs.push(0);
    }
  }

  function add_with_carry(v: Unpacked, i: number, step: number): boolean {
    for (;;) {
      v.limbs[i] = v.limbs[i] + step;
      if (v.limbs[i] < LIMB_MODULUS) return true;
      i--;
      if (i < 0) return false;
      step=1;
    }
  }

  function limb_val_lookup(v: Unpacked, i: Limb): number {
    if (i >= v.limbs.length) return 0;
    return v.limbs[i];
  }

  export function iter_upto(ap: Packed, bp: Packed, count: number):
  () => Packed {
    // result can be called count times to produce values > av, < bv
    let av = unpack(ap);
    let bv = unpack(bp);
    for (let i = 0;
	 ;
	 i++) {
      if (i >= av.limbs.length && i >= bv.limbs.length) {
	// Oh actually these numbers are equal!
	return function(){ return pack(av); }
      }
      let la = limb_val_lookup(av,i);
      let lb = limb_val_lookup(bv,i);
      if (la == lb) continue;
    
      let avail = limb_mask(lb - la);
      let current = clone(av);
      let step : number; // actual floating point!
      if (avail > count+1) {
	step = avail / (count+1);
      } else {
	current.limbs.push(0);
	i++;
	step = LIMB_MODULUS / (count+1);
      }
      step = Math.floor(step);
      return function() {
	current.limbs[i] += step;
	return pack(current);
      }
    }
  }
}
/*

  class Bigfloat {
  exponent: number | null;
  limbs: number[] | null;
// BE, limbs are each in [ 0, 2^48 )
  // binary point is just before limbs[0]



  private static l0_value(l0: number): number {
    return l0 > Bigfloat.LIMB_NEGATIVE ? l0 - Bigfloat.LIMB_MODULUS : l0;
  }

  private limb_lookup(i): number {
    if (i >= this.limbs.length) return 0;
    if (i >= 0) return this.limbs[i];
    let l0 = this.limbs[0];
    return (l0 < Bigfloat.LIMB_NEGATIVE ? 0 : Bigfloat.LIMB_MODULUS-1);
  }

  private static limb_mask(v: number): number {
    return (v + Bigfloat.LIMB_MODULUS*2) % Bigfloat.LIMB_MODULUS;
  }

  cmp(other: Bigfloat): number {
    let de = other.exponent - this.exponent;
    if (de) {
      if ((de > 0 ? this : other).limbs[0] > Bigfloat.LIMB_NEGATIVE)
	de *= -1;
      return de;
    }
    let lt0v = Bigfloat.l0_value(this.limbs[0]);
    let lo0v = Bigfloat.l0_value(other.limbs[0]);
    return lo0v - lt0v;
  }

  clone(): Bigfloat {
    return (Object as any).assign(
      Object.create(Object.getPrototypeOf(this)),
      {
	limbs: this.limbs.slice(),
	...this
      }
    );
  }

  private extend_left() {
    let l0 = this.limbs[0] > Bigfloat.LIMB_NEGATIVE
	? Bigfloat.LIMB_MODULUS-1 : 0;
    this.limbs.unshift(l0);
    this.exponent++;
  }
  
  iter_incdec(stepsign: number): () => Bigfloat {
    // stepsign should be roughly -1 or +1
    let start = this.clone();
    let i = this.exponent;
    while (i < 0) {
      start.extend_left();
      i++;
    }
    while (start.limbs.length <= i) {
      start.limb.push(0);
    }
    let delta = stepsign * 0x000001000000;
    return function(){
      for (;;) {
	start.limbs[i] += delta;
	let j=i;
	for (;;) {
	  let new = Bigfloat.limb_mask(start_limbs[j]);
	  if (sstart_limbs[j] != new) {
	    start.limbs[j] = new;
	    j--;
	  } else if (j==0 &&
		     (start_limbs[j] < Bigfoot.LIMB_NEGATIVE ||
		      start_limbs[j] >= Bigfoot.LIMB_NEGATIVE))
	    
		     
	    while (j<0) {
	      start.extend_left();
	      j++;
	    )
	    start.limbs[j] += Math.sign(stepsign);
	  } else {
	    
	  
	      
	    if (start_limbs[j] < Bigfloat.LIMB_MODULUS) 
	      
	if (it>0 && start.limbs[it] > 
    }
  }
}
*/
