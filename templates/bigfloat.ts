// -*- JavaScript -*-

// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

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

  }

  export function iter_upto(ap: Packed, bp: Packed, count: number):
  () => Packed {
    // result can be called count times to produce values > av, < bv
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
