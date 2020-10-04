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
// Transport, and main JS, representation is a string:
//   SEEEE VVVV_VVVV_VVVV VVVV_VVVV_VVVV ...
// where
//   S = ! or +
//   EEEE = 16 bit exponent (hex)
//   VVVV = 16 bits of mantissa, two's complement
// Value represented is
//    0x0.VVVV...VVVV * 2^0xEEEE
//  - 0x1.0           * 2^0eEEEE if S is !
// Mantissa comes in 48-bit groups, at least one sucb
//
// This is 20 bytes of string for a 1-limb number, which
// would be 64 bits if we made it as compact as possible.

type Bigfloat = Bigfloats.Packed;

namespace Bigfloats {
  type Json = string;
  export type Packed = string & { readonly phantom: unique symbol };

  export function from_json(s: Json): Packed { return s as Packed; }
  export function to_json(p: Packed): Json { return p; }

  const LIMB_BIT      : number = 48;
  const LIMB_NEGATIVE : number =  0x800000000000;
  const LIMB_MODULUS  : number = 0x1000000000000;

  var UNPACK_HEAD_RE = /^([!\+])([0-9a-f]{4}) /;
  var UNPACK_LIMB_RE = / ([0-9a-f]{4})_([0-9a-f]{4})_([0-9a-f]{4})/g;

  type Limb = number;

  type Unpacked = {
    sign: number,
    exponent: number,
    limbs: Limb[], // BE
  };

  export function unpack(p: Packed): Unpacked {
    let head = p.match(UNPACK_HEAD_RE);
    UNPACK_LIMB_RE.lastIndex = 0;
    let limbs = [];
    let m;
    while (m = UNPACK_LIMB_RE.exec(p)) {
      m[0] = '0x';
      limbs.push(+m.join(''));
    }
    return {
      sign: head[1] == '!' ? -1 : +1,
      exponent: +('0x' + head[2]),
      limbs,
    };
  }

  export function pack(v: Unpacked): Packed {
    function hex16(x: number) { return ('000' + x.toString(16)).slice(-4); }
    function hex48(x: Limb) {
      return (hex16(Math.floor(x / 0x100000000)       ) + '_' +
	      hex16(          (x &  0xffff0000) >> 16 ) + '_' +
	      hex16(           x &  0x0000ffff        )        );
    }
    return (
      (v.sign < 0 ? '!' : '+') +
	hex16(v.exponent) + ' ' +
	v.limbs.map(hex48).join(' ')
    ) as Packed;
  }
 
  function ms_limb_from_sign(v: Unpacked): Limb {
    return (v.sign < 0 ? LIMB_MODULUS-1 : 0);
  }

  function limb_lookup(v: Unpacked, i: Limb): number {
    if (i >= v.limbs.length) return 0;
    if (i < 0) return ms_limb_from_sign(v);
    return v.limbs[i];
  }

  function limb_mask(v: Limb): Limb {
    return (v + LIMB_MODULUS*2) % LIMB_MODULUS;
  }

  function clone(v: Unpacked): Unpacked {
    return {
      limbs: v.limbs.slice(),      
      ...v
    }
  }

  function extend_left_so_index_valid(v: Unpacked, i: number): number {
    // returns adjustment to apply to index
    let newlimb = ms_limb_from_sign(v);
    let adj = 0;
    while (i < 0) {
      this.limbs.unshift(newlimb);
      this.exponent++;
      i++;
      adj++;
    }
    return adj;
  }

  function add_to_limb(v: Unpacked, i: number, step: number): number {
    // returns adjustment to apply to index
    let totadj = 0;
    for (;;) {
      v.limbs[i] = Math.floor(v.limbs[i] + step);
      if (v.limbs[i] < LIMB_MODULUS) return totadj;
      i--;
      if (i < 0) {
	if (v.sign < 0) { v.sign = +1; return totadj; }
	let adj = extend_left_so_index_valid(v, i);
	i += adj;
	totadj += adj;
      }
      step=1;
    }
  }

  export function add(p: Bigfloat, step: number): Bigfloat {
    let v = unpack(p);
    add_to_limb(v, 0, step * 0x10000);
    return pack(v);
  }

  export function iter_upto(ap: Packed, bp: Packed, count: number):
  () => Packed {
    let av = unpack(ap);
    let bv = unpack(bp);
    // result can be called count times to produce values > av, < bv
    let e_out = Math.max(av.exponent, bv.exponent);
    for (let e = e_out;
	 ;
	 e--) {
      let ia = av.exponent - e;
      let ib = bv.exponent - e;
      if (ia >= av.limbs.length && ib >= bv.limbs.length) {
	// Oh actually these numbers are equal!
	return function(){ return this.pack(); }
      }
      let la = limb_lookup(av,ia);
      let lb = limb_lookup(bv,ib);
      if (la == lb) continue;
      let avail = limb_mask(lb - la);

      let current = clone(av);
      let i = ia + extend_left_so_index_valid(current, ia);
      let step; // floating!
      if (avail > count+1) {
	step = avail / (count+1);
      } else {
	i += add_to_limb(current, i, avail / 2);
	step = LIMB_MODULUS / (count+1);
	i++;
	current.limbs.length = i;
	current.limbs[i] = 0;
      }
      return function() {
	i += add_to_limb(current, i, step);
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
