// -*- JavaScript -*-

// Copyright 2020 Ian Jackson
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

// This is a really really shonky BigFloat with only a handful of
// operations available!

type Bigfloat_Json = number[];

class Bigfloat {
  exponent: number;
  limbs: number[]; // BE, limbs are each in [ 0, 2^48 )
  // binary point is just before limbs[0]
  // exponent is in limbs
  // sign bit is top bit of limbs[0]
  // always at least one limb

  static LIMB_BIT      : number = 48;
  private static LIMB_NEGATIVE : number =  0x800000000000;
  private static LIMB_MODULUS  : number = 0x1000000000000;

  constructor(j: Bigfloat_Json) {
    this.exponent = j[0];
    this.limbs = j.slice(1);
  }
  to_json(): Bigfloat_Json { return [this.exponent].concat(this.limbs); }

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

  iter_upto(endv: Bigfloat, count: number): () => Bigfloat {
    // next() can be called count times
    // to produce values > this, < endv
    let e_out = Math.max(this.exponent, endv.exponent);
    for (let e = e_out;
	 ;
	 e--) {
      let it = this.exponent - e;
      let ie = endv.exponent - e;
      if (it >= this.limbs.length && ie >= endv.limbs.length) {
	// Oh actually these numbers are equal!
	return function(){ return this.clone(); }
      }
      let lt = this.limb_lookup(it)
      let le = endv.limb_lookup(ie)
      if (lt == le) continue;

      let avail = Bigfloat.limb_mask(le - lt);
      let start = this.clone();
      while (it < 0) {
	start.extend_left();
	it++;
      }
      let step; // floating!
      if (avail > count+1) {
	step = avail / (count+1);
      } else {
	start.limbs[it] += Math.floor(avail / 2);
	step = Bigfloat.LIMB_MODULUS / (count+1);
	it++;
	start.limbs.length = it;
	start.limbs[it] = 0;
      }
      return function() {
	start.limbs[it] += step;
	start.limbs[it] = Bigfloat.limb_mask(Math.floor(start.limbs[it]));
	return start.clone();
      }
    }
  }
}
