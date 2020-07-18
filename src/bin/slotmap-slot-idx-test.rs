
// results:
// rustc 1.46.0-nightly --release
//
// slotmap::Keydata::get_idx_version               3.259 3.185 3.220
// slotmap_slot_idx::KeyData::get_idx_version      3.724 3.750 3.665

use game::slotmap_slot_idx;

const BATCH : usize = 128;
const ITERS : usize = 10*1024*1024;

fn main () {
  let mut vol_i_buffer = [ 0x123456789abcdefu64; BATCH ];
  let mut vol_o_buffer = [ 0xdeadbeef; BATCH ];

  for _it in 0..ITERS {
    let input = unsafe { std::ptr::read_volatile(&mut vol_i_buffer) };
    let mut output = [ 0u32; BATCH ];

    for (&i,o) in input.iter().zip(output.iter_mut()) {
      let kd = slotmap::KeyData::from_ffi(i);
      let (vsn,_) = slotmap_slot_idx::KeyData::get_idx_version(kd);
      *o = vsn;
    }

    unsafe { std::ptr::write_volatile(&mut vol_o_buffer, output) };
    //eprintln!("{:x?}", vol_o_buffer[0]);
  }
}
