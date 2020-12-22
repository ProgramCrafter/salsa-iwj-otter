// Copyright 2020 Ian Jackson
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use otter_webdriver_tests::*;

#[throws(AE)]
fn main(){
  let s = setup()?;

  println!("hi! {:#?}", &s);

  for _ in 0..2 {
    let mut c = Command::new("xdpyinfo");
    let s = c
      .spawn().context("spawn")?
      .wait().context("wait")?;
    println!("s = {:?}", &s);

    std::thread::sleep_ms(500);
  }
}
