// Copyright 2020 Ian Jackson
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use otter_webdriver_tests::*;

#[derive(StructOpt)]
struct Opts {
  #[structopt(long="--no-bwrap")]
  no_bwrap: bool,
}

#[throws(AE)]
fn main(){
  let opts = Opts::from_args();

  if !opts.no_bwrap {
    println!("running bwrap");

    let mut bcmd = Command::new("bwrap");
    bcmd.args("--unshare-net \
             --dev-bind / / \
             --tmpfs /tmp \
             --die-with-parent".split(" "))
      .arg(env::current_exe().expect("failed to find current executable"))
      .arg("--no-bwrap")
      .args(env::args_os().skip(1));

    std::io::stdout().flush().unwrap();
    Err(bcmd.exec()).unwrap()
  }

  println!("hi!");
}
