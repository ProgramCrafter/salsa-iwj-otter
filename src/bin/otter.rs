//

use structopt::StructOpt;

#[derive(Debug,StructOpt)]
#[structopt(rename_all="kebab-case")]
struct MainOpts {
  #[structopt(long,group="scope",overrides_with("scope"))] server_scope: bool,
  #[structopt(long,group="scope",overrides_with("scope"))] unix: bool,
  #[structopt(long,group="scope",overrides_with("scope"))] unix_user: Option<String>,
  #[structopt(subcommand)]
  cmd: Subcommand,
}

#[derive(Debug,StructOpt)]
enum Subcommand {
  CreateTable {
  }
}

fn main() {
  let opts = MainOpts::from_args();
  println!("{:?}", &opts);
}
