//

use game::imports::*;
use std::cell::Cell;

pub struct CellRef<'a, T> {
  origin: &'a mut T,
  current: Cell<T>,
}

impl<'a,T> CellRef<'a,T> {
  pub fn cloning(origin: &mut T) -> CellRef<T> where T: Clone {
    let current = Cell::new(origin.clone());
    CellRef { origin, current }
  }
  pub fn copying(origin: &mut T) -> CellRef<T> where T: Copy {
    let current = Cell::new(*origin);
    CellRef { origin, current }
  }
  pub fn defaulting(origin: &mut T) -> CellRef<T> where T: Default {
    let current = Cell::new(mem::take(origin));
    CellRef { origin, current }
  }
  pub fn with_placeholder(origin: &mut T, placeholder: T) -> CellRef<T> {
    let current = Cell::new(mem::replace(origin, placeholder));
    CellRef { origin, current }
  }
}

impl<'a,T> Drop for CellRef<'a,T> {
  fn drop(&mut self) {
    mem::swap(self.current.get_mut(), self.origin);
  }
}
impl<'a,T> Deref for CellRef<'a,T> {
  type Target = Cell<T>;
  fn deref(&self) -> &Cell<T> { &self.current }
}

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
