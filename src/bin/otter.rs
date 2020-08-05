//

use game::imports::*;
use argparse::{self,ArgumentParser,action::{TypedAction,ParseResult}};
use argparse::action::{Action,IArgAction};
use std::rc::Rc;
use std::cell::RefCell;

/*
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
 */

struct Call;
struct CallIArgAction<RRF>(RRF);
impl<F> IArgAction for CallIArgAction<Rc<RefCell<F>>>
where F : FnMut(&str) -> ParseResult {
  fn parse_arg(&self, arg: &str) -> ParseResult {
    self.0.borrow_mut()(arg)
  }
}
impl<F: FnMut(&str) -> ParseResult> TypedAction<F> for Call {
  fn bind<'x>(&self, f: Rc<RefCell<&'x mut F>>) -> Action<'x> {
    Action::Single(Box::new(CallIArgAction(f.clone())))
  }
//  fn bind<'x>(&self, t: Rc<RefCell<&'x mut F>>) -> Action<'x> {
}
/*<S,T> (&FnMut(
}
*/
//use structopt::StructOpt;

//#[derive(Debug,StructOpt)]
//#[structopt(rename_all="kebab-case")]
#[derive(Default)]
struct MainOpts {
  scope: Option<ManagementScope>,
}

//#[derive(Debug,StructOpt)]
enum Subcommand {
  CreateTable {
  }
}

fn main() {
  let mainopts : MainOpts = Default::default();
  {
    let ap = ArgumentParser::new();
    /*
    let scope = ap.refer(&mut scope);
    scope.add_option(

    Cell::from_mut(&mut mainopts.scope);
    ap.refer(&mut &scope).
    let opts = MainOpts::from_args();
*/
  }
//  println!("{:?}", &opts);
}
