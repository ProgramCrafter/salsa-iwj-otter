//

use game::imports::*;
use argparse::{self,ArgumentParser,action::{TypedAction,ParseResult}};
use argparse::action::{Action,IFlagAction,IArgAction};
use std::rc::Rc;
use std::cell::RefCell;
use std::cell::Cell;

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

struct CallAction<C,F>(F, PhantomData<C>);

struct CallFlag;

impl<F> IFlagAction for CallAction<CallFlag,Rc<RefCell<F>>>
where F : FnMut() -> ParseResult {
  fn parse_flag(&self) -> ParseResult {
    self.0.borrow_mut()()
  }
}

impl<F: FnMut() -> ParseResult> TypedAction<F> for CallFlag {
  fn bind<'x>(&self, f: Rc<RefCell<&'x mut F>>) -> Action<'x> {
    Action::Flag(Box::new(CallAction(f.clone(), PhantomData::<Self>)))
  }
}

struct CallArg;

impl<F> IArgAction for CallAction<CallArg,Rc<RefCell<F>>>
where F : FnMut(&str) -> ParseResult {
  fn parse_arg(&self, arg: &str) -> ParseResult {
    self.0.borrow_mut()(arg)
  }
}

impl<F: FnMut(&str) -> ParseResult> TypedAction<F> for CallArg {
  fn bind<'x>(&self, f: Rc<RefCell<&'x mut F>>) -> Action<'x> {
    Action::Single(Box::new(CallAction(f.clone(), PhantomData::<Self>)))
  }
}

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
  let mut mainopts : MainOpts = Default::default();
  {
    let ap = ArgumentParser::new();
    let scope = Cell::from_mut(&mut mainopts.scope);
    /*

    Cell::from_mut(&mut mainopts.scope);
    ap.refer(&mut &scope).
    let opts = MainOpts::from_args();
*/
  }
//  println!("{:?}", &opts);
}
