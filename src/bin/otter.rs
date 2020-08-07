//

#![allow(unused_imports)]

use game::imports::*;
use argparse::{self,ArgumentParser,action::{TypedAction,ParseResult}};
use argparse::action::{Action,IFlagAction,IArgAction};
use std::rc::Rc;
use std::cell::RefCell;
use std::cell::Cell;

use argparse::action::ParseResult::Parsed;

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

#[derive(Clone)]
struct MapStore<T, F: FnMut(&str) -> Result<T,String> > (
  F
);

struct BoundMapStore<'r, T, F: FnMut(&str) -> Result<T,String>> {
  f: Rc<RefCell<F>>,
  r: Rc<RefCell<&'r mut T>>,
}

impl<'f,T,F> TypedAction<T> for MapStore<T,F>
where F : 'static + Clone + FnMut(&str) -> Result<T,String> {
  fn bind<'x>(&self, r: Rc<RefCell<&'x mut T>>) -> Action<'x>
  {
    Action::Single(Box::new(BoundMapStore {
      f: Rc::new(RefCell::new(self.0.clone())),
      r
    }))
  }
}

impl<'x, T, F: FnMut(&str) -> Result<T,String>>
  IArgAction for BoundMapStore<'x, T, F>
{
  fn parse_arg(&self, arg: &str) -> ParseResult {
    let v = match self.f.borrow_mut()(arg) {
      Ok(r) => r,
      Err(e) => return ParseResult::Error(e),
    };
    *self.r.borrow_mut(arg) = v;
    ParseResult::Parsed
  }
}

//#[derive(Debug,StructOpt)]
//#[structopt(rename_all="kebab-case")]
#[derive(Debug,Default)]
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
    use argparse::*;
    let mut ap = ArgumentParser::new();
    let scope = ap.refer(&mut mainopts.scope);
    scope.add_option(&["--scope-server"],
                     Store(Some(ManagementScope::Server)),
                     "use Server scope");
    scope.add_option(&["--scope-unix-user"],
                     MapStore(|user| ManagementScope::Unix { user }),
                     "use specified unix user scope");
    let r = ap.parse_args();
    mem::drop(ap);
    r
    /*

    Cell::from_mut(&mut mainopts.scope);
    let opts = MainOpts::from_args();
*/
  }.expect("parse argw");
  println!("{:?}", &mainopts);
}
