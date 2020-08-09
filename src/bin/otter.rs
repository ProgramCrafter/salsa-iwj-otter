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
where F : 'f + Clone + FnMut(&str) -> Result<T,String>,
     'f : 'static // ideally TypedAction wuld have a lifetime parameter
{
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
    let v : T = match self.f.borrow_mut()(arg) {
      Ok(r) => r,
      Err(e) => return ParseResult::Error(e),
    };
    **self.r.borrow_mut() = v;
    ParseResult::Parsed
  }
}

const EXIT_USAGE :    i32 = 12;
const EXIT_DISASTER : i32 = 16;

#[derive(Debug,Default)]
struct MainOpts {
  scope: Option<ManagementScope>,
}

struct Subcommand (
  &'static str,
  fn(MainOpts, &[String]),
);
inventory::collect!(Subcommand);

inventory::submit!{Subcommand(
  "create-table", |mainopts, args|{
    eprintln!("CREATE-TABLE {:?} {:?}", &mainopts, &args);
  }
)}

#[derive(Error,Debug)]
struct ArgumentParseError(String);
display_as_debug!(ArgumentParseError);

fn parse_args<T,F,C>(
  args: Vec<String>,
  apmaker: &F,
  completer: &C,
  extra_help: Option<&mut dyn FnMut(&mut dyn Write) -> Result<(), io::Error>>,
) -> T
where T: Default,
      F: Fn(&mut T) -> ArgumentParser,
      C: Fn(&mut T) -> Result<(), ArgumentParseError>,
{
  let mut parsed = Default::default();
  let ap = apmaker(&mut parsed);

  let mut stdout = io::stdout();

  let r = ap.parse(args, &mut stdout, &mut io::stderr());
  if let Err(rc) = r {
    exit(match rc {
      0 => {
        if let Some(eh) = extra_help {
          eh(&mut stdout).unwrap_or_else(|e|{
            eprintln!("write help to stdout: {:?}", &e);
            exit(EXIT_DISASTER);
          });
        }
        0
      },
      _ => EXIT_USAGE,
    });
  }
  mem::drop(ap);
  completer(&mut parsed).unwrap_or_else(|e| {
    eprintln!("bad usage: {}", &e);
    exit(EXIT_USAGE);
  });
  parsed
}

fn main() {
  use argparse::*;

  #[derive(Default,Debug)]
  struct MainArgs {
    opts: MainOpts,
    subcommand: String,
    subargs: Vec<String>,
  };
  let ma = parse_args::<MainArgs,_,_>(
    env::args().collect(),
  &|ma|{
    let mut ap = ArgumentParser::new();
    ap.stop_on_first_argument(true);
    ap.silence_double_dash(true);
    ap.refer(&mut ma.subcommand).required().add_argument("SUBCOMMAND",Store,
                                      "subcommand");
    ap.refer(&mut ma.subargs).add_argument("...",Collect,
                                   "subcommand options/arguments");

    let mut scope = ap.refer(&mut ma.opts.scope);
    scope.add_option(&["--scope-server"],
                     StoreConst(Some(ManagementScope::Server)),
                     "use Server scope");
    scope.metavar("USER").add_option(&["--scope-unix-user"],
                     MapStore(|user| Ok(Some(ManagementScope::Unix {
                       user: user.into()
                     }))),
                     "use specified unix user scope");
    scope.add_option(&["--scope-unix"],
                     StoreConst(None),
                     "use unix user $USER scope (default)");
    ap
  }, &|ma| {
    if let ref mut scope @None = ma.opts.scope {
      let user = env::var("USER").map_err(|e| ArgumentParseError(
        format!("--scope-unix needs USER env var: {}", &e)
      ))?;
      *scope = Some(ManagementScope::Unix { user });
    }
    Ok(())
  }, None);

  let Subcommand(_,call) = inventory::iter::<Subcommand>.into_iter()
    .filter(|Subcommand(found,_)| found == &ma.subcommand)
    .next()
    .unwrap_or_else(||{
      eprintln!("subcommand `{}' not recognised", &ma.subcommand);
      exit(EXIT_USAGE);
    });

  call(ma.opts, &ma.subargs);
}
