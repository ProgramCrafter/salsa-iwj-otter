//

#![allow(unused_imports)]

use game::imports::*;
use argparse::{self,ArgumentParser,action::{TypedAction,ParseResult}};
use argparse::action::{Action,IFlagAction,IArgAction};
use std::rc::Rc;
use std::cell::RefCell;
use std::cell::Cell;

type E = anyhow::Error;

use argparse::action::ParseResult::Parsed;

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
  &'static str, // command
  &'static str, // desc
  fn(&Subcommand, MainOpts, Vec<String>) -> Result<(),E>,
);
inventory::collect!(Subcommand);

#[derive(Error,Debug)]
struct ArgumentParseError(String);
display_as_debug!(ArgumentParseError);

fn parse_args<T,F,C>(
  args: Vec<String>,
  apmaker: &F,
  completer: &C,
  extra_help: Option<&dyn Fn(&mut dyn Write) -> Result<(), io::Error>>,
) -> T
where T: Default,
      F: Fn(&mut T) -> ArgumentParser,
      C: Fn(&mut T) -> Result<(), ArgumentParseError>,
{
  let mut parsed = Default::default();
  let ap = apmaker(&mut parsed);
  let us = args.get(0).expect("argv[0] must be provided!").clone();

  let mut stdout = io::stdout();
  let mut stderr = io::stderr();

  let r = ap.parse(args, &mut stdout, &mut stderr);
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
      2 => EXIT_USAGE,
      _ => panic!("unexpected error rc {} from ArgumentParser::parse", rc),
    });
  }
  mem::drop(ap);
  completer(&mut parsed).unwrap_or_else(|e:ArgumentParseError| {
    let ap = apmaker(&mut parsed);
    ap.error(&us, &e.0, &mut stderr);
    exit(EXIT_USAGE);
  });
  parsed
}

fn main() {
  #[derive(Default,Debug)]
  struct MainArgs {
    opts: MainOpts,
    subcommand: String,
    subargs: Vec<String>,
  };
  let ma = parse_args::<MainArgs,_,_>(
    env::args().collect(),
  &|ma|{
    use argparse::*;
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
  }, Some(&|w|{
    writeln!(w, "\nSubcommands:")?;
    let maxlen = inventory::iter::<Subcommand>.into_iter()
      .map(|Subcommand(verb,_,_)| verb.len())
      .max().unwrap_or(0);
    for Subcommand(verb,desc,_) in inventory::iter::<Subcommand> {
      writeln!(w, "  {:width$}  {}", verb, desc, width=maxlen)?;
    }
    Ok(())
  }));

  let sc = inventory::iter::<Subcommand>.into_iter()
    .filter(|Subcommand(found,_,_)| found == &ma.subcommand)
    .next()
    .unwrap_or_else(||{
      eprintln!("subcommand `{}' not recognised", &ma.subcommand);
      exit(EXIT_USAGE);
    });
  let Subcommand(_,_,call) = sc;

  let mut subargs = ma.subargs;
  subargs.insert(0, format!("{} {}",
                            env::args().next().unwrap(),
                            &ma.subcommand));

  call(sc, ma.opts, subargs).expect("execution error");
}

type Conn = MgmtChannel;

#[throws(E)]
fn connect(_ma: &MainOpts) -> MgmtChannel {
  let unix = UnixStream::connect(SOCKET_PATH).context("connect to server")?;
  let chan = MgmtChannel::new(unix)?;
  // xxx set scope
  chan
}

mod create_table {
  use super::*;

  #[derive(Default,Debug)]
  struct Args {
    name: String,
    file: String,
  }

  fn subargs(sa: &mut Args) -> ArgumentParser {
      use argparse::*;
      let mut ap = ArgumentParser::new();
      ap.refer(&mut sa.name).required()
        .add_argument("TABLE-NAME",Store,"table name");
      ap.refer(&mut sa.file).required()
        .add_argument("TABLE-SPEC-TOML",Store,"table spec");
      ap
  }

  #[throws(ArgumentParseError)]
  fn complete(_sa: &mut Args) { }

  #[throws(E)]
  fn call(_sc: &Subcommand, ma: MainOpts, args: Vec<String>) {
    let args = parse_args::<Args,_,_>(args, &subargs, &complete, None);

    let spec = (||{
      let mut f = File::open(&args.file).context("open")?;
      let mut buf = String::new();
      f.read_to_string(&mut buf).context("read")?;
      let spec : TableSpec = toml::de::from_str(&buf).context("parse")?;
      <Result<_,AE>>::Ok(spec)
    })().context("game spec toml").with_context(|| args.file.to_owned())?;

    let chan = connect(&ma)?;

    /*

    chan.cmd(MgmtCommand::CreateGame {
      CreateGame {
        name: args.name,
        insns: vec![
          MgmtGameInstruction {

          },
        ]*/

    eprintln!("CREATE-TABLE {:?} {:?}", &ma, &args);
  }

  inventory::submit!{Subcommand(
    "create-table",
    "Create a new table",
    call,
  )}
}


/*
impl Default for Args {
  fn default() -> Args { Args { name: String::new(), file: String::new() }}
}*/
