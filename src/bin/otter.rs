// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

#![allow(unused_imports)]

use otter::imports::*;

use std::cell::Cell;
use std::cell::RefCell;
use std::rc::Rc;

use argparse::{self,ArgumentParser,action::{TypedAction,ParseResult}};
use argparse::action::{Action,IFlagAction,IArgAction};
use derive_more::Display;

use otter::prelude::*;
use otter::commands::*;

type APE = ArgumentParseError;
type E = anyhow::Error;
type PL = PresentationLayout;
type TP = TablePermission;

use argparse::action::ParseResult::Parsed;

#[derive(Clone)]
struct MapStore<T, F: FnMut(&str) -> Result<T, String>>(F);

struct BoundMapStore<'r, T, F: FnMut(&str) -> Result<T,String>> {
  f: Rc<RefCell<F>>,
  r: Rc<RefCell<&'r mut T>>,
}

impl<'f,T,F> TypedAction<T> for MapStore<T,F>
where F: 'f + Clone + FnMut(&str) -> Result<T,String>,
     'f: 'static // ideally TypedAction wuld have a lifetime parameter
{
  fn bind<'x>(&self, r: Rc<RefCell<&'x mut T>>) -> Action<'x> {
    Action::Single(Box::new(BoundMapStore {
      f: Rc::new(RefCell::new(self.0.clone())),
      r,
    }))
  }
}

impl<'x, T, F: FnMut(&str) -> Result<T,String>>
  IArgAction for BoundMapStore<'x, T, F>
{
  fn parse_arg(&self, arg: &str) -> ParseResult {
    let v: T = match self.f.borrow_mut()(arg) {
      Ok(r) => r,
      Err(e) => return ParseResult::Error(e),
    };
    **self.r.borrow_mut() = v;
    ParseResult::Parsed
  }
}

#[derive(Debug)]
struct MainOpts {
  account: AccountName,
  nick: Option<String>,
  timezone: Option<String>,
  layout: Option<PresentationLayout>,
  access: Option<AccessOpt>,
  socket_path: String,
  verbose: i32,
  superuser: bool,
  spec_dir: String,
}

impl MainOpts {
  pub fn instance_name(&self, table_name: &str) -> InstanceName {
    match table_name.strip_prefix(":") {
      Some(rest) => {
        InstanceName {
          account: self.account.clone(),
          game: rest.into(),
        }
      }
      None => {
        table_name.parse().unwrap_or_else(|e|{
          eprintln!(
            "instance name must start with : or be valid full name: {}",
            &e);
          exit(EXIT_USAGE);
        })
      }
    }
  }
}

struct Subcommand (
  &'static str, // command
  &'static str, // desc
  fn(&'static Subcommand, MainOpts, Vec<String>) -> Result<(),E>,
);
inventory::collect!(Subcommand);

#[derive(Error,Debug,Clone,Display)]
struct ArgumentParseError(String);

impl From<&anyhow::Error> for ArgumentParseError {
  fn from(ae: &anyhow::Error) -> ArgumentParseError {
    eprintln!("error during argument parsing/startup: {}\n", ae);
    exit(EXIT_USAGE);
  }
}

pub type ApMaker<'apm, T> =
  &'apm dyn for <'a> Fn(&'a mut T) -> ArgumentParser<'a>;

fn parse_args<T:Default,U>(
  args: Vec<String>,
  apmaker: ApMaker<T>,
  completer: &dyn Fn(T) -> Result<U, ArgumentParseError>,
  extra_help: Option<&dyn Fn(&mut dyn Write) -> Result<(), io::Error>>,
) -> U {
  let mut parsed = default();
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
  let completed  = completer(parsed)
    .unwrap_or_else(|e:ArgumentParseError| {
      let mut def = default();
      let ap = apmaker(&mut def);
      ap.error(&us, &e.0, &mut stderr);
      exit(EXIT_USAGE);
    });
  completed
}

pub fn ok_id<T,E>(t: T) -> Result<T,E> { Ok(t) }

pub fn clone_via_serde<T: Debug + Serialize + DeserializeOwned>(t: &T) -> T {
  (|| {
    let s = serde_json::to_string(t).context("ser")?;
    let c = serde_json::from_str(&s).context("de")?;
    Ok::<_,AE>(c)
  })()
    .with_context(|| format!("clone {:?} via serde failed", t))
    .unwrap()
}

#[derive(Debug)]
struct AccessOpt(Box<dyn PlayerAccessSpec>);
impl Clone for AccessOpt {
  fn clone(&self) -> Self { Self(clone_via_serde(&self.0)) }
}
impl<T: PlayerAccessSpec + 'static> From<T> for AccessOpt {
  fn from(t: T) -> Self { AccessOpt(Box::new(t)) }
}
impl From<AccessOpt> for Box<dyn PlayerAccessSpec> {
  fn from(a: AccessOpt) -> Self { a.0 }
}

type ExecutableRelatedError = AE;
fn ere(s: String) -> ExecutableRelatedError { anyhow!(s) }

#[throws(ExecutableRelatedError)]
pub fn find_executable() -> String {
  let e = env::current_exe()
    .map_err(|e| ere(
      format!("could not find current executable ({})", &e)
    ))?;
  let s = e.to_str()
    .ok_or_else(|| ere(
      format!("current executable has non-UTF8 filename!")
    ))?;
  s.into()
}

pub fn in_basedir(verbose: bool,
                  from: Result<String,ExecutableRelatedError>,
                  from_what: &str,
                  from_exp_in: &str, from_must_be_in_exp: bool,
                  now_what: &str,
                  then_in: &str,
                  leaf: &str,
                  local_subdir: &str)
                  -> String
{
  match (||{
    let from = from?;
    if from_must_be_in_exp {
      let mut comps = from.rsplitn(3,'/').skip(1);
      if_chain! {
        if Some(from_exp_in) == comps.next();
        if let Some(path) = comps.next();
        then { Ok(path.to_string()) }
        else { Err(ere(
          format!("{} is not in a directory called {}", from_what, from_exp_in)
        )) }
      }
    } else {
      let mut comps = from.rsplitn(2,'/');
      if_chain! {
        if let Some(dirname) = comps.nth(1);
        let mut dir_comps = dirname.rsplitn(2,'/');
        then {
          if_chain! {
            if Some(from_exp_in) == dir_comps.next();
            if let Some(above) = dir_comps.next();
            then { Ok(above.to_string()) }
            else { Ok(dirname.to_string()) }
          }
        }
        else {
          Ok(from.to_string())
        }
      }
    }
  })() {
    Err(whynot) => {
      let r = format!("{}/{}", local_subdir, leaf);
      if verbose {
        eprintln!("{}: looking for {} in {}", &whynot, now_what, &r);
      }
      r
    }
    Ok(basedir) => {
      format!("{}/{}/{}", basedir, then_in, leaf)
    }
  }
}

// argparse is pretty insistent about references and they are awkward
#[ext]
impl String {
  fn leak(self) -> &'static str { Box::<str>::leak(self.into()) }
}

fn main() {
  #[derive(Default,Debug)]
  struct RawMainArgs {
    account: Option<AccountName>,
    socket_path: Option<String>,
    nick: Option<String>,
    timezone: Option<String>,
    layout: Option<PresentationLayout>,
    access: Option<AccessOpt>,
    verbose: i32,
    config_filename: Option<String>,
    superuser: bool,
    subcommand: String,
    subargs: Vec<String>,
    spec_dir: Option<String>,
  }
  let (subcommand, subargs, mo) = parse_args::<RawMainArgs,_>(
    env::args().collect(),
  &|rma|{
    use argparse::*;
    let mut ap = ArgumentParser::new();
    ap.stop_on_first_argument(true);
    ap.silence_double_dash(true);

    ap.refer(&mut rma.subcommand).required()
      .add_argument("SUBCOMMAND", Store, "subcommand");
    ap.refer(&mut rma.subargs)
      .add_argument("...", Collect, "subcommand options/arguments");

    let mut account = ap.refer(&mut rma.account);
    account.metavar("ACCOUNT")
      .add_option(&["--account"],
                  StoreOption,
                  "use account ACCOUNT (default: unix:<current user>:)");
    ap.refer(&mut rma.nick).metavar("NICK")
      .add_option(&["--nick"],
                  StoreOption,
                  "use NICK as nick for joining games (now and in the future) \
                   (default: derive from account name");
    ap.refer(&mut rma.timezone).metavar("TZ")
      .add_option(&["--timezone"],
                  StoreOption,
                  "display times in timezone TZ (Olson timezone name) \
                   (default is to use server's default timezone)");

    let mut layout = ap.refer(&mut rma.layout);
    layout.add_option(&["--layout-portrait"],
                      StoreConst(Some(PL::Portrait)),
                      "set account to display in portrait (by default)");
    layout.add_option(&["--layout-landscape"],
                      StoreConst(Some(PL::Landscape)),
                      "set account to display in landscape (by default)");
    layout.add_option(&["--layout-default"],
                      StoreConst(None),
                      "do not modify default layout");

    let mut access = ap.refer(&mut rma.access);
    access.metavar("EMAIL-ADDRESS")
      .add_option(&["--email"],
                  MapStore(|addr| Ok(Some(
                    TokenByEmail { addr: addr.to_string() }.into()
                  ))),
                  "send token by email, to EMAIL-ADDRESS \
                   (RFC822 recipient field syntax)");
    access.add_option(&["--url-on-stdout"],
                      StoreConst(Some(UrlOnStdout.into())),
                      "show game access url by printing to stdout");
    access.metavar("TOKEN")
      .add_option(&["--fixed-token"],
                  MapStore(|s| Ok(Some(
                    FixedToken { token: RawToken(s.to_string()) }.into()
                  ))),
                  "use fixed game access token TOKEN \
                   (for administrators, with --super, only; \
                   only `reset', not `redelivery', of tokens is possible)");
    access.add_option(&["--no-access-token"],
                      StoreConst(Some(PlayerAccessUnset.into())),
                      "do not show game access info (for testing only)");

    ap.refer(&mut rma.socket_path)
      .add_option(&["--socket"], StoreOption,
                  "specify server socket path");
    ap.refer(&mut rma.config_filename)
      .add_option(&["-C","--config"], StoreOption,
                  "specify server config file (used for finding socket)");
    let mut verbose = ap.refer(&mut rma.verbose);
    verbose.add_option(&["-q","--quiet"], StoreConst(-1),
                       "set verbosity to error messages only");
    verbose.add_option(&["-v","--verbose"], IncrBy(1),
       "increase verbosity (default is short progress messages)");

    ap.refer(&mut rma.superuser)
      .add_option(&["--super"], StoreTrue,
                  "enable game server superuser access");

    ap.refer(&mut rma.spec_dir)
      .add_option(&["--spec-dir"], StoreOption,
                  "directory for table and game specs");

    ap
  }, &|RawMainArgs {
    account, nick, timezone,
    access, socket_path, verbose, config_filename, superuser,
    subcommand, subargs, spec_dir, layout,
  }|{
    env_logger::Builder::new()
      .filter_level(log::LevelFilter::Info)
      .parse_env("OTTER_CLI_LOG")
      .init();

    let config = Thunk::<Result<(Arc<ServerConfig>,String),APE>,_>::new(
      move ||{
        (||{
          let config_filename = config_filename
            .unwrap_or_else(||{
              let exe = find_executable();
              in_basedir(verbose > 1, exe, "current executable",
                         "bin", true,
                         "config file", "etc", DEFAULT_CONFIG_LEAFNAME,
                         ".")
            });
          ServerConfig::read(Some(&config_filename), default())
            .context("read config file")?;
          Ok::<_,AE>((otter::config::config(), config_filename))
        })().map_err(|e| ArgumentParseError(
          format!("failed to find/load config: {}", &e)
        ))
      });

    let spec_dir = spec_dir.map(Ok::<_,APE>).unwrap_or_else(||{
      let cfgf = config.clone().map(|(_c,f)| f).map_err(Into::into);
      let spec_dir = in_basedir(verbose > 1,
                                cfgf, "config filename",
                                "etc", false,
                                "game and table specs", "specs", "",
                                ".")
        .trim_end_matches('/').to_string();
      Ok(spec_dir)
    })?;

    let account: AccountName = account.map(Ok::<_,APE>).unwrap_or_else(||{
      let user = env::var("USER").map_err(|e| ArgumentParseError(
        format!("default account needs USER env var: {}", &e)
      ))?;
      Ok(AccountName {
        scope: AS::Unix { user },
        subaccount: "".into(),
      })
    })?;

    let socket_path = socket_path.map(Ok::<_,APE>).unwrap_or_else(||{
      Ok(
        config.clone()?.0
          .command_socket.clone()
      )
    })?;
    Ok((subcommand, subargs, MainOpts {
      account,
      access,
      nick,
      timezone,
      layout,
      socket_path,
      verbose,
      superuser,
      spec_dir,
    }))
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
    .filter(|Subcommand(found,_,_)| found == &subcommand)
    .next()
    .unwrap_or_else(||{
      eprintln!("subcommand `{}' not recognised", &subcommand);
      exit(EXIT_USAGE);
    });
  let Subcommand(_,_,call) = sc;

  let mut subargs = subargs;
  subargs.insert(0, format!("{} {}",
                            env::args().next().unwrap(),
                            &subcommand));

  call(sc, mo, subargs).unwrap_or_else(|e|{
    #[derive(Default,Debug)] struct Sol { any: bool }
    impl Sol {
      fn nl(&mut self) {
        if self.any { eprintln!("") };
        self.any = false;
      }
      fn head(&mut self) {
        if ! self.any { eprint!("otter: error"); }
        self.any = true
      }
    }
    let mut sol: Sol = default();
    for e in e.chain() {
      let s = e.to_string();
      let long = s.len() > 80;
      if long && sol.any { sol.nl() }
      sol.head();
      eprint!(": {}", s);
      if long { sol.nl() }
    }
    sol.nl();
    exit(12);
  })
}

struct Conn {
  chan: MgmtChannel,
}

deref_to_field_mut!{Conn, MgmtChannel, chan}

impl Conn {
  #[throws(AE)]
  fn prep_access_account(&mut self, ma: &MainOpts) {
    #[derive(Debug)]
    struct Wantup(bool);
    impl Wantup {
      fn u<T:Clone>(&mut self, rhs: &Option<T>) -> Option<T> {
        if rhs.is_some() { self.0 = true }
        rhs.clone()
      }
    }
    let mut wantup = Wantup(false);
    let mut ad = AccountDetails {
      account:  ma.account.clone(),
      nick:     wantup.u(&ma.nick),
      timezone: wantup.u(&ma.timezone),
      layout:   wantup.u(&ma.layout),
      access:   wantup.u(&ma.access).map(Into::into),
    };

    fn is_no_account<T>(r: &Result<T, anyhow::Error>) -> bool {
      if_chain! {
        if let Err(e) = r;
          if let Some(&ME::AccountNotFound(_)) = e.downcast_ref();
        then { return true }
        else { return false }
      }
    }

    {
      let mut desc;
      let mut resp;
      if wantup.0 {
        desc = "UpdateAccount";
        resp = self.cmd(&MC::UpdateAccount(clone_via_serde(&ad)));
      } else {
        desc = "CheckAccount";
        resp = self.cmd(&MC::CheckAccount);
      };
      if is_no_account(&resp) {
        ad.access.get_or_insert(Box::new(UrlOnStdout));
        desc = "CreateAccount";
        resp = self.cmd(&MC::CreateAccount(clone_via_serde(&ad)));
      }
      resp.with_context(||format!("response to {}", &desc))?;
    }
  }
}

#[throws(E)]
fn connect(ma: &MainOpts) -> Conn {
  let chan = MgmtChannel::connect(&ma.socket_path)?;
  let mut chan = Conn { chan };
  if ma.superuser {
    chan.cmd(&MC::SetSuperuser(true))?;
  }
  chan.cmd(&MC::SelectAccount(ma.account.clone()))?;
  chan
}

const PLAYER_ALWAYS_PERMS: &[TablePermission] = &[
  TP::TestExistence,
  TP::ShowInList,
  TP::ViewNotSecret,
  TP::Play,
];

const PLAYER_DEFAULT_PERMS: &[TablePermission] = &[
  TP::ChangePieces,
  TP::UploadBundles,
];

#[throws(AE)]
fn setup_table(_ma: &MainOpts, instance_name: &InstanceName, spec: &TableSpec)
               -> Vec<MGI> {
  let TableSpec { players, player_perms, acl, links } = spec;
  let mut player_perms = player_perms.clone()
    .unwrap_or(PLAYER_DEFAULT_PERMS.iter().cloned().collect());
  player_perms.extend(PLAYER_ALWAYS_PERMS.iter());

  let acl: RawAcl<_> =
    players.iter().map(|tps| AclEntry {
      account_glob: tps.account_glob(instance_name),
      allow: player_perms.clone(),
      deny: default(),
    })
    .chain(
      acl.ents.iter().cloned()
    )
    .collect();

  let acl = acl.try_into()?;

  let mut insns = vec![];
  insns.push(MGI::SetACL { acl });
  insns.push(MGI::SetLinks(links.clone()));
  insns
}

trait SomeSpec {
  const WHAT   : &'static str;
  const FNCOMP : &'static str;
}

impl SomeSpec for GameSpec {
  const WHAT   : &'static str = "game spec";
  const FNCOMP : &'static str = "game";
}

impl SomeSpec for TableSpec {
  const WHAT   : &'static str = "table spec";
  const FNCOMP : &'static str = "table";
}

trait SpecParse {
  type T;
  type S: SomeSpec;
  fn parse(s: String) -> Result<Self::T,AE>;
}
#[derive(Debug,Copy,Clone)]
struct SpecParseToml<T>(pub PhantomData<T>);
impl<T:DeserializeOwned+SomeSpec> SpecParse for SpecParseToml<T> {
  type T = T;
  type S = T;
  #[throws(AE)]
  fn parse(buf: String) -> T {
    let tv: toml::Value = buf.parse().context("parse TOML")?;
    let spec: T = toml_de::from_value(&tv).context("parse value")?;
    spec
  }
}
impl<T> SpecParseToml<T> { pub fn new() -> Self { Self(default()) } }
struct SpecRaw<T>(pub PhantomData<T>);
impl<T:SomeSpec> SpecParse for SpecRaw<T> {
  type T = String;
  type S = T;
  #[throws(AE)]
  fn parse(buf: String) -> String { buf }
}
impl<T> SpecRaw<T> { pub fn new() -> Self { Self(default()) } }

#[throws(AE)]
fn read_spec<P:SpecParse>(ma: &MainOpts, specname: &str, _: P) -> P::T
{
  let filename = if specname.contains('/') {
    specname.to_string()
  } else {
    format!("{}/{}.{}.toml", &ma.spec_dir, specname, P::S::FNCOMP)
  };
  (||{
    let mut f = File::open(&filename).context("open")?;
    let mut buf = String::new();
    f.read_to_string(&mut buf).context("read")?;
    let spec = P::parse(buf)?;
    Ok::<_,AE>(spec)
  })().with_context(|| format!("read {} {:?}", P::S::WHAT, &filename))?
}

#[throws(AE)]
fn access_account(ma: &MainOpts) -> Conn {
  let mut conn = connect(&ma)?;
  conn.prep_access_account(ma)?;
  conn
}

#[throws(AE)]
fn access_game(ma: &MainOpts, table_name: &String) -> MgmtChannelForGame {
  access_account(ma)?.chan.for_game(
    ma.instance_name(table_name),
    MgmtGameUpdateMode::Online,
  )
}

//---------- list-games ----------

mod list_games {
  use super::*;

  #[derive(Default,Debug)]
  struct Args {
    all: bool,
  }

  fn subargs(sa: &mut Args) -> ArgumentParser {
    use argparse::*;
    let mut ap = ArgumentParser::new();
    ap.refer(&mut sa.all)
      .add_option(&["--all"],StoreTrue,
                  "user superuser access to list *all* games");
    ap
  }

  fn call(_sc: &Subcommand, ma: MainOpts, args: Vec<String>) ->Result<(),AE> {
    let args = parse_args::<Args,_>(args, &subargs, &ok_id, None);
    let mut conn = connect(&ma)?;
    let mut games = match conn.cmd(&MC::ListGames { all: Some(args.all) })? {
      MR::GamesList(g) => g,
      x => throw!(anyhow!("unexpected response to ListGames: {:?}", &x)),
    };
    games.sort();
    for g in games {
      println!("{}", &g);
    }
    Ok(())
  }

  inventory::submit!{Subcommand(
    "list-games",
    "List games",
    call,
  )}
}

// todo: list-players
// todo: delete-account

//---------- reset-game ----------

mod reset_game {
  use super::*;

  #[derive(Default,Debug)]
  struct Args {
    table_name: String,
    game_file: String,
    table_file: Option<String>,
  }

  fn subargs(sa: &mut Args) -> ArgumentParser {
    use argparse::*;
    let mut ap = ArgumentParser::new();
    ap.refer(&mut sa.table_file).metavar("TABLE-SPEC[-TOML]")
      .add_option(&["--reset-table"],StoreOption,
                  "reset the players and access too");
    ap.refer(&mut sa.table_name).required()
      .add_argument("TABLE-NAME",Store,"table name");
    ap.refer(&mut sa.game_file).required()
      .add_argument("GAME-SPEC[-TOML]",Store,
                    "game spec (path to .toml file, \
                     or found in specs directory if no '/')");
    ap
  }

  fn call(_sc: &Subcommand, ma: MainOpts, args: Vec<String>) ->Result<(),AE> {
    let args = parse_args::<Args,_>(args, &subargs, &ok_id, None);
    let instance_name = ma.instance_name(&args.table_name);
    let mut chan = access_account(&ma)?.chan.for_game(
      instance_name.clone(),
      MgmtGameUpdateMode::Bulk,
    );
    let spec_toml = read_spec(&ma, &args.game_file,
                              SpecRaw::<GameSpec>::new())?;

    let mut insns = vec![];

    if let Some(table_file) = args.table_file {
      let table_spec = read_spec(&ma, &table_file, SpecParseToml::new())?;
      let game = chan.game.clone();
      chan.cmd(&MgmtCommand::CreateGame {
        game,
        insns: vec![],
      }).map(|_|()).or_else(|e| {
        if let Some(&MgmtError::AlreadyExists) = e.downcast_ref() {
          return Ok(())
        }
        Err(e)
      })?;

      insns.extend(setup_table(&ma, &instance_name, &table_spec)?);
    }

    insns.push(MGI::ResetFromGameSpec { spec_toml });

    chan.alter_game(insns, None)?;

    if ma.verbose >= 0 {
      eprintln!("reset successful.");
    }
    Ok(())
  }

  inventory::submit!{Subcommand(
    "reset",
    "Reset the state of the game table",
    call,
  )}
}

//---------- set-link ----------

mod set_link {

  use super::*;

  #[derive(Debug,Default)]
  struct Args {
    table_name: String,
    kind: Option<LinkKind>,
    url: Option<String>,
  }

  fn subargs(sa: &mut Args) -> ArgumentParser {
    use argparse::*;
    let mut ap = ArgumentParser::new();
    ap.refer(&mut sa.table_name).required()
      .add_argument("TABLE-NAME",Store,"table name");
    ap.refer(&mut sa.kind)
      .add_argument("LINK-KIND",StoreOption,"link kind");
    ap.refer(&mut sa.url)
      .add_argument("URL",StoreOption,"url (or empty for none)");
    ap
  }

  #[throws(AE)]
  fn call(_sc: &Subcommand, ma: MainOpts, args: Vec<String>) {
    let args = parse_args::<Args,_>(args, &subargs, &ok_id, None);
    let mut chan = access_game(&ma, &args.table_name)?;

    match args.url {
      None => {
        let MgmtGameResponseGameInfo { links, .. } = chan.info()?;
        for (tk, v) in links {
          let v: Url = (&v).try_into().context("reparse sererr's UrlSpec")?;
          match args.kind {
            None => {
              println!("{:<10} {}", tk, &v);
            }
            Some(wk) => {
              if wk == tk {
                println!("{}", &v);
              }
            }
          }
        }
      },

      Some(url) => {
        let kind = args.kind.unwrap();
        chan.alter_game(vec![
          if url == "" {
            MGI::RemoveLink { kind }
          } else {
            MGI::SetLink { kind, url: UrlSpec(url) }
          }
        ], None)?;
      },
    }
  }

  inventory::submit!{Subcommand(
    "set-link",
    "Set one of the info links visible from within the game",
    call,
  )}
}

//---------- join-game ----------

mod join_game {
  use super::*;

  #[derive(Default,Debug)]
  struct Args {
    reset_access: bool,
    table_name: String,
  }

  fn subargs(sa: &mut Args) -> ArgumentParser {
    use argparse::*;
    let mut ap = ArgumentParser::new();
    ap.refer(&mut sa.reset_access)
      .add_option(&["--reset"],StoreTrue,
                  "generate and deliver new player access token");
    ap.refer(&mut sa.table_name).required()
      .add_argument("TABLE-NAME",Store,"table name");
    ap
  }

  fn call(_sc: &Subcommand, ma: MainOpts, args: Vec<String>) ->Result<(),AE> {
    let args = parse_args::<Args,_>(args, &subargs, &ok_id, None);
    let mut chan = access_game(&ma, &args.table_name)?;

    let mut insns = vec![];
    match chan.has_player(&ma.account)? {
      None => {
        let nick = ma.nick.clone()
          .unwrap_or_else(|| ma.account.default_nick());
        let details = MgmtPlayerDetails { nick: Some(nick) };
        insns.push(MGI::JoinGame { details });
      }
      Some((player, mpi)) => {
        println!("already in game, as player #{} {:?}",
                 player.0.get_idx_version().0, &mpi.nick);
        let MgmtPlayerInfo { nick, account:_ } = mpi;
        if let Some(new_nick) = &ma.nick {
          if &nick != new_nick {
            println!("changing nick to {:?}", &new_nick);
            let details = MgmtPlayerDetails { nick: ma.nick.clone() };
            insns.push(MGI::UpdatePlayer { player, details });
          }
        }
        if args.reset_access {
          println!("resetting access token (invalidating other URLs)");
          insns.push(MGI::ResetPlayerAccess(player));
        } else {
          println!("redelivering existing access token");
          insns.push(MGI::RedeliverPlayerAccess(player));
        }
      }
    };

    fn deliver(token: &AccessTokenReport) {
      for l in &token.lines {
        if l.contains(char::is_control) {
          println!("Server token info contains control chars! {:?}", &l);
        } else {
          println!(" {}", &l);
        }
      }
    }

    for resp in chan.alter_game(insns, None)? {
      match resp {
        MGR::JoinGame { nick, player, token } => {
          println!("joined game as player #{} {:?}",
                   player.0.get_idx_version().0,
                   &nick);
          deliver(&token);
        }
        MGR::PlayerAccessToken(token) => {
          deliver(&token);
        }
        MGR::Fine => {}
        _ => throw!(anyhow!("unexpected response to instruction(s)")),
      }
    }

    Ok(())
  }

  inventory::submit!{Subcommand(
    "join-game",
    "Join a game or reset access token (creating or updating account)",
    call,
  )}
}

//---------- leave-game ----------

mod leave_game {
  use super::*;

  #[derive(Default,Debug)]
  struct Args {
    table_name: String,
  }

  fn subargs(sa: &mut Args) -> ArgumentParser {
    use argparse::*;
    let mut ap = ArgumentParser::new();
    ap.refer(&mut sa.table_name).required()
      .add_argument("TABLE-NAME",Store,"table name");
    ap
  }

  fn call(_sc: &Subcommand, ma: MainOpts, args: Vec<String>) ->Result<(),AE> {
    let args = parse_args::<Args,_>(args, &subargs, &ok_id, None);
    let mut chan = access_game(&ma, &args.table_name)?;

    let player = match chan.has_player(&ma.account)? {
      None => {
        println!("this account is not a player in that game");
        exit(EXIT_NOTFOUND);
      }
      Some((player, _)) => player,
    };

    chan.alter_game(vec![MGI::LeaveGame(player)], None)?;

    Ok(())
  }

  inventory::submit!{Subcommand(
    "leave-game",
    "Leave a game",
    call,
  )}
}

//---------- delete-game ----------

mod delete_game {
  use super::*;

  #[derive(Default,Debug)]
  struct Args {
    table_name: String,
  }

  fn subargs(sa: &mut Args) -> ArgumentParser {
    use argparse::*;
    let mut ap = ArgumentParser::new();
    ap.refer(&mut sa.table_name).required()
      .add_argument("TABLE-NAME",Store,"table name");
    ap
  }

  fn call(_sc: &Subcommand, ma: MainOpts, args: Vec<String>) ->Result<(),AE> {
    let args = parse_args::<Args,_>(args, &subargs, &ok_id, None);
    let mut chan = access_game(&ma, &args.table_name)?;
    let game = chan.game.clone();
    chan.cmd(&MC::DestroyGame { game })?;
    Ok(())
  }

  inventory::submit!{Subcommand(
    "delete-game",
    "Delete a game (throwing all the players out of it)",
    call,
  )}
}

//---------- library-list ----------

#[derive(Debug,Default)]
struct LibGlobArgs {
  table_name: String,
  lib: Option<String>,
  pat: Option<String>,
}

impl LibGlobArgs {
  fn add_arguments<'ap, 'tlg: 'ap>(
    &'tlg mut self,
    ap: &'_ mut ArgumentParser<'ap>
  ) {
    use argparse::*;
    ap.refer(&mut self.table_name).required()
      .add_argument("TABLE-NAME",Store,"table name");
    ap.refer(&mut self.lib).metavar("LIBRARY")
      .add_option(&["--lib"],StoreOption,"look only in LIBRARY");
    ap.refer(&mut self.pat)
      .add_argument("ITEM-GLOB-PATTERN",StoreOption,"item glob pattern");
  }

  fn lib(&self) -> Option<String> {
    self.lib.clone()
  }
  fn pat(&self) -> String {
    self.pat.as_ref().map(Deref::deref)
      .unwrap_or("*")
      .into()
  }
}

mod library_list {
  use super::*;

  type Args = LibGlobArgs;

  fn subargs(sa: &mut Args) -> ArgumentParser {
    use argparse::*;
    let mut ap = ArgumentParser::new();
    sa.add_arguments(&mut ap);
    ap
  }

  #[throws(AE)]
  fn call(_sc: &Subcommand, ma: MainOpts, args: Vec<String>) {
    let args = parse_args::<Args,_>(args, &subargs, &ok_id, None);
    let mut chan = access_game(&ma, &args.table_name)?;

    if args.lib.is_none() && args.pat.is_none() {
      let game = chan.game.clone();
      let libs = match chan.cmd(&MC::LibraryListLibraries { game })? {
        MgmtResponse::Libraries(libs) => libs,
        x => throw!(anyhow!(
          "unexpected response to LibrarylistLibraries: {:?}", &x)),
      };
      for lib in libs {
        println!("{}", lib);
      }
      return;
    }

    let items = chan.list_items(args.lib.clone(), args.pat())?;
    for it in &items {
      println!("{}", it);
    }
  }

  inventory::submit!{Subcommand(
    "library-list",
    "List pieces in the shape libraries",
    call,
  )}
}

//---------- library-sdd ----------

mod library_add {
  use super::*;

  #[derive(Default,Debug)]
  struct Args {
    tlg: LibGlobArgs,
    adjust_markers: Option<bool>,
    incremental: bool,
  }

  impl Args {
    fn adjust_markers(&self) -> bool { self.adjust_markers.unwrap_or(true) }
  }

  fn subargs(sa: &mut Args) -> ArgumentParser {
    use argparse::*;
    let mut ap = ArgumentParser::new();
    sa.tlg.add_arguments(&mut ap);
    ap.refer(&mut sa.adjust_markers)
      .add_option(&["--no-adjust-markers"],StoreConst(Some(false)),
                  "do not adjust the number of insertion markers, just fail")
      .add_option(&["--adjust-markers"],StoreConst(Some(true)),"");
    ap.refer(&mut sa.incremental)
      .add_option(&["--incremental"],StoreConst(true),
                  "do not place pieces already on the board; \
                   if they don't all fit, place as many as possible")
      .add_option(&["--no-incremental"],StoreConst(false),"");
    ap
  }

  fn call(_sc: &Subcommand, ma: MainOpts, args: Vec<String>) ->Result<(),AE> {
    const MAGIC: &str = "mgmt-library-load-marker";

    let args = parse_args::<Args,_>(args, &subargs, &ok_id, None);
    let mut chan = access_game(&ma, &args.tlg.table_name)?;
    let (pieces, _pcaliases) = chan.list_pieces()?;
    let markers = pieces.iter().filter(|p| p.itemname.as_str() == MAGIC)
      .collect::<Vec<_>>();

    let already = if args.incremental { Some(
      pieces.iter().map(|p| &p.itemname)
        .collect::<HashSet<_>>()
    )} else {
      None
    };

    if ma.verbose > 2 { dbgc!(&markers, &args, &already); }

    #[derive(Debug)]
    enum Situation {
      Poor(Vec<MGI>, &'static str),
      Good([Pos; 2]),
    }
    use Situation::*;

    const WANTED: usize = 2;
    let situation = if markers.len() < WANTED {
      let to_add = WANTED - markers.len();
      let spec = ItemSpec {
        lib: "wikimedia".to_string(), // todo: make an argument
        item: MAGIC.to_string(),
      };
      let spec = PiecesSpec {
        pos: None,
        posd: None,
        count: Some(to_add as u32),
        face: None,
        pinned: Some(false),
        angle: default(),
        info: Box::new(spec),
      };
      Poor(vec![ MGI::AddPieces(spec) ],
           "marker(s) created")
    } else if markers.len() > WANTED {
      let insns = markers[WANTED..].iter()
        .map(|p| MGI::DeletePiece(p.piece))
        .collect();
      Poor(insns,
           "surplus marker(s) removed")
    } else {
      let mut good: ArrayVec<_,2> = default();
      for p in &markers {
        good.push(p.visible.as_ref().ok_or_else(
          || anyhow!("library marker(s) with hidden position!")
        )?.pos);
      }
      Good(good.into_inner().unwrap())
    };
    if ma.verbose > 2 { dbgc!(&situation); }

    #[derive(Debug)]
    struct Placement {
      lhs: Coord, top: Coord, rhs: Coord, bot: Coord,
      clhs: Coord, cbot: Coord, // current line
    }

    let mut placement = match situation {
      Poor(insns, msg) => {
        if !args.adjust_markers() {
          throw!(anyhow!("only {} markers, wanted {}",
                         markers.len(), msg));
        }
        chan.alter_game(insns, None)?;
        eprintln!("updated game: {}\n\
                   please adjust markers as desired and run again",
                  msg);
        exit(EXIT_NOTFOUND);
      }
      Good([a, b]) => {
        // todo: take account of the space used by the markers themselves
        let lhs = min(a.x(), b.x());
        let rhs = max(a.x(), b.x());
        let top = min(a.y(), b.y());
        let bot = max(a.y(), b.y());
        Placement {
          lhs, rhs, top, bot,
          clhs: lhs, cbot: top,
        }
      }
    };
    if ma.verbose > 3 { dbgc!(&placement); }

    impl Placement {
      /// If returns None, has already maybe tried to take some space
      #[throws(AE)]
      fn place(&mut self, bbox: &Rect,
               pieces: &Vec<MgmtGamePieceInfo>, ma: &MainOpts)
               -> Option<Pos> {
        let PosC{ coords: [w,h] } = (bbox.br() - bbox.tl())?;

        let mut did_newline = false;
        let (ncbot, tlhs) = 'search: loop {
          let ncbot = max(self.cbot, self.top + h);
          if ncbot > self.bot { return None }
          let mut any_clash_bot = None;

          'within_line: loop {
            let tlhs = self.clhs;
            self.clhs += w;
            if self.clhs > self.rhs { break 'within_line }

            if let Some((nclhs, clash_bot)) = pieces.iter()
              .filter_map(|p| (|| if_chain! {
                if let Some(pv) = p.visible.as_ref();
                let tl = (pv.pos + pv.bbox.tl())?;
                let br = (pv.pos + pv.bbox.br())?;
                if !(tl.x() >= self.clhs
                    || tl.y() >= ncbot
                    || br.x() <= tlhs
                    || br.y() <= self.top);
                then {
                  if ma.verbose > 2 {
                    eprintln!(
                      "at {:?} tlhs={} ncbot={} avoiding {} tl={:?} br={:?}",
                      &self, tlhs, ncbot, &p.itemname, &tl, &br
                    )
                  }
                  Ok::<_,AE>(Some((br.x(), br.y())))
                } else {
                  Ok::<_,AE>(None)
                }
              })().transpose())
              .next().transpose()?
            {
              self.clhs = nclhs;
              any_clash_bot = Some(clash_bot);
              continue 'within_line;
            }

            break 'search (ncbot, tlhs);
          }
          // line is full
          self.top = self.cbot;
          if did_newline {
            if let Some(top) = any_clash_bot {
              self.top = top;
            } else {
              // if not, will never fit
              return None;
            }
          }
          did_newline = true;
          self.clhs = self.lhs;
          // if we are simply too wide, we'll just loop until off the bottom
        };
        self.cbot = ncbot;
        let ttopleft = PosC::new(tlhs, self.top);
        let tnominal = (ttopleft - bbox.tl())?;

        if ma.verbose > 3 { dbgc!(&self, &tnominal); }
        Some(tnominal)
      }
    }

    let mut items = chan.list_items(args.tlg.lib(), args.tlg.pat())?;

    fn k(ied: &ItemEnquiryData) -> (&str, &GoodItemName) { (
      &ied.lib.libname,
      &ied.itemname,
    ) }
    items.sort_by(|a,b| Ord::cmp( &k(a), &k(b) ));
    items.reverse();
    items.dedup_by(|a,b| PartialEq::eq( &k(a), &k(b) ));
    items.reverse();

    let mut exitcode = 0;
    let mut insns = vec![];
    for (ix, it) in items.iter().enumerate() {
      if ma.verbose > 2 { eprintln!(
        "item {}  {:?}", &it.itemname, &it.f0bbox
      )};
      if let Some(already) = &already {
        if already.contains(&it.itemname) { continue }
      }
      let pos = match placement.place(&items[0].f0bbox, &pieces, &ma)? {
        Some(pos) => pos,
        None => {
          let m = format!("out of space after {} at {}",
                          &ix, &it.itemname);
          exitcode = EXIT_SPACE;
          if args.incremental {
            println!("stopping: {}", &m);
            break;
          } else {
            eprintln!("error: {}", &m);
            exit(exitcode);
          }
        }
      };
      let spec = ItemSpec::from(it);
      let spec = PiecesSpec {
        pos: Some(pos),
        posd: None, count: Some(1), face: None, pinned: Some(false),
        angle: default(), info: Box::new(spec),
      };
      let insn = MGI::AddPieces(spec);
      insns.push(insn);
    }

    let count = insns.len();
    chan.alter_game(insns, None)?;
    println!("added {} pieces", count);
    exit(exitcode);
  }

  inventory::submit!{Subcommand(
    "library-add",
    "Add pieces from the shape libraries",
    call,
  )}
}

//---------- list-pieces ----------

mod list_pieces {
  use super::*;

  #[derive(Default,Debug)]
  struct Args {
    table_name: String,
  }

  fn subargs(sa: &mut Args) -> ArgumentParser {
    use argparse::*;
    let mut ap = ArgumentParser::new();
    ap.refer(&mut sa.table_name).required()
      .add_argument("TABLE-NAME",Store,"table name");
    ap
  }

  #[throws(AE)]
  fn call(_sc: &Subcommand, ma: MainOpts, args: Vec<String>) {
    let args = parse_args::<Args,_>(args, &subargs, &ok_id, None);
    let mut chan = access_game(&ma, &args.table_name)?;
    let (pieces, pcaliases) = chan.list_pieces()?;
    for p in pieces {
      println!("{:?}", p);
    }
    for a in pcaliases {
      println!("{:?}", a);
    }
  }

  inventory::submit!{Subcommand(
    "list-pieces",
    "List pieces in the game",
    call,
  )}
}

//---------- adhoc json/ron etc. ----------

#[derive(Debug,Copy,Clone)]
struct AdhocFormat(&'static str);

impl From<&'static Subcommand> for AdhocFormat {
  fn from(sc: &'static Subcommand) -> Self { AdhocFormat(
    sc.0.rsplitn(2,'-').next().unwrap()
  )}
}

impl AdhocFormat {
  pub fn metavar(&self) -> String { self.0.to_uppercase() }
  pub fn name(&self) -> String { match self.0 {
    // special cases go here
    _ => return self.0.to_uppercase(),
  }/*.into()*/ }

  #[throws(AE)]
  pub fn parse<T>(&self, input: Vec<String>, what: &str) -> Vec<T>
  where T: DeserializeOwned
  {
    input.into_iter().enumerate().map(|(i,s)| match self.0 {
      "json" => serde_json::from_str(&s).map_err(AE::from),
      "ron"  => ron::de::   from_str(&s).map_err(AE::from),
      _ => panic!(),
    }
        .with_context(|| s.clone())
        .with_context(|| format!("parse {} (#{})", what, i))
    ).collect::<Result<Vec<T>,AE>>()?
  }

  #[throws(AE)]
  pub fn report<T>(&self, resp: T)
  where T: Serialize
  {
    println!("{}", match self.0 {
      "json" => serde_json::to_string(&resp).map_err(AE::from),
      "ron"  => ron::ser::  to_string(&resp).map_err(AE::from),
      _ => panic!(),
    }
        .context("re-format response")?);
  }
}

//---------- alter game ----------

mod alter_game_adhoc {
  use super::*;

  #[derive(Default,Debug)]
  struct Args {
    table_name: String,
    insns: Vec<String>,
  }

  fn subargs<'ap,'a:'ap,'m:'ap>(
    sa: &'a mut Args,
    ahf: AdhocFormat,
  ) -> ArgumentParser<'ap> {
    use argparse::*;
    let mut ap = ArgumentParser::new();
    ap.refer(&mut sa.table_name).required()
      .add_argument("TABLE-NAME",Store,"table name");
    ap.refer(&mut sa.insns).required()
      .add_argument(format!("{}-INSN", ahf.metavar()).leak(),
                    Collect,
                    format!("{}-encoded MgmtGameInstruction", ahf.name())
                    .leak());
    ap
  }

  fn call(sc: &'static Subcommand, ma: MainOpts, args: Vec<String>)
          -> Result<(),AE> {
    let ahf = sc.into();

    let subargs: ApMaker<_> = &|sa| subargs(sa,ahf);
    let args = parse_args::<Args,_>(args, subargs, &ok_id, None);
    let mut chan = access_game(&ma, &args.table_name)?;

    let insns: Vec<MgmtGameInstruction> = ahf.parse(args.insns, "insn")?;
    let resps = chan.alter_game(insns,None)?;
    for resp in resps {
      ahf.report(resp)?;
    }

    Ok(())
  }

  inventory::submit!{Subcommand(
    "alter-game-json",
    "run an ad-hoc AlterGame command (JSON)",
    call,
  )}
  inventory::submit!{Subcommand(
    "alter-game-ron",
    "run an ad-hoc AlterGame command (Rusty Object Notation)",
    call,
  )}
}

//---------- upload-bundle ----------

mod upload_bundle {
  use super::*;

  #[derive(Default,Debug)]
  struct Args {
    table_name: String,
    bundle_file: String,
  }

  fn subargs(sa: &mut Args) -> ArgumentParser {
    use argparse::*;
    let mut ap = ArgumentParser::new();
    ap.refer(&mut sa.table_name).required()
      .add_argument("TABLE-NAME",Store,"table name");
    ap.refer(&mut sa.bundle_file).required()
      .add_argument("BUNDLE",Store,"bundle file");
    ap
  }

  #[throws(AE)]
  fn call(_sc: &Subcommand, ma: MainOpts, args: Vec<String>) {
    let args = parse_args::<Args,_>(args, &subargs, &ok_id, None);
    let instance_name = ma.instance_name(&args.table_name);
    let mut chan = access_game(&ma, &args.table_name)?;
    let f = File::open(&args.bundle_file)
      .with_context(|| args.bundle_file.clone())
      .context("open bundle file")?;
    let size = f
      .metadata().context("fstat bundle file")?
      .len()
      .try_into().map_err(|_| anyhow!("bundle file far too large"))?;
    let mut f = BufReader::new(f);
    let hash = bundles::DigestWrite::of(&mut f)
      .context("read bundle file (for hash)")?;
    let kind = bundles::Kind::only();
    f.rewind().context("rewind bundle file")?;
    let cmd = MC::UploadBundle {
      size,
      game: instance_name.clone(),
      hash: bundles::Hash(hash.into()), kind,
      progress: MgmtChannel::PROGRESS,
    };
    let mut progress = termprogress::new();
    let resp = chan.cmd_withbulk(&cmd, &mut f, &mut io::sink(),
                                 &mut *progress)?;
    if_let!{ MR::Bundle { bundle } = resp;
             else throw!(anyhow!("unexpected {:?}", &resp)) };
    progress.clear();
    println!("{}", bundle);
  }

  inventory::submit!{Subcommand(
    "upload-bundle",
    "Upload a bundle",
    call,
  )}
}

//---------- list-bundles ----------

mod list_bundles {
  use super::*;

  #[derive(Default,Debug)]
  struct Args {
    table_name: String,
  }

  fn subargs(sa: &mut Args) -> ArgumentParser {
    use argparse::*;
    let mut ap = ArgumentParser::new();
    ap.refer(&mut sa.table_name).required()
      .add_argument("TABLE-NAME",Store,"table name");
    ap
  }

  #[throws(AE)]
  fn call(_sc: &Subcommand, ma: MainOpts, args: Vec<String>) {
    let args = parse_args::<Args,_>(args, &subargs, &ok_id, None);
    let instance_name = ma.instance_name(&args.table_name);
    let mut chan = access_game(&ma, &args.table_name)?;
    let resp = chan.cmd(&MC::ListBundles {
      game: instance_name.clone(),
    })?;
    if_let!{ MR::Bundles { bundles } = resp;
             else throw!(anyhow!("unexpected {:?}", &resp)) };
    for (id, state) in bundles {
      println!("{} {}", id, &state);
    }
  }

  inventory::submit!{Subcommand(
    "list-bundles",
    "List bundles",
    call,
  )}
}

//---------- download-bundle ----------

mod download_bundle {
  use super::*;

  #[derive(Default,Debug)]
  struct Args {
    table_name: String,
    index: bundles::Index,
    output: Option<PathBuf>,
  }

  fn subargs(sa: &mut Args) -> ArgumentParser {
    use argparse::*;
    let mut ap = ArgumentParser::new();
    ap.refer(&mut sa.table_name).required()
      .add_argument("TABLE-NAME",Store,"table name");
    ap.refer(&mut sa.index).required()
      .add_argument("INDEX",Store,"bundle number");
    ap.refer(&mut sa.output).metavar("OUTPUT")
      .add_option(&["-o","--output"],StoreOption,
                  "write output to OUTPUT (rather than NNNNN.zip");
    ap
  }

  #[throws(AE)]
  fn call(_sc: &Subcommand, ma: MainOpts, args: Vec<String>) {
    let args = parse_args::<Args,_>(args, &subargs, &ok_id, None);
    let instance_name = ma.instance_name(&args.table_name);
    let mut chan = access_game(&ma, &args.table_name)?;
    let kind = bundles::Kind::only();
    let id = bundles::Id { kind, index: args.index };
    let path = args.output.unwrap_or_else(|| id.to_string().into());

    let (f, path_tmp): (Box<dyn Write>, _) =
      if path.as_os_str().as_bytes() == b"-"
    {
      (Box::new(io::stdout()), None)
    } else {
      let tmp = {
        let mut w = path.as_os_str().to_owned();
        w.push(".tmp");
        PathBuf::from(w)
      };
      let f = fs::File::create(&tmp)
        .with_context(|| tmp.to_debug()).context("create temporary")?;
      (Box::new(f), Some((path, tmp)))
    };
    let mut f = BufWriter::new(f);
    let cmd = MC::DownloadBundle {
      game: instance_name.clone(),
      id,
    };
    chan.cmd_withbulk(&cmd, &mut io::empty(), &mut f,
                      &mut termprogress::NullReporter)
      .context("download bundle")?;
    f.flush().context("flush bundle file")?;
    if let Some((path, tmp)) = path_tmp {
      fs::rename(&tmp,&path)
        .with_context(|| path.to_debug()).context("rename after download")?;
    }
  }

  inventory::submit!{Subcommand(
    "download-bundle",
    "download bundle",
    call,
  )}
}

//---------- clear game ----------

mod clear_game {
  use super::*;

  #[derive(Default,Debug)]
  struct Args {
    table_name: String,
  }

  fn subargs(sa: &mut Args) -> ArgumentParser {
    use argparse::*;
    let mut ap = ArgumentParser::new();
    ap.refer(&mut sa.table_name).required()
      .add_argument("TABLE-NAME",Store,"table name");
    ap
  }

  #[throws(AE)]
  fn call(_sc: &Subcommand, ma: MainOpts, args: Vec<String>) {
    let args = parse_args::<Args,_>(args, &subargs, &ok_id, None);
    let instance_name = ma.instance_name(&args.table_name);
    let mut chan = access_game(&ma, &args.table_name)?;

    chan.alter_game(vec![MGI::ClearGame{ }], None)
      .context("clear table")?;
    chan.cmd(&MC::ClearBundles { game: instance_name.clone() })
      .context("clear bundles")?;
  }

  inventory::submit!{Subcommand(
    "clear-game",
    "clear the table and clear out all bundles",
    call,
  )}
}

//---------- list-accounts ----------

mod list_accounts {
  use super::*;

  #[derive(Default,Debug)]
  struct Args {
    all: bool,
  }

  fn subargs(sa: &mut Args) -> ArgumentParser {
    use argparse::*;
    let mut ap = ArgumentParser::new();
    ap.refer(&mut sa.all)
      .add_option(&["--all"],StoreTrue,
                  "user superuser access to list all accounts");
    ap
  }

  #[throws(AE)]
  fn call(_sc: &Subcommand, ma: MainOpts, args: Vec<String>) {
    let args = parse_args::<Args,_>(args, &subargs, &ok_id, None);
    let mut conn = connect(&ma)?;
    let all = Some(args.all);
    let accounts = match conn.cmd(&MC::ListAccounts { all })? {
      MR::AccountsList(g) => g,
      x => throw!(anyhow!("unexpected response to ListAccounts: {:?}", &x)),
    };
    for a in accounts {
      println!("{}", a);
    }
  }

  inventory::submit!{Subcommand(
    "list-accounts",
    "List accounts in your account scope",
    call,
  )}
}
