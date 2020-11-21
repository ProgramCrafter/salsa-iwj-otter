// Copyright 2020 Ian Jackson
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

#![allow(unused_imports)]

use otter::imports::*;
use argparse::{self,ArgumentParser,action::{TypedAction,ParseResult}};
use argparse::action::{Action,IFlagAction,IArgAction};
use derive_more::Display;
use std::rc::Rc;
use std::cell::RefCell;
use std::cell::Cell;

type E = anyhow::Error;
type AS = AccountScope;
type APE = ArgumentParseError;
type MC = MgmtCommand;
type ME = MgmtError;
type MGI = MgmtGameInstruction;
type MGR = MgmtGameResponse;
type TP = TablePermission;

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

#[derive(Debug)]
struct MainOpts {
  account: AccountName,
  nick: Option<String>,
  timezone: Option<String>,
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
  fn(&Subcommand, MainOpts, Vec<String>) -> Result<(),E>,
);
inventory::collect!(Subcommand);

#[derive(Error,Debug,Clone)]
struct ArgumentParseError(String);
display_as_debug!(ArgumentParseError);

impl From<&anyhow::Error> for ArgumentParseError {
  fn from(ae: &anyhow::Error) -> ArgumentParseError {
    eprintln!("error during argument parsing/startup: {:?}\n", ae);
    exit(EXIT_USAGE);
  }
}

fn parse_args<T:Default,U>(
  args: Vec<String>,
  apmaker: &dyn Fn(&mut T) -> ArgumentParser,
  completer: &dyn Fn(T) -> Result<U, ArgumentParseError>,
  extra_help: Option<&dyn Fn(&mut dyn Write) -> Result<(), io::Error>>,
) -> U
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
  let completed  =
    completer(parsed).unwrap_or_else(|e:ArgumentParseError| {
      let mut def = default();
      let ap = apmaker(&mut def);
      ap.error(&us, &e.0, &mut stderr);
      exit(EXIT_USAGE);
    });
  completed
}

pub fn ok_id<T,E>(t: T) -> Result<T,E> { Ok(t) }

pub fn clone_via_serde<T: Debug + Serialize + DeserializeOwned>
  (t: &T) -> T
{
  (||{
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
                  from_exp_in: &str,
                  now_what: &str,
                  then_in: &str,
                  leaf: &str,
                  local_subdir: &str)
                  -> String
{
  match (||{
    let from = from?;
    let mut comps = from.rsplit('/').skip(1);
    if_chain! {
      if Some(from_exp_in) == comps.next();
      if let Some(path) = comps.next();
      then { Ok(path.to_string()) }
      else { Err(ere(
        format!("{} is not in a directory called {}", from_what, from_exp_in)
      )) }
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

fn main() {
  #[derive(Default,Debug)]
  struct RawMainArgs {
    account: Option<AccountName>,
    socket_path: Option<String>,
    nick: Option<String>,
    timezone: Option<String>,
    access: Option<AccessOpt>,
    verbose: i32,
    config_filename: Option<String>,
    superuser: bool,
    subcommand: String,
    subargs: Vec<String>,
    spec_dir: Option<String>,
  };
  let (subcommand, subargs, mo) = parse_args::<RawMainArgs,_>(
    env::args().collect(),
  &|rma|{
    use argparse::*;
    let mut ap = ArgumentParser::new();
    ap.stop_on_first_argument(true);
    ap.silence_double_dash(true);
    ap.refer(&mut rma.subcommand).required().add_argument("SUBCOMMAND",Store,
                                      "subcommand");
    ap.refer(&mut rma.subargs).add_argument("...",Collect,
                                   "subcommand options/arguments");

    let mut account = ap.refer(&mut rma.account);
    account.metavar("ACCOUNT").add_option(&["--account"],
                     StoreOption,
                     "use account ACCOUNT (default: unix:<current user>:)");

    ap.refer(&mut rma.nick).metavar("NICK").add_option(
      &["--nick"],
      StoreOption,
      "use NICK as nick for joining games (now and in the future) \
       (default: derive from account name");
    ap.refer(&mut rma.timezone).metavar("TZ").add_option(
      &["--timezone"],
      StoreOption,
      "display times in timezone TZ (Olson timezone name) \
       (default is to use server's default timezone)");

    let mut access = ap.refer(&mut rma.access);
    access.add_option(&["--url-on-stdout"],
                      StoreConst(Some(UrlOnStdout.into())),
                      "show game access url by printing to stdout");
    access.metavar("TOKEN").add_option(
      &["--fixed-token"],
      MapStore(|s| Ok(Some(
        FixedToken { token: RawToken(s.to_string()) }.into()
      ))),
 "use fixed game access token TOKEN (for administrators, with --super, only)"
    );
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
    subcommand, subargs, spec_dir,
  }|{
    let config = Thunk::<Result<(Arc<ServerConfig>,String),APE>,_>::new(
      move ||{
        (||{
          let config_filename = config_filename
            .unwrap_or_else(||{
              let exe = find_executable();
              in_basedir(verbose > 1, exe, "current executable", "bin",
                         "config file", "etc", DEFAULT_CONFIG_LEAFNAME,
                         ".")
            });
          ServerConfig::read(Some(&config_filename))
            .context("read config file")?;
          Ok::<_,AE>((otter::config::config(), config_filename))
        })().map_err(|e| ArgumentParseError(
          format!("failed to find/load config: {}", &e)
        ))
      });

    let spec_dir = spec_dir.map(Ok::<_,APE>).unwrap_or_else(||{
      let cfgf = config.clone().map(|(_c,f)| f).map_err(Into::into);
      let spec_dir = in_basedir(verbose > 1,
                                cfgf, "config filename", "etc",
                                "game and table specs", "specs", "",
                                ".")
        .trim_end_matches('/').to_string();
      Ok(spec_dir)
    })?;

    let account : AccountName = account.map(Ok::<_,APE>).unwrap_or_else(||{
      let user = env::var("USER").map_err(|e| ArgumentParseError(
        format!("default account needs USER env var: {}", &e)
      ))?;
      Ok(AccountName {
        scope: AS::Unix { user },
        subaccount: "".into(),
      })
    })?;

    let socket_path = socket_path.map(Ok::<_,APE>).unwrap_or_else(||{
      Ok(config.clone()?
         .0.command_socket.clone())
    })?;
    Ok((subcommand, subargs, MainOpts {
      account,
      access,
      nick,
      timezone,
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

  call(sc, mo, subargs).expect("execution error");
}

struct Conn {
  chan: MgmtChannel,
}

impl Conn {
  #[throws(AE)]
  fn cmd(&mut self, cmd: &MgmtCommand) -> MgmtResponse {
    use MgmtResponse::*;
    self.chan.write(&cmd).context("send command")?;
    let resp = self.chan.read().context("read response")?;
    match &resp {
      Fine | GamesList{..} | LibraryItems(_) => { },
      AlterGame { error: None, .. } => { },
      Error { error } => {
        Err(error.clone()).context(
          format!("got error response to: {:?}",&cmd)
        )?;
      },
      AlterGame { error: Some(error), .. } => {
        Err(error.clone()).context(format!(
          "game alternations failed (maybe partially); response to: {:?}",
          &cmd))?;
      },
    };
    resp
  }

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

  #[throws(AE)]
  fn list_items(&mut self, pat: &shapelib::ItemSpec)
                -> Vec<shapelib::ItemEnquiryData> {
    let cmd = MgmtCommand::LibraryListByGlob { glob: pat.clone() };
    let mut items = match self.cmd(&cmd)? {
      MgmtResponse::LibraryItems(items) => items,
      wat => Err(anyhow!("unexpected LibraryListByGlob response: {:?}",
                         &wat))?,
    };
    items.sort();
    items
  }
}

struct ConnForGame {
  pub conn: Conn,
  pub game: InstanceName,
  pub how: MgmtGameUpdateMode,
}
impl Deref for ConnForGame {
  type Target = Conn;
  fn deref(&self) -> &Conn { &self.conn }
}
impl DerefMut for ConnForGame {
  fn deref_mut(&mut self) -> &mut Conn { &mut self.conn }
}

impl ConnForGame {
  #[throws(AE)]
  fn join_game(&mut self, ma: &MainOpts) {
    let nick = ma.nick.clone()
      .or_else(|| Some(ma.account.default_nick()));

    let insns = vec![
      MGI::JoinGame { details: MgmtPlayerDetails { nick } },
    ];
    let resp = self.alter_game(insns, None)?;

    match resp.as_slice() {
      [MGR::JoinGame { nick, player, token }] => {
        println!("joined game as player #{} {:?}",
                 player.0.get_idx_version().0,
                 &nick);
        for l in &token.lines {
          if l.contains(char::is_control) {
            println!("Server token info contains control chars! {:?}",
                     &l);
          } else {
            println!(" {}", &l);
          }
        }
      }
      x => throw!(anyhow!("unexpected response to JoinGame: {:?}", &x)),
    }
  }

  #[throws(AE)]
  fn alter_game(&mut self, insns: Vec<MgmtGameInstruction>,
                f: Option<&mut dyn FnMut(&MgmtGameResponse) -> Result<(),AE>>)
                -> Vec<MgmtGameResponse> {
    let insns_len = insns.len();
    let cmd = MgmtCommand::AlterGame {
      game: self.game.clone(), how: self.how,
      insns
    };
    let responses = match self.cmd(&cmd)? {
      MgmtResponse::AlterGame { error: None, responses }
      if responses.len() == insns_len => {
        responses
      },
      wat => Err(anyhow!("unexpected AlterGame response: {:?} => {:?}",
                         &cmd, &wat))?,
    };
    if let Some(f) = f {
      for response in &responses {
        f(response)?;
      }
    }
    responses
  }
/*
  fn get_info(&mut self) -> Result<
      (MgmtGameResponseGameInfo, HashMap<String,PlayerId>
      ),AE>
  {
    let mut players = self.alter_game(
      vec![ MgmtGameInstruction::Info ],
      None,
    )?;
    let info = match players.pop() {
      Some(MgmtGameResponse::Info(info)) => info,
      wat => Err(anyhow!("GetGstate got {:?}", &wat))?,
    };
    let mut nick2id = HashMap::new();
    for (player, pstate) in info.players.iter() {
      use hash_map::Entry::*;
      match nick2id.entry(pstate.nick.clone()) {
        Occupied(oe) => Err(anyhow!("game has duplicate nick {:?}, {} {}",
                                    &pstate.nick, *oe.get(), player))?,
        Vacant(ve) => ve.insert(player),
      };
    }
    Ok((info, nick2id))
  }
*/
  #[throws(AE)]
  fn get_pieces(&mut self) -> Vec<MgmtGamePieceInfo> {
    let insns = vec![ MgmtGameInstruction::ListPieces ];
    let mut responses = self.alter_game(insns, None)?;
    match responses.as_mut_slice() {
      [MgmtGameResponse::Pieces(pieces)] => {
        return mem::take(pieces)
      },
      wat => Err(anyhow!("ListPieces => {:?}", &wat))?,
    }
  }
}

#[throws(E)]
fn connect(ma: &MainOpts) -> Conn {
  let unix = UnixStream::connect(&ma.socket_path)
    .with_context(||ma.socket_path.clone()).context("connect to server")?; 
  let chan = MgmtChannel::new(unix)?;
  let mut chan = Conn { chan };
  if ma.superuser {
    chan.cmd(&MC::SetSuperuser(true))?;
  }
  chan.cmd(&MC::SelectAccount(ma.account.clone()))?;
  chan
}

const PLAYER_ALWAYS_PERMS : &[TablePermission] = &[
  TP::TestExistence,
  TP::ViewPublic,
  TP::Play,
];

const PLAYER_DEFAULT_PERMS : &[TablePermission] = &[
  TP::ChangePieces,
];

#[throws(AE)]
fn setup_table(_ma: &MainOpts, spec: &TableSpec) -> Vec<MGI> {
  let TableSpec { players, player_perms, acl } = spec;
  let mut player_perms = player_perms.clone()
    .unwrap_or(PLAYER_DEFAULT_PERMS.iter().cloned().collect());
  player_perms.extend(PLAYER_ALWAYS_PERMS.iter());

  let acl : RawAcl<_> =
    players.iter().map(|tps| AclEntry {
      account_glob: tps.account_glob(),
      allow: player_perms.clone(),
      deny: default(),
    })
    .chain(
      acl.ents.iter().cloned()
    )
    .collect();

  let acl = acl.try_into()?;

  let mut insns = vec![];
  insns.push(MGI::ClearLog);
  insns.push(MGI::SetACL { acl });
  insns
}



/*

  let (_, nick2id) = chan.get_info()?;

  #[derive(Default)]
  struct St { id: PlayerId, old: bool, new: bool };

  let mut nick2st : HashMap<_,_> = {nick2id}
  .drain()
    .map(|(nick,id)| (nick, St { id, old: true, new: false }))
    .collect();

  for pspec in &spec.players {
    let nick = pspec.nick.unwrap_or_else(|| pspec.account.default_nick());
    let st = nick2st.entry(nick.clone()).or_default();
    if st.new {
      Err(anyhow!("duplicate player nick {:?} in spec", &nick))?;
    }
    st.new = true;
    let timezone = pspec.timezone.as_ref().or(
      spec.timezone.as_ref()
    ).cloned();
    // ^ todo use client program timezone?
    if !st.old {
      insns.push(MgmtGameInstruction::AddPlayer {
        account: pspec.account.clone(),
        details: MgmtPlayerDetails {
          nick: Some(nick),
          timezone,
        },
      });
    }
  }

  for (nick, st) in nick2st {
    if st.new { continue }
    if !st.old { continue }
    if ma.verbose >= 1 {
      eprintln!("removing old player {:?}", &nick);
    }
    insns.push(MGI::RemovePlayer { player: st.id });
  }

  let mut added_players = vec![];
  chan.alter_game(insns, Some(&mut |response| {
    match response {
      &Resp::AddPlayer { info, player, token } => {
        added_players.push((info.clone(), token.clone()));
      },
      _ => { },
    };
    Ok(())
  }))?;

  // report any new access tokens
  for (info, token) in added_players {
    
  access.deliver_tokens(&pspec, &ptokens)
        .with_context(||format!("deliver tokens for nick={:?}",
                                  &pspec.nick))?;
    }
  }
*/

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

#[throws(AE)]
fn read_spec<T: DeserializeOwned + SomeSpec>
  (ma: &MainOpts, specname: &str) -> T
{
  let filename = if specname.contains('/') {
    specname.to_string()
  } else {
    format!("{}/{}.{}.toml", &ma.spec_dir, specname, T::FNCOMP)
  };
  (||{
    let mut f = File::open(&filename).context("open")?;
    let mut buf = String::new();
    f.read_to_string(&mut buf).context("read")?;
    let spec : T = toml_de::from_str(&buf).context("parse")?;
    Ok::<_,AE>(spec)
  })().with_context(|| format!("read {} {:?}", T::WHAT, &filename))?
}

#[throws(AE)]
fn access_account(ma: &MainOpts) -> Conn {
  let mut conn = connect(&ma)?;
  conn.prep_access_account(ma)?;
  conn
}

#[throws(AE)]
fn access_game(ma: &MainOpts, table_name: &String) -> ConnForGame {
  ConnForGame {
    conn: access_account(ma)?,
    game: ma.instance_name(table_name),
    how: MgmtGameUpdateMode::Online,
  }
}

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
    ap.refer(&mut sa.table_file)
      .metavar("TABLE-SPEC[-TOML]")
      .add_option(&["--reset-table"],StoreOption,
                  "reset the players and access too");
    ap.refer(&mut sa.table_name).required()
      .add_argument("TABLE-NAME",Store,"table name");
    ap.refer(&mut sa.game_file).required()
      .add_argument("GAME-SPEC[-TOML]",Store,
 "game spec (path to .toml file, or found in specs directory if no '/')"
      );
    ap
  }

  fn call(_sc: &Subcommand, ma: MainOpts, args: Vec<String>) ->Result<(),AE> {
    let args = parse_args::<Args,_>(args, &subargs, &ok_id, None);
    let mut chan = ConnForGame {
      conn: access_account(&ma)?,
      game: ma.instance_name(&args.table_name),
      how: MgmtGameUpdateMode::Bulk,
    };
    let game: GameSpec = read_spec(&ma, &args.game_file)?;

    let mut insns = vec![];

    if let Some(table_file) = args.table_file {
      let table_spec = read_spec(&ma, &table_file)?;
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

      insns.extend(setup_table(&ma, &table_spec)?);
    }

    for p in chan.get_pieces()? {
      insns.push(MgmtGameInstruction::DeletePiece(p.piece));
    }

    if let Some(size) = game.table_size {
      insns.push(MGI::SetTableSize(size));
    }

    let mut game = game;
    for pspec in game.pieces.drain(..) {
      insns.push(MGI::AddPieces(pspec));
    }

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

//---------- join-game ----------

mod join_game {
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
    chan.join_game(&ma)?;
    Ok(())
  }

  inventory::submit!{Subcommand(
    "join-game",
    "Join a game or reset access token (creating or updating account)",
    call,
  )}
}

//---------- library-list ----------

#[derive(Debug)]
struct LibGlobArgs {
  pat: shapelib::ItemSpec,
}

impl Default for LibGlobArgs { fn default() -> Self { Self {
  pat: shapelib::ItemSpec { lib: default(), item: default() },
} } }

impl LibGlobArgs {
  fn add_arguments<'ap, 'tlg : 'ap>(
    &'tlg mut self,
    ap: &'_ mut ArgumentParser<'ap>
  ) {
    use argparse::*;
    ap.refer(&mut self.pat.lib).required()
      .add_argument("LIB-NAME",Store,"library name");
    ap.refer(&mut self.pat.item).required()
      .add_argument("ITEM-GLOB-PATTERN",Store,"item glob pattern");
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

  fn call(_sc: &Subcommand, ma: MainOpts, args: Vec<String>) ->Result<(),AE> {
    let args = parse_args::<Args,_>(args, &subargs, &ok_id, None);
    let mut chan = connect(&ma)?;

    let items = chan.list_items(&args.pat)?;
    for it in &items {
      println!("{:20}  {}", it.itemname, it.f0desc.0);
    }

    Ok(())
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
    table_name: String,
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
    ap.refer(&mut sa.table_name).required()
      .add_argument("TABLE-NAME",Store,"table name");
    ap.refer(&mut sa.adjust_markers)
      .add_option(&["--no-adjust-markers"],StoreConst(Some(false)),
                  "do not adjust the number of insertion markers, just fail")
      .add_option(&["--adjust-markers"],StoreConst(Some(true)),"");
    ap.refer(&mut sa.incremental)
      .add_option(&["--incremental"],StoreConst(true),
                  "do not place pieces already on the board; \
                   if they don't all fit, place as many as possible")
      .add_option(&["--no-incremental"],StoreConst(false),"");
    sa.tlg.add_arguments(&mut ap);
    ap
  }

  fn call(_sc: &Subcommand, ma: MainOpts, args: Vec<String>) ->Result<(),AE> {
    const MAGIC : &str = "mgmt-library-load-marker";

    let args = parse_args::<Args,_>(args, &subargs, &ok_id, None);
    let mut chan = access_game(&ma, &args.table_name)?;
    let pieces = chan.get_pieces()?;
    let markers = pieces.iter().filter(|p| p.itemname == MAGIC)
      .collect::<Vec<_>>();

    let already = if args.incremental { Some(
      pieces.iter().map(|p| &p.itemname)
        .collect::<HashSet<_>>()
    )} else {
      None
    };

    if ma.verbose > 2 { dbg!(&markers, &args, &already); }

    #[derive(Debug)]
    enum Situation {
      Poor(Vec<MGI>, &'static str),
      Good([Pos; 2]),
    };
    use Situation::*;

    const WANTED : usize = 2;
    let situation = if markers.len() < WANTED {
      let to_add = WANTED - markers.len();
      let spec = shapelib::ItemSpec {
        lib: "wikimedia".to_string(), // todo: make an argument
        item: MAGIC.to_string(),
      };
      let spec = PiecesSpec {
        pos: None,
        posd: None,
        count: Some(to_add as u32),
        face: None,
        pinned: Some(false),
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
      let mut good : ArrayVec<_> = default();
      for p in &markers {
        good.push(p.visible.as_ref().ok_or_else(
          || anyhow!("library marker(s) with hidden position!")
        )?.pos);
      }
      Good(good.into_inner().unwrap())
    };
    if ma.verbose > 2 { dbg!(&situation); }

    #[derive(Debug)]
    struct Placement {
      lhs: Coord, top: Coord, rhs: Coord, bot: Coord,
      clhs: Coord, cbot: Coord, // current line
    };

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
        return Ok(());
      }
      Good([a, b]) => {
        // todo: take account of the space used by the markers themselves
        let lhs = min(a.0[0], b.0[0]);
        let rhs = max(a.0[0], b.0[0]);
        let top = min(a.0[1], b.0[1]);
        let bot = max(a.0[1], b.0[1]);
        Placement {
          lhs, rhs, top, bot,
          clhs: lhs, cbot: top,
        }
      }
    };
    if ma.verbose > 3 { dbg!(&placement); }

    impl Placement {
      /// If returns None, has already maybe tried to take some space
      fn place(&mut self, bbox: &[Pos;2],
               pieces: &Vec<MgmtGamePieceInfo>, ma: &MainOpts)
               -> Option<Pos> {
        let PosC([w,h]) = bbox[1] - bbox[0];

        let mut did_newline = false;
        let (ncbot, tlhs) = 'search : loop {
          let ncbot = max(self.cbot, self.top + h);
          if ncbot > self.bot { None? }
          let mut any_clash_bot = None;

          'within_line: loop {
            let tlhs = self.clhs;
            self.clhs += w;
            if self.clhs > self.rhs { break 'within_line }

            if let Some((nclhs, clash_bot)) = pieces.iter()
              .filter_map(|p| {
                let pv = p.visible.as_ref()?;
                let tl = pv.pos + pv.bbox[0];
                let br = pv.pos + pv.bbox[1];
                if
                  tl.0[0] >= self.clhs ||
                  tl.0[1] >= ncbot     ||
                  br.0[0] <= tlhs      ||
                  br.0[1] <= self.top
                {
                  None
                } else {
                  if ma.verbose > 2 { eprintln!(
                    "at {:?} tlhs={} ncbot={} avoiding {} tl={:?} br={:?}",
                    &self, tlhs, ncbot,
                    &p.itemname, &tl, &br
                  )}
                  Some((br.0[0], br.0[1]))
                }
              }).next() {
                self.clhs = nclhs;
                any_clash_bot = Some(clash_bot);
                continue 'within_line;
              }
            
            break 'search (ncbot, tlhs);
          }
          // line is full
          self.top = self.cbot;
          if did_newline {
            self.top = any_clash_bot?; // if not, will never fit
          }
          did_newline = true;
          self.clhs = self.lhs;
          // if we are simply too wide, we'll just loop until off the bottom
        };
        self.cbot = ncbot;
        let ttopleft = PosC([tlhs, self.top]);
        let tnominal = ttopleft - bbox[0];

        if ma.verbose > 3 { dbg!(&self, &tnominal); }
        Some(tnominal)
      }
    }

    let items = chan.list_items(&args.tlg.pat)?;

    let mut exitcode = 0;
    let mut insns = vec![];
    for (ix, it) in items.iter().enumerate() {
      if ma.verbose > 2 { eprintln!(
        "item {}  {:?}", &it.itemname, &it.f0bbox
      )};
      if let Some(already) = &already {
        if already.contains(&it.itemname) { continue }
      }
      let pos = match placement.place(&items[0].f0bbox, &pieces, &ma) {
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
        },
      };
      let spec = shapelib::ItemSpec {
        lib: args.tlg.pat.lib.clone(),
        item: it.itemname.clone(),
      };
      let spec = PiecesSpec {
        pos: Some(pos),
        posd: None, count: Some(1), face: None, pinned: Some(false),
        info: Box::new(spec),
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
