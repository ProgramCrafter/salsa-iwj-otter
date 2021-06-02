// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

#![allow(unused_imports)]

pub type MgmtChannel = ClientMgmtChannel;

pub use otter::imports::*;

pub use std::cell::Cell;
pub use std::cell::RefCell;
pub use std::rc::Rc;

pub use argparse::{self,ArgumentParser,action::{TypedAction,ParseResult}};
pub use argparse::action::{Action,IFlagAction,IArgAction};
pub use derive_more::Display;

pub use otter::prelude::*;
pub use otter::commands::*;

pub type APE = ArgumentParseError;
pub type E = anyhow::Error;
pub type PL = PresentationLayout;
pub type TP = TablePermission;

pub use argparse::action::ParseResult::Parsed;

pub mod clisupport;
use clisupport::*;

mod forgame;

#[derive(Debug)]
enum ServerLocation {
  Socket(String),
  Ssh(String),
}
use ServerLocation as SL;

#[derive(Debug)]
pub struct MainOpts {
  account: AccountName,
  nick: Option<String>,
  timezone: Option<String>,
  layout: Option<PresentationLayout>,
  access: Option<AccessOpt>,
  server: ServerLocation,
  verbose: i32,
  superuser: bool,
  spec_dir: String,
  game: Option<String>,
  ssh_command: String,
  ssh_proxy_command: String,
  sc: &'static Subcommand,
}

#[derive(Debug)]
pub struct Subcommand {
  pub verb: &'static str,
  pub help: &'static str,
  pub call: fn(SubCommandCallArgs) -> Result<(),E>,
  pub props: SubcommandProperties,
}
inventory::collect!(Subcommand);

#[derive(Default,Debug)]
pub struct SubcommandProperties {
  suppress_selectaccount: bool,
}

pub struct SubCommandCallArgs {
  out:  CookedStdout,
  ma:   MainOpts,
  args: Vec<String>
}
pub type SCCA = SubCommandCallArgs;

fn main() {
  #[derive(Default,Debug)]
  struct RawMainArgs {
    account: Option<AccountName>,
    server: Option<ServerLocation>,
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
    game: Option<String>,
    ssh_command: Option<String>,
    ssh_proxy_command: Option<String>,
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
                  "use account ACCOUNT (default: unix/ssh:<current user>:)");
    ap.refer(&mut rma.nick).metavar("NICK")
      .add_option(&["--nick"],
                  StoreOption,
                  "use NICK as nick for joining games (now and in the future) \
                   (default: derive from account name");
    ap.refer(&mut rma.game).metavar("TABLE-NAME")
      .add_option(&["--game","-g"],
                  StoreOption,
                  "Select the game to operate on");
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

    let mut server = ap.refer(&mut rma.server);
    server
      .metavar("S") // one matavar for all the options, bah
      .add_option(&["--socket"],
                  MapStore(|path| Ok(Some(
                    SL::Socket(path.to_string())
                  ))),
                  "connect to server via this socket path S");
    server
      .add_option(&["--ssh"],
                  MapStore(|userhost| Ok(Some(
                    SL::Ssh(userhost.to_string())
                  ))),
                  "connect to server via ssh, S is [USER@]HOST");

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

    ap.refer(&mut rma.ssh_command)
      .metavar("SSH")
      .add_option(&["--ssh-command"], StoreOption,
                  "command to run instead of `ssh` for remote Otter \
                   (shell syntax, followex `exec`, \
                   interporeted by the local shell)");
    ap.refer(&mut rma.ssh_proxy_command)
      .metavar("OTTER-PROXY-COMMAND")
      .add_option(&["--ssh-proxy-command"], StoreOption,
                  Box::leak(Box::new(/* bug in argparse */ format!(
                    "command to run instead of `{}` for remote Otter \
                    (shell syntax, interpreted by the remote shell)",
                    default_ssh_proxy_command()))));

    ap.refer(&mut rma.spec_dir)
      .add_option(&["--spec-dir"], StoreOption,
                  "directory for table and game specs");

    ap
  }, &|RawMainArgs {
    account, nick, timezone,
    access, server, verbose, config_filename, superuser,
    subcommand, subargs, spec_dir, layout, game,
    ssh_command, ssh_proxy_command
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

    let ssh_proxy_command = ssh_proxy_command.unwrap_or_else(
      default_ssh_proxy_command
    );
    let ssh_command = ssh_command.unwrap_or_else(|| "ssh".to_owned());

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

    let server = server.map(Ok::<_,APE>).unwrap_or_else(||{
      Ok(SL::Socket(
        config.clone()?.0
          .command_socket.clone()
      ))
    })?;

    let account: AccountName = account.map(Ok::<_,APE>).unwrap_or_else(||{
      let user = env::var("USER").map_err(|e| ArgumentParseError(
        format!("default account needs USER env var: {}", &e)
      ))?;
      let scope = match &server {
        SL::Socket(..) => AS::Unix { user },
        SL::Ssh   (..) => AS::Ssh  { user },
      };
      Ok(AccountName {
        scope,
        subaccount: "".into(),
      })
    })?;

    let sc = inventory::iter::<Subcommand>.into_iter()
      .filter(|Subcommand{verb:found,..}| found == &subcommand)
      .next()
      .unwrap_or_else(||{
        eprintln!("subcommand `{}' not recognised", &subcommand);
        exit(EXIT_USAGE);
      });

    Ok((subcommand, subargs, MainOpts {
      account,
      access,
      nick,
      timezone,
      layout,
      server,
      verbose,
      superuser,
      spec_dir,
      game,
      ssh_command,
      ssh_proxy_command,
      sc,
    }))
  }, Some(&|w|{
    writeln!(w, "\nSubcommands:")?;
    let maxlen = inventory::iter::<Subcommand>.into_iter()
      .map(|Subcommand{verb,..}| verb.len())
      .max().unwrap_or(0);
    for Subcommand{verb,help,..} in inventory::iter::<Subcommand> {
      writeln!(w, "  {:width$}  {}", verb, help, width=maxlen)?;
    }
    Ok(())
  }));

  let stdout = CookedStdout::new();
  let mut subargs = subargs;
  subargs.insert(0, format!("{} {}",
                            env::args().next().unwrap(),
                            &subcommand));

  (mo.sc.call)(SubCommandCallArgs { ma: mo, out: stdout, args: subargs })
    .unwrap_or_else(|e| e.end_process(12));
}

//---------- library-list ----------

#[derive(Debug,Default)]
struct LibGlobArgs {
  lib: Option<String>,
  pat: Option<String>,
}

impl LibGlobArgs {
  fn add_arguments<'ap, 'tlg: 'ap>(
    &'tlg mut self,
    ap: &'_ mut ArgumentParser<'ap>
  ) {
    use argparse::*;
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
  fn call(SCCA{ mut out, ma, args,.. }:SCCA) {
    let args = parse_args::<Args,_>(args, &subargs, &ok_id, None);
    let mut chan = ma.access_game()?;

    if args.lib.is_none() && args.pat.is_none() {
      let game = chan.game.clone();
      let libs = match chan.cmd(&MC::LibraryListLibraries { game })? {
        MgmtResponse::Libraries(libs) => libs,
        x => throw!(anyhow!(
          "unexpected response to LibrarylistLibraries: {:?}", &x)),
      };
      for lib in libs {
        writeln!(out, "{}", lib)?;
      }
      return;
    }

    let items = chan.list_items(args.lib.clone(), args.pat())?;
    for it in &items {
      writeln!(out, "{}", it)?;
    }
  }

  inventory_subcmd!{
    "library-list",
    "List pieces in the shape libraries",
  }
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

  fn call(SCCA{ mut out, ma, args,.. }:SCCA) -> Result<(),AE> {
    const MAGIC: &str = "mgmt-library-load-marker";

    let args = parse_args::<Args,_>(args, &subargs, &ok_id, None);
    let mut chan = ma.access_game()?;
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
            writeln!(out, "stopping: {}", &m)?;
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
    writeln!(out, "added {} pieces", count)?;
    exit(exitcode);
  }

  inventory_subcmd!{
    "library-add",
    "Add pieces from the shape libraries",
  }
}

//---------- list-pieces ----------

mod list_pieces {
  use super::*;

  type Args = NoArgs;

  #[throws(AE)]
  fn call(SCCA{ mut out, ma, args,.. }:SCCA) {
    let _args = parse_args::<Args,_>(args, &noargs, &ok_id, None);
    let mut chan = ma.access_game()?;
    let (pieces, pcaliases) = chan.list_pieces()?;
    for p in pieces {
      writeln!(out, "{:?}", p)?;
    }
    for a in pcaliases {
      writeln!(out, "{:?}", a)?;
    }
  }

  inventory_subcmd!{
    "list-pieces",
    "List pieces in the game",
  }
}

//---------- adhoc json/ron etc. ----------

#[derive(Debug,Copy,Clone)]
struct AdhocFormat(&'static str);

impl From<&'static Subcommand> for AdhocFormat {
  fn from(sc: &'static Subcommand) -> Self { AdhocFormat(
    sc.verb.rsplitn(2,'-').next().unwrap()
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
  pub fn report<T>(&self, out: &mut CookedStdout, resp: T)
  where T: Serialize
  {
    writeln!(out, "{}", match self.0 {
      "json" => serde_json::to_string(&resp).map_err(AE::from),
      "ron"  => ron::ser::  to_string(&resp).map_err(AE::from),
      _ => panic!(),
    }
        .context("re-format response")?)?;
  }
}

//---------- adhoc command ----------

mod command_adhoc {
  use super::*;

  #[derive(Default,Debug)]
  struct Args {
    cmds: Vec<String>,
  }

  fn subargs<'ap,'a:'ap,'m:'ap>(
    sa: &'a mut Args,
    ahf: AdhocFormat,
  ) -> ArgumentParser<'ap> {
    use argparse::*;
    let mut ap = ArgumentParser::new();
    ap.refer(&mut sa.cmds).required()
      .add_argument(format!("{}-COMMAND", ahf.metavar()).leak(),
                    Collect,
                    format!("{}-encoded MgmtCommand", ahf.name())
                    .leak());
    ap
  }

  #[throws(AE)]
  fn call(SCCA{ mut out, ma, args,.. }:SCCA) {
    let ahf = ma.sc.into();

    let subargs: ApMaker<_> = &|sa| subargs(sa,ahf);
    let args = parse_args::<Args,_>(args, subargs, &ok_id, None);
    let mut conn = connect(&ma)?;

    let cmds: Vec<MgmtCommand> = ahf.parse(args.cmds, "cmd")?;
    for (i, cmd) in cmds.into_iter().enumerate() {
      let resp = conn.cmd(&cmd).with_context(|| format!("cmd #{}", i))?;
      ahf.report(&mut out, resp)?;
    }
  }

  inventory_subcmd!{
    "command-json",
    "run ad-hoc management command(s) (JSON)",
  }
  inventory_subcmd!{
    "command-ron",
    "run ad-hoc management command(s) (Rusty Object Notation)",
  }
}

//---------- alter game ----------

mod alter_game_adhoc {
  use super::*;

  #[derive(Default,Debug)]
  struct Args {
    insns: Vec<String>,
  }

  fn subargs<'ap,'a:'ap,'m:'ap>(
    sa: &'a mut Args,
    ahf: AdhocFormat,
  ) -> ArgumentParser<'ap> {
    use argparse::*;
    let mut ap = ArgumentParser::new();
    ap.refer(&mut sa.insns).required()
      .add_argument(format!("{}-INSN", ahf.metavar()).leak(),
                    Collect,
                    format!("{}-encoded MgmtGameInstruction", ahf.name())
                    .leak());
    ap
  }

  fn call(SCCA{ mut out, ma, args,.. }:SCCA) -> Result<(),AE> {
    let ahf = ma.sc.into();

    let subargs: ApMaker<_> = &|sa| subargs(sa,ahf);
    let args = parse_args::<Args,_>(args, subargs, &ok_id, None);
    let mut chan = ma.access_game()?;

    let insns: Vec<MgmtGameInstruction> = ahf.parse(args.insns, "insn")?;
    let resps = chan.alter_game(insns,None)?;
    for resp in resps {
      ahf.report(&mut out, resp)?;
    }

    Ok(())
  }

  inventory_subcmd!{
    "alter-game-json",
    "run an ad-hoc AlterGame command (JSON)",
  }
  inventory_subcmd!{
    "alter-game-ron",
    "run an ad-hoc AlterGame command (Rusty Object Notation)",
  }
}

//---------- upload-bundle ----------

#[derive(Debug)]
struct BundleForUpload {
  file: String,
  f: BufReader<File>,
  size: usize,
  hash: bundles::Hash,
  kind: bundles::Kind,
}

impl BundleForUpload {
  #[throws(AE)]
  fn prepare(file: String) -> Self {
    let f = File::open(&file)
      .with_context(|| file.clone())
      .context("open bundle file")?;
    let size = f
      .metadata().context("fstat bundle file")?
      .len()
      .try_into().map_err(|_| anyhow!("bundle file far too large"))?;
    let mut f = BufReader::new(f);
    let hash = bundles::DigestWrite::of(&mut f)
      .context("read bundle file (for hash)")?;
    let hash = bundles::Hash(hash.into());
    let kind = bundles::Kind::only();
    f.rewind().context("rewind bundle file")?;
    BundleForUpload { file, f, size, hash, kind }
  }

  #[throws(AE)]
  fn upload(self, ma: &MainOpts, chan: &mut MgmtChannelForGame,
            progress: &mut dyn termprogress::Reporter)
            -> bundles::Id {
    let BundleForUpload { mut f, size, hash, kind,.. } = self;
    let cmd = MC::UploadBundle {
      size,
      game: ma.instance(),
      hash, kind,
      progress: MgmtChannel::PROGRESS,
    };
    let resp = chan.cmd_withbulk(&cmd, &mut f, &mut io::sink(),
                                 &mut *progress)?;
    if_let!{ MR::Bundle { bundle } = resp;
             else throw!(anyhow!("unexpected {:?}", &resp)) };
    progress.clear();
    bundle
  }
}

mod upload_bundle {
  use super::*;

  #[derive(Default,Debug)]
  struct Args {
    bundle_file: String,
  }

  fn subargs(sa: &mut Args) -> ArgumentParser {
    use argparse::*;
    let mut ap = ArgumentParser::new();
    ap.refer(&mut sa.bundle_file).required()
      .add_argument("BUNDLE",Store,"bundle file");
    ap
  }

  #[throws(AE)]
  fn call(SCCA{ mut out, ma, args,.. }:SCCA) {
    let args = parse_args::<Args,_>(args, &subargs, &ok_id, None);
    let mut chan = ma.access_game()?;
    let mut progress = ma.progressbar()?;
    let for_upload = BundleForUpload::prepare(args.bundle_file)?;
    let bundle = for_upload.upload(&ma, &mut chan, &mut *progress)?;
    writeln!(out, "{}", bundle)?;
  }

  inventory_subcmd!{
    "upload-bundle",
    "Upload a bundle",
  }
}

//---------- list-bundles ----------

mod list_bundles {
  use super::*;

  type Args = NoArgs;

  #[throws(AE)]
  fn call(SCCA{ mut out, ma, args,.. }:SCCA) {
    let _args = parse_args::<Args,_>(args, &noargs, &ok_id, None);
    let mut chan = ma.access_game()?;
    let resp = chan.cmd(&MC::ListBundles {
      game: ma.instance(),
    })?;
    if_let!{ MR::Bundles { bundles } = resp;
             else throw!(anyhow!("unexpected {:?}", &resp)) };
    for (id, state) in bundles {
      writeln!(out, "{} {}", id, &state)?;
    }
  }

  inventory_subcmd!{
    "list-bundles",
    "List bundles",
  }
}

//---------- download-bundle ----------

mod download_bundle {
  use super::*;

  #[derive(Default,Debug)]
  struct Args {
    index: bundles::Index,
    output: Option<PathBuf>,
  }

  fn subargs(sa: &mut Args) -> ArgumentParser {
    use argparse::*;
    let mut ap = ArgumentParser::new();
    ap.refer(&mut sa.index).required()
      .add_argument("INDEX",Store,"bundle number");
    ap.refer(&mut sa.output).metavar("OUTPUT")
      .add_option(&["-o","--output"],StoreOption,
                  "write output to OUTPUT (rather than NNNNN.zip");
    ap
  }

  #[throws(AE)]
  fn call(SCCA{ out, ma, args,.. }:SCCA) {
    let args = parse_args::<Args,_>(args, &subargs, &ok_id, None);
    let mut chan = ma.access_game()?;
    let kind = bundles::Kind::only();
    let id = bundles::Id { kind, index: args.index };
    let path = args.output.unwrap_or_else(|| id.to_string().into());

    let (f, path_tmp): (Box<dyn Write>, _) =
      if path.as_os_str().as_bytes() == b"-"
    {
      drop(out);
      (Box::new(RawStdout::new()), None)
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
      game: ma.instance(),
      id,
    };
    chan.cmd_withbulk(&cmd, &mut io::empty(), &mut f,
                      &mut termprogress::Null)
      .context("download bundle")?;
    f.flush().context("flush bundle file")?;
    if let Some((path, tmp)) = path_tmp {
      fs::rename(&tmp,&path)
        .with_context(|| path.to_debug()).context("rename after download")?;
    }
  }

  inventory_subcmd!{
    "download-bundle",
    "download bundle",
  }
}

//---------- clear game ----------

#[throws(AE)]
fn clear_game(ma: &MainOpts, chan: &mut MgmtChannelForGame) {
  chan.alter_game(vec![MGI::ClearGame{ }], None)
    .context("clear table")?;
  chan.cmd(&MC::ClearBundles { game: ma.instance() })
    .context("clear bundles")?;
}

mod clear_game {
  use super::*;

  type Args = NoArgs;

  #[throws(AE)]
  fn call(SCCA{ ma, args,.. }:SCCA) {
    let _args = parse_args::<Args,_>(args, &noargs, &ok_id, None);
    let mut chan = ma.access_game()?;
    clear_game(&ma, &mut chan)?;
  }

  inventory_subcmd!{
    "clear-game",
    "clear the table and clear out all bundles",
  }
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
  fn call(SCCA{ mut out, ma, args,.. }:SCCA) {
    let args = parse_args::<Args,_>(args, &subargs, &ok_id, None);
    let mut conn = connect(&ma)?;
    let all = Some(args.all);
    let accounts = match conn.cmd(&MC::ListAccounts { all })? {
      MR::AccountsList(g) => g,
      x => throw!(anyhow!("unexpected response to ListAccounts: {:?}", &x)),
    };
    for a in accounts {
      writeln!(out, "{}", a)?;
    }
  }

  inventory_subcmd!{
    "list-accounts",
    "List accounts in your account scope",
  }
}

//---------- mgmtchannel-proxy ----------

mod mgmtchannel_proxy {
  use super::*;

  #[derive(Default,Debug)]
  struct Args {
    restrict: Option<sshkeys::KeySpec>,
  }

  fn subargs(sa: &mut Args) -> ArgumentParser {
    use argparse::*;
    let mut ap = ArgumentParser::new();
    ap.refer(&mut sa.restrict).metavar("ID:NONCE")
      .add_option(&["--restrict-ssh"],StoreOption,
                  "restrict to access available to registred OpenSSH key \
                   (used in runes generated by server for authorized_keys)");
    ap
  }

  #[throws(AE)]
  fn call(SCCA{ out, ma, args,.. }:SCCA) {
    set_program_name("otter (remote)".into());

    let args = parse_args::<Args,_>(args, &subargs, &ok_id, None);
    let mut chan = connect_chan(&ma)?;

    if let Some(ref restrict) = args.restrict {
      chan.cmd(&MC::SetRestrictedSshScope { key: restrict.clone() })
        .context("specify authorisation")?;
    }

    let MgmtChannel { read, write } = chan;
    let mut read = read.into_stream()?;
    let mut write = write.into_stream()?;

    drop(out); // do our own stdout

    let tcmds = thread::spawn(move || {
      io_copy_interactive(&mut BufReader::new(io::stdin()), &mut write)
        .map_err(|e| match e {
          Left(re)  => AE::from(re).context("read cmds from stdin"),
          Right(we) => AE::from(we).context("forward cmds to servvr"),
        })
        .unwrap_or_else(|e| e.end_process(8));
      exit(0);
    });
    let tresps = thread::spawn(move || {
      io_copy_interactive(&mut read, &mut RawStdout::new())
        .map_err(|e| match e {
          Left(re)  => AE::from(re).context("read resps from server"),
          Right(we) => AE::from(we).context("forward cmds to stdout"),
        })
        .context("copy responses")
        .unwrap_or_else(|e| e.end_process(8));
      exit(0);
    });
    tcmds.join().expect("collect commands copy"); 
    tresps.join().expect("collect responses copy");
  }

  inventory_subcmd!{
    SSH_PROXY_SUBCMD,
    "connect to management channel and copy raw message data back and forth",
    suppress_selectaccount: true,
  }
}

//---------- set-ssh-keys ----------

mod set_ssh_keys {
  use super::*;

  #[derive(Default,Debug)]
  struct Args {
    add: bool,
    allow_non_ssh: bool,
    remove_current: bool,
    keys: String,
  }

  fn subargs(sa: &mut Args) -> ArgumentParser {
    use argparse::*;
    let mut ap = ArgumentParser::new();
    ap.refer(&mut sa.add)
      .add_option(&["--add"],StoreTrue,
                  "add keys, only (ie, leave all existing keys)");
    ap.refer(&mut sa.allow_non_ssh)
      .add_option(&["--allow-non-ssh-account"],StoreTrue,
                  "allow settings ssh key access for a non-ssh: account");
    ap.refer(&mut sa.remove_current)
      .add_option(&["--allow-remove-current"],StoreTrue,
                  "allow removing the key currently being used for access");
    ap.refer(&mut sa.keys).required()
      .add_argument("KEYS-FILE", Store,
                  "file of keys, in authorized_keys formaat \
                   (`-` means stdin)");
    ap
  }

  #[throws(AE)]
  fn call(SCCA{ ma, args,.. }:SCCA) {
    let args = parse_args::<Args,_>(args, &subargs, &ok_id, None);
    let mut conn = connect(&ma)?;

    if ! ma.account.subaccount.is_empty() {
      throw!(ME::NoSshKeysForSubaccount);
    }
    let is_ssh_account = matches!(ma.account.scope, AS::Ssh{..});
    if ! (args.allow_non_ssh || is_ssh_account) {
      throw!(anyhow!("not setting ssh keys for non-ssh: account; \
                      use --allow-non-ssh-account to override"));
    }

    conn.prep_access_account(&ma, false)?;

    use sshkeys::*;

    #[derive(Debug)]
    struct Currently {
      index: usize,
      mkr: MgmtKeyReport,
      thisconn_retain: bool,
    }

    #[derive(Debug,Default)]
    struct St {
      wanted: Option<(usize, AuthkeysLine)>,
      currently: Vec<Currently>,
    }
    type PubDataString = String;
    let mut states: HashMap<PubDataString, St> = default();

    // read wanted set

    let akf: Box<dyn Read> = if args.keys == "-" { Box::new(io::stdin()) }
    else {
      Box::new(File::open(&args.keys)
        .with_context(|| args.keys.clone())
        .context("KEYS-FILE")?)
    };
    let akf = BufReader::new(akf);

    for (l, lno) in akf.lines().zip(1..) {
      let l = l.context("read KEYS-FILE")?;
      let l = l.trim();
      if l.starts_with("#") || l == "" { continue }
      let l = AuthkeysLine(l.to_owned());
      let (pubdata, _comment) = l.parse()
        .with_context(|| format!("parse KEYS-FILE line {}", lno))?;
      let st = states.entry(pubdata.to_string()).or_insert_with(default);
      if let Some((lno0,_)) = st.wanted { throw!(
        anyhow!("KEYS-FILE has duplicate key, lines {} {}", lno0, lno)
      )}
      st.wanted = Some((lno, l));
    }

    // find the one we're using now

    let using = if args.remove_current { None } else {
      use MgmtResponse::ThisConnAuthBy as TCAB;
      use MgmtThisConnAuthBy as MTCAB;
      match conn.cmd(&MC::ThisConnAuthBy).context("find current auth")? {
        TCAB(MTCAB::Ssh { key }) => Some(key),
        TCAB(MTCAB::Local) => None,
        #[allow(unreachable_patterns)] TCAB(q) => throw!(anyhow!(
          "unexpected ThisConnAuthBy {:?}, \
           cannot check if we are removing the current authentication, \
           (and --allow-remove-current not specified)", q)),
        _ => throw!(anyhow!("unexpected response to ThisConnAuthBy")),
      }
    };

    // obtain current set

    for (index, mkr) in match conn.cmd(&MC::SshListKeys)
      .context("list existing keys")?
    {
      MR::SshKeys(report) => report,
      _ => throw!(anyhow!("unexpected response to SshListKeys")),
    }
      .into_iter().enumerate()
    {
      let pubdata_s = mkr.data.to_string();
      let st = states.entry(pubdata_s).or_insert_with(default);
      let thisconn_retain = Some(&mkr.key) == using.as_ref();
      st.currently.push(Currently { index, mkr, thisconn_retain });
    }

    // check we don't want to bail

    for st in states.values() {
      if st.wanted.is_none() {
        for c in &st.currently {
          if c.thisconn_retain {
            throw!(anyhow!(
              "refusing to remove currently-being-used key #{} {}",
              c.index, &c.mkr));
          }
        }
      }
    }

    // delete any with problems
    // doing this even for wanted keys prevents constant buildup
    // of problem keys

    for st in states.values() {
      for c in &st.currently {
        if c.mkr.problem.is_some() {
          conn.cmd(&MC::SshDeleteKey { index: c.index, id: c.mkr.key.id })
            .with_context(|| format!("delete broken key #{}", c.index))?;
        }
      }
    }

    // add new keys

    for st in states.values() {
      if_let!{ Some((lno, akl)) = &st.wanted; else continue };
      if st.currently.iter().any(|c| c.mkr.problem.is_none()) { continue }
      conn.cmd(&MC::SshAddKey { akl: akl.clone() })
        .with_context(|| format!("add key from KEYS-FILE line {}", lno))?;
    }

    // delete old keys

    for st in states.values() {
      if st.wanted.is_some() { continue }
      for c in &st.currently {
        if c.mkr.problem.is_some() { continue /* we deleted it already */ }
        conn.cmd(&MC::SshDeleteKey { index: c.index, id: c.mkr.key.id })
          .with_context(|| format!("delete old key #{}", c.index))?;
      }
    }
  }

  inventory_subcmd!{
    "set-ssh-keys",
    "set SSH keys for remote management access authentication",
  }
}

//---------- list-ssh-keys ----------

mod list_ssh_keys {
  use super::*;

  type Args = NoArgs;

  #[throws(AE)]
  fn call(SCCA{ mut out, ma, args,.. }:SCCA) {
    let _args = parse_args::<Args,_>(args, &noargs, &ok_id, None);
    let mut conn = connect(&ma)?;
    conn.prep_access_account(&ma, false)?;

    use sshkeys::*;

    // find the one we're using now

    let using = {
      use MgmtResponse::ThisConnAuthBy as TCAB;
      use MgmtThisConnAuthBy as MTCAB;
      match conn.cmd(&MC::ThisConnAuthBy).context("find current auth")? {
        TCAB(MTCAB::Ssh { key }) => Some(key),
        TCAB(_) => None,
        _ => throw!(anyhow!("unexpected response to ThisConnAuthBy")),
      }
    };

    // obtain current set

    for (_index, mkr) in match conn.cmd(&MC::SshListKeys)
      .context("list existing keys")?
    {
      MR::SshKeys(report) => report,
      _ => throw!(anyhow!("unexpected response to SshListKeys")),
    }
      .into_iter().enumerate()
    {
      let s = mkr.to_string();
      use unicode_width::UnicodeWidthChar;
      if s.chars().any(|c| c.width() == None /* control char */) {
        write!(&mut out, "# FUNKY! # {:?}", &s)?;
      } else {
        write!(&mut out, "{}", &s)?;
      }

      if Some(&mkr.key) == using.as_ref() {
        write!(&mut out, "# <- this connection!")?;
      }
      writeln!(&mut out, "")?;
    }

    out.flush()?;
  }

  inventory_subcmd!{
    "list-ssh-keys",
    "set SSH keys for remote management access authentication",
  }
}
