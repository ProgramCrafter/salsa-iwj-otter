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

mod adhoc;
mod forgame;
mod usebundles;
mod uselibs;

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
