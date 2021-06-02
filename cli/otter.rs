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
mod forssh;
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
