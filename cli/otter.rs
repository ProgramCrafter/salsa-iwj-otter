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
mod admin;
mod forssh;
mod forgame;
mod usebundles;
mod uselibs;

#[derive(Debug,Deserialize)]
struct Prefs {
  #[serde(default)] options: toml::value::Table,
}

#[derive(Debug,Deserialize)]
#[serde(rename_all="snake_case")]
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
    prefs_path: Option<String>,
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

  fn apmaker_gen<'a>(rma: &'a mut RawMainArgs, want_args: bool)
                     -> ArgumentParser<'a> {
    use argparse::*;
    let mut ap = ArgumentParser::new();
    ap.stop_on_first_argument(true);
    ap.silence_double_dash(true);

    if want_args {
      ap.refer(&mut rma.subcommand).required()
        .add_argument("SUBCOMMAND", Store, "subcommand");
      ap.refer(&mut rma.subargs)
        .add_argument("...", Collect, "subcommand options/arguments");
    }

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

    ap.refer(&mut rma.prefs_path)
      .add_option(&["--prefs"], StoreOption,
                  "preferences file (usually ~/.config/otter/prefs.toml)");

    ap.refer(&mut rma.spec_dir)
      .add_option(&["--spec-dir"], StoreOption,
                  "directory for table and game specs");

    ap
  }

  let apmaker: ApMaker<RawMainArgs> = &|a| apmaker_gen(a, true);

  let ap_completer = |RawMainArgs {
    account, nick, timezone,
    access, server, verbose, config_filename, superuser,
    subcommand, subargs, spec_dir, layout, game,
    ssh_command, ssh_proxy_command, prefs_path:_
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
  };

  let extra_help: ExtraMessage = &|w|{
    writeln!(w, "\nSubcommands:")?;
    let maxlen = inventory::iter::<Subcommand>.into_iter()
      .map(|Subcommand{verb,..}| verb.len())
      .max().unwrap_or(0);
    for Subcommand{verb,help,..} in inventory::iter::<Subcommand> {
      writeln!(w, "  {:width$}  {}", verb, help, width=maxlen)?;
    }
    Ok(())
  };

  let mut parsed: RawMainArgs = default();
  let args: Vec<String> = env::args().collect();

  let mut rapc = RawArgParserContext::new(&args);
  let mut ap = apmaker(&mut parsed);
  rapc.run(&mut ap, args.clone(), Some(extra_help), None);
  let us = rapc.done();
  drop(ap);

  argparse_more(us.clone(), apmaker, || (||{
    let prefs_path: PathBuf = parsed.prefs_path.as_ref()
      .map(|s| Ok(s.into()))
      .unwrap_or_else(|| -> Result<PathBuf,AE> {
        static VAR: &str = "OTTER_PREFS";
        if let Some(ev) = env::var_os(VAR) {
          return ev.try_into().context(VAR);
        }
        let dir =
          directories::ProjectDirs::from("uk.org","greenend","Otter")
          .ok_or_else(||anyhow!("failed to find home directory"))?
          .config_dir()
          .to_owned();
        Ok::<_,AE>(dir.join("prefs.toml"))
      })
      .context("locate preferences file (prefs.toml)")?;

    let data: Option<Prefs> = (||{
      let data = match fs::read_to_string(&prefs_path) {
        Err(e) if e.kind() == ErrorKind::NotFound => {
          if parsed.verbose >= 2 {
            eprintln!("prefs file {:?} does not exist, skipping",
                      &prefs_path);
          }
          return Ok(None)
        },
        Err(e) => throw!(AE::from(e).context("open and read")),
        Ok(data) => data,
      };
      let data = data.parse().context("parse as toml")?;
      let data = toml_de::from_value(&data).context("parse as preferences")?;
      Ok::<_,AE>(Some(data))
    })().context(prefs_path.display().to_string()).context("prefs file")?;
    if_let!{ Some(data) = data; else return Ok(()); }

    let mut redo: RawMainArgs = default();
    let mut rapc = RawArgParserContext::new(&args);
    let mut ap = apmaker_gen(&mut redo, false);

    for (k, v) in &data.options {
      let context = || format!(
        "prefs file {} option.{}",
        prefs_path.display(), k,
      );
      use toml::value::Value as TV;
      let synth_arg = match v {
        TV::Boolean(true)  => format!("--{}",    k),
        TV::Boolean(false) => format!("--no-{}", k),
        TV::String(x)      => format!("--{}={}", k, x),
        TV::Integer(x)     => format!("--{}={}", k, x),
        _ => throw!(
          anyhow!("cannot handle this toml value type ({})", v.type_str())
            .context(context())
        ),
      };
      let synth_args = vec![args[0].clone(), synth_arg.clone()];

      rapc.run(&mut ap, synth_args, None, Some(&|stderr: &mut dyn Write|{
        writeln!(stderr, "Error processing {}\n\
                          Prefs option interpreted as {}",
                 context(), &synth_arg)
      }));
    }

    drop(ap);
    let mut ap = apmaker_gen(&mut redo, true);

    rapc.run(&mut ap, args.clone(), Some(extra_help), None);
    drop(ap);

    parsed = redo;
    Ok(())
  })()
                .map_err(|ae| ArgumentParseError::from(&ae))
  );

  let completed = argparse_more(us, apmaker, || ap_completer(parsed));

  let (subcommand, subargs, mo) = completed;

  let stdout = CookedStdout::new();
  let mut subargs = subargs;
  subargs.insert(0, format!("{} {}",
                            &args[0],
                            &subcommand));

  (mo.sc.call)(SubCommandCallArgs { ma: mo, out: stdout, args: subargs })
    .unwrap_or_else(|e| e.end_process(12));
}
