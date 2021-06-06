// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use super::*;

#[derive(Clone)]
pub struct MapStore<T, F: FnMut(&str) -> Result<T, String>>(pub F);

pub struct BoundMapStore<'r, T, F: FnMut(&str) -> Result<T,String>> {
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

#[derive(Error,Debug,Clone,Display)]
pub struct ArgumentParseError(pub String);

impl From<&anyhow::Error> for ArgumentParseError {
  fn from(ae: &anyhow::Error) -> ArgumentParseError {
    eprintln!("error during argument parsing/startup: {}\n", ae);
    exit(EXIT_USAGE);
  }
}

pub fn default_ssh_proxy_command() -> String {
  format!("{} {}", DEFAULT_SSH_PROXY_CMD, SSH_PROXY_SUBCMD)
}

impl MainOpts {
  pub fn game(&self) -> &str {
    self.game.as_ref().map(|s| s.as_str()).unwrap_or_else(||{
      eprintln!(
        "game (table) name not specified; pass --game option");
      exit(EXIT_USAGE);
    })
  }

  pub fn instance(&self) -> InstanceName {
    match self.game().strip_prefix(":") {
      Some(rest) => {
        InstanceName {
          account: self.account.clone(),
          game: rest.into(),
        }
      }
      None => {
        self.game().parse().unwrap_or_else(|e|{
          eprintln!(
            "game (table) name must start with : or be valid full name: {}",
            &e);
          exit(EXIT_USAGE);
        })
      }
    }
  }

  #[throws(AE)]
  pub fn access_account(&self) -> Conn {
    let mut conn = connect(self)?;
    conn.prep_access_account(self, true)?;
    conn
  }

  #[throws(AE)]
  pub fn access_game(&self) -> MgmtChannelForGame {
    self.access_account()?.chan.for_game(
      self.instance(),
      MgmtGameUpdateMode::Online,
    )
  }

  #[throws(AE)]
  pub fn progressbar(&self) -> Box<dyn termprogress::Reporter> {
    if self.verbose >= 0 {
      termprogress::new()
    } else {
      termprogress::Null::new()
    }
  }
}

#[derive(Default,Debug)]
pub struct NoArgs { }
pub fn noargs(_sa: &mut NoArgs) -> ArgumentParser { ArgumentParser::new() }

pub type ApMaker<'apm, T> =
  &'apm dyn for <'a> Fn(&'a mut T) -> ArgumentParser<'a>;

pub type ExtraHelp<'exh> =
  &'exh dyn Fn(&mut dyn Write) -> Result<(), io::Error>;

pub fn run_argparse<T>(parsed: &mut T, apmaker: ApMaker<T>,
                       args: Vec<String>, extra_help: Option<ExtraHelp>)
                       -> String /* us */{
  let ap = apmaker(parsed);
  let us = args.get(0).expect("argv[0] must be provided!").clone();

  let mut stdout = CookedStdout::new();
  let mut stderr = io::stderr();

  let r = ap.parse(args, &mut stdout, &mut stderr);
  if let Err(rc) = r {
    exit(match rc {
      0 => {
        if let Some(eh) = extra_help {
          eh(&mut stdout).unwrap();
        }
        0
      },
      2 => EXIT_USAGE,
      _ => panic!("unexpected error rc {} from ArgumentParser::parse", rc),
    });
  }
  mem::drop(stdout);
  mem::drop(ap);

  us
}

pub fn parse_args<T:Default,U>(
  args: Vec<String>,
  apmaker: ApMaker<T>,
  completer: &dyn Fn(T) -> Result<U, ArgumentParseError>,
  extra_help: Option<ExtraHelp>,
) -> U {
  let mut stderr = io::stderr();

  let mut parsed = default();
  let us = run_argparse(&mut parsed, apmaker, args, extra_help);
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
pub struct AccessOpt(Box<dyn PlayerAccessSpec>);
impl Clone for AccessOpt {
  fn clone(&self) -> Self { Self(clone_via_serde(&self.0)) }
}
impl<T: PlayerAccessSpec + 'static> From<T> for AccessOpt {
  fn from(t: T) -> Self { AccessOpt(Box::new(t)) }
}
impl From<AccessOpt> for Box<dyn PlayerAccessSpec> {
  fn from(a: AccessOpt) -> Self { a.0 }
}

pub type ExecutableRelatedError = AE;
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
#[ext(pub)]
impl String {
  fn leak(self) -> &'static str { Box::<str>::leak(self.into()) }
}

pub struct Conn {
  pub chan: ClientMgmtChannel,
}

deref_to_field_mut!{Conn, MgmtChannel, chan}

impl Conn {
  #[throws(AE)]
  pub fn prep_access_account(&mut self, ma: &MainOpts,
                         maybe_update_account: bool) {
    #[derive(Debug)]
    struct Wantup(bool);
    impl Wantup {
      fn u<T:Clone>(&mut self, rhs: &Option<T>) -> Option<T> {
        if rhs.is_some() { self.0 = true }
        rhs.clone()
      }
    }
    let mut wantup = Wantup(false);

    let mut ad = if maybe_update_account { AccountDetails {
      account:  ma.account.clone(),
      nick:     wantup.u(&ma.nick),
      timezone: wantup.u(&ma.timezone),
      layout:   wantup.u(&ma.layout),
      access:   wantup.u(&ma.access).map(Into::into),
    } } else {
      AccountDetails::default(ma.account.clone())
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
pub fn connect_chan(ma: &MainOpts) -> MgmtChannel {
  match &ma.server {

    SL::Socket(socket) => {
      MgmtChannel::connect(socket)?
    },

    SL::Ssh(user_host) => {
      
      let user_host = {
        let (user,host) =
          user_host.split_once('@')
          .unwrap_or_else(|| ("Otter", user_host));
        format!("{}@{}", user, host)
      };
      
      let mut cmd = Command::new("sh");
      cmd.arg(if ma.verbose > 2 { "-xec" } else { "-ec" });
      cmd.arg(format!(r#"exec {} "$@""#, &ma.ssh_command));
      cmd.arg("x");
      let args = [
        &user_host,
        &ma.ssh_proxy_command,
      ];
      cmd.args(args);

      let desc = format!("ssh: {:?} {:?}", &ma.ssh_command, &args);

      let (w,r) = childio::run_pair(cmd, desc.clone())
        .with_context(|| desc.clone())
        .context("run remote command")?;
      MgmtChannel::new_boxed(r,w)
    },

  }
}

#[throws(E)]
pub fn connect(ma: &MainOpts) -> Conn {
  let chan = connect_chan(ma)?;
  let mut chan = Conn { chan };
  if ma.superuser {
    chan.cmd(&MC::SetSuperuser(true))?;
  }
  if ! ma.sc.props.suppress_selectaccount {
    chan.cmd(&MC::SelectAccount(ma.account.clone()))?;
  }
  chan
}

pub const PLAYER_ALWAYS_PERMS: &[TablePermission] = &[
  TP::TestExistence,
  TP::ShowInList,
  TP::ViewNotSecret,
  TP::Play,
];

pub const PLAYER_DEFAULT_PERMS: &[TablePermission] = &[
  TP::ChangePieces,
  TP::UploadBundles,
];

#[throws(AE)]
pub fn setup_table(_ma: &MainOpts, instance_name: &InstanceName, spec: &TableSpec)
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

pub trait SomeSpec {
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

pub trait SpecParse {
  type T;
  type S: SomeSpec;
  fn parse(s: String) -> Result<Self::T,AE>;
}
#[derive(Debug,Copy,Clone)]
pub struct SpecParseToml<T>(pub PhantomData<T>);
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
pub struct SpecRaw<T>(pub PhantomData<T>);
impl<T:SomeSpec> SpecParse for SpecRaw<T> {
  type T = String;
  type S = T;
  #[throws(AE)]
  fn parse(buf: String) -> String { buf }
}
impl<T> SpecRaw<T> { pub fn new() -> Self { Self(default()) } }

pub fn spec_arg_is_path(specname: &str) -> Option<String> {
  if specname.contains('/') {
    Some(specname.to_string())
  } else {
    None
  }
}

#[throws(AE)]
pub fn read_spec<P:SpecParse>(ma: &MainOpts, specname: &str, p: P) -> P::T
{
  let filename = spec_arg_is_path(specname).unwrap_or_else(
    || format!("{}/{}.{}.toml", &ma.spec_dir, specname, P::S::FNCOMP)
  );
  read_spec_from_path(filename, p)?
}

#[throws(AE)]
pub fn read_spec_from_path<P:SpecParse>(filename: String, _: P) -> P::T
{
  (||{
    let mut f = File::open(&filename).context("open")?;
    let mut buf = String::new();
    f.read_to_string(&mut buf).context("read")?;
    let spec = P::parse(buf)?;
    Ok::<_,AE>(spec)
  })().with_context(|| format!("read {} {:?}", P::S::WHAT, &filename))?
}

#[macro_export]
macro_rules! inventory_subcmd {
  {$verb:expr, $help:expr $(,)?} => {
    inventory::submit!{Subcommand {
      verb: $verb,
      help: $help,
      call,
      props: default(),
    }}
  };
  {$verb:expr, $help:expr, $($prop:tt)+} => {
    inventory::submit!{Subcommand {
      verb: $verb,
      help: $help,
      call,
      props: SubcommandProperties { $($prop)* ..default() },
    }}
  };
}
