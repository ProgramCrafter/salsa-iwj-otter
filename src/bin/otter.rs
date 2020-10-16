// Copyright 2020 Ian Jackson
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

#![allow(unused_imports)]

use otter::imports::*;
use argparse::{self,ArgumentParser,action::{TypedAction,ParseResult}};
use argparse::action::{Action,IFlagAction,IArgAction};
use std::rc::Rc;
use std::cell::RefCell;
use std::cell::Cell;

type E = anyhow::Error;
type Insn = MgmtGameInstruction;
type Resp = MgmtGameResponse;

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

const EXIT_SPACE :    i32 =  2;
const EXIT_USAGE :    i32 = 12;
const EXIT_DISASTER : i32 = 16;

#[derive(Debug,Default)]
struct MainOpts {
  scope: Option<ManagementScope>,
  socket_path: Option<String>,
  verbose: i32,
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

impl From<anyhow::Error> for ArgumentParseError {
  fn from(ae: anyhow::Error) -> ArgumentParseError {
    eprintln!("error during argument parsing/startup: {:?}\n", &ae);
    exit(EXIT_USAGE);
  }
}

fn parse_args<T,F>(
  args: Vec<String>,
  apmaker: &F,
  completer: Option<&dyn Fn(&mut T) -> Result<(), ArgumentParseError>>,
  extra_help: Option<&dyn Fn(&mut dyn Write) -> Result<(), io::Error>>,
) -> T
where T: Default,
      F: Fn(&mut T) -> ArgumentParser,
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
  if let Some(completer) = completer {
    completer(&mut parsed).unwrap_or_else(|e:ArgumentParseError| {
      let ap = apmaker(&mut parsed);
      ap.error(&us, &e.0, &mut stderr);
      exit(EXIT_USAGE);
    });
  }
  parsed
}

fn main() {
  #[derive(Default,Debug)]
  struct MainArgs {
    opts: MainOpts,
    config_filename: Option<String>,
    subcommand: String,
    subargs: Vec<String>,
  };
  let ma = parse_args::<MainArgs,_>(
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
    ap.refer(&mut ma.opts.socket_path)
      .add_option(&["--socket"], StoreOption,
                  "specify server socket path");
    ap.refer(&mut ma.config_filename)
      .add_option(&["-C","--config"], StoreOption,
                  "specify server config file (used for finding socket)");
    let mut verbose = ap.refer(&mut ma.opts.verbose);
    verbose.add_option(&["-q","--quiet"], StoreConst(-1),
                       "set verbosity to error messages only");
    verbose.add_option(&["-v","--verbose"], IncrBy(1),
       "increase verbosity (default is short progress messages)");
    ap
  }, Some(&|ma| {
    if let ref mut scope @None = ma.opts.scope {
      let user = env::var("USER").map_err(|e| ArgumentParseError(
        format!("--scope-unix needs USER env var: {}", &e)
      ))?;
      *scope = Some(ManagementScope::Unix { user });
    }
    if ma.config_filename.is_some() || ma.opts.socket_path.is_none() {
      ServerConfig::read(ma.config_filename.as_ref().map(String::as_str))
        .context("read config file")?;
    }
    ma.opts.socket_path.get_or_insert_with(
      || config().command_socket.clone()
    );
    Ok(())
  }), Some(&|w|{
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
}

struct ConnForGame {
  pub conn: Conn,
  pub name: String,
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
  fn alter_game(&mut self, insns: Vec<MgmtGameInstruction>,
                f: Option<&mut dyn FnMut(&MgmtGameResponse) -> Result<(),AE>>)
                -> Vec<MgmtGameResponse> {
    let insns_len = insns.len();
    let cmd = MgmtCommand::AlterGame {
      name: self.name.clone(), how: self.how,
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

#[throws(E)]
fn connect(ma: &MainOpts) -> Conn {
  let socket_path = ma.socket_path.as_ref().unwrap();
  let unix = UnixStream::connect(&socket_path)
    .with_context(||socket_path.clone()).context("connect to server")?; 
  let chan = MgmtChannel::new(unix)?;
  let mut chan = Conn { chan };
  chan.cmd(&MgmtCommand::SetScope(ma.scope.clone().unwrap()))?;
  chan
}

fn setup_table(ma: &MainOpts, chan: &mut ConnForGame,
               spec: &TableSpec) -> Result<(),AE> {
  // create missing players and delete old players
  let (added_players,) = {
    let (_, nick2id) = chan.get_info()?;

    #[derive(Default)]
    struct St { id: PlayerId, old: bool, new: bool };

    let mut nick2st : HashMap<_,_> = {nick2id}
      .drain()
      .map(|(nick,id)| (nick, St { id, old: true, new: false }))
      .collect();

    let mut insns = vec![];
    for pspec in &spec.players {
      let st = nick2st.entry(pspec.nick.clone()).or_default();
      if st.new {
        Err(anyhow!("duplicate player nick {:?} in spec", &pspec.nick))?;
      }
      st.new = true;
      let timezone = pspec.timezone.as_ref().or(
        spec.timezone.as_ref()
      ).cloned();
      // ^ todo use client program timezone?
      if !st.old {
        insns.push(MgmtGameInstruction::AddPlayer(MgmtPlayerState {
          timezone,
          st: PlayerState {
            nick: pspec.nick.clone(),
          },
        }));
      }
    }

    for (nick, st) in nick2st {
      if st.new { continue }
      if !st.old { continue }
      if ma.verbose >= 1 {
        eprintln!("removing old player {:?}", &nick);
      }
      insns.push(Insn::RemovePlayer(st.id));
    }

    let mut added_players = HashSet::new();
    chan.alter_game(insns, Some(&mut |response| {
      match response {
        &Resp::AddPlayer(player) => {
          added_players.insert(player);
        },
        _ => { },
      };
      Ok(())
    }))?;

    (added_players,)
  };

  // ensure players have access tokens
  {
    let (_, nick2id) = chan.get_info()?;
    let mut insns = vec![];
    let mut resetreport = vec![];
    let mut resetspecs = vec![];
    for pspec in &spec.players {
      let player = *nick2id.get(&pspec.nick)
        .ok_or_else(||anyhow!("player {:?} vanished or renamed!",
                              &pspec.nick))?;
      if let Some(access) = &pspec.access {
        match access.token_mgi(player) {
          Some(insn) => insns.push(insn),
          None if added_players.contains(&player) => {
            resetreport.push(player);
            resetspecs.push((pspec, access));
          },
          None => (),
        }
      };
    }
    insns.push(MgmtGameInstruction::ResetPlayerAccesses {
      players: resetreport.clone(),
    });
    let mut got_tokens = None;
    chan.alter_game(insns, Some(&mut |response| {
      if let MgmtGameResponse::PlayerAccessTokens(tokens) = response {
        got_tokens = Some(tokens.clone());
      }
      Ok(())
    }))?;
    let got_tokens = match got_tokens {
      Some(t) if t.len() == resetreport.len() => t,
      wat => Err(anyhow!("Did not get expected ReportPlayerAccesses! {:?}",
                         &wat))?,
    };
    for ((pspec, access), ptokens)
      in resetspecs.iter().zip(got_tokens.iter()) {
      access.deliver_tokens(&pspec, &ptokens)
          .with_context(||format!("deliver tokens for nick={:?}",
                                  &pspec.nick))?;
    }
  }

  Ok(())
}

#[throws(AE)]
fn read_spec<T: DeserializeOwned>(filename: &str, what: &str) -> T {
  (||{
    let mut f = File::open(filename).context("open")?;
    let mut buf = String::new();
    f.read_to_string(&mut buf).context("read")?;
    let spec : T = toml::de::from_str(&buf).context("parse")?;
    Ok::<_,AE>(spec)
  })().with_context(|| format!("read {} {:?}", what, filename))?
}

//---------- create-game ----------

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

  #[throws(E)]
  fn call(_sc: &Subcommand, ma: MainOpts, args: Vec<String>) {
    let args = parse_args::<Args,_>(args, &subargs, None, None);
    let spec : TableSpec = read_spec(&args.file, "table spec")?;
    let mut chan = connect(&ma)?;

    chan.cmd(&MgmtCommand::CreateGame {
      name: args.name.clone(), insns: vec![]
    })?;

    let mut chan = ConnForGame {
      conn: chan,
      name: args.name.clone(),
      how: MgmtGameUpdateMode::Bulk,
    };

    setup_table(&ma, &mut chan, &spec)?;

    if ma.verbose >= 0 {
      eprintln!("create-table successful.  game still needs setup.");
    }
  }

  inventory::submit!{Subcommand(
    "create-table",
    "Create a new table",
    call,
  )}
}

//---------- reset-game ----------

mod reset_game {
  use super::*;

  #[derive(Default,Debug)]
  struct Args {
    name: String,
    game_file: String,
    table_file: Option<String>,
  }

  fn subargs(sa: &mut Args) -> ArgumentParser {
    use argparse::*;
    let mut ap = ArgumentParser::new();
    ap.refer(&mut sa.table_file)
      .add_option(&["--reset-table"],StoreOption,
                  "reset the players and access too");
    ap.refer(&mut sa.name).required()
      .add_argument("TABLE-NAME",Store,"table name");
    ap.refer(&mut sa.game_file).required()
      .add_argument("GAME-SPEC-TOML",Store,"game spec");
    ap
  }

  fn call(_sc: &Subcommand, ma: MainOpts, args: Vec<String>) ->Result<(),AE> {
    let args = parse_args::<Args,_>(args, &subargs, None, None);
    let mut chan = ConnForGame {
      conn: connect(&ma)?,
      name: args.name.clone(),
      how: MgmtGameUpdateMode::Bulk,
    };
    let game : GameSpec = read_spec(&args.game_file, "game spec")?;

    if let Some(table_file) = args.table_file {
      let table_spec = read_spec(&table_file, "table spec")?;
      chan.cmd(&MgmtCommand::CreateGame {
        name: args.name.clone(),
        insns: vec![],
      }).map(|_|()).or_else(|e| {
        if let Some(&MgmtError::AlreadyExists) = e.downcast_ref() {
          return Ok(())
        }
        Err(e)
      })?;

      setup_table(&ma, &mut chan, &table_spec)?;
    }

    let mut insns = vec![];

    for p in chan.get_pieces()? {
      insns.push(MgmtGameInstruction::DeletePiece(p.piece));
    }

    if let Some(size) = game.table_size {
      insns.push(Insn::SetTableSize(size));
    }

    let mut game = game;
    for pspec in game.pieces.drain(..) {
      insns.push(Insn::AddPieces(pspec));
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

//---------- library-list ----------

#[derive(Debug)]
struct TableLibGlobArgs {
  name: String,
  pat: shapelib::ItemSpec,
}

impl Default for TableLibGlobArgs { fn default() -> Self { Self {
  name: default(),
  pat: shapelib::ItemSpec { lib: default(), item: default() },
} } }

impl TableLibGlobArgs {
  fn add_arguments<'ap, 'tlg : 'ap>(
    &'tlg mut self,
    ap: &'_ mut ArgumentParser<'ap>
  ) {
    use argparse::*;
    ap.refer(&mut self.name).required()
      .add_argument("TABLE-NAME",Store,"table name");
    ap.refer(&mut self.pat.lib).required()
      .add_argument("LIB-NAME",Store,"library name");
    ap.refer(&mut self.pat.item).required()
      .add_argument("ITEM-GLOB-PATTERN",Store,"item glob pattern");
  }
}

mod library_list {
  use super::*;

  type Args = TableLibGlobArgs;

  fn subargs(sa: &mut Args) -> ArgumentParser {
    use argparse::*;
    let mut ap = ArgumentParser::new();
    sa.add_arguments(&mut ap);
    ap
  }

  fn call(_sc: &Subcommand, ma: MainOpts, args: Vec<String>) ->Result<(),AE> {
    let args = parse_args::<Args,_>(args, &subargs, None, None);
    let mut chan = ConnForGame {
      conn: connect(&ma)?,
      name: args.name.clone(),
      how: MgmtGameUpdateMode::Bulk,
    };

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
    tlg: TableLibGlobArgs,
    adjust_markers: Option<bool>,
    incremental: bool,
  }

  impl Args {
    fn adjust_markers(&self) -> bool { self.adjust_markers.unwrap_or(true) }
  }

  fn subargs(sa: &mut Args) -> ArgumentParser {
    use argparse::*;
    let mut ap = ArgumentParser::new();
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

    let args = parse_args::<Args,_>(args, &subargs, None, None);
    let mut chan = ConnForGame {
      conn: connect(&ma)?,
      name: args.tlg.name.clone(),
      how: MgmtGameUpdateMode::Online,
    };
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
      Poor(Vec<Insn>, &'static str),
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
      Poor(vec![ Insn::AddPieces(spec) ],
           "marker(s) created")
    } else if markers.len() > WANTED {
      let insns = markers[WANTED..].iter()
        .map(|p| Insn::DeletePiece(p.piece))
        .collect();
      Poor(insns,
           "surplus marker(s) removed")
    } else {
      let good : ArrayVec<_> = markers.iter().map(|p| p.pos).collect();
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
                let tl = p.pos + p.bbox[0];
                let br = p.pos + p.bbox[1];
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
      let insn = Insn::AddPieces(spec);
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
