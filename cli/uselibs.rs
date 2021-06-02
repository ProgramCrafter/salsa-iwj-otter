// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use super::*;

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
