// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use crate::imports::*;

use super::*;

#[derive(Serialize,Debug)]
struct SessionRenderContext {
  table_colour: Colour,
  ptoken: RawToken,
  ctoken: RawToken,
  player: PlayerId,
  gen: Generation,
  space_attrs: Html,
  rect_attrs: Html,
  uses: Vec<SessionPieceContext>,
  defs: Vec<(VisiblePieceId, Html)>,
  nick: String,
  load: String,
  log: Vec<SessionFormattedLogEntry>,
  sse_url_prefix: String,
  links: Html,
  player_info_pane: Html,
  bundles_info_pane: Html,
  fake_rng: bool,
}

#[derive(Debug,Serialize)]
struct SessionFormattedLogEntry {
  when: String,
  logent: Arc<CommittedLogEntry>,
}

#[derive(Serialize,Debug,Eq,PartialEq,Ord,PartialOrd)]
struct SessionPieceContext {
  z: ZLevel,
  id: VisiblePieceId,
  pos: Pos,
  info: String, // SessionPieceLoadJson as JSON
}

#[derive(Serialize,Debug)]
struct SessionPieceLoadJson<'r> {
  held: &'r Option<PlayerId>,
  z: ZCoord,
  zg: Generation,
  pinned: bool,
  angle: CompassAngle,
  desc: Html,
  uos: &'r [UoDescription],
  moveable: PieceMoveable,
  rotateable: bool,
  occregion: Option<JsonString<&'r Region>>,
  pub bbox: &'r Rect,
}

#[derive(Serialize,Debug)]
struct DataLoad {
  last_log_ts: String,
  players: HashMap<PlayerId, DataLoadPlayer>,
  held_surround_colour: &'static str,
  movehist: movehist::PlHist,
}

#[derive(Deserialize)]
struct SessionForm {
  ptoken: RawToken,
}
#[post("/_/session/<layout>", format="json", data="<form>")]
#[throws(OER)]
fn session(form: Json<SessionForm>,
           layout: Option<Parse<PresentationLayout>>)
           -> Template {
  session_inner(form, layout.map(|pl| pl.0))?
}

#[ext]
impl SvgAttrs {
  fn to_html(&self) -> Html {
    let mut o = Html::new();
    for (k,v) in self {
      hwrite!(&mut o, r##"{}="{}""##, k, v).unwrap();
    }
    o
  }
} 

fn session_inner(form: Json<SessionForm>,
                 layout: Option<PresentationLayout>)
                 -> Result<Template,OE> {
  // make session in this game, log a message to other players
  let iad = lookup_token(form.ptoken.borrow())?;
  let player = iad.ident;
  let (c, client, layout) = {
    let mut ig = iad.gref.lock()?;
    let cl = Client { player, lastseen: Instant::now() };
    let client = ig.clients.insert(cl);

    let ciad = InstanceAccessDetails {
      gref: iad.gref.clone(),
      ident: client,
      acctid: iad.acctid,
    };
    let ctoken = record_token(&mut ig, ciad)?;
    ig.save_game_later(); // in case we changed anything eg gpl.layout
    let ig = &mut *ig;
    let ioccults = &ig.ioccults;

    let mut uses = vec![];
    let mut alldefs = vec![];

    let player_info_pane = ig.player_info_pane()?;

    let mut load_players = HashMap::new();
    for (player, _pl) in &ig.gs.players {
      let dataload = DataLoadPlayer::from_player(ig, player);
      load_players.insert(player, dataload);
    }

    let gpl = ig.gs.players.byid_mut(player)?;
    let pr = ig.iplayers.byid(player)?;
    let tz = &pr.ipl.tz;
    if let Some(layout) = layout {
      gpl.layout = layout;
    }
    let layout = gpl.layout;
    let nick = gpl.nick.clone();
    let movehist = gpl.movehist.clone();

    for (piece, gpc) in ig.gs.pieces.iter() {
      let ipc = if let Some(pto) = ig.ipieces.get(piece) { pto }
      else { continue /* was deleted */ };

      let pri = piece_pri(ioccults, &ig.gs.occults, player,
                          // we need to re-obtain gpl, because
                          // make_defs needs to borrow the whole of &gs
                          ig.gs.players.byid_mut(player)?,
                          piece, gpc, ipc);
      let pri = if let Some(pri) = pri { pri } else { continue /*invisible*/};

      let (defs, bbox) = pri.make_defs(ioccults, &ig.gs, gpc, ipc)?;
      alldefs.push((pri.vpid, defs));
      let desc = pri.describe(ioccults,&ig.gs.occults, &gpc, ipc);

      let vangle = pri.angle(gpc).to_compass();
      let (pos, zlevel) = pri.pos_zlevel(gpc);
      let occregion = gpc.occult.active_region(&ig.gs.occults)?
        .map(|r| JsonString(r));

      let for_info = SessionPieceLoadJson {
        held: &gpc.held,
        z: zlevel.z.clone(),
        zg: zlevel.zg,
        pinned: gpc.pinned,
        angle: vangle,
        desc,
        bbox: &bbox,
        moveable: gpc.moveable(),
        rotateable: gpc.rotateable(),
        uos: &pri.ui_operations(&ig.gs, gpc, ipc)?,
        occregion,
      };

      let for_piece = SessionPieceContext {
        z: zlevel.clone(),
        id: pri.vpid,
        pos: pos,
        info: serde_json::to_string(&for_info)
          .map_err(|e| InternalError::JSONEncode(e))?,
      };
      uses.push(for_piece);
    }

    uses.sort();

    let mut timestamp_abbrev: Option<String> = None;

    let log = itertools::chain(
      ig.gs.log.iter()
        .map(|(_, logent)| logent)
        .cloned(),
      {
        let tr = &pr.ipl.tokens_revealed;
        let y = tr.len() > 1;
        let mut l = tr.iter()
          .filter(move |_| y)
          .collect::<Vec<_>>();
        l.sort_unstable_by_key(
          |(trk,trv)| (trv.latest, trv.earliest, &trk.account, &trk.desc)
        );
        l.into_iter()
          .map(|(trk,trv)|{
            let when = trv.latest;
            let html = hformat!(
              "player state accessed via {} [{}]",
              trk.desc, trk.account
            );
            Arc::new(CommittedLogEntry { when, logent: LogEntry { html } })
          })
      },
    ).map(|logent|{
      let mut when = logent.when.render(tz);
      if layout.abbreviate_timestamps() {
        let (abbrev, _) = base_misc::timestring_abbreviate(
          timestamp_abbrev.get_or_insert(default()),
          &when
        );
        let abbrev = abbrev.into();
        timestamp_abbrev = Some(mem::replace(&mut when, abbrev));
      }
      SessionFormattedLogEntry { when, logent }
    }).collect();

    let sse_url_prefix = match &config().sse_wildcard_url {
      Some((lhs, rhs)) => {
        let mut clpart = client.to_string();
        clpart.make_ascii_lowercase();
        format!("{}{}{}", lhs, clpart, rhs)
      },
      None => "".into(),
    };

    let table_size = ig.gs.table_size.promote();

    let src = SessionRenderContext {
      table_colour: ig.gs.table_colour.clone(),
      ctoken,
      gen: ig.gs.gen,
      log,
      player,
      defs: alldefs,
      uses,
      space_attrs: space_table_attrs(table_size).to_html(),
      rect_attrs: space_table_attrs(table_size).to_html(),
      nick,
      sse_url_prefix,
      player_info_pane,
      bundles_info_pane: ig.bundle_list.info_pane(ig)?,
      ptoken: form.ptoken.clone(),
      links: (&*ig.links).into(),
      fake_rng: config().game_rng.is_fake(),
      load: serde_json::to_string(&DataLoad {
        movehist,
        players: load_players,
        last_log_ts: timestamp_abbrev.unwrap_or_default(),
        held_surround_colour: HELD_SURROUND_COLOUR,
      }).map_err(|e| InternalError::JSONEncode(e))?,
    };
    trace!("SessionRenderContext {:?}", &src);
    (src, client, layout)
  };
  info!("rendering /_/session for {:?} {:?} {:?} {:?} {:?}",
        &player, client, &c.nick, &c.ctoken,
        iad.gref.lock().ok().as_ref().map(|ig| &**ig));

  Ok(Template::render(layout.template(),&c))
}

pub fn mount(rocket_instance: Rocket) -> Rocket {
  rocket_instance.mount("/", routes![
    session,
  ])
}
