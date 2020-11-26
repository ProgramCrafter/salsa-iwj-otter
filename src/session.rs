// Copyright 2020 Ian Jackson
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use crate::imports::*;

#[derive(Serialize,Debug)]
struct SessionRenderContext {
  // xxx abbrev timestamps in Landscape
  ptoken : RawToken,
  ctoken : RawToken,
  player : PlayerId,
  gen : Generation,
  table_size : Pos,
  uses : Vec<SessionPieceContext>,
  defs : Vec<(VisiblePieceId,Html)>,
  nick : String,
  load : String,
  log : Vec<SessionFormattedLogEntry>,
  sse_url_prefix : String,
}

#[derive(Debug,Serialize)]
struct SessionFormattedLogEntry {
  when: String,
  logent: Arc<CommittedLogEntry>,
}

#[derive(Serialize,Debug)]
struct SessionPieceContext {
  id: VisiblePieceId,
  pos: Pos,
  info: String, // SessionPieceLoadJson as JSON
}

#[derive(Serialize,Debug)]
struct SessionPieceLoadJson<'r> {
  held : &'r Option<PlayerId>,
  z : ZCoord,
  zg : Generation,
  pinned : bool,
  uos: &'r [UoDescription],
}

#[derive(Serialize,Debug)]
struct DataLoad {
  players : HashMap<PlayerId, DataLoadPlayer>,
}
#[derive(Serialize,Debug)]
struct DataLoadPlayer {
  dasharray : String,
}

#[derive(Deserialize)]
struct SessionForm {
  ptoken : RawToken,
}
#[post("/_/session/<layout>", format="json", data="<form>")]
fn session(form : Json<SessionForm>, layout: PresentationLayout)
           -> Result<Template,OE> {
  // make session in this game, log a message to other players
  let iad = lookup_token(form.ptoken.borrow())?;
  let player = iad.ident;
  let (c, client) = {
    let mut ig = iad.gref.lock()?;
    let cl = Client { player, lastseen: Instant::now() };
    let client = ig.clients.insert(cl);

    let ciad = InstanceAccessDetails {
      gref : iad.gref.clone(),
      ident : client,
      acctid: iad.acctid,
    };
    let ctoken = record_token(&mut ig, ciad)?;
    let ig = &mut *ig;

    let mut uses = vec![];
    let mut alldefs = vec![];

    let mut load_players = HashMap::new();
    for (player, _pl) in &ig.gs.players {
      let kd : slotmap::KeyData = player.into();
      let n = kd.get_idx_version().0;
      let n = if n != 0 { n.try_into().unwrap() }
              else { ig.gs.players.capacity() };
      assert!(n != 0);
      let mut dasharray = String::with_capacity(n*3 + 4);
      for dash in iter::once("3").chain(
        iter::repeat("1").take(n-1))
      {
        write!(&mut dasharray, "{} 1 ", &dash).unwrap();
      }
      let spc = dasharray.pop();
      assert_eq!(spc,Some(' '));

      load_players.insert(player, DataLoadPlayer {
        dasharray,
      });
    }

    let gpl = ig.gs.players.byid_mut(player)?;
    let pr = ig.iplayers.byid(player)?;
    let tz = &pr.ipl.tz;
    let mut pieces : Vec<_> = ig.gs.pieces.iter().collect();

    pieces.sort_by_key(|(_,pr)| &pr.zlevel);

    for (gpid, pr) in pieces {
      let pri = PieceRenderInstructions {
        id : make_pieceid_visible(gpid),
        face : pr.face,
      };
      let p = if let Some(p) = ig.ipieces.get(gpid) { p }
      else { continue /* was deleted */ };
      let defs = p.make_defs(&pri)?;
      alldefs.push((pri.id, defs));

      let for_info = SessionPieceLoadJson {
        held : &pr.held,
        z  : pr.zlevel.z.clone(),
        zg : pr.zlevel.zg,
        pinned : pr.pinned,
        uos : &p.ui_operations()?,
      };

      let for_piece = SessionPieceContext {
        id: pri.id,
        pos : pr.pos,
        info : serde_json::to_string(&for_info)
          .map_err(|e| InternalError::JSONEncode(e))?,
      };
      uses.push(for_piece);
    }

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
            let html = Html(format!(
              "player state accessed via {} [{}]",
              &trk.desc.0, &trk.account
            ));
            Arc::new(CommittedLogEntry { when, logent: LogEntry { html } })
          })
      },
    ).map(|logent|{
      let when = logent.when.render(tz);
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

    let src = SessionRenderContext {
      ctoken,
      gen : ig.gs.gen,
      log,
      table_size : ig.gs.table_size,
      player,
      defs : alldefs,
      uses,
      nick : gpl.nick.clone(),
      sse_url_prefix,
      ptoken: form.ptoken.clone(),
      load : serde_json::to_string(&DataLoad {
        players : load_players,
      }).map_err(|e| InternalError::JSONEncode(e))?,
    };
    trace!("SessionRenderContext {:?}", &src);
    (src, client)
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
