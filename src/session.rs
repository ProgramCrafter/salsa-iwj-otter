
use crate::imports::*;
use crate::http::*;

#[derive(Serialize,Debug)]
struct SessionRenderContext {
  ctoken : String,
  player : PlayerId,
  gen : Generation,
  uses : Vec<SessionPieceContext>,
  defs : Vec<(VisiblePieceId,String)>,
  nick : String,
  load : String,
}

#[derive(Serialize,Debug)]
struct SessionPieceContext {
  id: VisiblePieceId,
  pos: Pos,
  info: String,
}

#[derive(Serialize,Debug)]
struct SessionPieceLoadJson<'r> {
  held : &'r Option<PlayerId>,
  z : ZCoord,
  zg : Generation,
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
  ptoken : String,
}
#[post("/_/session", format="json", data="<form>")]
fn session(form : Json<SessionForm>) -> Result<Template,OE> {
  // make session in this game, log a message to other players
  let iad = lookup_token(&form.ptoken)?;
  let player = iad.ident;
  let c = {
    let mut ig = iad.g.lock()?;
    let ig = &mut *ig;
    let cl = Client { player };
    let client = ig.clients.insert(cl);

    let ciad = InstanceAccessDetails {
      g : iad.g.clone(),
      ident : client,
    };
    let ctoken = record_token(ciad);

    let mut uses = vec![];
    let mut alldefs = vec![];

    let mut load_players = HashMap::new();
    for (player, _pl) in &ig.gs.players {
      let dasharray = String::new();

      load_players.insert(player, DataLoadPlayer {
        dasharray,
      });
    }

    let pl = ig.gs.players.byid_mut(player)?;
    let mut pieces : Vec<_> = ig.gs.pieces.iter().collect();

    pieces.sort_by_key(|(_,pr)| &pr.zlevel);

    for (gpid, pr) in pieces {
      let pri = PieceRenderInstructions {
        id : make_pieceid_visible(gpid),
        face : pr.face,
      };
      let defs = pr.make_defs(&pri);
      alldefs.push((pri.id, defs));

      let for_info = SessionPieceLoadJson {
        held : &pr.held,
        z  : pr.zlevel.z,
        zg : pr.zlevel.zg,
      };

      let for_piece = SessionPieceContext {
        id: pri.id,
        pos : pr.pos,
        info : serde_json::to_string(&for_info)?,
      };
      uses.push(for_piece);
    }

    let src = SessionRenderContext {
      ctoken : ctoken.0,
      gen : ig.gs.gen,
      player,
      defs : alldefs,
      uses,
      nick : pl.nick.clone(),
      load : serde_json::to_string(&DataLoad {
        players : load_players,
      })?,
    };
    eprintln!("SRC {:?}", &src);
    src
  };
  Ok(Template::render("session",&c))
}

pub fn mount(rocket_instance: Rocket) -> Rocket {
  return rocket_instance.mount("/", routes![
    session,
  ]);
}
