
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
    let pl = ig.gs.players.byid_mut(player)?;
    let cl = Client { player };
    let client = ig.clients.insert(cl);

    let ciad = InstanceAccessDetails {
      g : iad.g.clone(),
      ident : client,
    };
    let ctoken = record_token(ciad);

    let mut uses = vec![];
    let mut alldefs = vec![];

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
    };
    eprintln!("SRC {:?}", &src);
    src
  };
  Ok(Template::render("session",&c))
}

pub fn mount(_rocket_instance: &mut Rocket) {
  //rocket_instance.mount(&session);  xxx
}
