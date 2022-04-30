// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

pub use super::*;

#[derive(Clone,Debug)]
pub struct InstanceAccess<Id> {
  pub raw_token: RawToken,
  pub i: InstanceAccessDetails<Id>,
}

impl<Id> FromStr for InstanceAccess<Id>
  where Id: AccessId, Fatal: From<Id::Error>
{
  type Err = FER;
  #[throws(FER)]
  fn from_str(s: &str) -> Self {
    let raw_token = RawToken(s.to_owned());
    let i = InstanceAccessDetails::from_token(raw_token.borrow())?;
    InstanceAccess { raw_token, i }
  }
}

#[derive(Debug,Serialize,Deserialize)]
struct ApiPiece<O:op::Complex> {
  ctoken: RawToken,
  piece: VisiblePieceId,
  gen: Generation,
  cseq: ClientSequence,
  #[serde(default)] loose: bool,
  op: O,
}

#[derive(Debug)]
pub struct ContinueDespiteConflict;

mod op {
  use super::*;

  pub trait Core: Debug { 
    #[throws(Inapplicable)]
    fn check_held(&self, pc: &GPiece, player: PlayerId) {
      if pc.held != None && pc.held != Some(player) {
        throw!(Ia::PieceHeld)
      }
    }

    fn conflict_loose_check(&self, _gpc: &GPiece, _client: ClientId)
        -> Result<ContinueDespiteConflict, ApiPieceOpError> {
      throw!(Fatal::BadLoose)
    }
  }

  pub trait Simple: Debug { 
    #[throws(ApiPieceOpError)]
    fn op(&self, a: ApiPieceOpArgs) -> PieceUpdate;
  }
 
  pub trait Complex: Core + Debug { 
    #[throws(ApiPieceOpError)]
    fn op_complex(&self, a: ApiPieceOpArgs) -> OpOutcomeThunk;
  }

  impl<T> Complex for T where T: Core + Simple {
    #[throws(ApiPieceOpError)]
    fn op_complex(&self, a: ApiPieceOpArgs) -> OpOutcomeThunk {
      (self.op(a)?, default()).into()
    }
  }
}

#[derive(Error,Debug)]
#[error("{0}")]
pub struct FatalErrorResponse(#[from] Fatal);

impl ResponseError for FatalErrorResponse {
  fn status_code(&self) -> StatusCode {
    use Fatal::*;
    match self.0 {
      ServerFailure(_)
        => StatusCode::INTERNAL_SERVER_ERROR,
      NoClient | NoPlayer(_) | GameBeingDestroyed(_)
        => StatusCode::NOT_FOUND,
      BadJSON(_) | BadLoose
        => StatusCode::BAD_REQUEST,
    }
  }

  fn error_response(&self) -> HttpResponse<BoxBody> { error_response(self) }
}

#[throws(Fatal)]
fn api_piece_op<O: op::Complex>(form: Json<ApiPiece<O>>)
                   -> impl Responder {
//  thread::sleep(Duration::from_millis(2000));
  let iad = lookup_token(form.ctoken.borrow())?;
  let client = iad.ident;
  let mut ig = iad.gref.lock()?;
  ig.save_game_later();

  let (ok, unprepared_outer) = ToRecalculate::with(|mut to_recalculate| {
    let r = (||{

  let g = &mut *ig;
  let cl = &mut g.clients.byid_mut(client)?;
  // ^ can only fail if we raced
  cl.lastseen = Instant::now();
  let player = cl.player;
  let gs = &mut g.gs;
  let ioccults = &g.ioccults;
  let ipieces = &g.ipieces;
  let iplayers = &g.iplayers;
  let _ = iplayers.byid(player)?;
  let gpl = gs.players.byid(player)?;
  let piece = vpiece_decode(gs, player, gpl, form.piece);
  if_let!{ Some(piece) = piece; else return Ok(()) }
  let was_held = gs.pieces.get(piece).as_ref().map(|gpc| gpc.held);

  match (||{
    let ipc = ipieces.get(piece).ok_or(Ia::PieceGone)?;
    let gpc = gs.pieces.byid_mut(piece)?;

    let q_gen = form.gen;
    let u_gen =
      if client == gpc.lastclient { gpc.gen_before_lastclient }
      else { gpc.gen };

    debug!("client={:?} pc.lastclient={:?} pc.gen_before={:?} pc.gen={:?} q_gen={:?} u_gen={:?} form={:?}", &client, &gpc.lastclient, &gpc.gen_before_lastclient, &gpc.gen, &q_gen, &u_gen, &form);

    let loose_conflict = if u_gen <= q_gen { None } else {
      if ! form.loose { throw!(Inapplicable::Conflict); }
      Some(form.op.conflict_loose_check(gpc, client)?)
    };
    trace_dbg!("form.op", player, piece, &form.op, &gpc);
    form.op.check_held(gpc,player)?;
    let update =
      form.op.op_complex(ApiPieceOpArgs {
        ioccults, gs, player, piece, ipieces, ipc, client,
        cseq: form.cseq,
        ig: &iad.gref,
        to_recalculate: &mut to_recalculate,
      })?;
    Ok::<_,ApiPieceOpError>((update, loose_conflict))
  })().and_then(|(thunk, loose_conflict)| Ok((
    match thunk {
      OpOutcomeThunk::Immediate(r) => r,
      OpOutcomeThunk::Reborrow(f) => f(&mut ig, player, piece)?,
    }, loose_conflict
  ))) {
    Err(APOE::Inapplicable(poe)) => {
      PrepareUpdatesBuffer::piece_report_error(
        &mut ig, poe,
        piece, vec![], POEPP::Unprocessed, client, form.cseq,
      )?;
      debug!("api_piece_op Err(RVU): {:?}", &form);
    },
    Err(APOE::PartiallyProcessed(poe, logents)) => {
      PrepareUpdatesBuffer::piece_report_error(
        &mut ig, poe,
        piece, logents, POEPP::Partially, client, form.cseq,
      )?;
      debug!("api_piece_op Err(PP): {:?}", &form);
    },
    Err(APOE::Fatal(err)) => {
      warn!("api_piece_op ERROR {:?}: {:?}", &form, &err);
      Err(err)?;
    },
    Ok(((PieceUpdate { wrc, log, ops }, unprepared), loose_conflict)) => {
      let by_client =
        if let Some(ContinueDespiteConflict) = loose_conflict {
          None
        } else {
          Some((wrc, client, form.cseq))
        };
      let mut buf = PrepareUpdatesBuffer::new(&mut ig,
                                              Some(1 + log.len()));

      buf.piece_update(piece, &by_client, ops);
      buf.log_updates(log);
      buf.add_unprepared(unprepared);

      debug!("api_piece_op OK: {:?}", &form);
    }
  };

  if_chain! {
    let g = &mut *ig;
    if let Some(was_held) = was_held;
    if let Some(gpc) = g.gs.pieces.get_mut(piece);
    if gpc.held != was_held;
    if let Some(ipc) = &g.ipieces.get(piece);
    if let Ok(unprepared) = ipc.direct_trait_access().held_change_hook(
      &iad.gref,
      &mut g.gs.pieces,
      piece,
      was_held,
    ).map_err(|e| error!("internal error on change hook: {:?}", e));
    then {
      PrepareUpdatesBuffer::only_unprepared(&mut ig, unprepared);
    }
  }

      Ok::<(),Fatal>(())
    })();

    let g = &mut *ig;
    let gs = &mut g.gs;

    (r, to_recalculate.implement(&mut gs.players,
                             &mut gs.pieces,
                             &mut gs.occults,
                             &g.ipieces))
  });

  PrepareUpdatesBuffer::only_unprepared(&mut ig, unprepared_outer);

  ok?;
  ""
}

macro_rules! api_route_core {
  { $fn:ident, $path:expr, $form:ident, $formdef:item,
    $( impl $trait:path as { $($impl:tt)* } )*
    $( as: $($simple_impl:tt)* )?
  } => {
    #[derive(Debug,Serialize,Deserialize)]
    $formdef

    #[post($path)]
    #[throws(FER)]
    async fn $fn(form: Json<ApiPiece<$form>>)
           -> impl Responder {
      api_piece_op(form)?
    }

    $(
      impl $trait for $form { $($impl)* }
    )*
    $(
      impl op::Core for $form { }
      impl op::Simple for $form { $($simple_impl)* }
    )?
  }
}

macro_rules! api_route {
  { $fn:ident, $path:expr,
    $( #[ $attrs:meta ] )* struct $form:ident { $( $body:tt )* }
    $( $impl:tt )*
  } => {
    api_route_core!{
      $fn, $path, $form,
      $( #[ $attrs ] )* struct $form { $( $body )* },
      $( $impl )*
    }
  };
  { $fn:ident, $path:expr,
    $( #[ $attrs:meta ] )* struct $form:ident ( $( $body:tt )* );
    $( $impl:tt )*
  } => {
    api_route_core!{
      $fn, $path, $form,
      $( #[ $attrs ] )* struct $form ( $( $body )* );,
      $( $impl )*
    }
  }
}

api_route!{
  api_grab, "/_/api/grab",
  struct ApiPieceGrab {
  }

  impl op::Core as {
    #[throws(ApiPieceOpError)]
    fn conflict_loose_check(&self, gpc: &GPiece, client: ClientId)
                            -> ContinueDespiteConflict {
      if gpc.occult.is_active()             { throw!(Ia::Occultation    ) }
      if gpc.moveable != PieceMoveable::Yes { throw!(Ia::PieceImmoveable) }
      if gpc.last_released != client        { throw!(Ia::PieceHeld      ) }
      ContinueDespiteConflict
    }
  }

  impl op::Simple as {
    #[throws(ApiPieceOpError)]
    fn op(&self, a: ApiPieceOpArgs) -> PieceUpdate {
      let ApiPieceOpArgs { gs,ioccults,player,piece,ipc, .. } = a;
      let gpl = gs.players.byid_mut(player)?;
      let gpc = gs.pieces.byid_mut(piece)?;

      let logents = log_did_to_piece(
        ioccults,&gs.occults, gpl,gpc,ipc,
        "grasped"
      )?;

      if gpc.held.is_some() { throw!(Ia::PieceHeld) }
      gpc.held = Some(player);
    
      let update = PieceUpdateOp::ModifyQuiet(());

      (WhatResponseToClientOp::Predictable,
       update, logents).into()
    }
  }
}

api_route!{
  api_wrest, "/_/api/wrest",
  struct ApiPieceWrest {
  }

  impl op::Core as {
    #[throws(Ia)]
    fn check_held(&self, _pc: &GPiece, _player: PlayerId) { }
  }

  impl op::Simple as {
    #[throws(ApiPieceOpError)]
    fn op(&self, mut a: ApiPieceOpArgs) -> PieceUpdate {
      let pri = a.pri()?;
      let ApiPieceOpArgs { gs,ioccults,player,piece,ipc, .. } = a;
      let gpc = gs.pieces.byid_mut(piece)?;
      let players = &mut gs.players;
      let was = gpc.held;
      let was = was.and_then(|p| players.get(p));
      let was = was.map(|was| htmlescape::encode_minimal(&was.nick));

      let gpl = players.byid_mut(player)?;

      let pcs = pri.describe(ioccults,&gs.occults, gpc, ipc);

      gpc.held = Some(player);
      gpc.last_released = default();

      let update = PieceUpdateOp::Modify(());

      let pls = &htmlescape::encode_minimal(&gpl.nick);

      let logent = LogEntry { html: match was {
        Some(was) => hformat!("{} wrested {} from {}", pls, pcs, was),
        None => hformat!("{} wrested {}", pls, pcs),
      }};

      (WhatResponseToClientOp::Predictable,
       update, vec![logent]).into()
    }
  }
}

api_route!{
  api_ungrab, "/_/api/ungrab",
  struct ApiPieceUngrab {
    #[serde(default)] autoraise: bool,
  }

  as:
  #[throws(ApiPieceOpError)]
  fn op(&self, a: ApiPieceOpArgs) -> PieceUpdate {
    let ApiPieceOpArgs {
      gs,ioccults,player,piece,ipc,ipieces,to_recalculate, ..
    } = a;
    let gpl = gs.players.byid_mut(player).unwrap();

    let new_z = if_chain! {

      if self.autoraise;
      let tgpc = gs.pieces.byid(piece)?;
      if gs.max_z > tgpc.zlevel;

      let use_region = |rpiece: PieceId, rgpc: &GPiece| if_chain!{
        if let Some(rvis) = rgpc.fully_visible_to_everyone();
        if let Some(ripc) = wants!( ipieces.get(rpiece), ?rpiece );
        if let Some(rregion) = wantok!( ripc.show(rvis).abs_bbox(rgpc) );
        then { Some(rregion) }
        else { None }
      };

      if gs.pieces.iter().any(|(opiece, ogpc)| {

        if ogpc.zlevel < tgpc.zlevel { return false }

        let cannot_overlap = if_chain! {
          if let Some(tregion) = use_region( piece, tgpc);
          if let Some(oregion) = use_region(opiece, ogpc);
          if ! tregion.overlaps(&oregion);
          then { true }
          else { false }
        };
        return ! cannot_overlap;

      });

      then {
        let z = gs.max_z.z.clone_mut().increment().map_err(
          |e| APOE::Fatal(IE::from(e).into()))?;
        Some(ZLevel { z, zg: gs.gen })
      }
      else { None }

    };

    let gpc = gs.pieces.byid_mut(piece).unwrap();

    let (logents, who_by) = log_did_to_piece_whoby(
      ioccults,&gs.occults,gpl,gpc,ipc,
      "released"
    )?;
    let who_by = who_by.ok_or(Ia::PieceGone)?;

    if gpc.held != Some(player) { throw!(Ia::PieceNotHeld) }
    gpc.held = None;
    gpc.last_released = a.client;

    let wrc = if let Some(zlevel) = new_z {
      gpc.zlevel = zlevel;
      WhatResponseToClientOp::Unpredictable
    } else {
      WhatResponseToClientOp::Predictable
    };

    let update = PieceUpdateOp::Modify(());

    let vanilla = (wrc, update, logents);
      
    if let Some(occid) = gpc.occult.passive_occid() {
      // if piece is occulted, definitely repermute its occultation
      // so that we don't leak which piece is which over repeated
      // adjustment clicks
      to_recalculate.mark_dirty(occid);
    };

    let update=
      recalculate_occultation_piece(
        gs,
        who_by,
        ipieces,
        ioccults,
        to_recalculate,
        piece,
        vanilla,
      ).map_err(|e| Fatal::from(e))?;

    update
  }
}

api_route!{
  api_setz, "/_/api/setz",
  struct ApiPieceSetZ {
    z: ZCoord,
  }

  as:
  #[throws(ApiPieceOpError)]
  fn op(&self, a: ApiPieceOpArgs) -> PieceUpdate {
    let ApiPieceOpArgs { gs,piece, .. } = a;
    let gpc = gs.pieces.byid_mut(piece)?;
    if gpc.occult.is_active() {
      if self.z >= gpc.zlevel.z { throw!(Ia::Occultation) }
    }
    gpc.zlevel = ZLevel { z: self.z.clone(), zg: gs.gen };
    let update = PieceUpdateOp::SetZLevel(());
    (WhatResponseToClientOp::Predictable,
     update, vec![]).into()
  }
}

api_route!{
  api_move, "/_/api/m",
  struct ApiPieceMove(Pos);

  impl op::Core as {
    #[throws(Ia)]
    fn check_held(&self, gpc: &GPiece, player: PlayerId) {
      // This will ensure that occultations are (in general) properly
      // updated, because the player will (have to) release the thing
      // again
      if gpc.held != Some(player) { throw!(Ia::PieceNotHeld) }
      if gpc.occult.is_active() { throw!(Ia::Occultation) }
      if matches_doesnot!(
        gpc.moveable(),
        = PieceMoveable::No,
        ! PieceMoveable::Yes | PieceMoveable::IfWresting,
      ) { throw!(Ia::PieceImmoveable) }
    }
  }

  impl op::Simple as {
    #[throws(ApiPieceOpError)]
    fn op(&self, a: ApiPieceOpArgs) -> PieceUpdate {
      let ApiPieceOpArgs { gs,piece, .. } = a;
      let gpc = gs.pieces.byid_mut(piece).unwrap();
      let logents = vec![];
      match self.0.clamped(gs.table_size) {
        Ok(pos) => gpc.pos = pos,
        Err(pote) => {
          gpc.pos = pote.clamped;
          throw!(ApiPieceOpError::PartiallyProcessed(
            Inapplicable::PosOffTable,
            logents,
          ));
        }
      };
      let update = PieceUpdateOp::Move(self.0);
      (WhatResponseToClientOp::Predictable,
       update, logents).into()
    }
  }
}

api_route!{
  api_rotate, "/_/api/rotate",
  struct ApiPieceRotate(CompassAngle);

  as:
  #[throws(ApiPieceOpError)]
  fn op(&self, a: ApiPieceOpArgs) -> PieceUpdate {
    let ApiPieceOpArgs { gs,ioccults,player,piece,ipc, .. } = a;
    let gpc = gs.pieces.byid_mut(piece).unwrap();
    if ! gpc.rotateable() || gpc.occult.is_active() {
      throw!(Ia::PieceUnrotateable)
    }
    let gpl = gs.players.byid_mut(player).unwrap();
    let logents = log_did_to_piece(
      ioccults,&gs.occults,gpl,gpc,ipc,
      "rotated"
    )?;
    gpc.angle = PieceAngle::Compass(self.0);
    let update = PieceUpdateOp::Modify(());
    (WhatResponseToClientOp::Predictable,
     update, logents).into()
  }
}

api_route!{
  api_pin, "/_/api/pin",
  struct ApiPiecePin (bool);

  as:
  #[throws(ApiPieceOpError)]
  fn op(&self, a: ApiPieceOpArgs) -> PieceUpdate {
    let ApiPieceOpArgs { gs,ioccults,player,piece,ipc, .. } = a;
    let ops = PUOs_Simple_Modify;
    let new_z = piece_make_heavy(&gs.pieces, piece)?;
    let gpc = gs.pieces.byid_mut(piece).unwrap();
    let gpl = gs.players.byid_mut(player).unwrap();
    let log = log_did_to_piece(
      ioccults,&gs.occults,gpl,gpc,ipc,
      if gpc.pinned { "pinned" } else { "unpinned" },
    )?;
    gpc.forbid_involved_in_occultation()?;
    gpc.pinned = self.0;
    gpc.zlevel.z = new_z;
    PieceUpdate {
      wrc: WhatResponseToClientOp::Predictable,
      ops: ops.into(),
      log,
    }
  }
}

api_route!{
  api_multigrab, "/_/api/multigrab",
  struct ApiPieceMultigrab {
    n: MultigrabQty,
  }

  impl op::Core as { }
  impl op::Complex as {
    #[throws(ApiPieceOpError)]
    fn op_complex(&self, mut a: ApiPieceOpArgs) -> OpOutcomeThunk {
      if ! a.ipc.special.multigrab { throw!(Ia::BadPieceStateForOperation) }
      let pri = a.pri()?;
      let y = pri.fully_visible().ok_or(Ia::Occultation)?;
      let gpc = a.gs.pieces.byid_mut(a.piece)?;
      if gpc.held != None { throw!(Ia::PieceHeld) }
      a.ipc.show(y).op_multigrab(a, pri, self.n)?
    }
  }
}

api_route!{
  api_uo, "/_/api/k",
  struct ApiPieceUo {
    opname: String,
    wrc: WhatResponseToClientOp,
  }

  impl op::Core as { }
  impl op::Complex as {
    #[throws(ApiPieceOpError)]
    fn op_complex(&self, mut a: ApiPieceOpArgs) -> OpOutcomeThunk {
      let pri = a.pri()?;
      let ApiPieceOpArgs { ioccults,player,piece,ipc, .. } = a;
      let gs = &mut a.gs;
      let y = pri.fully_visible().ok_or(Ia::Occultation)?;

      '_normal_global_ops__not_loop: loop {
        let gpc = gs.pieces.byid_mut(piece)?;
        let gpl = gs.players.byid_mut(player)?;
        let _: Void = match (self.opname.as_str(), self.wrc) {

          ("flip", wrc@ WRC::UpdateSvg)
            if ipc.show(y).ui_permit_flip(gpc)?
            =>
          {
            let nfaces = ipc.show(y).nfaces();
            gpc.face = ((RawFaceId::from(gpc.face) + 1) % nfaces).into();
            // todo: name the most visible aspect in the log ?
            let logents = log_did_to_piece(
              ioccults,&gs.occults,gpl,gpc,ipc,
              "flipped"
            )?;
            return ((
              wrc,
              PieceUpdateOp::Modify(()),
              logents,
            ).into(), default()).into()
          },

          _ => break,
        };
      }

      '_abnormal_global_ops__notloop: loop {
        let _: Void = match self {

          _ => break,
        };
      }

      ipc.show(y).ui_operation(y, a, &self.opname, self.wrc)?
    }
  }
}

pub fn routes() -> impl HttpServiceFactory {
  services![
    api_grab,
    api_ungrab,
    api_setz,
    api_move,
    api_rotate,
    api_wrest,
    api_pin,
    api_uo,
    api_multigrab,
  ]
}
