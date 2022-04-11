// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use crate::prelude::*;

pub type VisiblePieceAngle = PieceAngle;

#[derive(Clone,Debug)]
#[derive(Serialize,Deserialize)]
#[serde(transparent)]
pub struct VisibleAngleTransform(Html);

const DEFKEY_FLIP: UoKey = 'f';

#[derive(Debug,Clone)]
pub struct PieceRenderInstructions {
  pub vpid: VisiblePieceId,
  pub occulted: PriOcculted,
}
deref_to_field!{PieceRenderInstructions, PriOcculted, occulted}

#[derive(Debug,Clone)]
pub enum PriOccultedGeneral<P,Z> {
  Visible(ShowUnocculted),
  Occulted,
  Displaced(P, Z),
}

pub type PriOcculted = PriOccultedGeneral<Pos, ZLevel>;

impl VisiblePieceAngle {
  pub fn to_transform(self) -> VisibleAngleTransform {
    VisibleAngleTransform(Html::from_html_string(
      base_misc::raw_angle_transform(
        self.to_compass().into()
      )))
  }

  pub fn to_compass(self) -> CompassAngle {
    match self {
      PieceAngle::Compass(compass) => compass,
    }
  }
}

impl<P,Z> PriOccultedGeneral<P,Z> {
  #[throws(IE)]
  pub fn instead<'p>(&self, ioccults: &'p IOccults, p: &'p IPiece)
                 -> Either<ShowUnocculted, /*occulted*/ &'p dyn InertPieceTrait>
  {
    p.show_or_instead(ioccults, self.fully_visible())?
  }

  pub fn fully_visible(&self) -> Option<ShowUnocculted> {
    use PriOG::*;
    match self {
      Visible(v) => Some(*v),
      Occulted | Displaced(..) => None,
    }
  }

  pub fn describe(&self, ioccults: &IOccults, goccults: &GameOccults,
                  gpc: &GPiece, ipc: &IPiece) -> Html
  {
    self.describe_fallible(ioccults, goccults, gpc, ipc)
      .unwrap_or_else(|e| {
        error!("error describing piece: {:?}", e);
        "<internal error describing piece>".to_html()
      })
  }

  #[throws(IE)]
  pub fn describe_fallible(&self, ioccults: &IOccults,
                           goccults: &GameOccults,
                           gpc: &GPiece, ipc: &IPiece) -> Html {
    match self.instead(ioccults, ipc)? {
      Left(y) => ipc.show(y).describe_html(gpc, goccults)?,
      Right(i) => i.describe_html()?,
    }
  }
}

impl PieceRenderInstructions {
  #[throws(IE)]
  pub fn map_piece_update_op(&self, ioccults: &IOccults, gs: &GameState,
                             gpc: &GPiece, ipc: &IPiece,
                             op: PieceUpdateOp<(),()>
  ) -> Option<PieceUpdateOp<PreparedPieceState, ZLevel>>
  {
    use PieceUpdateOp::*;
    use PriOG::*;
    if matches_doesnot!(
      op,
      = Move(_) | MoveQuiet(_) | SetZLevel(_) | SetZLevelQuiet(_),
      ! Delete() | Insert(_) | Modify(_) | ModifyQuiet(_),
    ) {
      match self.occulted {
        Visible(_) | Occulted => (),
        Displaced(..) => return None,
      }
    }

    let op = op.try_map(
      |()|{
        let ns = self.prep_piecestate(ioccults, gs, gpc, ipc)?;
        <Result<_,InternalError>>::Ok(ns)
      },
      |()|{
        Ok(gpc.zlevel.clone())
      }
    )?;
    Some(op)
  }
  
  #[throws(IE)]
  pub fn prep_piecestate(&self, ioccults: &IOccults, gs: &GameState,
                         gpc: &GPiece, ipc: &IPiece)
                         -> PreparedPieceState {
    let pri = self;
    let (pos, zlevel) = pri.pos_zlevel(gpc);
    let occregion = gpc.occult.active_region(&gs.occults)?
      .map(|r| JsonString(r.clone()));
    let (svg, bbox) = pri.make_svg_defs(ioccults, gs, gpc, ipc)?;
    let r = PreparedPieceState {
      pos, svg, occregion, bbox,
      held       : gpc.held,
      z          : zlevel.z.clone(),
      zg         : zlevel.zg,
      angle      : pri.angle(gpc).to_compass(),
      pinned     : gpc.pinned,
      rotateable : gpc.rotateable(),
      uos        : pri.ui_operations(gs, gpc, ipc)?,
      moveable   : gpc.moveable(),
      facehint   : pri.facehint(gpc),
    };
    dbgc!(pri, ipc, gpc, r);
    r
  }

  #[throws(IE)]
  pub fn prep_pieceimage(&self, ioccults: &IOccults, gs: &GameState,
                         gpc: &GPiece, ipc: &IPiece)
                         -> PreparedPieceImage {
    let pri = self;
    let (svg, bbox) = pri.make_svg_defs(ioccults, gs, gpc, ipc)?;
    let r = PreparedPieceImage {
      svg, bbox,
      uos: pri.ui_operations(gs, gpc, ipc)?,
    };
    dbgc!(pri, ipc, gpc, r);
    r
  }

  pub fn angle(&self, gpc: &GPiece) -> VisiblePieceAngle {
    match self.occulted {
      PriOcculted::Visible(_)                            => gpc.angle,
      PriOcculted::Occulted | PriOcculted::Displaced(..) => default(),
    }
  }

  pub fn facehint(&self, gpc: &GPiece) -> Option<FaceId> {
    match self.occulted {
      PriOcculted::Visible(_)                            => Some(gpc.face),
      PriOcculted::Occulted | PriOcculted::Displaced(..) => None,
    }
  }

  pub fn pos_zlevel<'r>(&'r self, gpc: &'r GPiece) -> (Pos, &'r ZLevel) {
    use PriOcculted as PO;
    match &self.occulted {
      PO::Visible(_) | PO::Occulted => (gpc.pos, &gpc.zlevel),
      PO::Displaced(pos, zlevel) => (*pos, zlevel),
    }
  }

  #[throws(IE)]
  pub fn make_svg_defs(&self, ioccults: &IOccults, gs: &GameState,
                       gpc: &GPiece, ipc: &IPiece) -> (Html, Rect)
  {
    let pri = self;
    let instead = pri.instead(ioccults, ipc)?;

    let o: &dyn OutlineTrait = match instead {
      Left(y) => Borrow::<dyn PieceTrait>::borrow(ipc.show(y)).dyn_upcast(),
      Right(i) => i.dyn_upcast(),
    };

    let angle = pri.angle(gpc);
    let bbox = o.bbox_approx()?;

    let dragraise = match o.thresh_dragraise()? {
      Some(n) if n < 0 => throw!(SvgE::NegativeDragraise),
      Some(n) => n,
      None => -1,
    };

    let transform = angle.to_transform();

    let mut defs = Html::new();
    hwrite!(&mut defs,
           r##"<g id="piece{}" transform="{}" data-dragraise="{}">"##,
           pri.vpid, &transform.0, dragraise)?;

    match instead {
      Left(y) => {
        ipc.show(y).svg_piece(&mut defs, gpc, gs, pri.vpid)?;
      },
      Right(i) => {
        i.svg(&mut defs, pri.vpid)?;
      },
    };

    hwrite!(&mut defs, r##"</g>"##)?;
    hwrite!(&mut defs,
           r##"<path id="surround{}" d="{}"/>"##,
           pri.vpid, o.surround_path()?)?;
    (defs, bbox)
  }

  #[throws(InternalError)]
  pub fn ui_operations(&self, gs: &GameState, gpc: &GPiece, ipc: &IPiece)
                       -> Vec<UoDescription>
  {
    let y = match self.occulted {
      PriOcculted::Visible(y)                            => y,
      PriOcculted::Occulted | PriOcculted::Displaced(..) => return vec![],
    };

    type WRC = WhatResponseToClientOp;

    let mut out = vec![];
    if ipc.show(y).nfaces() > 1 {
      out.push(UoDescription {
        wrc: WRC::UpdateSvg,
        kind: UoKind::Global,
        def_key: DEFKEY_FLIP,
        opname: "flip".to_string(),
        desc: Html::lit("flip").into(),
      })
    }
    ipc.show(y).add_ui_operations(y, &mut out, gs, gpc)?;
    out
  }
}
