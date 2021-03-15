// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use crate::prelude::*;

pub type VisiblePieceAngle = PieceAngle;

#[derive(Clone,Debug)]
#[derive(Serialize,Deserialize)]
#[serde(transparent)]
pub struct VisibleAngleTransform(String);

#[derive(Debug,Clone)]
pub struct PieceRenderInstructions {
  pub vpid: VisiblePieceId,
  pub occulted: PriOcculted,
}

#[derive(Debug,Clone)]
pub enum PriOcculted {
  Visible(ShowUnocculted),
  Occulted,
  Displaced(Pos, ZLevel),
}

impl VisiblePieceAngle {
  pub fn to_transform(self) -> VisibleAngleTransform {
    VisibleAngleTransform(base_misc::raw_angle_transform(
      self.to_compass().into()
    ))
  }

  pub fn to_compass(self) -> CompassAngle {
    match self {
      PieceAngle::Compass(compass) => compass,
    }
  }
}

impl PieceRenderInstructions {
  #[throws(IE)]
  pub fn map_piece_update_op(&self, ioccults: &IOccults,
                             gpc: &GPiece, ipc: &IPiece,
                             op: PieceUpdateOp<(),()>
  ) -> Option<PieceUpdateOp<PreparedPieceState, ZLevel>>
  {
    use PieceUpdateOp::*;
    use PriOcculted::*;
    if matches_doesnot!(
      op,
      = Move(_) | SetZLevel(_),
      ! Delete() | Insert(_) | Modify(_) | ModifyQuiet(_),
    ) {
      match self.occulted {
        Visible(_) | Occulted => (),
        Displaced(..) => return None,
      }
    }

    let op = op.try_map(
      |()|{
        let ns = self.prep_piecestate(ioccults, gpc, ipc)?;
        <Result<_,InternalError>>::Ok(ns)
      },
      |()|{
        Ok(gpc.zlevel.clone())
      }
    )?;
    Some(op)
  }
  
  #[throws(IE)]
  pub fn prep_piecestate(&self, ioccults: &IOccults,
                         gpc: &GPiece, ipc: &IPiece)
                         -> PreparedPieceState {
    let pri = self;
    let (pos, zlevel) = pri.pos_zlevel(gpc);
    dbgc!(pos, pri, gpc, ioccults, ipc);
    PreparedPieceState {
      pos        : pos,
      held       : gpc.held,
      svg        : pri.make_defs(ioccults, gpc, ipc)?,
      z          : zlevel.z.clone(),
      zg         : zlevel.zg,
      angle      : pri.angle(gpc).to_compass(),
      pinned     : gpc.pinned,
      uos        : pri.ui_operations(gpc, ipc.p.borrow())?,
    }
  }

  #[throws(IE)]
  fn instead<'p>(&self, ioccults: &'p IOccults, p: &'p IPiece)
                 -> Either<ShowUnocculted, &'p dyn OccultedPieceTrait>
  {
    match self.occulted {
      PriOcculted::Visible(v) => Left(v),
      PriOcculted::Occulted | PriOcculted::Displaced(..) => {
        Right({
          let occilk = p.occilk.as_ref()
            .ok_or_else(|| internal_logic_error(format!(
              "occulted non-occultable {:?}", p)))?
            .borrow();
          let occ_data = ioccults.ilks.get(occilk)
            .ok_or_else(|| internal_logic_error(format!(
              "occulted ilk vanished {:?} {:?}", p, occilk)))?;
          occ_data.p_occ.as_ref()
        })
      },
    }
  }

  pub fn angle(&self, gpc: &GPiece) -> VisiblePieceAngle {
    match self.occulted {
      PriOcculted::Visible(_)                            => gpc.angle,
      PriOcculted::Occulted | PriOcculted::Displaced(..) => default(),
    }
  }

  pub fn pos_zlevel<'r>(&'r self, gpc: &'r GPiece) -> (Pos, &'r ZLevel) {
    use PriOcculted as PO;
    match &self.occulted {
      PO::Visible(_) | PO::Occulted => (gpc.pos, &gpc.zlevel),
      PO::Displaced(pos, zlevel) => (*pos, &zlevel),
    }
  }

  #[throws(IE)]
  pub fn make_defs<'p>(&self, ioccults: &IOccults,
                         gpc: &GPiece, ipc: &IPiece) -> Html
  {
    let pri = self;
    let instead = pri.instead(ioccults, ipc)?;

    let o: &dyn OutlineTrait = match instead {
      Left(_) => Borrow::<dyn PieceTrait>::borrow(&ipc.p).dyn_upcast(),
      Right(i) => i.dyn_upcast(),
    };

    let angle = pri.angle(gpc);

    let dragraise = match o.thresh_dragraise()? {
      Some(n) if n < 0 => throw!(SvgE::NegativeDragraise),
      Some(n) => n,
      None => -1,
    };

    let transform = angle.to_transform();

    let mut defs = Html(String::new());
    write!(&mut defs.0,
           r##"<g id="piece{}" transform="{}" data-dragraise="{}">"##,
           pri.vpid, &transform.0, dragraise)?;

    match instead {
      Left(y) => {
        ipc.p.svg_piece(&mut defs, gpc, pri.vpid, y)?;
      },
      Right(i) => {
        i.svg(&mut defs, pri.vpid)?;
      },
    };

    write!(&mut defs.0, r##"</g>"##)?;
    write!(&mut defs.0,
           r##"<path id="surround{}" d="{}"/>"##,
           pri.vpid, o.surround_path()?.0)?;
    defs
  }

  pub fn describe(&self, ioccults: &IOccults,
                  gpc: &GPiece, ipc: &IPiece) -> Html
  {
    self.describe_fallible(ioccults, gpc, ipc)
      .unwrap_or_else(|e| {
        error!("error describing piece: {:?}", e);
        Html::lit("<internal error describing piece>")
      })
  }

  #[throws(IE)]
  pub fn describe_fallible(&self, ioccults: &IOccults,
                           gpc: &GPiece, ipc: &IPiece) -> Html {
    match self.instead(ioccults, ipc)? {
      Left(_y) => ipc.p.describe_html(gpc)?,
      Right(i) => i.describe_html()?,
    }
  }

  #[throws(InternalError)]
  pub fn ui_operations(&self, gpc: &GPiece, p: &dyn PieceTrait)
                   -> Vec<UoDescription>
  {
    match self.occulted {
      PriOcculted::Visible(_)                            => (),
      PriOcculted::Occulted | PriOcculted::Displaced(..) => return vec![],
    };

    type WRC = WhatResponseToClientOp;

    let mut out = vec![];
    if p.nfaces() > 1 {
      out.push(UoDescription {
        wrc: WRC::UpdateSvg,
        kind: UoKind::Global,
        def_key: 'f'.into(),
        opname: "flip".to_string(),
        desc: Html::lit("flip"),
      })
    }
    p.add_ui_operations(&mut out, gpc)?;
    out
  }
}
