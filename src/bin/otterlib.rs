// Copyright 2020 Ian Jackson
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

// target/debug/otterlib --libs ~ian/Rustup/Game/server/library/\*.toml preview

pub use otter::imports::*;

pub use shapelib::*;

use structopt::StructOpt;

#[derive(Debug,Clone)]
#[derive(StructOpt)]
pub struct Opts {
  #[structopt(long="--libs", default_value="library/*.toml")]
  libs: String,

  #[structopt(long="--items", default_value="*")]
  items: String,

  #[structopt(flatten)]
  outkind: OutputKind,
}

#[derive(StructOpt,Debug,Clone,Copy)]
pub enum OutputKind {
  List,
  Preview,
}

pub type ItemForOutput = (String, ItemEnquiryData);

#[throws(AE)]
fn preview(items: Vec<ItemForOutput>) {
  const BORDER: f64 = 1.;

  struct Prep {
    spec: ItemSpec,
    pc: Box<dyn Piece>,
    uos: Vec<String>
  };

  let pieces : Vec<Prep> = items.into_iter().map(|it| {
    let spec = ItemSpec { lib: it.0, item: it.1.itemname };
    (||{
      let pc = spec.clone().load().context("load")?;
      let mut uos = vec![];
      pc.add_ui_operations(&mut uos).context("add uos")?;
      let uos = uos.into_iter().map(|uo| uo.opname).collect::<Vec<_>>();
      let spec = spec.clone();

      Ok::<_,AE>(Prep { spec, pc, uos })
    })().with_context(|| format!("{:?}", &spec))
  }).collect::<Result<Vec<_>,_>>()?;

  let max_faces = pieces.iter().map(|s| s.pc.nfaces()).max().unwrap_or(1);
  let max_uos = pieces.iter().map(|s| s.uos.len()).max().unwrap_or(0);

  println!(r#"<table rules="all">"#);
  for Prep { spec, pc, uos } in &pieces {
    println!(r#"<tr>"#);
    println!(r#"<th align="left"><kbd>{}</kbd><th>"#, &spec.lib);
    println!(r#"<th align="left"><kbd>{}</kbd><th>"#, &spec.item);
    println!(r#"<th align="left">{}</th>"#, pc.describe_html(None).0);
    let only1 = pc.nfaces() == 1;
    let getpri = |face: FaceId| PieceRenderInstructions {
      id: default(),
      angle: VisiblePieceAngle(default()),
      face
    };

    let bbox = pc
      .bbox_approx();
    let mut bbox = bbox
      .iter()
      .map(|PosC(xy)| xy.iter().map(|&p| p as f64).collect::<Vec<_>>())
      .collect::<Vec<_>>();
    for xy in &mut bbox[0] { *xy -= BORDER }
        for xy in &mut bbox[1] { *xy += BORDER }
    let size = izip!(&bbox[0], &bbox[1])
      .map(|(min,max)| max-min)
      .collect::<Vec<_>>();

    for face in 0..(if only1 { 1 } else { max_faces }) {
      print!(r#"<td align="center""#);
      if only1 { print!(r#" colspan="{}""#, max_faces); }
      println!(r#">"#);
      if face < pc.nfaces() {
        let pri = getpri(face.into());
        let viewport =
          [bbox[0].clone(), size.clone()]
          .iter().cloned()
          .flatten()
          .map(|c| c.to_string())
          .join(" ");
        let wh = size.iter().map(|&s| s * SVG_SCALE)
          .collect::<Vec<_>>();
        let surround = pc.surround_path(&pri);
        print!(r#"<svg xmlns="http://www.w3.org/2000/svg"
                       viewBox="{}" width={} height={}>"#,
               &viewport, wh[0], wh[1]);
        let mut html = Html("".into());
        pc.svg_piece(&mut html, &pri)?;
        println!("{}</svg>", html.0);
//        
//        println!(r#"<svg viewBox="{}"> width={} height={} {:?}"#,
//                 &viewport, size[0], size[1], &surround);
//        println!("SVG\n{}\n\n", html.0);
      }
      println!("</td>");
    };
    println!("</tr>");
  }
  println!("</table>");
}

#[throws(anyhow::Error)]
fn main() {
  let opts = Opts::from_args();

  env_logger::Builder::new()
//    .format_timestamp_micros()
//    .format_level(true)
//    .filter_module(exe_module_path, log::LevelFilter::Debug)
    .filter_level(log::LevelFilter::Info)
    .parse_env("OTTERLIB_LOG")
    .init();

  let libs = Config1::PathGlob(opts.libs.clone());
  load(&vec![libs.clone()])?;
  let mut items : Vec<ItemForOutput> =
    libs_list()
    .into_iter()
    .map(|lib| {
      let contents = libs_lookup(&lib)?;
      let items = contents.list_glob(&opts.items)?;
      Ok::<_,AE>((lib, items))
    })
    .collect::<Result<Vec<_>,_>>()?
    .into_iter()
    .map(|(lib, items)| {
      items.into_iter().map(|item| (lib.clone(),item)).collect::<Vec<_>>()
    }).flatten()
    .collect();
  items.sort();

  match opts.outkind {
    OutputKind::List => for item in &items {
      println!("{:<10} {}", &item.0, item.1.line_for_list());
    }
    OutputKind::Preview => {
      preview(items)?
    }
  }
}
