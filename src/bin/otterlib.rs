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
  let pieces = items.into_iter().map(|it| {
    let spec = ItemSpec { lib: it.0, item: it.1.itemname };
    (||{
      let pc = spec.clone().load().context("load")?;
      let mut uos = vec![];
      pc.add_ui_operations(&mut uos).context("add uos")?;
      let uos = uos.into_iter().map(|uo| uo.opname).collect::<Vec<_>>();
      Ok::<_,AE>((pc, uos))
    })().with_context(|| format!("{:?}", &spec))
  }).collect::<Result<Vec<_>,_>>()?;

  let max_faces = pieces.iter().map(|(p,_)| p.nfaces()).max().unwrap_or(1);
  let max_uos = pieces.iter().map(|(_,uos)| uos.len()).max().unwrap_or(0);

  println!("<table>");
  for (pc, uos) in &pieces {
    println!("<tr><th>{}</th>", pc.describe_html(None).0);
    for face in 0..max_faces {
      println!("<td>");
      if face < pc.nfaces() {
        let pri = PieceRenderInstructions {
          id: default(),
          angle: VisiblePieceAngle(default()),
          face: face.into(),
        };
        let bbox = pc.bbox_approx();
        let surround = pc.surround_path(&pri);
        println!(r#"<--svg viewBox="{:?} {:?}""#, &bbox, &surround);
        let mut html = Html("".into());
        pc.svg_piece(&mut html, &pri)?;
        println!("SVG\n{}\n\n", html.0);
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
