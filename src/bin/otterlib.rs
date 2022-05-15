// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

// target/debug/otterlib --libs ~ian/Rustup/Game/server/library/\*.toml preview

const HTML_PRELUDE: &str = r##"
<!-- Copyright 2020u Ian Jackson
     SPDX-License-Identifier: AGPL-3.0-or-later
     There is NO WARRANTY. -->
<html>
  <head>
    <title>Otter builtin shape library</title>
  </head>
<body>
<style>
  html { background-color: #ddf; }
  table { background-color: #eef; }
</style>
<h1>Otter builtin shape library</h1>
This lists all the shapes provided by the library in this version of Otter.
</p>
"##;

const HTML_TRAILER: &str = r##"
<hr>
<address>
Otter and its shape (piece picture) libraries
are <a href="/_/libre">Free Software</a> and come with NO
WARRANTY.
<p>
The shapes come from a variety of sources and are the work of many
contributors.  <a href="/_/LICENCE">Further information about
their authorship and licensing</a> etc. is available.
<p>
If you wish to edit these shapes you should probably start with
the <a href="/_/src/">source code</a>.
"##;

pub use otter::prelude::*;

pub use shapelib::*;

use structopt::StructOpt;

#[derive(Debug,Clone)]
#[derive(StructOpt)]
pub struct Opts {
  #[structopt(long="--nwtemplates", default_value="./nwtemplates")]
  nwtemplates: String,

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

pub type ItemForOutput = ItemEnquiryData;

pub const VIS: ShowUnocculted = ShowUnocculted::new_visible();

#[throws(AE)]
fn preview(opts: &Opts, items: Vec<ItemForOutput>) {
  const BORDER: f64 = 1.;

  struct Prep {
    spec: ItemSpec,
    sortkey: Option<String>,
    p: Box<dyn PieceTrait>,
    uos: Vec<String>,
    bbox: Vec<Vec<f64>>,
    size: Vec<f64>,
  }

  const SEVERAL: usize = 3;
  let ig_dummy = Instance::dummy();

  nwtemplates::init_from_dir(&opts.nwtemplates)?;

  impl Prep {
    fn large(&self) -> bool {
      self.size[0] >= 50.0
    }
    fn face_want_span(&self) -> usize {
      if self.large() { 6 } else { 1 }
    }
    fn face_want_several(&self, face: RawFaceId) -> usize {
      if self.large() || face >= 2 { 1 } else { SEVERAL }
    }
    fn face_want_cols(&self, face: RawFaceId) -> usize {
      self.face_want_several(face) *
      self.face_want_span()
    }
    fn want_cols(&self) -> usize {
      (0 .. self.p.nfaces())
        .map(|face| self.face_want_cols(face))
        .sum()
    }
  }

  let mut pieces: Vec<(Prep, GPiece)> = items.into_iter().map(|it| {
    let spec = ItemSpec::from(&it);
    let sortkey = it.sortkey;
    (||{
      let mut gpc = GPiece::dummy();
      let SpecLoaded { p, .. } = spec.clone()
        .load(PieceLoadArgs {
          ig: &ig_dummy,
          gpc: &mut gpc,
          depth: SpecDepth::zero(),
          i: 0,
        })
        .context("load")?;
      p.loaded_hook_preview(&mut gpc)?;
      // todo show occulted version too
      let mut uos = vec![];
      p.add_ui_operations(VIS, &mut uos, &GameState::dummy(), &GPiece::dummy())
        .context("add uos")?;
      let uos = uos.into_iter().map(|uo| uo.opname).collect::<Vec<_>>();
      let spec = spec.clone();

      let bbox = p
        .bbox_preview()?;
      let mut bbox = bbox.corners.iter()
        .map(|PosC{coords}| coords.iter().map(|&p| p as f64)
             .collect::<Vec<_>>())
        .collect::<Vec<_>>();
      for xy in &mut bbox[0] { *xy -= BORDER }
      for xy in &mut bbox[1] { *xy += BORDER }
      let size = izip!(&bbox[0], &bbox[1])
        .map(|(min,max)| max-min)
        .collect::<Vec<_>>();

      Ok::<_,AE>((Prep { spec, p, sortkey, uos, bbox, size }, gpc ))
    })().with_context(|| format!("{:?}", &spec))
  }).collect::<Result<Vec<_>,_>>()?;

  // clones as a bodge for https://github.com/rust-lang/rust/issues/34162
  pieces.sort_by_key(|(p,_)| (p.spec.item.clone(), p.spec.lib.clone()));
                     
  let total_cols = pieces.iter().map(|s| s.0.want_cols()).max().unwrap_or(1);
  let max_uos = pieces.iter().map(|s| s.0.uos.len()).max().unwrap_or(0);

  println!("{}", &HTML_PRELUDE);
  println!(r#"<table rules="all">"#);
  for (s, gpc) in &mut pieces {
    let Prep { spec, sortkey, p, uos, bbox, size } = &*s;

    macro_rules! println {
      ($($x:tt)*) => ({
        let mut s = Html::new();
        hwrite!(&mut s, $($x)*).unwrap();
        std::println!("{}", &s.as_html_str());
      })
    }

    println!(r#"<tr>"#);
    println!(r#"<th align="left"><kbd>{}</kbd></th>"#,
             Html::from_txt(&spec.lib));
    println!(r#"<th align="left"><kbd>{}</kbd>"#,
             Html::from_txt(&spec.item));
    if let Some(sortkey) = sortkey {
      println!(r#"<br><kbd>[{}]</kbd>"#,
               Html::from_txt(sortkey));
    }
    println!(r#"</th>"#);
    println!(r#"<th align="left">{}</th>"#,
             p.describe_html(&GPiece::dummy(), &default())?);
    if max_uos > 0 {
      println!(r#"<td>{}</td>"#,
               uos.iter()
               .map(|uo| Html::from_txt(uo))
               .collect::<Vec<Html>>()
               .iter()
               .hjoin(&Html::lit(" ")));
    }

    let mut cols_done = 0;
    let face_span = s.face_want_span();

    for face in 0.. s.p.nfaces() {
      for inseveral in 0.. s.face_want_several(face) {
        print!(r#"<td align="center""#);
        if face_span != 1 {
          print!(r#" colspan="{}""#, face_span);
        }
        cols_done += face_span;

        print!(r##" bgcolor="#{}""##,
               match inseveral {
                 0 | 1 => "eee",
                 2 => "555",
                 _ => panic!(),
               });
        println!(r#">"#);

        let viewport =
          [bbox[0].clone(), size.clone()]
          .iter().flatten().cloned()
          .map(|c| c.to_string())
          .join(" ");
        let wh = size.iter().map(|&s| s * SVG_SCALE)
          .collect::<Vec<_>>();
        let surround = p.surround_path()?;
        print!(r#"<svg xmlns="http://www.w3.org/2000/svg"
                       viewBox="{}" width={} height={}>"#,
               &viewport, wh[0], wh[1]);
        let mut html = Html::lit("").into();
        gpc.face = face.into();
        p.svg_piece(&mut html, &gpc, &GameState::dummy(), default())?;
        println!("{}", html);
        if inseveral == 1 {
          let dasharray = player_num_dasharray(1.try_into().unwrap());
          println!(r#"<path d="{}" stroke-dasharray="{}"
                          fill="none" stroke="{}" />"#,
                 &surround, &dasharray, HELD_SURROUND_COLOUR);
        }
        println!("</svg>");
        println!("</td>");
      }}
    
    if cols_done < total_cols {
      print!(r#"<td colspan="{}"></td>"#, total_cols - cols_done);
    }

    println!("</tr>");
  }
  println!("</table>");
  println!("{}", &HTML_TRAILER);
}

#[throws(anyhow::Error)]
fn main() {
  let opts = Opts::from_args();

  env_logger::Builder::new()
//    .format_timestamp_micros()
//    .format_level(true)
//    .filter_module(exe_module_path, log::LevelFilter::Debug)
    .filter_level(log::LevelFilter::Info)
    .parse_env("OTTER_CLI_LOG")
    .init();

  const SPLIT: &[char] = &[',', ' '];

  for libs in opts.libs.split(SPLIT) {
    let tlibs = ShapelibConfig1::PathGlob(libs.to_owned());
    load_global_libs(&[tlibs.clone()])?;
  }
  let mut items: Vec<ItemForOutput> = default();
  let ig_dummy = Instance::dummy();
  let all_registries = ig_dummy.all_shapelibs();
  for lib in lib_name_list(&ig_dummy) {
    for contents in all_registries.lib_name_lookup(&lib)? {
      for pat in opts.items.split(SPLIT) {
        for item in contents.list_glob(pat)? {
          items.push(item)
        }
      }
    }
  }
  items.sort();

  match opts.outkind {
    OutputKind::List => for item in items {
      println!("{}", item);
    }
    OutputKind::Preview => {
      preview(&opts, items)?
    }
  }
}
