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

pub const VIS: ShowUnocculted = ShowUnocculted::new_visible();

#[throws(AE)]
fn preview(items: Vec<ItemForOutput>) {
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

  impl Prep {
    fn want_several(&self) -> bool {
      self.size[0] < 50.0
    }
    fn face_cols(&self) -> usize {
      usize::from(self.p.nfaces())
        * if self.want_several() { SEVERAL } else { 1 }
    }
  }

  let mut pieces: Vec<Prep> = items.into_iter().map(|it| {
    let spec = ItemSpec { lib: it.0, item: it.1.itemname.into() };
    let sortkey = it.1.sortkey;
    (||{
      let (p, _occultable) = spec.clone()
        .find_load(&ig_dummy, SpecDepth::zero())
        .context("load")?;
      // todo show occulted version too
      let mut uos = vec![];
      p.add_ui_operations(VIS, &mut uos, &GameState::dummy(), &GPiece::dummy())
        .context("add uos")?;
      let uos = uos.into_iter().map(|uo| uo.opname).collect::<Vec<_>>();
      let spec = spec.clone();

      let bbox = p
        .bbox_approx()?;
      let mut bbox = bbox.corners.iter()
        .map(|PosC{coords}| coords.iter().map(|&p| p as f64)
             .collect::<Vec<_>>())
        .collect::<Vec<_>>();
      for xy in &mut bbox[0] { *xy -= BORDER }
      for xy in &mut bbox[1] { *xy += BORDER }
      let size = izip!(&bbox[0], &bbox[1])
        .map(|(min,max)| max-min)
        .collect::<Vec<_>>();

      Ok::<_,AE>(Prep { spec, p, sortkey, uos, bbox, size })
    })().with_context(|| format!("{:?}", &spec))
  }).collect::<Result<Vec<_>,_>>()?;

  // clones as a bodge for https://github.com/rust-lang/rust/issues/34162
  pieces.sort_by_key(|p| (p.spec.item.clone(), p.spec.lib.clone()));
                     
  let max_facecols = pieces.iter().map(|s| s.face_cols()).max().unwrap_or(1);
  let max_uos = pieces.iter().map(|s| s.uos.len()).max().unwrap_or(0);

  println!("{}", &HTML_PRELUDE);
  println!(r#"<table rules="all">"#);
  for s in &pieces {
    let Prep { spec, sortkey, p, uos, bbox, size } = s;

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
    let only1 = s.face_cols() == 1;

    for facecol in 0..(if only1 { 1 } else { max_facecols }) {
      let (face, inseveral) = if s.want_several() {
        (facecol / SEVERAL, facecol % SEVERAL)
      } else {
        (facecol, 0)
      };
      print!(r#"<td align="center""#);
      if only1 {
        assert!(!s.want_several());
        print!(r#" colspan="{}""#, max_facecols);
      }
      print!(r##" bgcolor="#{}""##,
             match inseveral {
               0 | 1 => "eee",
               2 => "555",
               _ => panic!(),
             });
      println!(r#">"#);
      if face < (p.nfaces() as usize) {
        let viewport =
          [bbox[0].clone(), size.clone()]
          .iter().cloned()
          .flatten()
          .map(|c| c.to_string())
          .join(" ");
        let wh = size.iter().map(|&s| s * SVG_SCALE)
          .collect::<Vec<_>>();
        let surround = p.surround_path()?;
        print!(r#"<svg xmlns="http://www.w3.org/2000/svg"
                       viewBox="{}" width={} height={}>"#,
               &viewport, wh[0], wh[1]);
        if inseveral == 1 {
          let dasharray = player_num_dasharray(1.try_into().unwrap());
          println!(r#"<path d="{}" stroke-dasharray="{}"
                          fill="none" stroke="{}" />"#,
                 &surround, &dasharray, HELD_SURROUND_COLOUR);
        }
        let mut html = Html::lit("").into();
        let gpc = GPiece { face: face.into(), ..GPiece::dummy() };
        p.svg_piece(&mut html, &gpc, &GameState::dummy(), default())?;
        println!("{}</svg>", html);
      }
      println!("</td>");

      if max_uos > 0 {
        println!(r#"<td>{}</td>"#,
                 uos.iter()
                 .map(|uo| Html::from_txt(uo))
                 .collect::<Vec<Html>>()
                 .iter()
                 .hjoin(&Html::lit(" ")));
      }
    };
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
    let tlibs = Config1::PathGlob(libs.to_owned());
    load_global_libs(&vec![tlibs.clone()])?;
  }
  let mut items: Vec<ItemForOutput> = default();
  for lib in lib_name_list() {
    for contents in &*lib_name_lookup(&lib)? {
      for pat in opts.items.split(SPLIT) {
        for item in contents.list_glob(pat)? {
          items.push((lib.clone(), item))
        }
      }
    }
  }
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
