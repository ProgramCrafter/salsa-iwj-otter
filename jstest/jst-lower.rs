// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use otter_nodejs_tests::*;

#[derive(StructOpt,Debug,Clone)]
pub struct Opts {
  pub nodejs: String,
  pub script: String,
}

#[derive(Debug,Clone)]
pub struct StartPieceSpec {
  id: &'static str,
  pinned: bool,
  moveable: PieceMoveable,
}

#[macro_export]
macro_rules! sp {
  { $id:expr, $pinned:expr, $moveable:ident } => {
    StartPieceSpec { id: $id, pinned: $pinned,
                     moveable: PieceMoveable::$moveable }
  };
}

#[derive(Debug,Clone)]
#[derive(Eq,PartialEq,Ord,PartialOrd)]
#[derive(Serialize)]
pub struct StartPiece {
  id: VisiblePieceId,
  pinned: bool,
  moveable: PieceMoveable,
  z: ZCoord,
}

#[derive(Debug,Clone,Default)]
pub struct Tests {
  tests: IndexMap<String, Test>,
}

#[derive(Debug,Clone,Default)]
#[derive(Serialize)]
pub struct Test {
  name: String,
  pieces: Vec<StartPiece>,
  targets: Vec<VisiblePieceId>,
}

#[derive(Debug)]
pub struct TestsAccumulator {
  tests: Tests,
  script: BufWriter<fs::File>,
  tera: tera::Tera,
}

impl Test {
  #[throws(Explode)]
  pub fn check(&self) {
    let mut updated: HashMap<VisiblePieceId, ZCoord> = default();
    for l in BufReader::new(
      fs::File::open(format!("{}.did",self.name))?
    ).lines() {
      let l = l?;
      let (op, id, z) = l.splitn(3,' ').collect_tuple().unwrap();
      assert_eq!(op, "setz");
      let id = id.try_into()?;
      let z = z.parse()?;
      let was = updated.insert(id, z);
      assert!(was.is_none(), "{:?}", id);
    }

    // targets' new z coords are in increasing z coord
    // no bottom are newly above non-bottom
    // z coords in updates all decrease
    // all targets now below all non-bottom non-targets
    // xxx ^ unimplemented checks

    dbg!(updated);
  }
}

static TEMPLATE: &'static str = r#"
//-------------------- {{ name }} --------------------

jstest_did = fs.openSync('{{ name }}.did', 'w');

pieces = {
{% for p in pieces -%}
  '{{ p.id }}': {
    pinned: {{ p.pinned }},
    moveable: '{{ p.moveable }}',
    z: '{{ p.z }}',
  },
{% endfor -%}
}

fake_dom = [
  { special: "pieces_marker", dataset: { } },
{% for p in pieces -%}
  { dataset: { piece: "{{ p.id }}" } },
{% endfor -%}
  { special: "defs_marker", dataset: { } },
];

pieces_marker = fake_dom[0];
defs_marker   = fake_dom[{{ pieces | length + 1 }}];

{% for p in pieces -%}
fake_dom[{{ loop.index0 }}].nextElementSibling = fake_dom[{{ loop.index }}];
{% endfor %}
fake_dom[{{ pieces | length }}].nextElementSibling = fake_dom[{{ pieces | length + 1 }}];

{% for t in targets -%}
uorecord = {
  targets: ['{{ t }}'],
};
{%- endfor %}

lower_targets(uorecord);

fs.closeSync(jstest_did);
jstest_did = -1;

"#;


impl TestsAccumulator {
  #[throws(Explode)]
  pub fn new(opts: &Opts) -> Self {
    let mut tera = tera::Tera::default();
    tera.add_raw_template("js", TEMPLATE)?;

    let script = fs::OpenOptions::new()
      .write(true)
      .append(true)
      .truncate(false)
      .create(false)
      .open(&opts.script)?;
    let script = BufWriter::new(script);

    TestsAccumulator {
      tests: default(),
      script, tera,
    }
  }

  #[throws(Explode)]
  pub fn finalise(mut self) -> Tests {
    self.script.flush()?;
    self.tests
  }
    
  #[throws(Explode)]
  pub fn add_test(&mut self, name: &'static str,
                  pieces: Vec<StartPieceSpec>,
                  targets: Vec<&'_ str>) {
    let mut zm = ZCoord::default().clone_mut();
    let pieces = pieces.into_iter().map(
      |StartPieceSpec { id, pinned, moveable }| {
        let id = id.try_into().unwrap();
        let z = zm.increment().unwrap();
        StartPiece { id, pinned, moveable, z }
      }
    ).collect_vec();

    let targets = targets.into_iter().map(
      |s| s.try_into().unwrap()
    ).collect_vec();

    let test = Test {
      name: name.into(),
      pieces, targets,
    };
    let context = tera::Context::from_serialize(&test)?;
    self.tera.render_to("js", &context, &mut self.script)?;

    let already = self.tests.tests.insert(name.to_owned(), test);
    assert!(already.is_none(), "{:?}", &name);
  }
}

#[throws(Explode)]
fn main() {
  let opts = Opts::from_args();

  let mut ta = TestsAccumulator::new(&opts)?;

  ta.add_test("simple", vec![
    sp!("1.1", false, Yes),
    sp!("2.1", false, Yes),
  ], vec![
    "2.1",
  ])?;

  let tests = ta.finalise()?;

  let mut cmd = Command::new(opts.nodejs);
  cmd.arg(opts.script);
  let status = cmd.status()?;
  assert!(status.success(), "{}", status);

  for test in tests.tests.values() {
    println!("checking results for {:?}", &test.name);
    test.check()?;
  }
}
