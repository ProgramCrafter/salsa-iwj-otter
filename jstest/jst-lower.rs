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
  #[serde(with = "indexmap::serde_seq")]
  pieces: IndexMap<VisiblePieceId, StartPiece>,
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

    // non-bottom targets are in same stacking order as before
    {
      #[derive(Debug,Ord,PartialOrd,Eq,PartialEq)]
      struct Nbt<'o,'n> {
        old_z: &'o ZCoord,
        new_z: &'n ZCoord,
        id: VisiblePieceId,
      }
      let mut nbts = self.targets.iter()
        .map(|&id| {
          let old_z = &self.pieces[&id].z;
          let new_z = updated.get(&id).unwrap_or(old_z);
          Nbt { new_z, old_z, id }
        })
        .collect_vec();
      nbts.sort();
      nbts.iter().fold1( |nbt0, nbt1| {
        assert!( nbt0.new_z < nbt1.new_z,
                 "{:?} {:?} {:?} {:#?}", &self.name, nbt0, nbt1,
                 &nbts);
        nbt1
      });
    }

    // no bottom are newly above non-bottom
    // no non-bottom non-targets moved
    // z coords in updates all decrease
    // all targets now below all non-bottom non-targets
    // xxx ^ unimplemented checks

    dbg!(updated);
  }
}

impl StartPiece {
  pub fn bottom(&self) -> bool {
    use PieceMoveable::*;
    match (self.pinned, self.moveable) {
      (true , _  ) => true,
      (false, Yes) => false,
      (false, No ) => true,
      (_, IfWresting) => panic!(),
    }
  }
}

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
        (id, StartPiece { pinned, moveable, z })
      }
    ).collect();

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
    assert!(already.is_none(), "duplicate test {:?}", &name);
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

  ta.add_test("pair", vec![
    sp!("1.1", false, Yes),
    sp!("2.1", false, Yes),
    sp!("3.1", false, Yes),
  ], vec![
    "3.1",
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

static TEMPLATE: &'static str = r#"

console.log('-------------------- {{ name }} --------------------')
jstest_did = fs.openSync('{{ name }}.did', 'w');

pieces = {
{% for p in pieces -%}
  '{{ p.0 }}': {
    pinned: {{ p.1.pinned }},
    moveable: '{{ p.1.moveable }}',
    z: '{{ p.1.z }}',
  },
{% endfor -%}
}

fake_dom = [
  { special: "pieces_marker", dataset: { } },
{%- for p in pieces %}
  { dataset: { piece: "{{ p.0 }}" } },
{%- endfor -%}
  { special: "defs_marker", dataset: { } },
];

pieces_marker = fake_dom[0];
defs_marker   = fake_dom[{{ pieces | length + 1 }}];

{%- for p in pieces %}
fake_dom[{{ loop.index0 }}].nextElementSibling = fake_dom[{{ loop.index }}];
{%- endfor %}
fake_dom[{{ pieces | length }}].nextElementSibling = fake_dom[{{ pieces | length + 1 }}];

uorecord = {
  targets: [
{%- for t in targets %}
      '{{ t }}',
{%- endfor %}
  ],
};

lower_targets(uorecord);

fs.closeSync(jstest_did);
jstest_did = -1;

"#;
