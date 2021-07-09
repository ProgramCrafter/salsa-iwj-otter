// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use otter_nodejs_tests::*;

pub type Vpid = VisiblePieceId;

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
  pieces: IndexMap<Vpid, StartPiece>,
  targets: IndexSet<Vpid>,
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
    eprintln!("-------------------- {} --------------------", &self.name);

    let mut updated: HashMap<Vpid, ZCoord>
      = default();

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

    #[derive(Debug)]
    struct PieceCollated<'o,'n> {
      id: Vpid,
      old_z: &'o ZCoord,
      new_z: &'n ZCoord,
      target: bool,
      bottom: bool,
      updated: bool,
    }

    let coll = self.pieces.iter().map(|(&id, start)| {
      let old_z = &start.z;
      let new_z = updated.get(&id);
      let updated = new_z.is_some();
      let new_z = new_z.unwrap_or(&start.z);
      PieceCollated {
        id, new_z, old_z, updated,
        bottom: start.bottom(),
        target: self.targets.contains(&id),
      }
    }).collect_vec();

    let sorted = | kf: &dyn for <'r> Fn(&'r PieceCollated<'r,'r>) -> &'r _ | {
      let mut v: Vec<&PieceCollated> = coll.iter().collect_vec();
      v.sort_by_key(|p| kf(p));
      v
    };
    let old = sorted(&|p: &PieceCollated| p.old_z);
    let new = sorted(&|p: &PieceCollated| p.new_z);
    for (o, n) in izip!(&old, &new).rev() {
      let pr = |p: &PieceCollated| {
        eprint!("    {} {}{}{} ",
                p.id,
                if p.target  { "T" } else { "_" },
                if p.bottom  { "B" } else { "_" },
                if p.updated { "U" } else { "_" });
      };
      pr(o);
      eprint!("{:<20}    ", o.old_z.as_str());
      pr(n);
      eprintln!("{}"        , n.new_z.as_str());
    }

    // non-bottom targets are in same stacking order as before
    {
      for (o, n) in izip!(
        old.iter().filter(|p| p.target && ! p.bottom),
        new.iter().filter(|p| p.target && ! p.bottom),
      ) {
        assert_eq!(o.id, n.id);
      }
    }

    // no bottom are newly above non-bottom
    {
      let misbottom = |on: &[&PieceCollated]| {
        let mut misbottom = HashSet::new();
        for i in 0..on.len() {
          for j in i+1..on.len() {
            // j is above i
            if on[j].bottom && ! on[i].bottom {
              // bottom above non-bottom
              misbottom.insert((on[j].id, on[i].id));
            }
          }
        }
        misbottom
      };
      let old = misbottom(&old);
      let new = misbottom(&new);
      let newly = new.difference(&old).collect_vec();
      assert!( newly.is_empty(), "{:?}", &newly );
    }

    // no non-bottom non-targets moved
    {
      for n in &new {
        if ! n.bottom && ! n.target {
          assert!( ! n.updated, "{:?}", n );
        }
      }
    }

    // z coords (at least of bottom) in updates all decrease
    {
      for n in &new {
        if n.bottom && n.updated {
          assert!( n.new_z < n.old_z, "{:?}", &n );
        }
      }
    }

    // all targets now below all non-bottom non-targets
    {
      let mut had_nonbottom_nontarget = None;
      for n in &new {
        if ! n.bottom && ! n.target {
          had_nonbottom_nontarget = Some(n);
        }
        if n.target {
          assert!( had_nonbottom_nontarget.is_none(),
                   "{:?} {:?}", &n, had_nonbottom_nontarget);
        }
      }
    }

    // xxx ^ unimplemented checks
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
    ).collect();

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
