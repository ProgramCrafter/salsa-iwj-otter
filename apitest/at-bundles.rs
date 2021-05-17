// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use crate::*;

type Ctx = UsualCtx;

impl Ctx {
  #[throws(Explode)]
  fn check_library_item(&mut self, itemlib: &str, item: &str,
                        desc: &str) {
    let ds = self.su().ds.also(&[
      ("itemlib", itemlib),
      ("item",    item   ),
    ]);
    let command = ds.ss("library-add --lib @itemlib@ @table@ @item@")?;
    let added = self.some_library_add(&command)?;
    assert_eq!( added.len(), 1 );

    let output: String = self.otter(&ds.ss("list-pieces @table@")?)?.into();
    assert!( Regex::new(
      &format!(
        r#"(?m)(?:[^\w-]|^){}[^\w-].*\W{}(?:\W|$)"#,
        item, desc,
      )
    )?
             .find(&output)
             .is_some(),
             "got: {}", &output);
  }

  #[throws(Explode)]
  fn bundles(&mut self) {
    let bundle_file = self.su().ds.example_bundle();
    let ds = self.su().ds.also(&[("bundle", &bundle_file)]);
    self.otter(&ds.ss("upload-bundle @table@ @bundle@")?)?;
    let mut bundles = self.otter(&ds.ss("list-bundles @table@")?)?;
    let bundles = String::from(&mut bundles);
    assert!(bundles.starts_with("00000.zip Loaded"));
    self.otter(&ds.ss("download-bundle @table@ 0")?)?;
    let st = Command::new("cmp").args(&[&bundle_file, "00000.zip"]).status()?;
    if ! st.success() { panic!("cmp failed {}", st) }

    self.check_library_item("lemon","example-lemon","a lemon")?;

    self.otter(&ds.ss("clear-game @table@")?)?;
    self.otter(&ds.ss("reset @table@ demo")?)?;
  }
}

#[throws(Explode)]
fn tests(mut c: Ctx) {
  test!(c, "bundles",                       c.bundles()      ?);
}

#[throws(Explode)]
pub fn main() {
  tests(Ctx::setup()?)?;
}
