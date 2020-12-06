pub use otter::imports::*;

#[throws(anyhow::Error)]
fn main() {
  let mut a = env::args();
  a.next().unwrap();
  let name = a.next().unwrap();
  let dirname = a.next().unwrap();
  let catalogue = format!("{}.toml", &dirname);
  let e = shapelib::Explicit1 {
    name, dirname, catalogue
  };
  shapelib::load1(&e).unwrap();
}
