pub use otter::imports::*;

#[throws(anyhow::Error)]
fn main(){
  let f = env::args().nth(1).unwrap();
  let l = shapelib::Library::load(&f)?;
  dbg!(l);
}
