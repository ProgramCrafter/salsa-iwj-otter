pub use otter::imports::*;

#[throws(anyhow::Error)]
fn main(){
  let f = env::args().nth(1).unwrap();
  let l = shapelib::LibraryContents::load(f)?;
  dbg!(l);
}
