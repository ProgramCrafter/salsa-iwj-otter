pub use otter::imports::*;

#[throws(anyhow::Error)]
fn main(){
  let f = env::args().nth(1).unwrap();
  shapelib::load(f.clone(),f)?;
}
