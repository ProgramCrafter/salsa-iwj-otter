pub use otter::imports::*;

#[throws(anyhow::Error)]
fn main(){
  let mut s = String::new();
  io::stdin().read_to_string(&mut s)?;
  let l : shapelib::Library = toml::from_str(&s)?;
  dbg!(l);
}
