
#[throws(E)]
fn create_instance_access(name : &str, i : Rc<Instance>) {
  let w = GLOBAL.instances().write();
  match w.entry(name) {
    Occupied(oe) => throw!(anyhow!("access key alreay defined"));
    Vacant(ve) => ve.insert(i);
  }
}

/*
impl<'r> FromParam<'r> for InstanceGuard<'r> {
  type Error = AE;
  #[throws(E)]
  fn from_param(param: &'r RawStr) -> Self {
    let g = GLOBAL.instances().read();
    let iname = param.as_str();
    let i = g.get(iname);
    let i = i.ok_or(anyhow!("unnown instance"))?;
    i.lock(iname)
  }
}
*/

