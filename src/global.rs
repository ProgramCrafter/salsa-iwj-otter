
#[derive(Default)]
struct Global {
  instances : RwLock<HashMap<InstanceName, Rc<Instance>>>,
  // xxx delete instances at some point!
}

lazy_static! {
  static ref GLOBAL : Global = Default::default();
}

fn lookup_instance(name : &str) -> Option<Rc<Instance>> {
  GLOBAL.instances().read().get(name)
}

#[throws(TE)]
fn create_instance(name : &str, i : Rc<Instance>) {
  let w = GLOBAL.instances().write();
  match w.entry(name) {
    Occupied(oe) => throw!(TE::InstanceAlreadyExists);
    Vacant(ve) => ve.insert(i);
  }
}

impl<'r> FromParam<'r> for InstanceGuard<'r> {
  // xxx any additional auth should go here
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

