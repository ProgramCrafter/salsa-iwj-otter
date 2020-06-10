
strut InstanceAccess {
  inst : Rc<Instance>,
  user : usize,
}

#[derive(Default)]
struct Global {
  tokens : RwLock<HashMap<RawToken, InstanceAccess>,
  // xxx delete instances at some point!
}

lazy_static! {
  static ref GLOBAL : Global = Default::default();
}

fn lookup_token(s : &str) -> Option<InstanceAccess> {
  GLOBAL.instances().read().get(s)
}
  
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

impl<'r> FromParam<'r> for InstanceAccess<'r> {
  type Error = AE;
  #[throws(E)]
  fn from_param(param: &'r RawStr) -> Option<Self> {
    let g = GLOBAL.instances().read();
    let iname = param.as_str();
    g.get(iname);
  }
}

