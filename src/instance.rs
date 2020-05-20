
const RECENT_BUFFER : usize = 10;
 
#[derive(Debug)]
pub struct Instance {
  g : RwLock<Game>,
  g_notify : Condvar,
}

#[derive(Debug)]
struct Game {
  gen : Counter,
  gs : GameState,
  recent : VecDeque<MsgUpdate>,
}

impl Instance {
  fn new(gs : GameState) -> Instance {
    Instance {
      g_notify : Condvar::new(),
      g : RwLock::new(Game {
        gen : 0,
        gs,
        recent : VecDequeue::with_capacity(RECENT_BUFFER),
      }),
    }
  }
}

pub struct InstanceGuard<'r> {
  iname : &'r str;
  g : RwLockWriteGuard<Game>,
  g_notify : &'r Condvar,
}

impl Instance {
  fn lock(&'r self, iname : &'r str) -> InstanceGuard<'r> {
    let g = self.g.lock();
    InstanceGuard { g, iname, g_notify : &self.g_notify }
  }
}

impl InstanceGuard {
  fn read(&self) -> &GameState { &self.g.deref().gs }
  fn iname(&self) -> &str { self.iname }

  fn<F> update(&mut self, f : F)
  where F : FnOnce(&mut GameState) -> MsgUpdate {
    let msg = f(&mut self.gs.g),
    if let MsgNoUpdate = msg { return }
    self.gs.gen += 1,
    if self.gw.recent.len() >= RECENT_BUFFER { self.pop_front() }
    self.g.recent.push_back(msg);
    self.g_notify.notify_all();
  }
}
