// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use crate::prelude::*;

visible_slotmap_key!{ Id(b'k') }

static RESTRICTIONS: &str =
  concat!("restrict,no-agent-forwarding,no-port-forwarding,",
          "no-pty,no-user-rc,no-X11-forwarding");

static MAGIC_BANNER: &str = 
  "# WARNING - AUTOMATICALLY GENERAED FILE - DO NOT EDIT\n";

#[derive(Copy,Clone,Serialize,Deserialize)]
#[derive(Eq,PartialEq,Hash,Ord,PartialOrd)]
#[serde(transparent)]
// This will detecte if the slotmap in Accounts gets rewound, without
// updating the authorzed keys.  That might reuse Id values but it
// can't reuse Nonces.
pub struct Nonce([u8; 32]);

// States of a key wrt a particular scope:
//
//                   PerScope       in authorized_keys     leftover key
//                   core    file           a.k._dirty     refcount==0
//                                       if us only        core   file
//
//    ABSENT         -       -         maybe[1] -          -      -
//    GARBAGE        -       -         maybe[1] -          -      y [2]
// **undesriable**   -       -         -                   y      -
//  **illegal**      -       y
//    UNSAVED        y       -         maybe    true       n/a    n/a
//    BROKEN         y       y         maybe    true       n/a    n/a
//    PRESENT        y       y         y        -          n/a    n/a
//
// [1] garbage in the authorised_keys is got rid of next time we
//     write it, so it does not persist indefinitely.
// [2] garbage in Global is deleted when we do the key hashing (which
//     iterates over all keys), which will happen before we add any
//     key.

#[derive(Debug,Clone,Serialize,Deserialize,Default)]
pub struct Global {
  keys: DenseSlotMap<Id, Key>,
  authkeys_dirty: bool,
  #[serde(skip)] fps: Option<FingerprintMap>,
}
type FingerprintMap = HashMap<Arc<Fingerprint>, Id>;

#[derive(Debug,Clone,Serialize,Deserialize)]
pub struct Key {
  refcount: usize,
  data: PubData,
  nonce: Nonce,
  #[serde(skip)] fp: Option<Result<Arc<Fingerprint>, KeyError>>,
}

#[derive(Debug,Clone,Serialize,Deserialize,Default)]
pub struct PerScope {
  authorised: Vec<Option<ScopeKey>>,
}
#[derive(Debug,Clone,Serialize,Deserialize)]
pub struct ScopeKey {
  id: Id, // owns a refcount
  comment: Comment,
}

#[derive(Debug,Clone,Serialize,Deserialize)]
#[derive(Eq,PartialEq,Hash,Ord,PartialOrd)]
pub struct KeySpec {
  pub id: sshkeys::Id,
  pub nonce: sshkeys::Nonce,
}

#[derive(Error,Copy,Clone,Debug,Hash,Serialize,Deserialize)]
#[error("ssh authorized_keys manipulation failed")]
pub struct AuthKeysManipError { }
impl From<anyhow::Error> for AuthKeysManipError {
  fn from(ae: anyhow::Error) -> AuthKeysManipError {
    error!("authorized_keys manipulation error: {}: {}",
           &config().authorized_keys, ae.d());
    AuthKeysManipError { }
  }
}
impl From<AuthKeysManipError> for MgmtError {
  fn from(akme: AuthKeysManipError) -> MgmtError {
    IE::from(akme).into()
  }
}

mod veneer {
  // openssh_keys's API is a little odd.  We make our own mini-API.
  use crate::prelude::*;
  extern crate openssh_keys;
  use openssh_keys::errors::OpenSSHKeyError;

  // A line in nssh authorized keys firmat.  Might have comment or
  // options.  Might or might not have a newline.  String inside
  // this newtype has not necessarily been syntax checked.
  #[derive(Debug,Clone,Serialize,Deserialize)]
  #[serde(transparent)]
  pub struct AuthkeysLine(pub String);

  // In nssh authorized keys firmat.  No options, no comment.
  #[derive(Debug,Clone,Serialize,Deserialize)]
  #[serde(transparent)]
  pub struct PubData(String);

  #[derive(Debug,Clone,Serialize,Deserialize)]
  #[serde(transparent)]
  pub struct Comment(String /* must not contain newline/cr */);

  // Not Serialize,Deserialize, so we can change the hash, etc.
  #[derive(Debug,Clone,Hash,Eq,PartialEq,Ord,PartialOrd)]
  pub struct Fingerprint(String);

  #[derive(Error,Debug,Clone,Serialize,Deserialize)]
  pub enum KeyError {
    #[error("bad key data: {0}")]                        BadData(String),
    #[error("failed to save key data, possibly broken")] Dirty,
  }

  impl From<OpenSSHKeyError> for KeyError {
    fn from(e: OpenSSHKeyError) -> Self { KeyError::BadData(e.to_string()) }
  }

  impl Display for Comment {
    #[throws(fmt::Error)]
    fn fmt(&self, f: &mut fmt::Formatter) { write!(f, "{}", &self.0)? }
  }
  impl Display for PubData {
    #[throws(fmt::Error)]
    fn fmt(&self, f: &mut fmt::Formatter) { write!(f, "{}", &self.0)? }
  }

  impl AuthkeysLine {
    #[throws(KeyError)]
    pub fn parse(&self) -> (PubData, Comment) {
      let openssh_keys::PublicKey {
        options:_, data, comment
      } = self.0.parse()?;
      let data = openssh_keys::PublicKey {
        data,
        options: None,
        comment: None,
      };
      (PubData(data.to_string()), Comment(comment.unwrap_or_default()))
    }
  }

  impl PubData {
    #[throws(KeyError)]
    pub fn fingerprint(&self) -> Fingerprint {
      let k: openssh_keys::PublicKey = self.0.parse()?;
      Fingerprint(k.fingerprint())
    }
  }

  impl Display for Fingerprint {
    #[throws(fmt::Error)]
    fn fmt(&self, f: &mut Formatter) { write!(f, "{}", self.0)? }
  }

}
pub use veneer::*;

format_by_fmt_hex!{Display, for Nonce, .0}
impl Debug for Nonce {
  #[throws(fmt::Error)]
  fn fmt(&self, f: &mut Formatter) {
    write!(f,"Nonce[")?;
    fmt_hex(f, &self.0)?;
    write!(f,"]")?;
  }
}

impl FromStr for KeySpec {
  type Err = anyhow::Error;
  #[throws(anyhow::Error)]
  fn from_str(s: &str) -> KeySpec {
    (||{
      let (id, nonce) = s.split_once(':')
        .ok_or_else(|| anyhow!("missing `:`"))?;
      let id    = id.try_into().context("bad id")?;
      let nonce = nonce.parse().context("bad nonce")?;
      Ok::<_,AE>(KeySpec { id, nonce })
    })().context("failed to parse ssh key spec")?
  }
}

impl FromStr for Nonce {
  type Err = anyhow::Error;
  #[throws(anyhow::Error)]
  fn from_str(s: &str) -> Nonce {
    Nonce(parse_fixed_hex(s).ok_or_else(|| anyhow!("bad nonce syntax"))?)
  }
}

impl PerScope {
  pub fn check(&self, ag: &AccountsGuard, authed_key: &KeySpec,
               auth_in: Authorisation<KeySpec>)
               -> Option<Authorisation<AccountScope>> {
    let gl = &ag.get().ssh_keys;
    for sk in &self.authorised {
      if_chain!{
        if let Some(sk) = sk;
        if sk.id == authed_key.id;
        if let Some(want_key) = gl.keys.get(sk.id);
        if &want_key.nonce == &authed_key.nonce;
        then {
          // We have checked id and nonce, against those allowed
          let auth = auth_in.so_promise();
          return Some(auth);
        }
      }
    }
    None
  }
}

#[derive(Debug,Clone,Serialize,Deserialize)]
pub struct MgmtKeyReport {
  pub key: KeySpec,
  pub data: PubData,
  pub comment: Comment,
  pub problem: Option<KeyError>,
}

impl Display for KeySpec {
  #[throws(fmt::Error)]
  fn fmt(&self, f: &mut fmt::Formatter) {
    write!(f, "{}:{}", self.id, &self.nonce)?;
  }
}

impl Display for MgmtKeyReport {
  #[throws(fmt::Error)]
  fn fmt(&self, f: &mut fmt::Formatter) {
    if let Some(problem) = &self.problem {
      write!(f, "# PROBLEM {} # ", &problem)?;
    }
    write!(f, "{} {} # {}", &self.data, &self.comment, &self.key)?;
  }
}

macro_rules! def_pskeys_get {
  ($trait:ident, $f:ident, $get:ident, $($mut:tt)?) => {
    #[ext(name=$trait)]
    impl DenseSlotMap<AccountId, AccountRecord> {
      #[throws(MgmtError)]
      fn $f(& $($mut)? self, acctid: AccountId) -> & $($mut)? PerScope {
        let record = self.$get(acctid).ok_or(AccountNotFound)?;
        if ! record.account.subaccount.is_empty() {
          throw!(ME::NoSshKeysForSubaccount)
        }
        // We do *not* check that the account is of scope kind Ssh.
        // Installing ssh keys for other scopes is fine.
        // otter(1) will check this.

        & $($mut)? record.ssh_keys
      }
    }
  }
}

def_pskeys_get!{ RecordsExtImm, pskeys_get, get    ,     }
def_pskeys_get!{ RecordsExtMut, pskeys_mut, get_mut, mut }

type Auth = Authorisation<AccountScope>;

impl AccountsGuard {
  #[throws(MgmtError)]
  pub fn sshkeys_report(&self, acctid: AccountId, _:Auth)
                        -> Vec<MgmtKeyReport> {
    let accounts = self.get();
    let gl = &accounts.ssh_keys;
    let ps = &accounts.records.pskeys_get(acctid)?;
    let dirty_error =
      if gl.authkeys_dirty { Some(KeyError::Dirty) }
      else { None };
    ps.authorised.iter().filter_map(|sk| Some({
      let sk = sk.as_ref()?;
      let key = gl.keys.get(sk.id)?;
      let problem = if let Some(Err(ref e)) = key.fp { Some(e) }
                    else { dirty_error.as_ref() };
      MgmtKeyReport {
        key: KeySpec {
        id:      sk.id,
        nonce:   key.nonce.clone(),
        },
        data:    key.data.clone(),
        comment: sk.comment.clone(),
        problem: problem.cloned(),
      }
    }))
      .collect()
  }

  // not a good idea to speicfy a problem, but "whatever"
  #[throws(ME)]
  pub fn sshkeys_add(&mut self, acctid: AccountId,
                     new_akl: AuthkeysLine, _:Auth) -> (usize, Id) {
    let accounts = self.get_mut();
    let gl = &mut accounts.ssh_keys;
    let ps = accounts.records.pskeys_mut(acctid)?;
    let (data, comment) = new_akl.parse()?;
    let fp = data.fingerprint().map_err(KeyError::from)?;
    let fp = Arc::new(fp);
    let _ = gl.fps();
    let fpe = gl.fps.as_mut().unwrap().entry(fp.clone());
    // ABSENT
    let id = {
      let keys = &mut gl.keys;
      *fpe.or_insert_with(||{
        keys.insert(Key {
          data,
          refcount: 0,
          nonce: Nonce(thread_rng().gen()),
          fp: Some(Ok(fp.clone())),
        })
      })
    };
    // **undesirable**
    let key = gl.keys.get_mut(id)
      .ok_or_else(|| internal_error_bydebug(&(id, &fp)))?;
    // GARBAGE
    key.refcount += 1;
    let new_sk = Some(ScopeKey { id, comment });
    let index =
      if let Some((index,_)) = ps.authorised.iter()
          .find_position(|sk| sk.is_none())
      {
        ps.authorised[index] = new_sk;
        index
      } else {
        let index = ps.authorised.len();
        ps.authorised.push(new_sk);
        index
      };
    gl.authkeys_dirty = true;
    // UNSAVED
    self.save_accounts_now()?;
    // BROKEN
    self.get_mut().ssh_keys.rewrite_authorized_keys()?;
    // PRESENT
    (index, id)
  }

  #[throws(ME)]
  pub fn sshkeys_remove(&mut self, acctid: AccountId,
                        index: usize, id: Id, _:Auth) {
    let accounts = self.get_mut();
    let gl = &mut accounts.ssh_keys;
    let ps = accounts.records.pskeys_mut(acctid)?;
    if id == default() {
      throw!(ME::InvalidSshKeyId);
    }
    match ps.authorised.get(index) {
      Some(&Some(ScopeKey{id:tid,..})) if tid == id => { },
      _ => throw!(ME::SshKeyNotFound), /* [ABSEMT..GARBAGE] */
    }
    let key = gl.keys.get_mut(id).ok_or_else(|| internal_logic_error(
      format!("corrupted accounts db: key id {} missing", id)))?;

    // [UNSAVED..PRESENT]
    
    key.refcount -= 1;
    let previously = mem::take(&mut ps.authorised[index]);
    // Now **illegal**, briefly, don't leave it like this!  No-one can
    // observe the illegal state since we have the accounts lock.  If
    // we abort, the in-core version vanishes, leaving a legal state.
    let r = std::panic::catch_unwind(std::panic::AssertUnwindSafe(
      || self.save_accounts_now()
    ));

    // Must re-borrow everything since save_accounts_now needed a
    // reference to all of it.
    let accounts = self.get_mut();
    let gl = &mut accounts.ssh_keys;
    let ps = accounts.records.pskeys_mut(acctid).expect("aargh!");
    let key = gl.keys.get_mut(id).expect("aargh!");

    if ! matches!(r, Ok(Ok(()))) {
      key.refcount += 1;
      ps.authorised[index] = previously;
      // [UNSAVED..PRESENT]
      match r {
        Err(payload) => std::panic::resume_unwind(payload),
        Ok(Err(e)) => throw!(e),
        Ok(Ok(())) => panic!(), // handled that earlier
      }
    }
    // [ABSENT..GARBAGE]

    if key.refcount == 0 {
      let key = gl.keys.remove(id).unwrap();
      if let Some(Ok(ref fp)) = key.fp {
        gl.fps().remove(fp);
      }
    }
    // ABSENT
  }
}

impl Global {
  fn fps(&mut self) -> &mut FingerprintMap {
    let keys = &mut self.keys;
    self.fps.get_or_insert_with(||{

      let mut fps = FingerprintMap::default();
      let mut garbage = vec![];

      for (id, key) in keys.iter_mut() {
        if key.refcount == 0 { garbage.push(id); continue; }

        if_let!{
          Ok(fp) = {
            let data = &key.data;
            key.fp.get_or_insert_with(
              || Ok(Arc::new(data.fingerprint()?))
            )
          };
          else continue;
        }

        use hash_map::Entry::*;
        match fps.entry(fp.clone()) {
          Vacant(ve) => { ve.insert(id); },
          Occupied(mut oe) => {
            error!("ssh key fingerprint collision! \
                    fp={} newid={} oldid={} newdata={:?}",
                   &fp, id, oe.get(), &key.data);
            oe.insert(Id::default());
          },
        }
      }

      for id in garbage { keys.remove(id); }

      fps

    })
  }

  #[throws(AuthKeysManipError)]
  fn write_keys(&self, w: &mut BufWriter<File>) {
    for (id, key) in &self.keys {
      let fp = match key.fp { Some(Ok(ref fp)) => fp, _ => continue };
      if key.refcount == 0 { continue }
      writeln!(w, r#"{},command="{} mgmtchannel-proxy --restrict-ssh {}:{}" \
                     {} {}:{}"#, 
               RESTRICTIONS,
               &config().ssh_proxy_bin, id, key.nonce,
               &key.data,
               key.refcount, &fp)
        .context("write new auth keys")?;
    }
  }

  // Caller should make sure accounts are saved first, to avoid
  // getting the authkeys_dirty bit wrong.
  #[throws(AuthKeysManipError)]
  fn rewrite_authorized_keys(&mut self) {
    let config = config();
    let path = &config.authorized_keys;
    let tmp = format!("{}.tmp", &path);

    (||{
      let f = File::open(path).context("open")?;
      let l = BufReader::new(f).lines().next()
        .ok_or_else(|| anyhow!("no first line!"))?
        .context("read first line")?;
      if l != MAGIC_BANNER {
        throw!(anyhow!(
          "first line is not as expected (manually written/edited?)"
        ));
      }
      Ok::<_,AE>(())
    })()
      .with_context(|| path.clone())
      .context("check authorized_keys magic/banner")?;

    let mut f = fs::OpenOptions::new()
      .write(true).truncate(true).create(true)
      .mode(0o644)
      .open(&tmp)
      .with_context(|| tmp.clone()).context("open new auth keys file")?;

    let include = &config.authorized_keys_include;

    (||{
      let mut f = BufWriter::new(&mut f);
      write!(f, "{}", MAGIC_BANNER)?;
      writeln!(f, "# YOU MAY EDIT {:?} INSTEAD - THAT IS INCLUDED HERE",
               include)?;
      f.flush()?;
      Ok::<_,io::Error>(())
    })().with_context(|| tmp.clone()).context("write header")?;

    if let Some(mut sf) = match File::open(include) {
      Ok(y) => Some(y),
      Err(e) if e.kind() == ErrorKind::NotFound => None,
      Err(e) => throw!(AE::from(e).context(include.clone())
                       .context("open static auth keys")),
    } {
      io::copy(&mut sf, &mut f).context("copy data into new auth keys")?;
    }

    let mut f = BufWriter::new(f);
    self.write_keys(&mut f)?;
    f.flush().context("finish writing new auth keys")?;

    fs::rename(&tmp, &path).with_context(|| path.clone())
      .context("install new auth keys")?;

    self.authkeys_dirty = false;
  }
}
