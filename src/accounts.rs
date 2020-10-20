// Copyright 2020 Ian Jackson
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use crate::imports::*;

use parking_lot::{RwLock, const_rwlock,
                  MappedRwLockReadGuard, MappedRwLockWriteGuard};

pub type AccountName = ScopedName;

#[derive(Debug,Clone,Deserialize,Serialize)]
#[derive(Eq,PartialEq,Ord,PartialOrd,Hash)]
pub enum AccountScope {
  Server,
  Unix { user : String },
}

type AS = AccountScope;

#[derive(Debug,Clone)]
#[derive(Eq,PartialEq,Ord,PartialOrd,Hash)]
#[derive(DeserializeFromStr,SerializeDisplay)]
pub struct ScopedName {
  pub scope: AccountScope,
  pub scoped_name: String,
}

impl Display for ScopedName {
  #[throws(fmt::Error)]
  /// Return value is parseable but not filesystem- or html-safe
  fn fmt(&self, f: &mut fmt::Formatter) {
    match &self.scope {
      AS::Server => {
        write!(f, "server")?
      },
      AS::Unix { user } => {
        assert_eq!(user.find('.'), None);
        write!(f, "unix:{}", user)?;
      },
    };
    match self.scoped_name.as_str() {
      "" => {},
      suffix => write!(f, ":{}", &suffix)?,
    };
  }
}

#[derive(Error,Debug)]
pub enum InvalidScopedName {
  #[error("Unknown scope kind (expected unix or server)")]
  UnknownScopeKind,
}

impl FromStr for ScopedName {
  type Err = InvalidScopedName;

  #[throws(InvalidScopedName)]
  fn from_str(s: &str) -> Self {
    let (kind, rhs) = s.split_at_delim(':');
    let (scope, scoped_name) = match kind {
      "server" => {
        (AccountScope::Server, rhs)
      },
      "unix" => {
        let (user, rhs) = s.split_at_delim(':');
        let user = user.to_owned();
        (AccountScope::Unix { user }, rhs)
      },
      _ => {
        throw!(InvalidScopedName::UnknownScopeKind)
      },
    };
    let scoped_name = scoped_name.to_string();
    ScopedName { scope, scoped_name }
  }
}

#[derive(Serialize,Deserialize)]
pub struct AccountRecord {
  pub nick: String,
  pub timezone: String,
  pub access: Arc<dyn PlayerAccessSpec>,
  pub tokens_revealed: HashMap<Html, TokenRevelation>,
}

#[derive(Copy,Clone,Debug,Ord,PartialOrd,Eq,PartialEq)]
pub struct TokenRevelation {
  pub latest: Timestamp,
  pub earliest: Timestamp,
}

static ACCOUNTS : RwLock<Option<HashMap<AccountName, AccountRecord>>>
  = const_rwlock(None);

// xxx load, incl reveleation expiry
// xxx periodic token reveleation expiry

pub fn save_accounts_now() -> Result<(), InternalError> {
  panic!("xxx")
}

impl AccountRecord {
  pub fn lookup(account: &AccountName,  _: Authorisation<AccountName>)
            -> Option<MappedRwLockReadGuard<'static, AccountRecord>> {
    ACCOUNTS.read().map(
      |accounts| accounts?.get(account)
    )
  }
  pub fn lookup_mut_caller_must_save(account: &AccountName,
                                      _: Authorisation<AccountName>)
            -> Option<MappedRwLockWriteGuard<'static, AccountRecord>> {
    ACCOUNTS.write().map(
      |accounts| accounts?.get(account)
    )
  }
  pub fn with_entry_mut<T, F>(account: &AccountName,
                              _: Authorisation<AccountName>,
                              f: F)
                              -> Result<T, (InternalError, T)>
  where F: FnOnce(Option<&mut AccountRecord>) -> T
  {
    let entry = AccountRecord::lookup_mut_caller_must_save(account);
    let output = f(*entry);
    let ok = if entry.is_some() { save_accounts_now() } else { Ok(()) };
    match ok {
      Ok(()) => Ok(output),
      Err(e) => Err((e, output))
    }
  }

  pub fn expire_tokens_revealed(&mut self) {
    panic!("xxx")
  }
}

//---------- acl handling ----------

pub mod loaded_acl {
  use crate::imports::*;

  pub trait Perm : FromPrimitive + ToPrimitive
    + Copy + Eq + Hash + Sync + Send { }

  #[derive(Copy,Clone,Debug)]
  pub struct PermSet<P: Perm> (u64, PhantomData<&'static P>);

  #[derive(Debug,Clone)]
  pub struct EffectiveAcl<'i, P: Perm> {
    pub owner_account: Option<&'i str>,
    pub acl: LoadedAcl<P>,
  }

  #[derive(Debug,Clone)]
  pub struct LoadedAcl<P: Perm> (Vec<LoadedAclEntry<P>>);

  #[derive(Debug,Clone)]
  struct LoadedAclEntry<P: Perm> {
    pat: glob::Pattern,
    allow: Bitmap,
    deny: Bitmap,
    ptype: PhantomData<&'static P>,
  }

  #[derive(Debug)]
  struct AclEntryRef<'r, P: Perm> {
    pat: Either<&'r str, &'r glob::Pattern>,
    allow: u64,
    deny: u64,
    ptype: PhantomData<&'static P>,
  }

  impl<P:Perm> LoadedAcl<P> {
    fn entries(&'s self) -> impl Iterator<Item=AclEntryRef<'s>> {
      self.owner_account.map(
        |owner|
        AclEntryRef { pat: Left(owner), allow: !0, deny: 0, ptype }
      ).chain(self.entries.map(
        |LoadedAclEntry { pat, allow, deny }|
        AclEntryRef { pat: Left(pat), allow: allow.0, deny: deny.0 }
      ))
    }

    #[throws(MgmtError)]
    fn check(&self, account_name: &str, p: PermSet<P>) {
      let mut needed = p.0;
      assert!(needed != 0);
      for AclEntryRef { pat, allow, deny } in self.entries() {
        if !match pat {
          Left(owner) => owner == account_name,
          Right(pat) => pat.matches(account_name),
        } { continue }
        if needed & deny != 0 { break }
        needed &= !allow;
        if needed == 0 { return Ok(()) }
      }
      Err(ME::PermissionDenied)
    }
  }

  impl<P:Perm> From<I> for PermSet<P> where I: IntoIterator<Item=&P> {
    fn from(i: I) -> Self {
      i.into_iter().fold(0, |b, i| b | i.to_u64().unwrap())
    }
  }

  fn unpack<P:Perm>(unpacked: Bitmap) -> HashSet<P> {
    let mut s = HashSet::new();
    for n in 0.. {
      let v = match FromPrimitive::from_u64(n) { Some(v) => v, None => break };
      if unpacked & n != 0 { s.insert(v) }
    }
    s
  }

  impl<P:Perm> From<Acl<P>> for LoadedAcl<P> {
    fn from(acl: Acl<P>) -> LoadedAcl<P> {
      let ents = acl.into_iter().map(
        |AclEntry { account_glob, allow, deny }|
        LoadedAclEntry { account_glob, allow: allow.into(), deny: deny.into() }
      ).collect();
      LoadedAcl(ents)
    }
  }

  impl<P:Perm> From<LoadedAcl<P>> for Acl<P> {
    fn from(acl: LoadedAcl<P>) -> Acl<P> {
      let LoadedAcl(ents) = acl;
      ents.into_iter().map(
        |LoadedAclEntry { account_glob, allow, deny }|
        AclEntry { account_glob, allow: unpack(allow), deny: unpack(deny) }
      ).collect()
    }
  }
}
