// Copyright 2020 Ian Jackson
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use crate::imports::*;

use parking_lot::{RwLock, const_rwlock,
                  MappedRwLockReadGuard, MappedRwLockWriteGuard};

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
pub struct AccountName {
  pub scope: AccountScope,
  pub subaccount: String,
}

impl AccountScope {
  /// Return value is parseable and filesystem- and html-safe
  #[throws(E)]
  pub fn display_name<'out,
                  NS: IntoIterator<Item=&'out &'out str>,
                  E,
                  F: FnMut(&str) -> Result<(),E>
                  >
    (&'out self, ns: NS, f: F)
  {
    const ENCODE : percent_encoding::AsciiSet =
      percent_encoding::NON_ALPHANUMERIC.remove(b':');

    let mut out = String::new();
    match &self {
      AS::Server => {
        f("server")?;
      },
      AS::Unix { user } => {
        f("unix")?;
        f(":")?;
        f(user)?;
      },
    };
    for n in ns {
      for frag in utf8_percent_encode(n, &ENCODE) {
        f(frag)?;
      }
    }
  }

  #[throws(InvalidScopedName)]
  pub fn parse_name(s: &str, names_out: &mut [String]) -> AccountScope {
    let mut split = s.split(':');

    let scope = {
      let next = ||{
        split.next()
          .ok_or(InvalidScopedName::MissingScopeComponent)
      };
      let kind = next()?;
      match kind {
        "server" => {
          AccountScope::Server
        },
        "unix" => {
          let user = next()?.to_owned();
          AccountScope::Unix { user }
        },
        _ => {
          throw!(InvalidScopedName::UnknownScopeKind)
        },
      }
    };

    for eb in names_out.iter_mut().zip_longest(split) {
      use EitherOrBoth::*;
      match eb {
        Both(out, got) => {
          *out = percent_decode_str(got)
            .decode_utf8()
            .map_err(|_| InvalidScopedName::BadUTF8)?
            .into();
        },
        Left(_out) => throw!(InvalidScopedName::TooFewComponents),
        Right(_got) => throw!(InvalidScopedName::TooFewComponents),
      };
    }
    scope
  }
}

impl Display for AccountName {
  #[throws(fmt::Error)]
  /// Return value is parseable, and filesystem- and html-safeb
  fn fmt(&self, f: &mut fmt::Formatter) {
    self.scope.display_name(&[ self.subaccount.as_str() ], |s| f.write_str(s))
  }
}

#[derive(Error,Debug)]
pub enum InvalidScopedName {
  #[error("Unknown scope kind (expected unix or server)")]
  UnknownScopeKind,
  #[error("Missing scope component (scope scheme, or scope element)")]
  MissingScopeComponent,
  #[error("Too few components for scope")]
  TooFewComponents,
  #[error("Too many components for scope")]
  TooManyComponents,
  #[error("bad (percent-encoded) UTF-8")]
  BadUTF8,
}

impl FromStr for AccountName {
  type Err = InvalidScopedName;

  #[throws(InvalidScopedName)]
  fn from_str(s: &str) -> Self {
    let subaccount = default();
    let names = std::slice::from_mut(&mut subaccount);
    let scope = AccountScope::parse_name(s, names)?;
    AccountName { scope, subaccount }
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

  pub trait Perm : FromPrimitive + ToPrimitive +
    Copy + Eq + Hash + Sync + Send + 'static
  {
    type Auth;
    const NOT_FOUND : MgmtError;
    const TEST_EXISTENCE : Self;
  }

  #[derive(Copy,Clone,Debug)]
  pub struct PermSet<P: Perm> (u64, PhantomData<&'static P>);

  #[derive(Debug,Clone)]
  pub struct EffectiveAcl<'i, P: Perm> {
    pub owner_account: Option<&'i str>,
    pub acl: &'i LoadedAcl<P>,
  }

  #[derive(Debug,Clone)]
  #[derive(Serialize,Deserialize)]
  #[serde(from="Acl")]
  #[serde(into="Acl")]
  pub struct LoadedAcl<P: Perm> (Vec<LoadedAclEntry<P>>);

  impl<P:Perm> Default for LoadedAcl<P> {
    fn default() -> Self { LoadedAcl(default()) }
  }

  #[derive(Debug,Clone)]
  struct LoadedAclEntry<P: Perm> {
    pat: glob::Pattern,
    allow: PermSet<P>,
    deny: PermSet<P>,
    ptype: PhantomData<&'static P>,
  }

  #[derive(Debug)]
  struct AclEntryRef<'r, P: Perm> {
    pat: Either<&'r str, &'r glob::Pattern>,
    allow: u64,
    deny: u64,
    ptype: PhantomData<&'static P>,
  }

  impl<'e, P:Perm> EffectiveAcl<'e, P> {
    fn entries(&self) -> impl Iterator<Item=AclEntryRef<'_, P>> {
      self.owner_account.iter().map(
        |owner|
        AclEntryRef { pat: Left(owner), allow: !0, deny: 0,
                      ptype: PhantomData }
      ).chain(self.acl.0.iter().map(
        |&LoadedAclEntry { ref pat, ref allow, ref deny, ptype }|
        AclEntryRef { pat: Right(pat), allow: allow.0, deny: deny.0, ptype }
      ))
    }

    #[throws(MgmtError)]
    pub fn check(&self, subject: &str, p: PermSet<P>)
                 -> Authorisation<P::Auth> {
      let mut needed = p.0;
      assert!(needed != 0);
      let test_existence = P::test_existence().to_primitive();
      needed |= test_existence;
      for AclEntryRef { pat, allow, deny } in self.entries() {
        if !match pat {
          Left(owner) => owner == subject,
          Right(pat) => pat.matches(subject),
        } { continue }
        if needed & deny != 0 { break }
        if allow != 0 { needed &= !(allow | test_existence) }
        if needed == 0 { return Ok(Authorisation::authorise_any()) }
      }
      Err(if needed & test_existence != 0 {
        P::NOT_FOUND
      } else {
        MgmtError::PermissionDenied
      })
    }
  }

  impl<'i, P:Perm, I> From<I> for PermSet<P>
  where I: IntoIterator<Item=&'i P>
  {
    fn from(i: I) -> Self {
      i.into_iter().fold(0, |b, i| b | i.to_u64().unwrap())
    }
  }

  fn unpack<P:Perm>(unpacked: PermSet<P>) -> HashSet<P> {
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
