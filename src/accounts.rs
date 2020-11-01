// Copyright 2020 Ian Jackson
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use crate::imports::*;

use parking_lot::{Mutex, const_mutex, MutexGuard};

slotmap::new_key_type!{
//  #[derive(Serialize,Deserialize,Debug)] xxx
  pub struct AccountId;
}

#[derive(Debug,Clone,Deserialize,Serialize)]
#[derive(Eq,PartialEq,Ord,PartialOrd,Hash)]
pub enum AccountScope {
  Server,
  Unix { user : String },
}

type AS = AccountScope;
type ME = MgmtError;

#[derive(Error,Debug,Clone,Copy,Serialize,Deserialize)]
#[derive(Hash,Ord,Eq,PartialOrd,PartialEq)]
#[error("Account not found")]
pub struct AccountNotFound;

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

#[derive(Serialize,Deserialize,Debug)]
pub struct AccountRecord {
  pub account: Arc<AccountName>,
  pub nick: String,
  pub timezone: String,
  pub tokens_revealed: HashMap<Html, TokenRevelation>,
  pub access: AccessRecord,
}

#[derive(Serialize,Deserialize,Debug)]
#[serde(transparent)]
pub struct AccessRecord (Arc<dyn PlayerAccessSpec>);

#[derive(Copy,Clone,Debug,Ord,PartialOrd,Eq,PartialEq)]
#[derive(Serialize,Deserialize)]
pub struct TokenRevelation {
  pub latest: Timestamp,
  pub earliest: Timestamp,
}

#[derive(Debug,Default)]
#[derive(Serialize,Deserialize)]
struct Accounts {
  names: HashMap<Arc<AccountName>, AccountId>,
  records: DenseSlotMap<AccountId, AccountRecord>,
}

static ACCOUNTS : Mutex<Option<Accounts>> = const_mutex(None);

#[derive(Debug)]
pub struct AccountsGuard (MutexGuard<'static, Option<Accounts>>);

impl Deref for AccessRecord {
  type Target = Arc<dyn PlayerAccessSpec>;
  fn deref(&self) -> &Self::Target { return &self.0 }
}

impl AccessRecord {
  pub fn new_unset() -> Self{ Self( Arc::new(PlayerAccessUnset) ) }
}

impl From<Box<dyn PlayerAccessSpec>> for AccessRecord {
  fn from(spec: Box<dyn PlayerAccessSpec>) -> Self { Self(spec.into()) }
}

// xxx load, incl reveleation expiry
// xxx periodic token reveleation expiry

trait AccountNameOrId : Copy {
  fn initial_lookup(self, accounts: &Accounts) -> Option<AccountId>;
}

impl<'n> AccountNameOrId for &'n AccountName {
  #[throws(as Option)]
  fn initial_lookup(self, accounts: &Accounts) -> AccountId {
    *accounts.names.get(self)?
  }
}

impl AccountNameOrId for AccountId {
  #[throws(as Option)]
  fn initial_lookup(self, _: &Accounts) -> AccountId { self }
}

impl AccountsGuard {
  pub fn lock() -> Self { Self(ACCOUNTS.lock()) }

  #[throws(AccountNotFound)]
  pub fn check<K:AccountNameOrId>(
    &self,
    key: K
  ) -> AccountId {
    (||{
      let accounts = self.0.as_ref()?;
      let acctid = key.initial_lookup(accounts)?;
      let _record = accounts.records.get(acctid)?;
      Some(acctid)
    })().ok_or(AccountNotFound)?
  }

  pub fn bulk_check<K:AccountNameOrId>(
    &self,
    keys: &[K]
  ) -> Vec<Option<AccountId>> {
    keys.iter().map(|&key| self.check(key).ok()).collect()
  }

  #[throws(AccountNotFound)]
  pub fn lookup<K: AccountNameOrId>(&self, key: K)
                                    -> (&AccountRecord, AccountId)
  {
    (||{
      let accounts = self.0.as_ref()?;
      let acctid = key.initial_lookup(accounts)?;
      Some((accounts.records.get(acctid)?, acctid))
    })().ok_or(AccountNotFound)?
  }

  #[throws(as Option)]
  pub fn _lookup_mut<K: AccountNameOrId>(&mut self, key: K,
                                         _auth: Authorisation<AccountName>)
                                         -> (&mut AccountRecord, AccountId)
  {
    let accounts = self.0.as_mut()?;
    let acctid = key.initial_lookup(accounts)?;
    (accounts.records.get_mut(acctid)?, acctid)
  }

  #[throws(AccountNotFound)]
  pub fn lookup_mut_caller_must_save<K: AccountNameOrId>(
    &mut self,
    key: K, auth: Authorisation<AccountName>
  ) -> (&mut AccountRecord, AccountId)
  {
    self._lookup_mut(key, auth).ok_or(AccountNotFound)?
  }

  #[throws(MgmtError)]
  pub fn with_entry_mut
    <T,
     K: AccountNameOrId,
     F: FnOnce(&mut AccountRecord, AccountId) -> T
     >(
      &mut self,
      key: K,
      auth: Authorisation<AccountName>,
      set_access: Option<Arc<dyn PlayerAccessSpec>>,
      f: F
    )
    -> Result<T, (InternalError, T)>
  {
    let (entry, acctid) = self.lookup_mut_caller_must_save(key, auth)?;

    if let Some(new_access) = set_access {
      process_all_players_for_account(
        acctid,
        |ig, player| ig.invalidate_tokens(player)
      )?;
      entry.access = AccessRecord(new_access);
    }      
    let output = f(&mut *entry, acctid);
    let ok = self.save_accounts_now();
    match ok {
      Ok(()) => Ok(output),
      Err(e) => Err((e, output))
    }
  }

  #[throws(MgmtError)]
  pub fn insert_entry(&mut self,
                      new_record: AccountRecord,
                      _auth: Authorisation<AccountName>)
  {
    use hash_map::Entry::*;
    let accounts = self.0.get_or_insert_with(default);
    let name_entry = accounts.names.entry(new_record.account.clone());
    if_chain!{
      if let Occupied(oe) = name_entry;
      let acctid = *oe.get();
      if let Some(old_record) = accounts.records.get_mut(acctid);
      then {
        *old_record = new_record;
      } else {
        let acctid = accounts.records.insert(new_record);
        match name_entry {
          Vacant(ve) => { ve.insert(acctid); }
          Occupied(oe) => { oe.insert(acctid); }
        }
      }
    }
    self.save_accounts_now()?;
  }

  #[throws(MgmtError)]
  pub fn remove_entry(&mut self,
                      account: &AccountName,
                      _auth: Authorisation<AccountName>)
  {
    let (accounts, acctid) = if_chain! {
      if let Some(accounts) = self.0.as_mut();
      if let Some(&acctid) = accounts.names.get(account);
      then { (accounts, acctid) }
      else { throw!(AccountNotFound) }
    };
    process_all_players_for_account(acctid, |ig,player| {
      ig.player_remove(player)?;
      Ok::<_,ME>(())
    })?;
    accounts.names.remove(account);
    accounts.records.remove(acctid);
    self.save_accounts_now()?;
  }

  pub fn save_accounts_now(&self) -> Result<(), InternalError> {
    panic!("xxx")
  }
}

impl AccountRecord {
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
  #[serde(from="Acl<P>")]
  #[serde(into="Acl<P>")]
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
      let test_existence = P::TEST_EXISTENCE.to_u64().unwrap();
      needed |= test_existence;
      for AclEntryRef { pat, allow, deny, .. } in self.entries() {
        if !match pat {
          Left(owner) => owner == subject,
          Right(pat) => pat.matches(subject),
        } { continue }
        if needed & deny != 0 { break }
        if allow != 0 { needed &= !(allow | test_existence) }
        if needed == 0 { return Authorisation::authorise_any() }
      }
      Err(if needed & test_existence != 0 {
        P::NOT_FOUND
      } else {
        MgmtError::AuthorisationError
      })?
    }
  }

  impl<'i,
       P: Perm,
       II: Borrow<P>,
       I: IntoIterator<Item=II>
       >
    From<I> for PermSet<P>
  {
    fn from(i: I) -> Self {
      PermSet(
        i.into_iter().fold(0, |b, i| b | i.borrow().to_u64().unwrap()),
        PhantomData,
      )
    }
  }

  fn unpack<P:Perm>(packed: &PermSet<P>) -> HashSet<P> {
    let mut s = HashSet::new();
    for n in 0.. {
      let v = match FromPrimitive::from_u64(n) { Some(v) => v, None => break };
      if packed.0 & n != 0 { s.insert(v); }
    }
    s
  }

  impl<P:Perm> From<Acl<P>> for LoadedAcl<P> {
    fn from(acl: Acl<P>) -> LoadedAcl<P> {
      (&acl).into()
    }
  }

  impl<P:Perm> From<&Acl<P>> for LoadedAcl<P> {
    fn from(acl: &Acl<P>) -> LoadedAcl<P> {
      let ents = acl.ents.into_iter().filter_map(
        |AclEntry { account_glob, allow, deny }|
        {
          let pat = glob::Pattern::new(&account_glob)
            .ok().or_else(||{
              eprintln!("discarding malformed acl glob pattern {:?}",
                        account_glob);
              None
            })?;
          Some(LoadedAclEntry {
            pat,
            allow: allow.into(),
            deny: deny.into(),
            ptype: PhantomData,
          })
        }
      ).collect();
      LoadedAcl(ents)
    }
  }

  impl<P:Perm> From<LoadedAcl<P>> for Acl<P> {
    fn from(acl: LoadedAcl<P>) -> Acl<P> {
      (&acl).into()
    }
  }

  impl<P:Perm> From<&LoadedAcl<P>> for Acl<P> {
    fn from(acl: &LoadedAcl<P>) -> Acl<P> {
      let LoadedAcl(ents) = acl;
      Acl { ents: ents.into_iter().map(
        |LoadedAclEntry { pat, allow, deny, .. }|
        AclEntry {
          account_glob: pat.to_string(),
          allow: unpack(allow),
          deny: unpack(deny),
        }
      ).collect() }
    }
  }
}
