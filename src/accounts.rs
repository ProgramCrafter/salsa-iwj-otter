// Copyright 2020 Ian Jackson
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use crate::imports::*;

use parking_lot::{RwLock, const_rwlock,
                  RwLockReadGuard, RwLockWriteGuard,
                  MappedRwLockReadGuard, MappedRwLockWriteGuard};

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

#[derive(Serialize,Deserialize,Debug)]
pub struct AccountRecord {
  pub nick: String,
  pub timezone: String,
  pub tokens_revealed: HashMap<Html, TokenRevelation>,
  access: Arc<dyn PlayerAccessSpec>,
}

#[derive(Copy,Clone,Debug,Ord,PartialOrd,Eq,PartialEq)]
#[derive(Serialize,Deserialize)]
pub struct TokenRevelation {
  pub latest: Timestamp,
  pub earliest: Timestamp,
}

#[derive(Debug,Default)]
#[derive(Serialize,Deserialize)]
struct Accounts {
  names: HashMap<AccountName, AccountId>,
  records: DenseSlotMap<AccountId, AccountRecord>,
}

static ACCOUNTS : RwLock<Option<Accounts>> = const_rwlock(None);

// xxx load, incl reveleation expiry
// xxx periodic token reveleation expiry

pub fn save_accounts_now() -> Result<(), InternalError> {
  panic!("xxx")
}

#[derive(Default,Debug)]
struct LookupHelper {
  acctid_r: Option<AccountId>,
}

impl LookupHelper {
  #[throws(as Option)]
  fn get(&mut self, accounts: &Accounts, account: &AccountName) -> AccountId {
    let acctid = *accounts.names.get(account)?;
    self.acctid_r = Some(acctid);
    acctid
  }
/*
  fn finish<T, E>(self) -> impl FnOnce(<T,E>) -> Option<(T, AccountId)>
  {
    move |got: Result<T,E>| Some((got.ok()?, self.acctid_r?))
  }
*/
  fn wrap<T, E>(self, got: Result<T,E>) -> Option<(T, AccountId)> {
    Some((got.ok()?, self.acctid_r?))
  }
}

impl AccountRecord {
  pub fn bulk_check(names: &[&AccountName]) -> Vec<Option<AccountId>> {
    let accounts = ACCOUNTS.read();
    names.iter().map(|&name|{
      accounts.as_ref()?.names.get(name).map(|&acctid| acctid)
    }).collect()
  }

  pub fn lookup(account: &AccountName,  _: Authorisation<AccountName>)
                -> Option<(MappedRwLockReadGuard<'static, AccountRecord>,
                           AccountId)>
  {
    let mut helper : LookupHelper = default();
    helper.wrap(
      RwLockReadGuard::try_map(ACCOUNTS.read(), |accounts| {
        let accounts = accounts.as_ref()?;
        let acctid = helper.get(accounts, account)?;
        accounts.records.get(acctid)
      })
    )
  }

  pub fn lookup_mut_caller_must_save
    ( account: &AccountName, _auth: Authorisation<AccountName>)
    -> Option<(MappedRwLockWriteGuard<'static, AccountRecord>,
               AccountId)>
  {
    let mut helper : LookupHelper = default();
    helper.wrap(
      RwLockWriteGuard::try_map(ACCOUNTS.write(), |accounts| {
        let accounts = accounts.as_mut()?;
        let acctid = helper.get(accounts, account)?;
        accounts.records.get_mut(acctid)
      })
    )
  }

  #[throws(MgmtError)]
  pub fn with_entry_mut<T, F>(account: &AccountName,
                              auth: Authorisation<AccountName>,
                              set_access: Option<Arc<dyn PlayerAccessSpec>>,
                              f: F)
                              -> Result<T, (InternalError, T)>
  where F: FnOnce(&mut AccountRecord, AccountId) -> T
  {
    let (entry, acctid)
      = AccountRecord::lookup_mut_caller_must_save(account, auth)
      .ok_or(MgmtError::AccountNotFound)?;

    if let Some(new_access) = set_access() {
      invalidate_all_tokens_for_account(acctid)?;
      entry.access = new_access;
    }      
    let output = f(&mut *entry, acctid);
    let ok = save_accounts_now();
    match ok {
      Ok(()) => Ok(output),
      Err(e) => Err((e, output))
    }
  }

  #[throws(MgmtError)]
  pub fn insert_entry(account: AccountName,
                      _auth: Authorisation<AccountName>,
                      new_record: AccountRecord)
  {
    let accounts = ACCOUNTS.write();
    use hash_map::Entry::*;
    let accounts = accounts.get_or_insert_with(default);
    let name_entry = accounts.names.entry(account);
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
    save_accounts_now()?;
  }

  #[throws(MgmtError)]
  pub fn remove_entry(account: &AccountName,
                      _auth: Authorisation<AccountName>)
  {
    let accounts = ACCOUNTS.write();
    let oe = if_chain! {
      if let Some(accounts) = accounts.as_mut();
      let entry = accounts.names.entry(account);
      if let hash_map::Entry::Occupied(oe) = entry;
      then { oe }
      else { throw!(AccountNotFound) }
    };
    let acctid = *oe.key();
    process_all_players_for_account(acctid, InstanceGuard::player_remove)?;
    oe.remove();
    accounts.records.remove(acctid);
    save_accounts_now()?;
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

  fn unpack<P:Perm>(packed: PermSet<P>) -> HashSet<P> {
    let mut s = HashSet::new();
    for n in 0.. {
      let v = match FromPrimitive::from_u64(n) { Some(v) => v, None => break };
      if packed.0 & n != 0 { s.insert(v); }
    }
    s
  }

  impl<P:Perm> From<Acl<P>> for LoadedAcl<P> {
    fn from(acl: Acl<P>) -> LoadedAcl<P> {
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
