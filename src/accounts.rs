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
  latest: Timestamp,
  earliest: Timestamp,
}

static ACCOUNTS : RwLock<Option<HashMap<AccountName, AccountRecord>>>
  = const_rwlock(None);

impl AccountRecord {
  fn lookup(account: AccountName)
            -> Option<MappedRwLockReadGuard<'static, AccountRecord>> {
    ACCOUNTS.read().map(
      |accounts| accounts?.get(account)
    )
  }
  fn lookup_mut(account: AccountName)
            -> Option<MappedRwLockWriteGuard<'static, AccountRecord>> {
    ACCOUNTS.write().map(
      |accounts| accounts?.get(account)
    )
  }
}
