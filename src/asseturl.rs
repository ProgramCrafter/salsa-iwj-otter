// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use crate::prelude::*;

use bundles::{DigestWrite, Digester};

//---------- public types ----------

#[derive(Clone,Serialize,Deserialize)]
pub enum AssetUrlKey {
  Dummy,
  Y(AssetUrlKeyRaw),
}

#[derive(Error,Debug,Copy,Clone,Serialize)]
pub struct BadAssetUrlToken;
display_as_debug!{BadAssetUrlToken}

#[derive(Clone)]
pub struct AssetUrlToken(AssetUrlTokenRaw);

//---------- private types ----------

type AssetUrlKeyRaw = [u8; 32];

//---------- primary functionality ----------

impl AssetUrlKey {
  pub fn token<V>(&self, what: &str, v: V) -> AssetUrlToken
  where V: Serialize {
    let k = match self {
      AssetUrlKey::Y(k) => k,
      _ => panic!("dummy AssetUrlKey being used!"),
    };
    let mut dw = DigestWrite::sink();
    write!(dw, "{}\0", what).unwrap();
    dw.write(&k[..]).unwrap();
    rmp_serde::encode::write(&mut dw, &v).expect("serialize failed!");
    AssetUrlToken(dw.finish().0)
  }

  #[throws(BadAssetUrlToken)]
  pub fn check<V>(&self, what: &str, v: &V, got: &AssetUrlToken)
                  -> Authorisation<V>
  where V: Serialize {
    let exp = self.token(what, v);
    if ! bool::from(ConstantTimeEq::ct_eq(
      &exp.0[..],
      &got.0[..],
    )) { throw!(BadAssetUrlToken) }
    else { Authorisation::promise_for(v) }
  }
}

//---------- AssetUrlKey impl's ----------

impl Debug for AssetUrlKey {
  #[throws(fmt::Error)]
  fn fmt(&self, f: &mut Formatter) {
    use AssetUrlKey::*;
    match self {
      Y(_) => write!(f, "AssetUrlKey::Y{{..}}")?,
      Dummy => write!(f, "AssetUrlKey::Dummy")?,
    }
  }
}

impl Default for AssetUrlKey { fn default() -> Self { Self::Dummy } }

impl AssetUrlKey {
  #[throws(IE)]
  pub fn new_random() -> AssetUrlKey {
    let mut buf: AssetUrlKeyRaw = default();
    let mut rng: rand::rngs::ThreadRng = thread_rng();
    rand::RngCore::try_fill_bytes(&mut rng, &mut buf)
      .context("generate new AssetUrlKey")?;
    AssetUrlKey::Y(buf)
  }
}

//---------- AssetUrlToken impl's ----------

type AssetUrlTokenRaw = digest::Output<Digester>;

impl Debug for AssetUrlToken {
  #[throws(fmt::Error)]
  fn fmt(&self, f: &mut Formatter) { write!(f, "AssetUrlToken{{..}}")?; }
}

impl Display for AssetUrlToken {
  #[throws(fmt::Error)]
  fn fmt(&self, f: &mut Formatter) {
    f.write_str(&base64::encode_config(&self.0, base64::URL_SAFE_NO_PAD))?
  }
}
impl FromStr for AssetUrlToken {
  type Err = BadAssetUrlToken;
  #[throws(BadAssetUrlToken)]
  fn from_str(s: &str) -> Self {
    let mut buf: AssetUrlTokenRaw = default();
    let l = base64::decode_config_slice(
      s.as_bytes(), base64::URL_SAFE_NO_PAD, &mut buf)
      .map_err(|_| BadAssetUrlToken)?;
    if l != buf.len() { throw!(BadAssetUrlToken) }
    AssetUrlToken(buf)
  }
}
hformat_as_display!{AssetUrlToken}
