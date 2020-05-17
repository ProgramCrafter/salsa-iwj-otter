
pub trait Piece {
}

#[derive(Debug)]
pub struct PieceRecord {
  x : Coord,
  y : Coord,
  p : Box<dyn Piece>,
  held : Option<PlayerRef>,
}

#[derive(Debug)]
pub struct GameState {
  gen : Counter,
  data : GameStateData,
  clients,
}


#[derive(Debug)]
pub struct GameStateData {
  pub pieces : Vec<PieceRecord>,
  pub players : Vec<PlayerRecord>,
}

impl Deref for GameState {
  type Output = GamStateData;
  fn deref(&self) -> &GamStateData { &self.data }
}

impl GameState {
  fn as_ref(&self) -> (&usize, &GameStateData) { (&self.gen, &self.data) }
  fn gen(&self) -> usize { self.gen }
}



#[derive(Serialize)]
enum MsgUpdate {
  InsertPiece(usize, MsgPiece),
  DeletePiece(usize),
  UpdatePiece(usize, MsgPiece),
}

struct DataGuard<'gs> {
  gs : &'gs mut GameState,
  msg : MsgUpdate,
}
impl<'gs> Deref for DataGuard<'gs> {
  type Output = GameState;
  fn deref(&self) -> GameState<'gs> { self.gs }
}
impl<'gs> DerefMut for DataGuard<'gs> {
  fn deref_mut(&mut self) -> GameState<'gs> { self.gs }
}

impl GameState {
  fn update(&mut self, msg : MsgUpdate) -> DataGuard<'_> {
    DataGuard { gs : self, msg }
  }
}

impl Drop for DataGuard {
  
}
