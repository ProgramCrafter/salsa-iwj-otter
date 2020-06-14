
fn testload() -> E {
  let disc = Disc { size : 10, colours : vec![
    Colour::literal("red"),
    Colour::literal("pink"),
  ] };
  let pr = PieceRecord {
    pos : [40,40],
    p : Rc::new(disc),
    held : None,
  };
  let g = GameState {
    pieces : vec![ pr ],
    players : vec![
      PlayerRecord { nick : "alice".to_owned() },
      PlayerRecord { nick : "bob"  .to_owned() },
    ],
  };
  create_instance_access("alice", 

