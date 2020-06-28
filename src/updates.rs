
struct Update {
  gen : Counter,
  u : UpdatePayload,
}

enum UpdatePayload {
  NoUpdate,
  ClientSequence(ClientCounter),
  PieceDelete(PieceId),
  PieceInsert(PieceId, PieceUpdate),
  PieceUpdate(PieceId, PieceUpdate),
}
