
#[derive(Error)]
enum Error {
  [error("attempt to create instance with same name as existing instance")]
  InstanceAlreadyExists,
  [error("attempt to create instance with same name as existing instance")]
  UnknownInstance,
}
