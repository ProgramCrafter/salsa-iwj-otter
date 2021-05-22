Shape libaries
==============

Most pieces in a game in Otter will be from a shape library.

Introduction; general structure
-------------------------------

A shape library is a set of named pieces.  Mostly, it defines their
appearance.

Shape library pieces can have more or more sides (**faces**), and they
can also have an **occulted view** which is used when the identity of
the piece is to be hidden (possibly from only some of the players).

For example:

Playing cards have two faces: one is the front, which identifies the
card, and the other is the back, which is a generic card back.  Cards
also have an occulted view: again, the back.

Chess pieces usually have only one face.  The Knight has two faces,
because it is not symmetrical: the first face looks to the right, and
the other face to the left.  Chess pieces can be occulted, but when
they are occulted they reveal their shape but not their colour.  The
occulted view of a chess piece is a grey version of the piece.

Pieces in shape libraries cannot have "behaviours": they can't do
anything "special" or react to being moved or clicked on.  Pieces with
special functionality do exist, but they are built into Otter.  (See
_`Piece specs` for all the kinds of piece.)

A library has a **library name**.  This is a string.  For a library in
a bundle, it's the ``LIB`` part of the filename
``libraries/LIB.toml``.

Each piece in a library has an **item name**.  Item names are unique
within a library.  Item names do not need to be unique within a game,
but there are places where a piece is found *just* by the item name,
so pieces should have the same item name (only) if they are in some
sense equivalent.  The item name is a string but may contain only
ASCII alphanumerics, plain ASCII spaces, and the punctuation
characters ``-._``.
