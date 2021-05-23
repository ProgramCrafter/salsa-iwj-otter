Game specs
==========

A game spec defines the starting layout for a game: **which pieces start
on the table, and where**.

It also defines some ancillary information about the game and its
layout.  It does not contain information about the players.
Players join a game as permitted by the game access control list,
which is specified in a *table specification* (sadly not currently
properly documented, but look at the Rustdoc for ``TableSpec``).

General
-------

A game spec is a TOML file.  It has the following entries at the top
level:

 * ``table_size`` [array of 2 numbers].  Size of the table playing
   area, in Otter internal units.  The default is ``[300,200]``.

 * ``table_colour`` [string: colour].  Table backdrop colour.
   Only certain colour syntaxes are supported.  Default: ``green``.

 * ``pcaliases``: [table, values are Piece spec table].  Piece alias
   table.  When a piece is being loaded, an alias  xxxx grep for where
   referred to

 * ``pieces``: Array of `Piece Specs`_.  Defines the initial pieces
   and their layout.  Each entry is a piece spec dictionary.

Piece Specs
-----------

A piece spec is a dictionary defining one or more pieces.  When part
of a game spec, it appears as an entry in the top-level ``pieces``
array, and also defines the location(s) on the table to place the
pieces, too.

There is a required entry``type``, a string.  This determines how the
rest of the table is interpreted.  It is one of the `Piece Spec
Types`_.

Universal parameters
````````````````````

These apply regardless of the value of ``type``.

 * ``type``: Piece type or piece spec type.  One of the types listed
   in `Piece Spec Types`_.  [string, enum, required]

 * ``pos``:Position, in game coordinates, of
   the centre of the piece.  The origin is at the top left.
   [2-element array, default ``[20,20]``]

 * ``count``: Place multiple identical copies of this piece.  [number]

 * ``posd``: Position delta.  When a spec specifies multiple pieces,
   each successive piece will be shifted by this amount.  [2-element
   array, default ``[5,5]``]

 * ``face``: Initial face to show (ie, "which way up" the piece
   starts).  The default face is ``0``.  For most pieces that is the
   front, and is usually a good choice.  [number]

 * ``pinned``: Whether the piece is pinned to the table.  Players can
   pin and unpin pieces during the game; this is the initial state.
   [boolean]

 * ``angle``: Initial orientation of the piece.  The
   specified value is multiplied by 45 degrees, increasing values
   rotating anticlockwise.  So for example ``6`` would mean to rotate
   90 degrees clockwise.  [integer 0..7]


Common parameters
`````````````````

Depending on the ``type``, some of these parameters will be honoured.
This is discussed in the descriptions for each piece spec type.

 * ``colour``: The fill colour.  For a piece type which supports only
   one face.  [string, colour]

 * ``faces``: The main fill colour(s).  [array of string(s), colours]

 * ``edge``: The edge colour to draw.  For a piece with supports only
   one face.  Default is not to draw edges.  [string, colour]

 * ``edges``: The colour of edges to draw.  Default is not to draw
   edges.  Must either be a 1-element array, or as long as ``faces``
   (specifying a different edge colour for each face).  [array of
   string(s), colours]

 * ``edge_width`` [number, default 0.2 if `edge` or `edges` is
   specified]

 * ``label``.  Controls display of the label with informationa about
   the in-game state.  Dictionary with two sub-entries:
    * ``colour`` [string, defaults to the edge colour].
    * ``place`` [string]: One of ``"BottomLeft"`` (default),
      ``"TopLeft"``, ``"BottomLeftOutside"``, ``"TopLeftOutside"``.

 * ``shape``.  The shape of a piece.  Dictionary with two sub-entries:
    * ``type``.  ``"Circle"`` or ``"Rect"`` [required]
    * ``size`` [array of 1 or 2 numbers]: required if ``type="Rect"``.
    * ``diam`` [number]: required if ``type="Circle"``.

 * ``itemname``: Used when other parts of the game want to refer to
   this one.  [string]


Piece Spec Types
````````````````

``"Lib"``
`````````

A single shape from a piece library.

 * ``lib``: The library name.  [string, required]
 
 * ``item``: The item name within that library.  [string, required]

Example::

  [[pieces]]
  pos = [150,100]
  type = "Lib"
  lib = "edited"
  item = "chess-board"
  pinned = true


``"LibList"``
`````````````

Multiple shapes from a piece library.  Cannot be used with the `count`
universal parameter.

 * ``lib``: The library name. [string, required]

 * ``items``: The item names. [array of strings, required]

 * ``prefix``, ``suffix``: Prepended and appended to each
   entry in ``items``.  Useful for abbreviating.  [strings]

Example::

  [[pieces]]
  pos = [150, 84]
  type = "LibList"
  lib = "cards-oxymoron"
  prefix = "card-oxymoron-"
  suffix = "-s"
  items = [
      "2","3","4","5","6","7","8","9","T","J","Q","K","A",
      "2","3","4","5","6","7","8","9","T","J","Q","K","A",
      "2","3","4","5","6","7","8","9","T","J","Q","K","A",
  ]
  posd = [0, 0]


``"ChessClock"``
````````````````

A chess clock.  Additional parameters:

 * ``time``: Initial time for each player. [number, in seconds;
   required]

 * ``per_move``: Time to add per moove.  [number, in seconds]

(These clock settings cannot be reconfigured via the game UI.)

Example::

  [[pieces]]
  pos = [240, 100]
  type = "ChessClock"
  time = 900
  per_move = 30


``"PickupDeck"``
````````````````

A pickup or play deck.  This can occult the pieces (eg, cards) you put
on it, shuffling them and hiding their identity.

Requires ``face`` and ``shape``.  Only ``shape.type="Rect"`` is supported.

Honours ``edges``, ``edge_width``.

Honours ``label``, displaying the number of of pieces in (on) this deck.

Example::
  
  [[pieces]]
  pos = [136,115]
  type = "PickupDeck"
  faces = ["lightblue", "grey"]
  edges = ["black", "white"]
  label.colour = "black"
  label.place = "BottomLeftOutside"
  shape.type = "Rect"
  shape.xy = [25,30]


`Hand`
``````

A player hand.  When active, arranges for only that player to be able
tos see the contents.  The other players see the occulted view (eg,
the backs of cards).

Requires ``colour`` and ``shape``.  Only ``shape.type="Rect"`` is
supported.

Honours ``edge``, ``edge_width``.

Honours ``label``, displaying the player whose hand this is, when
active.


``"PlayerLabel"``
`````````````````

A simple label which can display a player name.

Requires ``colour`` and ``shape``.  Only ``shape.type="Rect"`` is supported.

Honours ``edge``, ``edge_width``.

Honours ``label``.


``"Rect"``
``````````

A plain rectangular piece.

 * ``size``: Size and shape  [array of 1 or 2 numbers, required]

Requires ``faces``.

Honours ``itemname``, ``edges`` and ``edge_width``.

Exammple::

  [[pieces]]
  pos = [20, 85]
  type = "Rect"
  faces = ["yellow","#f4f"]
  posd = [10, 0]
  size = [7,7]
  count = 8


``"Disc"``
``````````

A plain circular piece.

 * ``diam`` [number, required].

Requires ``faces``.

Honours ``itemname``, ``edges`` and ``edge_width``.


