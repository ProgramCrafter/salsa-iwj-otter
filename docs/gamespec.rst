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
   and their layout.  Each entry is a piece spec sub-table.

Piece Specs
-----------

A piece spec is a table defining one or more pieces.  When part of a
game spec, it appears as an entry in the top-level ``pieces`` array,
and defines the location(s) on the table to place the pieces, too.

There is a mandatory key ``type``, a string.  This determines how the
rest of the table is intesrpreted.

Umiversal parameters
````````````````````

 * ``type``: Piece type or piece spec type.  One of the types listed
   in the sections below.  [string, enum, mandatory]

 * ``pos``:Position, in game coordinates, of
   the centre of the piece.  The origin is at the top left.
   [2-element array, default ``[20,20]``]

 * `count` [number, default=1].  Place multiple copies of this piece.

 * `posd` [2-element array].  Position delta.  When a spec
   specifies multiple pieces, each successive piece will be shifted by
   this amount.  Default: `[5,5]`.

 * `face` [number, default=0].  Initial face to show (ie, "which way
   up" the piece starts).  The default face for most pieces is the
   front, and this is usually a good choice.

 * `pinned` [boolean].  Whether the piece is pinned to the table.
   (Players can pin and unpin pieces during the game; this is the
   initial state.)

 * `angle` [number 0..7].  Initial rotation of the piece.  The
   specified value is multiplied by 45 degrees, increasing values
   rotating anticlockwise.


``"Lib"``
`````````

A single shape from a piece library.

 * `lib` [string, mandatory]: The library name.
 
 * `item` [string, mandatory]: The item name within that library.


`LibList`
`````````

Multiple shapes from a piece library.  Cannot be used with the `count`
universal parameter.

 * `lib` [string, mandatory]: The library name.

 * `items` [array of strings, mandatory]: The item names.

 * `prefix`, `suffix` [strings]: Prepended and appended to each
   entry in `items`.  Useful for abbreviating.


`ChessClock`
````````````

A chess clock.  Additional parameters:

 * `time` [number, in seconds; mandatory].  Initial time for each
   player.

 * `per_move` [number, in seconds].  Time to add per moove.

(The clock settings cannot be reconfigured via the game UI.)


`PickupDeck`
````````````

A pickup or play deck.  This can occult the pieces (eg, cards) you put
on it, shuffling them and hiding their identity.

Requires `face` and `shape`.  Only `shape.type="Rect"` is supported.

Honours `edges`, `edge_width`.

Honours `label`, displaying the number of of pieces in (on) this deck.


`Hand`
``````

A player hand.  When active, arranges for only that player to be able
tos see the contents.  The other players see the occulted view (eg,
the backs of cards).

Requires `colour` and `shape`.  Only `shape.type="Rect"` is supported.

Honours `edge`, `edge_width`.

Honours `label`, displaying the player whose hand this is, when
active.


`PlayerLabel`
`````````````

A simple label which can display a player name.

Requires `colour` and `shape`.  Only `shape.type="Rect"` is supported.

Honours `edge`, `edge_width`.

Honours `label`.


`Rect`
``````

A plain rectangular piece.

 * `size` [array of 1 or 2 numbers]: Size and shape.

Requires `faces`.

Honours `itemname`, `edges` and `edge_width`.


`Disc`
``````

A plain circular piece.

 * `diam` [number, mandatory].

Requires `faces`.

Honours `itemname`, `edges` and `edge_width`.


Common parameters
`````````````````

 * `colour` [string, colour].  The fill colour For a piece which
   supports only one face.

 * `faces` [array of string(s), colours, mandatory].  The main fill
   colour.

 * `edge` [string, colour].  The edge colour to draw for a piece with
   supports only one face.  Default is not to draw edges.

 * `edges` [array of string(s), colours].  The colours of edges to
   draw.  Default is not to draw edges.  Must either be a 1-element
   array, or as long as `faces` (specifying a different edge colour
   for each face).

 * `edge_width` [number].  Default is 0.2 if `edge` or `edges` is
   specified.

 * `label` [table].  Displays a label with informationa about the
   in-game state.  There are two sub-keys:
    * `colour` [string, defaults to the edge colour].
    * `place` [string]: One of `BottomLeft` (default), `TopLeft`,
      `BottomLeftOutside`, `TopLeftOutside`.

 * `shape` [table].  The shape of a piece.  There are two sub-keys:
    * `type`, [string, `Circle` or `Rect` ].
    * Either `size` [array of 1 or 2 numbers] (for a square
      or rectangle) or `diam` [number] (for a circle).

 * `itemname` [string].  Used when other parts of the game want to
   refer to this one.
