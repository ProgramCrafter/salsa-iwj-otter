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

 * ``table_size``: Size of the table playing area, in Otter internal
   units.  [array of 2 numbers, default ``[300,200]``]

 * ``table_colour``: Table backdrop colour.  Only certain colour
   syntaxes are supported.   [string, colour; default ``green``]

 * ``pcaliases``: [dictionary, values are Piece Spec dicts].  Piece
   alias definitions.  This is used by the ``"Alias"`` piece spec.

 * ``pieces``: Array of `Piece Specs`_.  Defines the initial pieces
   and their layout.  Each entry is a piece spec dictionary.

 * ``format`` [integer]: Specifies which version of the Otter data
   formats the spec file was written to.
   This document describes ``format=1``.
   See :ref:`bundle-compatibility`.


Templating
``````````

Before being parsed as TOML, the spec can be processed as a one-off
Tera template.  This is useful because game setups can be rather
repetitive.

Template expansion is done if the file starts, precisely, with the
characters ``{#`` (which are the Tera comment introducer).

Tera does not natively support within-same-file macros.  To allow use
of macros in Otter's single-file game specs, Otter can automatically
split a game spec file into two pieces and feed them to Tera as two
files.

Splitting is done if the file contains a line starting with precisely
``{% endmacro`` (the usual form of the Tera macro end sequence).

Everything up to and including the `last` such line is fed to Tera as
if it were a file called ``m``.  Everything else is fed to Tera as if
it were a second file called ``spec``.  Additionally, a line ``{%
import "m" as m %}`` is added to the top of ``spec``, so that the
macros are automatically imported and can be called with ``{[
m::MACRO...}}``.  And enough blank lines are added to make the input
line numbers match up with the lines in the notional ``spec`` file.

Note that line numbers reported during TOML parsing
refer to lines in the template-expanded output.
To help diagnose your templates, ``otter -vv reset ...`` will expand
the templates itself and print out the results of the file splitting,
and then of the expansion, before sending the file to the server.

For details of the template syntax, see `the Tera documentation`_.
For examples, see the built-in game specs.

.. _the Tera documentation: https://tera.netlify.app/docs/#templates

Piece Specs
-----------

A piece spec is a dictionary defining one or more pieces.  When part
of a game spec, it appears as an entry in the top-level ``pieces``
array, and also defines the location(s) on the table to place the
pieces, too.

There is a required entry ``type``, a string.  This determines how the
rest of the table is interpreted.  It is one of the `Piece Spec
Types`_.

Universal parameters
````````````````````

These apply regardless of the value of ``type``.

 * ``type``: Piece type or piece spec type.  One of the types listed
   in `Piece Spec Types`_.  [string, enum, required]

 * ``pos``: Position, in game coordinates, of
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

 * ``label``.  Controls display of the label with information about
   the in-game state.  Dictionary with two sub-entries:

    - ``colour`` [string, defaults to the edge colour].
    - ``place`` [string]: One of ``"BottomLeft"`` (default),
      ``"TopLeft"``, ``"BottomLeftOutside"``, ``"TopLeftOutside"``.

 * ``shape``.  The shape of a piece.  Dictionary with two sub-entries:

    - ``type``.  ``"Circle"`` or ``"Rect"`` [required]
    - ``size`` [array of 1 or 2 numbers]: required if ``type="Rect"``.
    - ``diam`` [number]: required if ``type="Circle"``.

 * ``itemname``: Used when other parts of the game want to refer to
   this one.  [string]


Piece Spec Types
----------------

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

 * ``per_move``: Time to add per move.  [number, in seconds]

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


``"Hand"``
``````````

A player hand.  When active, arranges for only that player to be able
tos see the contents.  The other players see the occulted view (eg,
the backs of cards).

Requires ``colour`` and ``shape``.  Only ``shape.type="Rect"`` is
supported.

Honours ``edge``, ``edge_width``.

Honours ``label``, displaying the player whose hand this is, when
active.

Example::

  [[pieces]]
  pos = [53, 25]
  colour = "brown"
  label.place = "BottomLeftOutside"
  label.colour = "black"

  type = "Hand"
  edge = "white"
  edge_width = 0.75
  shape.type = "Rect"
  shape.xy = [93,25]


``"PlayerLabel"``
`````````````````

A simple label which can display a player name.

Requires ``colour`` and ``shape``.  Only ``shape.type="Rect"`` is supported.

Honours ``edge``, ``edge_width``.

Honours ``label``.


``"Die"``
`````````

A die (or coin), which can choose randomly
from a fixed set of aspects.
Can be "rolled" to have the server show a randomly-chosen face.

You must either specify an ``image`` with multiple faces,
and/or ``labels``,
so that the faces can be distinguished.
If the ``image`` has multiple faces *and* you specify ``labels``,
the number of faces implied by each must be the same.

The die will display a circular "cooldown timer",
after it has been rolled.
This makes rolling the die visually noticeable for all the players.
After the die has been rolled,
it cannot be flipped to a different face, or re-rolled,
until the timer expires.
Apart from that, you can see all the faces in sequence,
or make the die show a particular face,
with the standard flip operation ("f").

Dice can (possibly) be occulted.
An occultable die will, if placed in a player's active hand,
obscure its face (but, generally, not existence, nor cooldown time),
from other players.
Dice aren't "shuffled" with other piece, the way (say) cards are.
A die is occultable if its image is occultable,
or if ``occult`` is explicitly specified.


Parameters:

 * ``desc``: Descriptive string,
   used in log messages reporting player actions.
   The actual description shown to users will also report
   the description provided by the image,
   for the particular face showing,
   and the number of faces.
   [string; optional]

 * ``image``: Specifies what this die should look like.
   [inner piece spec, as dictionary; required].

 * ``labels``: Text strings to superimpose on the image.

    - [list of strings] One string per face.
    - [list of two numbers] Label faces numerically (inclusive).
    - [single number] Label faces numerically from 1 to n (inclusive).

 * ``label.colour`` [string, defaults to black]:
   Colour to write the ``labels`` Text strings.

 * ``cooldown``: Duration of the cooldown time.
   [duration - number(s) with units; default "4s"]

 * ``circle_scale``: Adjusts the size of the cooldown timer circle.
   The default is an estimate of the best size,
   calculated from the image's bounding box.
   [floating point number; default is 1.0, representing the estimated size]

 * ``occult``: If supplied,
   specifies that the die should be occultable.
   In this case either the specified ``image``
   must itself be occultable,
   or it must have only one face
   (since otherwise we wouldn't know what to display when occulted).
   [dictionary; optional;
   presence of even an empty dictionary is meaningful;
   default if absent is to occult if the specified image is occultable]

 * ``occult.label``: The text string to display when the die is occulted.
   [string; if not specified in ``occult``,
   defaults to ``"?"`` if any nonempty ``labels`` were specified,
   or the empty string otherwise.]
   
The common parameter ``itemname`` is also supported.

Example::
  
  [[pieces]]
  pos = [155, 15]
  type = "Die"
  labels = ["A", "B"]
  image.type = "Disc"
  image.diam = 12
  circle_scale = 0.833
  image.edges = ["black","black"]
  image.faces = ["#ccccff", "#ccffcc"]


``"Currency"``
```````````````

Quantified resources of any kind.
This could be money, or "resources", or perhaps VPs.
In what follows we call each piece representing some currency a "banknote",
but it might represent some resource cubes, or whatever.

Each banknote has an integer quantity.
Currency is fungible: it can be split and merged.
Dropping a banknote onto another banknote of the same currency will
merge the two.
The quantity selection function in the game UI can be used to take
a subset of the value out of a banknote,
splitting the amount requested off the note,
and leaving the change behind.

So individual banknotes can be created or destroyed in play.
Each note can represent either resources being moved
from one place to another,
or a repository such as a bank or a player's stash.
(There is no enforced distinction between banknotes
in motion or lying about randomly on the table,
and a "bank", or players' money.
Players are expected to use location on the table to indicate
whose money is what, as they would with physical money.)

Each currency is identified by a string.
Different banknotes with the same currency can be merged,
even if they look totally different.
To avoid confusion,
it is a good idea for all banknotes of the same
currency in the same game to have the same image.

The total amount in the game of each currency
remains constant during play.
(More can be introduced by using ``otter`` to add more banknotes.)

Currency is not currently occultable:
banknotes are always publicly visible.

Parameters:

 * ``image``: Specifies what each banknote should look like.
   The image must have only one face.
   [inner piece spec, as dictionary; required].

 * ``qty``: The initial amount of this banknote or stash.
   [nonnegative integer; required]

 * ``currency``: The currency, which defines which other
   banknotes this one can interchange with.

Exammple::

  [[pieces]]
  pos = [125, 45]
  type = "Currency"
  qty = 400
  min_unit = 5
  currency = "ƒ"
  image.type = "Rect"
  image.size = [20,7]
  image.edges = ["#00ff00"]
  image.faces = ["#008800"]


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


``"Alias"``
```````````

An alias (generally defined in ``pcaliases`` in the game spec).

This allows a piece spec (which can be found in a shape library) to
refer to something which depends on the game spec.

 * ``target``: Alias name.

Example, in ``GAME.game.toml``::

  [pcaliases.card-back]
  type = "Lib"
  lib = "wikimedia"
  item = "card-plain-back-maroon"

And in ``library/LIB.toml``::

  [group.clubs]
  item_prefix = "card-oxymoron-"
  outline = "Rect"
  size = [73, 97]
  centre = [36.5, 48.5]
  scale = 0.25

  item_suffix = "-c"
  sort = "card-playing-c_s"
  desc_template = "the _desc of clubs"

  occulted.method = "ByBack"
  occulted.ilk = "card-back"

  files = """
  :             sort
  2     -       02      two
  3     -       03      three
  4     -       04      four
  5     -       05      five
  6     -       06      six
  7     -       07      seven
  8     -       08      eight
  9     -       09      nine
  T     -       10      ten
  J     -       11      jack
  Q     -       12      queen
  K     -       13      king
  A     -       14      ace
  """

  [group.clubs.back]
  type = "Alias"
  target = "card-back"
