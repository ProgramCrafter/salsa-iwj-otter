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

A library has a **library name**.  This is a string.  For a library in
a bundle, it's the ``LIB`` part of the filename
``libraries/LIB.toml``.

Each piece in a library has an **item name**.  Item names are unique
within a library.  The item name is used within Otter to refer to the
piece (for example, with ``otter library-add``).

Item names do not need to be unique within a game, but there are
places where a piece is found *just* by the item name, so pieces
should have the same item name (only) if they are in some sense
equivalent.  The item name is a string but may contain only ASCII
alphanumerics, plain ASCII spaces, and the punctuation characters
``-._``.

Pieces in shape libraries cannot have "behaviours": they can't do
anything "special" like react to being moved or clicked on.  Pieces
with special functionality do exist, but they are built into Otter.
(See _`Piece specs` for all the kinds of piece.)

A library consists of a **catalogue**, and a set of **image files**
which contain the actual appearances.  When a library is in a bundle
the catalogue is ``libraries/LIB.toml`` and the image files are in a
directory ``libraries/LIB.toml``.  The layout of Otter's builtin
libraries is similar.

Catalogue
---------

The catalogue defines what pieces the library contains.  For each
piece it defines each face looks like, how big it is on the screen,
whether and how the piece can be occulted, and what source image files
are to be used to display it.

The catalogue is a TOML file.  Its main contents is a table
``groups``, mapping each group name to a sub-table of parameters:

Each catalogue is organised into named **groups**.  Each group defines
some pieces.  It specifies various **parameters**, and also gives a
list of indvidual image files which should be processed according to
those parameters.

For example::

  [group.dried]
  outline = "Circle"
  size = [14]
  orig_size = [64, 48]
  centre = [32,24]

  files = """
  dried-lemon	-	a dried lemon
  """

This defines a group ``dried``, with parameters such as ``size`` and
``outline``.  The ``files`` entry defines the list of pieces.

The group names are not visible when using the library, but they can
be used within the library using the ``inherit`` feature.

The builtin catalogues also have a toplevel table ``scraper``, which
controls how the builtin shape data is processed during the build, and
how it is to be updated.  (Downloads are never automatically run
during the build.  If you updated the catalougue in a way that means
files should be re-downloaded, you should re-run ``./media-scraper
library/LIB.toml``.)

Files entry
-----------

Each group has a table key ``files``.  This is a string, which is
treated as a series of lines (so it is best to use the TOML multi-line
string syntax).

Each line of which has (normally) three fields (the leading ones
terminated by whitespace).  ``#`` comment lines are supported and
blank lines are ignored.

Each non-empty non-comment line in ``files`` specifies a single piece,
like this::

   ITEM-SPEC SRC DESCRIPTION...

The **item name** of the piece will be ``ITEM-SPEC`` sandwiched
between the ``item_prefix`` and ``item_suffix`` parameters (see
below).

The **image filename** is derived from ``SRC`` or the item name, as
follows: ``library/LIB/SRC.svg`` or ``.png``.  (Builtin libraries
support SVG only.)  If ``SRC`` is ``-`` then the item name is used for
``SRC``.

``DESCRIPTION`` is the **description**, a string which will be used to
describe the piece (eg in in-game log messages).  In English, it
should contain an article.  Eg, ``the black queen``, ``a white pawn``.

It is also possible to specify additional data for each piece by
adding fields to each line in ``files``.  This is done by adding a
line at the start starting with ``:`` listing the extra fields, and
then additng one additional whitespace separated value on each data
line.  Values given for unknown field are ignored.

Currently the extra fields supported are:

 * ``sort``: Specifies the sort key.  See the ``sort`` group
   definition property.

The values for these extra fields come just before the
``DWSCRIPTION``, after the other whitespace-delimited fields, in the
same order as specified in the ``:`` heading line.

Item names
``````````

Item names are conventionally structured using a hierarchical name
with ``-`` between the components.  Do not put ``/`` in item names use
``_`` only as a word separator within ``-``-separated parts.

See the existing examples to see what item names usually look like.
