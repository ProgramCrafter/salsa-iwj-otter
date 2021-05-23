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

Pieces in shape libraries cannot have "behaviours": they can't do
anything "special" like react to being moved or clicked on.  Pieces
with special functionality do exist, but they are built into Otter.
(See :ref:`gamespec:Piece specs` for all the kinds of piece.)

A library consists of a **catalogue**, and a set of **image files**
which contain the actual appearances.  When a library is in a bundle
the catalogue is ``libraries/LIB.toml`` and the image files are in a
directory ``libraries/LIB.toml``.  The layout of Otter's builtin
libraries is similar.

Image files
-----------

Otter supports uploading of SVG and PNG files.  They should be in the
directory ``library/LIB``, named after the item.  (See `Files entry`_
for details of how to specify the file names.)

Image files should be small --- ideally a handful of kilobytes, or
less.  SVG images should be of modest complexity.

Large image files don't just upload slowly; they also make the game
perform poorly when playing.  This is because the image files are
frequently (re)transmitted by the server to each client.

It is not normally necessary to specify images in great detail: they
take only a small space on the players' screens, so the resolution
does not need to be awesome.  As an example, the playing cards in the
builtin ``cards-oxymoron`` library are image files of just 73x97
pixels.

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
follows: ``library/LIB/SRC.svg`` or ``.png``.  If ``SRC`` is ``-``
then the item name is used for ``SRC``.  (Builtin libraries: these
support SVG only; and the ``SRC`` is not used at runtime, or when
loading shapes --- rather, only when scraping or building.)

``DESCRIPTION`` is the **description**, a string which will be used to
describe the piece (eg in in-game log messages).  In English, it
should contain an article.  Eg, ``the black queen``, ``a white pawn``.
It will be HTML-escaped, so it should be plain text, not HTML.

It is also possible to specify additional data for each piece by
adding fields to each line in ``files``.  This is done by adding a
line at the start starting with ``:`` listing the extra fields, and
then additng one additional whitespace separated value on each data
line.  Values given for unknown field are ignored.

Currently the extra fields supported are:

 * ``sort``: Specifies the sort key.  See the ``sort`` group
   parameter.

The values for these extra fields come just before the
``DWSCRIPTION``, after the other whitespace-delimited fields, in the
same order as specified in the ``:`` heading line.

Item names
``````````

Item names do not need to be unique within a game, but there are
places where a piece is found *just* by the item name, so pieces
should have the same item name (only) if they are in some sense
equivalent.

Item names are conventionally structured using a hierarchical name
with ``-`` between the components.

The item name is a string but may contain only ASCII alphanumerics,
plain ASCII spaces, and the punctuation characters ``-._``.  Do not
put ``/`` or ``_`` in item names.  ``/`` is forbidden and ``_`` can
interfere with the template substitution system.

See the existing examples to see what item names usually look like.

Parameters
----------

Mandatory parameters
`````````````````````

 * ``size`` [1- or 2-element array of numbers: width and height].
   The size at which the piece will show up in the game, in nominal
   game coordinate units.
   NB, this value can be affected by ``scale``.

   For reference: the builtin library's chess
   pieces are 9.5 units; the builtin playing cards are 9.65, 17.125.

 * ``outline`` [string, one of ``"Circle"`` or ``"Rect"``].
   Defines the outline shape.  This is used for drawing selection
   highlights, etc.  The size is taken from ``size``.  If ``outline``
   is ``Circl``, ``size`` must be a 1-element array: ellipses are not
   supported.

 * ``files``: [multi-line string].  The list of pieces to define,
   one per line.  See `Files entry`_.

Important parameters
````````````````````

 * ``inherit`` [string: group name].  Causes this group to inherit
   every parameter (except ``files``) from the group named by
   ``inherit`` (recursively, if applicable).

   When inheritance is happening, there is of course a difference
   between leaving a value unspecified, and specifying it to have
   the usual default value: the latter would override any inherited
   setting.

 * ``item_prefix``, ``item_suffix`` [strings, default ``""``].
   Prepaended and appended to ``ITEM-SPEC`` in ``files`` to
   produce the item name.

Geometry parameters
```````````````````

 * ``centre`` [2-element array].  The centre of the image, measured
   from the top left in the image's own internal units.  If not
   supplied, calculated from the size.

 * ``orig_size`` [1- or 2-element array, or (default) empty array]: If
   non-empty, the supplied image is first scaled from ``orig_size``
   to ``size``.  If both ``size`` and ```orig_size`` are 2 elements,
   this can scale by different amounts in x and y, distorting the
   image.

 * ``scale`` [number, default=1].  Scale the image by a factor (in
   both x and y).  ``size`` and ``centre`` are in the image file's
   own internal coordinate system, not the Otter scaled coordinates
   which result from multiplying by by this scale factor.

Parameters for defining faces
`````````````````````````````

 * ``flip`` [boolean, default: false].  Whether this piece can "flip".
   If true, the piece will have two faces, one of which is a mirror
   image of the other.  The default face will be un-reflected version;
   the other face is the same image, but flipped left-to-right.  It
   doesn't make sense to enable this for pieces with a symmetrical
   appearance.  (It is a bad idea to have the game contain state which
   is not visible to the players.)  Not compatible with ``back``.

 * ``back``: [:ref:`Piece spec <gamespec:Piece Specs>`].  The back of
   this piece looks like some other piece (typically, another library
   item such as a card back).  If specified, the piece will have two
   faces: the one implied by its ``files`` entry, and a 2nd face
   looking like ``back``.  If you want to make the piece be
   occultable, you must also specify ``occulted``.  ``back`` is not
   compatible with ``flip``.

Other group parameters
```````````````````````
   
 * ``sort`` [string].  The sort key.  This is used for item sorting in
   hands.  When the user asks ot sort their hand, all the items in a
   hand are sorted according to (primarily) simply this sort key,
   interpreted lexicographically.

   The sort key should generally contakn all of the information in the
   item name; if the item name contains an element referring to style
   or aesthetic, that should appear at the end of the sort key (if at
   all)>.

   If neither the group parameter, nor the ``files`` extra field
   ``sort``, are specified, the item name is used as the sort key.

   If both are specified, the group parameter is used as a template:
   ``_s`` is replaced by the sort extra field from the ``files`` list;
   ``_c`` is replaced by the colour, if applicable.

 * ``colors`` [table].
   If specified and non-empty, specifies that this group should be
   instantiated multiple times, for different colours.

   For each entry in the ``colours`` table, a separate piece is
   generated for each item in the ``files`` list.  The keys of the
   ``colours`` are recolouring names, and the values are sub-tables.

   Every effective item name (i.e., after the prefix and suffix have
   been added) must contain the substring ``_c`` exactly once, and
   every item description must contain the substring ``_colour``
   exactly once.  ``_c`` will be replaced with the value of the
   recoluring's ``abbrev``, and ``_colour`` with the recolouring name
   (the key of the ``colours`` table).

   For libraries in bundles, a separate image file must be supplied
   for each recolouring.  If ``SRC`` is not ``-``, it also must
   contain ``_c`` which will be substitued with ``abbrev`` to find the
   colour-specific image file.

   For builtin libraries, the Otter build system will do the
   recolouring automatically at build time.  Each recolouring should
   hae a ``map`` entry which is a sub-sub-table mapping inputcolours
   (strings in ``#rrggbb`` format) to output colours.

 * ``desc``: [string: template].  If specified, provides a template
   for the description, to allow formulaic descriptions of pieces in
   this group.  The string specified ``desc`` must contain ``_desc``
   exaclty once; that will be replaced with the description calculated
   according to the other rules.  (``_desc`` substitution happens
   after ``_colour`` substitution.)

 * ``occulted`` [table, contents depend on ``occulted.method``].  If
   specified, these pieces be occulted.  For example, when a player
   has them in their hand and the hand is active and owned by them,
   only the occulted view (eg, the back of a playing card) will be
   shown.  This a table whose other contents depend on its key
   ``method``, which must be a string:

  * ``"ByColour"``: Occult by displaying a particular recolouring of
    this piece.  The sub-key ``colour`` names a recolouring - one of
    the keys of the ``colours`` group parameter.  When the piece is
    occulted it will show that colour, instead of its actual colour.
    In the description, ``_colour`` will be elided rather than
    substitued (along with up to one of any spaces either side of it).

  * ``"ByBack"``: Occult by displaying the back of this piece, as
    specified by the ``back`` group parameter.  The ``occulted`` table
    must also contain a sub-entry ``ilk``, a string.  Pieces which
    have the same ``ilk`` display identically when occulted, even if
    the different piece definitions imply different backs.  (Whichever
    pieces are first loaded define what the backs of a particular ilk
    look.)

    For pieces that are like cards, the ilk should be different for
    cards which have different backs in the game.  Generally, standard
    playing cards should all specify ``card-back``.
