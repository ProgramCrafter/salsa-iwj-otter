Bundles - uploadable game materials
===================================

Otter supports uploading game materials (pieces, and game specs) to
the server.  That way you can play games with pieces that you have
designed yourself without having to share them publicly or get them
incorporated into Otter itself.

These **bundles** are per-game.  They are accessible to the players
but not made public.  The usual way to use bundles is to specify them
on the ``otter reset`` command line:

::

  otter --game unix:myself:: reset stoat-fest stoaty-games.zip
              /^^^^^^^^^^^^^      /^^^^^^^^^^ ^^^^^^^^^^^^^^^^\
     game (instance) name     game spec name       bundle file'

The zipfile will be uploaded the server, and then Otter will look for
the game spec ``stoat-fest`` amongst its builtin games and also in the
zipfile.  Probably, ``stoat-fest`` is ``specs/stoat-fest.game.toml`` in
the zipfile, and it will refer to piece elements also found there.

Bundle format
-------------

Bundles are zipfiles.  They can contain:

 * ``otter.toml`` at the toplevel, with some basic metadata.
   This file is required.

 * ``specs/GAME.game.toml``:  Description of what a particular
   game looks like: what shape and colour the table is, what pieces
   the game contains (at least initially), and where they start.
   ``GAME`` is the game spec name (e.g., ``stoat-fest`` above.)

 * ``library/LIB.toml``: Description of a piece shape library,
   for a library named ``LIB``.  See :doc:`shapelibs`.

These ``.toml`` files are all in TOML format.  TOML ia an
INI-file-like format designed for human editing and flexibility.  See
the `TOML documentation <https://toml.io/en/>`_ for information about
the syntax.

The TOML specification has confusing terminology.  In this manual we
use "dictionary" (or "dict") for a mapping from (string) keys to
values (what TOML calls a "table").  "Array" is an array or list of
(more or less similar) values.

Any unrecognised files which might be present in the zipfile are
ignored.  (Future versions of Otter might define a meaning for them.)

Character set and case-sensitivity
``````````````````````````````````

Everything in Otter is UTF-8.

All filenames in bundles are treated case-insensitively (according to
Unicode).  So a bundle cannot have game specs, or piece image files,
which differ ony in the case of their name.

Note, however, that library item names, TOML config file keys, and,
indeed everything else *except* filenames within the zipfiles, are
case-sensitive.

Library names in bundles are treated as lowercase, no matter the case
in the zipfile.  So when piece specs refer to a bundle library, they
must refer to it in lowercase.

``otter.toml`` bundle top-level metadata
----------------------------------------

The top-level ``otter.toml`` contains a single entry ``title``, which
should be a string.  This is useful because the zipfile's name is not
stored in the server.

Bundle management
-----------------

Each game on the server can contain multiple bundles.  They are
ordered and numbered, in order of upload.

When Otter needs to look for a game spec or a piece, the bundles are
searched in reverse order: from most recent, to least recent.  So a
game might contain elements from multiple bundles.

Usually, the right approach is to pass all the needed bundle filenames
to ``otter reset``.  The right set of bundles will automatically be
uploaded as necessary.

When a library piece is added to a game, its appearance is fixed.  You
cannot retrospectively update existing pieces.  You can only delete
them and replace them with a new versions (perhaps as part of ``otter
reset``).

It is not possible to delete or replace individual bundles, only to
clear out all the bundles of an empty game.  When developing a bundle,
new versions can be uploaded to the server by hand with ``otter
upload-bundle`` and their contents will be found first.  But ``otter
reset`` will usually be less trouble and will avoid cluttering the
game (and the server) with previous versions.

Examples
--------

There are two examples of bundles:

 * `test-bundle.zip <examples/test-bundle.zip>`__: A small test
   bundle, containing three yellow objects and one game spec.

 * `big-bundle.zip <examples/big-bundle.zip>`__: The Otter builtin
   shape libraries automatically massaged into Otter bundle format,
   and one massaged game spec.

There are also several game spec examples, such as:

 * `mao.game.toml <examples/mao.game.toml>`__ Mao, or many other
   similar card games.  Three decks, 9 jokers.

 * `penultima.game.toml <examples/penultima.game.toml>`__ Manipulation
   Rummy variants with two decks.

 * `vatikan.game.toml <examples/vatikan.game.toml>`__ Manipulation
   Rummy variants with two decks.

Consult the `webserver directory listing <examples/>`__ for a complete
list.
