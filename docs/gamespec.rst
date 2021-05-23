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


 * xxx
