Introduction
============

Otter, the Online Table Top Environment Renderer, is an online game
system.

But it is not like most online game systems.  It does not know (nor
does it need to know) the rules of the game you are playing.  Instead,
it lets you and your friends play with common tabletop/boardgame
elements such as hands of cards, boards, and so on.

So it's something like a "tabletop simulator" (but it does not have
any 3D, or a physics engine, or anything like that).

This means that with Otter:

 * Supporting a new game, that Otter doesn't know about yet, would
   usually not involve writing or modifying any computer programs.

 * If Otter already has the necessary game elements (cards, say) all
   you need to do is write a spec file saying what should be on the
   table at the start of the game.  For example, most Whist variants
   that start with a standard pack of 52 cards are already playable.

 * You can play games where the rules change as the game goes along,
   or are made up by the players, or are too complicated to write as a
   computer program.

 * House rules are no problem, since the computer isn't enforcing the
   rules - you and your friends are.

 * Everyone can interact with different items on the game table, at
   any time.  (Otter doesn't know about your game's turn-taking, so
   doesn't know whose turn it might be.)

We have played successful and fun online games of both Penultima and
Mao with Otter.


Playing a game with Otter
-------------------------

The Otter game environment is accessed from a web browser running
JavaScript, using a magic https link obtained from joining the game.

You will need to be able to talk to your friends about the game, while
you play.  Otter works well when used together with a voice chat - we
have had success with Jitsi in voice-only mode.

Most relatively modern desktop browsers should be able to work with Otter.
(The most advanced feature needed is support for WebAssembly.)


Predefined games and pieces currently available
-----------------------------------------------

Otter currently has, in its pieces library:

  * Ingredients for chess, including fairy chess.  So there's a board,
    pieces, including many fairy chess pieces, and a chess clock.

  * Ingredients for card games.  So, a deck of 52 standard playing
    cards, plus two kinds of joker.  Special "hand" and "deck" pieces
    for player hands and a pickup deck.

Currently there are game definitions for:

 * Penultima.  This can be used directly to play standard chess and
   some fairy chess variants.

 * Mao.  This can be used to play any game of roughly that shape.

Defining new games using the existing pieces from the library is
fairly easy.  It is also possible to add elements from the library
ad-hoc, even while a game is in progress.


Limitations
-----------

Currently, joining a game requires a unix shell account on the server
host (or help from a shell account user).

There is not currently a publicly available server.  The server code
is Free Software and if you have a suitable vm or server you are
encouraged to build and run it yourself, for you and your friends.

Mobile phones are not really suitable for playing on Otter because
their screens are too small.  Tablets and other touchscreen based
systems could be made to work, but don't work well right now.

Otter does not currently have even a built-in text chat facility.  It
does have a way to share a URL for a voice chat.


Free software, and user freedom
-------------------------------

Otter is Free Software.  I wrote it to liberate game players from the
need to encode their game rules as computer programs and thus from the
tyranny of programmers `:-)`.

I would love contributions, particularly to address the limitations I
mention above, and to improve the user experience.

I am also working to make it possible to let users define their own
games (including their own pieces, cards, boards, and so on) without
having to install them on the server.

The Otter software project is hosted on Debian's GitLab, at
<https://salsa.debian.org/iwj/otter>.

Merge requests (accompanied by a `Signed-off-by` indicating sign-off
of the Developer Certificate of Origin) would be very welcome.


References
----------

 * [Source repository on Salsa, Debian's GitLab](https://salsa.debian.org/iwj/otter)
 * [This documentation, online copy](https://www.chiark.greenend.org.uk/~ianmdlvl/otter/docs/)
 * Mailing lists for [annoucements](https://www.chiark.greenend.org.uk/mailman/listinfo/sgo-software-announce) and [discussion](https://www.chiark.greenend.org.uk/mailman/listinfo/sgo-software-discuss)
