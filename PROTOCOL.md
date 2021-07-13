Copyright 2020-2021 Ian Jackson and contributors to Otter
SPDX-License-Identifier: AGPL-3.0-or-later
There is NO WARRANTY.


CONCURRENT UPDATE PROTOCOL AND ANALYSIS
=======================================

(Assumption: our network techniques deliver messages in order in both
directions between a server and a client; failure of the communication
stream is allowed to break things - we treat it as fatal.)

We run the following algorithm / data model separately for each piece.


Model
-----

There is:

  * actual committed history, a list of updates
  * the history seen so far by the client
    this can only advance, which is a prefix of the above
  * possibly branch(es) of history made by the client;
    this can rewind and be discarded

There is a diagraph of updates.  Each update specifies the complete
state of the piece.  The server maintains an authoritative history;
the client only maintains the state of the piece.

Updates may be from this client ("`Client` updates") or from another
client or made by the server itself.  We look at the protocol from a
single client's point of view, and call all the other updates
"`Server` updates".

Each update has a single direct ancestor, but this is not recorded in
the message nor by the client.  Updates may be some combination of:

  * `Recorded` - stored by server.
  * `Downbound` - on way from server to client.
  * `Discarded` - received by server and thrown away.
  * `Upbound` - on way from client to server.
  * `Displayed` - seen by client, is most recent state there
  * `Superseded` - seen by client, is no longer most recent state.

Server assigns generation number (strictly increasing but maybe with
gaps) to each `Recorded` update.

Each `Upbound` update is tagged with the generation of the most recent
Processed update.

(Actually the most recent Processed generation stored by the client is
global across all pieces, and the server's generations are likewise
generated globally although recorded separately for each piece.  So
the update contain s a generation >= the most recently Processed
update for that piece, and < any subseuent Processed upate for that
piece.  The server only compares the incoming update generation for >=
with a recorded value, so this makes no difference.)

Desirable properties and invariants
-----------------------------------

Legal combinations are:

  * `Server` + `Recorded` + `Downbound`
  * `Server` + `Recorded` + `Displayed`
  * `Server` + `Recorded` + `Superseded`
  * `Client` + (`Displayed` / `Superseded`) + `Upbound`
  * `Client` + (`Displayed` / `Superseded`) + `Recorded`
  * `Client` + (`Displayed` / `Superseded`) + `Discarded`

_linear_: The `Recorded` updates form a linear sequence (so it is only
ever appended to).

_forwards_: Each update has at most one `Server` descendant, and at most
one `Client` descendant.

_display_: Exactly one update is `Displayed` at any one time.  It has no
`Client` child and no `Superseded` descendants.  Each update is only
`Displayed` at most once, and can only become `Superseded` after having
been `Displayed`.

_discard_: For every `Discarded` update K there is a `Recorded` `Server`
update S which not an ancestor of K; and with S's generation > K's
tag.

_record_: Sequence of `Recorded` updates is as follows:

  * `Recorded` `Superseded` (zero or more)
  * `Recorded` `Displayed` (zero or one)
  * `Recorded` `Downbound` (zero or more)


Update generation operations
----------------------------

### I. Server update ###

Server may invent and transmit an update at any time.  Its ancestor is
the most recent `Recorded` update.  It is `Recorded`+`Downbound`.  No other
updates change their state.

Properties and invariants: _linear_, _forwards_, _record_ are
obvious.  _display_ is not affected.

### II. Client update ###

Client may invent and transmit an update at any time; it must have the
`Displayed` update as its parent.  That update becomes `Superseded`.
The new update is `Displayed`+`Upbound`.

_linear_ and _record_ are not affected.  _forwards_ is honoured
because of _display_.  _display_ is preserved.

Server message reception
------------------------

Suppose the server receives a message, Q.  It was `Client` `Upbound`.

Let T be the most recent `Recorded` message.  The server finds the most
recent `Server` `Recorded` message, U.

### I. Q's generation is >= U's generation. ###

The server records Q with T as parent.  Q becomes `Client` `Recorded`; it
is `Displayed` or `Superseded` as before.

_linear_: We need to show that T really is Q's parent.

If Q's generation >= U's generation, the ancestry path <U..Q] exists
and contains only `Client` updates: if there were some `Server` update S
in U..Q, it would be `Recorded` (since all `Server` messages are) and more
recent than U, contradicting the definition of U.

Because of in-order message delivery, all updates in <U..Q> have been
received by the server.  So they must be `Recorded` or `Discarded`.

Suppose one, K, was `Discarded`.  Then by _discard_ there is some
`Recorded` `Server` update S which is not an ancestor of Q.  S's
generation must be > Q's tag, since Q's tag is >= all of its `Recorded`
ancestors.  So S's generation > U's.  But this means that S is a
`Server` update more recent than U. _|_

So everything in <U..Q> is `Recorded`.  Only Q is not `Recorded`.
Therefore Q's parent is indeed the most recent `Recorded` update, T.

_forwards_: We aren't making new messages.  _display_: We aren't
changing any of this.  _discard_: we aren't discarding anything.

_record_: Let S be the most recent `Recorded` `Downbound` update S.  This
must be U.  But Q's generation >= U's generation so U can no longer be
`Downbound`.  So there are no `Downbound` updates.  Maybe Q is `Displayed`;
in any case all older `Recorded` updates are `Superseded`.

### II. Q's generation is < U's generation ###

The server discards Q.  It becomes `Discarded`.

_linear_, _forwards_, _display_, _record_: No change.

_discard_: Q becomes K.  Use U as S.  U is `Recorded`.  If Q had U as an
ancestor, U's generation would have been visible to the client when it
generated Q, so Q's tag would have been >= U's generation.  S's
generation is U's generation and is >= Q's by case assumption.


Client message reception
------------------------

Suppose the client receives a message Q.

It displays it.  (The currently `Displayed` update becomes `Superseded`.
Q becomes `Displayed`.)

_linear_, _forwards_, discard_: No change.

_display_: Q is a `Server` update because it was `Downbound`.  We have
just received it so it cannot yet have any `Client` descendant.  It
might have `Server` descendants, but those are also `Recorded` `Downbound`,
by _record_.

_record_: By order of delivery, Q is the most recent `Recorded`
`Downbound`.


Liveness property
-----------------

"When things ahve settled, all is consistent."

If there are no `Upbound` or `Downbound` updates, the most recent `Recorded`
update R is `Displayed`.

Suppose some update D is displayed instead.

D can't be `Recorded`.  If it were, R would have to be `Downbound`, by
_record_.

So D can't be `Server`.  Suppose D is `Client`.  It must be `Recorded`,
`Upbound` or `Discarded`.  Only `Discarded` is left.

If D is `Discarded`, by _discard_, there is some `Recorded` `Server` S which
is not an ancestor of D, with S's generation > D's.  S must be
`Displayed` or `Superseded`, since there are no `Downbound` updates.  If S
were `Displayed` it would be equal to D, hence D would be an ancestor of
S.  So S must be `Superseded`.

If S is `Recorded` `Superseded`, at some point it was `Displayed`.  D is now
`Displayed`.  So D was `Displayed` more recently than S.  D is `Client` so
when it was `Displayed` it became `Upbound`, tagged with a generation of
at least S's; i.e. D's generation >= S's.  _|_


SERVER ACKS
===========

Actually, the client wants to know when conflicts occur, so that it
can report to the user, and interrupt drag operations etc.
To this end:

Update messages from the client to the server also contain a client
sequence number (monotonically but not strictly increasing).  When the
client sends an update, it makes a note of the sequence number.  This
is the "outstanding Client updates sequence number note", or the
"oustanding note".

When the server processes a message from the client and Records it, it
puts the client sequence number in the update stream (as a non-update
message).  (This is skipped if it's the same as the last client
sequence number for that client.)

If the echoed sequence number is equal to the client's oustanding
note, the client knows the server is up to date, and deletes the note.

If the client sees a Server update message, and the client has a note,
it knows that there was a conflict.


LEVEL (Z ORDER)
===============

Each piece has a Z level (an arbitrary-precision value in <0,1>), set
by the client which manipulates the piece, according to the protocol
above.

Each piece *also* has a Z level generation.  This is set by the
server.  The server guarantees to set it to the server generation, and
guarantees to do so as the result of any client Z level update.  

So the client which sends a Z level update can assume that a server
update to the generation will turn up, and with a higher value.

The Z generation is used to disambiguate the Z order for pieces with
identical Z level.  Higher values are closer to the user (ie, occlude
lower values).


ERRORS
======

Most of the time, the client will be able to predict what the server's
response to an update will be.  So usually, a `Client` `Upbound`
update will become `Recorded`, unless there was a conflict.

However, sometimes this is not the case: a piece can have behaviours
which are too complicated to model in the client.  In such cases, the
server will be processing a `Client` `Upbound` update, and find that
it cannot be applied and made `Recorded`.  (Call that an
**impossible** update.)

When the server receives an update it deems _impossible_, it will
generate a `Server` update and process that before the incoming
`Client` update.  The `Server` update will inform the client of the
problem, and, by its existence cause the `Client` _impossible_ update
to become `Discarded`.  When the client receives the `Server` update,
it can describe the problem to its user, and also note that the
`Client` update has become `Superseded`.

Additionally, of course, the client might have bugs (or be malicious).
So there can also be `Client` updates which the client ought to know
are **bogus**.  This includes syntactically malformed updates, but it
could also include updates with semantic errors.

_Bogus_ updates might not fit into the update synchronisation
protocool.  Since they arrive by HTTP, the server can reject them with
an HTTP error code.  A non-malicious client can deal with that by
reporting the problem immediately to its user (although
synchronisation will be lost).

In theory the server might be able to predict precisely what the
client can know, and precisely distinguish between `Client` updates
which are unprocessable because of something only known by the server,
and ones which are unprocessable because of misbehaviour (eg, bugs) at
the client.  However, getting this right is very fiddly and complex.

In the absence of buggy or malicious clients, there will be no _bogus_
updates.  And since even _impossible_ updates are rejected by the
server, there is no harm to anyone else of treating an update as
_impossible_ rather than _bogus_.  So we err on the side of treating
client mistakes as due to synchronisation and incomplete modelling:
ie, we only treat a client update as _bogus_ if it is patently
obviously wrong (for example, it is syntactically invalid).
