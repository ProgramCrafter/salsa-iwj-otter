CONCURRENT UPDATE PROTOCOL AND ANALYSIS
=======================================

(Assumption: our network techniques deliver messages in order in both
directions between a server and a client; failure of the communication
stream is allowed to break things - we treat it as fatal.)

We run the following algorithm / data model separately for each piece.


Model
-----

There is:

 - actual committed history, a list of updates with gen
 - the actual history seen so far by the client
    this can only advance, which is a prefix of the above
 - possibly a branch of history made by the client
    this can rewind and be discarded

There is a diagraph of updates.  Each update specifies the complete
state of the piece.  The server maintains an authoritative history;
the client only maintains the state of the piece.

Updates may be from this client ("Client updates") or from another
client or the server.  We look at the protocol from a single client's
point of view, and call all the other updates "Server updates".

Each update has a single direct ancestor, but this is not recorded in
the message nor by the client.  Updates may be some combination of:

 Recorded - stored by server.
 Downbound - on way from server to client.
 Discarded - received by server and thrown away.
 Upbound - on way from client to server.
 Displayed - seen by client, is most recent state
 Superseded - seen by client, is no longer most recent state.

Server assigns generation number (strictly increasing but maybe with
gaps) to each Recorded update.

Each Upbound update is tagged with the generation of the most recent
Processed update.

(Actually the most recent Processed generation stored by the client is
global across all pieces, and the server's generations are likewise
generated globally although recorded separately for each piece.  So
the update contain s a generation >= the most recently Processed
update for that piece, and < any subseuent Processed upate for that
piece.  The server only compares the incoming update generation for >=
with a recorded value, so this makes no difference.)

Desirable properties
--------------------

Legal combinations are:
  Server + Recorded + Downbound
  Server + Recorded + Displayed
  Server + Recorded + Superseded
  Client + (Displayed / Superseded) + Upbound
  Client + (Displayed / Superseded) + Recorded
  Client + (Displayed / Superseded) + Discarded

_linear_: The Recorded updates form a linear sequence (so it is only
ever appended to).

_forwards_: Each update has at most one Server descendant, and at most
one Client descendant.

_display_: Exactly one update is Displayed at any one time.  It has no
Client child and no Superseded descendants.  Each update is only
Displayed at most once, and can only become Superseded after having
been Displayed.

_discard_: For every Discarded update K there is a Recorded Server
update S which not an ancestor of K; and with S's generation > K's
tag.

Invariants
----------

_record_: Sequence of Recorded updates is as follows.
  Recorded Superseded     zero or more
  Recorded Displayed      zero or one
  Recorded Downbound      zero or more




Update generation operations
----------------------------

I. Server update

Server may invent and transmit an update at any time.  Its ancestor is
the most recent Recorded update.  It is Recorded+Downbound.  No other
updates change their state.

Properties and invariants: _linear_, _forwards_, _record_ are
obvious.  _display_ is not affected.

II. Client update

Client may invent and transmit an update at any time; it must have the
Displayed update as its parent.  That update becomes Superseded.
The new update is Displayed+Upbound.

_linear_ and _record_ are not affected.  _forwardxs_ is honoured
because of _display_.  _display_ is preserved.

Server message reception
------------------------

Suppose the server receives a message, Q.  It was Client Upbound.

Let T be the most recent Recorded message.  The server finds the most
recent Server Recorded message, U.

I. Q's generation is >= U's generation.

The server records Q with T as parent.  Q becomes Client Recorded; it
is Displayed or Superseded as before.

_linear_: We need to show that T really is Q's parent.

If Q's generation >= U's generation, the ancestry path <U..Q] exists
and contains only Client updates: if there were some Server update S
in U..Q, it would be Recorded (since all Server messages are) and more
recent than U, contradicting the definition of U.

Because of in-order message delivery, all updates in <U..Q> have been
received by the server.  So they must be Recorded or Discarded.

Suppose one, K, was Discarded.  Then by _discard_ there is some
Recorded Server update S which is not an ancestor of Q.  S's
generation must be > Q's tag, since Q's tag is >= all of its Recorded
ancestors.  So S's generation > U's.  But this means that S is a
Server update more recent than U. _|_

So everything in <U..Q> is Recorded.  Only Q is not Recorded.
Therefore Q's parent is indeed the most recent Recorded update, T.

_forwards_: We aren't making new messages.  _display_: We aren't
changing any of this.  _discard_: we aren't discarding anything.

_record_: Let S be the most recent Recorded Downbound update S.  This
must be U.  But Q's generation >= U's generation so U can no longer be
Downbound.  So there are no Downbound updates.  Maybe Q is Displayed;
in any case all older Recorded updates are Superseded.

II. Q's generation is < U's generation

The server discards Q.  It becomes Discarded.

_linear_, _forwards_, _display_, _record_: No change.

_discard_: Q becomes K.  Use U as S.  U is Recorded.  If Q had U as an
ancestor, U's generation would have been visible to the client when it
generated Q, so Q's tag would have been >= U's generation.  S's
generation is U's generation and is >= Q's by case assumption.


Client message reception
------------------------

Suppose the client receives a message Q.

It displays it.  (The currently Displayed update becomes Superseded.
Q becomes Displayed.)

_linear_, _forwards_, discard_: No change.

_display_: Q is a Server message because it was Downbound.  We have
just received it so it cannot yet have any Client descendant.  It
might have Server descendants, but those are also Recorded Downbound,
by _record_.

_record_: By order of delivery, Q is the most recent Recorded
Downbound.


Liveness property
-----------------

"When things ahve settled, all is consistent."

If there are no Upbound or Downbound updates, the most recent Recorded
update R is Displayed.

Suppose some update D is displayed instead.

D can't be Recorded.  If it were, R would have to be Downbound, by
_record_.

So D can't be Server.  Suppose D is Client.  It must be Recorded,
Upbound or Discarded.  Only Discarded is left.

If D is Discarded, by _discard_, there is some Recorded Server S which
is not an ancestor of D, with S's generation > D's.  S must be
Displayed or Superseded, since there are no Downbound updates.  If S
were Displayed it would be equal to D, hence D would be an ancestor of
S.  So S must be Superseded.

If S is Recorded Superseded, at some point it was Displayed.  D is now
Displayed.  So D was Displayed more recently than S.  D is Client so
when it was Displayed it became Upbound, tagged with a generation of
at least S's; i.e. D's generation >= S's.  _|_
