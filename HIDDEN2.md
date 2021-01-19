
Concepts
--------

Views

	When a Thing is happening, the happener divides players into
	equiv classes (usu, (i) our player (ii) other players)

	View specifies some Occultations

	Each Occultation is applied equally to all players within a
	View so that they all see the same

Occulations
	Some of these these occur at a point in time and then
		wait to become relevant.
	Some have a continuous effect.

	Only one occultation for each piece for each player at a time

Identity scrambled
	Instantaneous effect:
		- Choose a permutation of affected pieces
		- Permute the visible/real ID mapping
		- If the players in this view can see the real locations,
		  permute the locations too (actually moving the pieces)
	Typ., redone when new pieces are added to the affected set

Location hidden
	Occultor provides layout area
	Pieces laid out in order according to err something ?
	Occultees see these synthetic locations

	When an occultee moves, that becomes actual location,
		now piece is no longer occulted
		Z level needs special handling - overwrite
			original Z with something
	When a non-occulteee moves, that is still actual location
		but it isn't visible

Totally invisible
	The piece disappears from view.  Message explaining why
	"Alice puts a black token into Alice's bag"

Other
-----

Default hidden face
	When something was occulted it is "face down", mostly,
		until it is put somewhere with a faceupness field

When happens
	When piece is released, occultation may change or be effected
		or something


pinning/enablement etc.
	Need way to introduce these per-player pieces
	Also need way to de-activate them


Future
------

	SVG ondemand or hashes or something to avoid
		(i) retransmissions of lots of data
		(ii) leaks by data sizes
		(iii) huge a-* files
