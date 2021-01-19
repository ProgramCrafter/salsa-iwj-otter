
Data structures
---------------

* Occultations for a piece
	Vec<Vec<PlayerId>, Occultation>
	where final entry has empty Vec<PlayerId> and is default

	occulter (piece which generates the occultation)
	enum OccultationKind
		Visible,
		Scrambled,
		Displaced { layout area etc. }
		Invisible,
	}

* Visible piece id map for each player

Methods
-------

	scramble piece set
		Vec<PlayerId>		affected players (View)
		see Concepts "Identity scrambled"

	set occultation


Filtering
---------

Updates filtered when generated
