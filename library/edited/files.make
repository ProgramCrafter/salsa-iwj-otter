LIBRARY_FILES += library/edited/chess-board.usvg
library/edited/chess-board.usvg: library/edited/chess-board.svg library/edited/LICENCE library/edited.toml
	$(LIBRARY_PROCESS_SVG)
