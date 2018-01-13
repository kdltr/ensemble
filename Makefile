ensemble: *.scm
	(cd chicken-ncurses; chicken-install -s)
	chicken-install -s
	csc ensemble.scm
