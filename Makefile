ensemble: *.scm
	(cd chicken-ncurses; chicken-install -s)
	chicken-install
	csc ensemble.scm
