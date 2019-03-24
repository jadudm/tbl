all:
	-raco pkg remove tbl
	raco pkg install
	-raco setup --pkgs