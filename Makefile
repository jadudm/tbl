clean:
	-for i in `find . -name compiled` ; do echo $$i ; rm -rf $$i ; done
all:
	-raco pkg remove tbl
	raco pkg install
	-raco setup --pkgs
