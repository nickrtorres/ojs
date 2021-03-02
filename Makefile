.SILENT: check
.POSIX:

all:
	dune build

check:
	cd acceptance && ./check.sh
