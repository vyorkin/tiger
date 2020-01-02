# Frontend to dune.

default: exe

exe:
	dune exe bin/driver.exe -- ../testcases/break_for/test_break_for3.tig

test:
	dune runtest -f

utop:
	dune utop --

clean:
	dune clean
# Optionally, remove all files/folders ignored by git as defined
# in .gitignore (-X).
	git clean -dfXq

.PHONY: default exe test clean utop
