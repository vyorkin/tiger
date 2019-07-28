# Frontend to dune.

default: build

build:
	dune build

test:
	dune runtest -f

utop:
	dune utop --

clean:
	dune clean
# Optionally, remove all files/folders ignored by git as defined
# in .gitignore (-X).
	git clean -dfXq

.PHONY: default build test clean utop
